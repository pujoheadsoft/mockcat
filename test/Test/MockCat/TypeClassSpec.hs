{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Test.MockCat.TypeClassSpec (spec) where

import Control.Exception (ErrorCall(..), displayException)
import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe, shouldThrow, describe, Selector, pendingWith)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find, isInfixOf)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.SharedSpecDefs
import Test.MockCat.TypeClassCommonSpec (Environment, specEcho, specFileOperation, specFileOperationApi, specFileOperationReaderEnvironment, specApiRenaming, specTestClass, specMultiApply, specSubVars, specMonadState, specExplicitReturn, specDefaultMethod, specAssocType, specMonadAsync, specMonadReaderEnvironment, specVerifyFailureFileOp, specVerifyFailureApi, specVerifyFailureReaderEnvironment, specVerifyFailureTestClass, specVerifyFailureSubVars, specVerifyFailureMultiApply, specVerifyFailureParam3, specVerifyFailureExplicit, specVerifyFailureDefaultAndAssoc, specVerifyFailureTTY)
import Test.MockCat.Internal.Types (BuiltMock(..))
import qualified Test.MockCat.Internal.MockRegistry as Registry (register)

missingCall :: String -> Selector ErrorCall
missingCall name err =
  let needle = "function `" <> name <> "` was not applied the expected number of times."
   in needle `isInfixOf` displayException err

ensureVerifiable ::
  ( MonadIO m
  , Verify.ResolvableMock target
  ) =>
  target ->
  m ()
ensureVerifiable target =
  liftIO $ do
    m <- Verify.resolveForVerification target
    case m of { Just _ -> pure (); Nothing -> Verify.verificationFailure }

apiFileOperationProgram ::
  (MonadReader String m, FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
apiFileOperationProgram inputPath outputPath modifyText = do
  e <- ask
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post $ modifiedContent <> pack ("+" <> e)

instance MonadIO m => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `readFile`.") $ findParam (Proxy :: Proxy "readFile") defs
      !result = mockFn path
    pure result

  writeFile path content = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `writeFile`.") $ findParam (Proxy :: Proxy "writeFile") defs
      !result = mockFn path content
    pure result

instance MonadIO m => ApiOperation (MockT m) where
  post content = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `post`.") $ findParam (Proxy :: Proxy "post") defs
      !result = mockFn content
    pure result

instance MonadIO m => MonadReader String (MockT m) where
  ask = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `ask`.") $ findParam (Proxy :: Proxy "ask") defs
      !result = mock
    pure result
  local = undefined

-- instance MonadState s m => MonadState s (MockT m) where
--   get = lift get
--   put = lift . put
--   state f = lift (state f)

_ask ::
  ( Verify.ResolvableParamsOf env ~ ()
  , MonadIO m
  , Typeable env
  ) =>
  env ->
  MockT m env
_ask p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "ask" (Head :> param p)
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "ask") mockInstance NoVerification)
  pure mockInstance

_readFile ::
  ( MockBuilder params (FilePath -> Text) (Param FilePath)
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text)
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "readFile" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance NoVerification)
  pure mockInstance

_writeFile ::
  ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text -> ())
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "writeFile" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance NoVerification)
  pure mockInstance

_post ::
  ( MockBuilder params (Text -> ()) (Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m (Text -> ())
_post p = MockT $ do
  mockFn <- liftIO $ createNamedMockFnWithParams "post" p
  ensureVerifiable mockFn
  addDefinition (Definition (Proxy :: Proxy "post") mockFn NoVerification)
  pure mockFn

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ f _) -> unsafeCoerce f) definition

echoProgram :: MonadIO m => TestClass m => String -> m ()
echoProgram s = do
  v <- getBy s
  liftIO $ print v
  echo $ show v

instance AssocTypeTest IO where
  type ResultType IO = Int
  produce = pure 0

instance MonadIO m => TestClass (MockT m) where
  getBy a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_getBy`.") $ findParam (Proxy :: Proxy "_getBy") defs
      !result = mockFn a
    lift result

  echo a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_echo`.") $ findParam (Proxy :: Proxy "_echo") defs
      !result = mockFn a
    lift result

instance
  ( Eq s
  , Show s
  , MonadState s m
  , MonadIO m
  ) =>
  MonadStateSub s (MockT m)
  where
  fnState maybeValue = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fnState`.") $ findParam (Proxy :: Proxy "_fnState") defs
      !result = mockFn maybeValue
    lift result

instance
  ( MonadState String m
  , MonadIO m
  ) =>
  MonadStateSub2 s (MockT m)
  where
  fnState2 label = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fnState2`.") $ findParam (Proxy :: Proxy "_fnState2") defs
      !result = mockFn label
    lift result

instance Monad m => MonadVar2_1 (MockT m) a
instance MonadIO m => MonadVar2_1Sub (MockT m) a where
  fn2_1Sub tag = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fn2_1Sub`.") $ findParam (Proxy :: Proxy "_fn2_1Sub") defs
      !result = mockFn tag
    lift result

instance MonadIO m => MonadVar3_1 (MockT m) a b
instance MonadIO m => MonadVar3_1Sub (MockT m) a b where
  fn3_1Sub tag = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fn3_1Sub`.") $ findParam (Proxy :: Proxy "_fn3_1Sub") defs
      !result = mockFn tag
    lift result

instance MonadIO m => MonadVar3_2 a (MockT m) b
instance MonadIO m => MonadVar3_2Sub a (MockT m) b where
  fn3_2Sub tag = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fn3_2Sub`.") $ findParam (Proxy :: Proxy "_fn3_2Sub") defs
      !result = mockFn tag
    lift result

instance Monad m => MonadVar2_2 a (MockT m)
instance MonadIO m => MonadVar2_2Sub a (MockT m) where
  fn2_2Sub tag = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fn2_2Sub`.") $ findParam (Proxy :: Proxy "_fn2_2Sub") defs
      !result = mockFn tag
    lift result

instance MonadIO m => MonadVar3_3 a b (MockT m)
instance MonadIO m => MonadVar3_3Sub a b (MockT m) where
  fn3_3Sub tag = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fn3_3Sub`.") $ findParam (Proxy :: Proxy "_fn3_3Sub") defs
      !result = mockFn tag
    lift result

instance MonadIO m => MultiApplyTest (MockT m) where
  getValueBy key = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_getValueBy`.") $ findParam (Proxy :: Proxy "_getValueBy") defs
      !result = mockFn key
    lift result

instance MonadIO m => ExplicitlyReturnMonadicValuesTest (MockT m) where
  getByExplicit label = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_getByExplicit`.") $ findParam (Proxy :: Proxy "_getByExplicit") defs
      !result = mockFn label
    lift result
  echoExplicit label = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_echoExplicit`.") $ findParam (Proxy :: Proxy "_echoExplicit") defs
      !result = mockFn label
    lift result

instance MonadIO m => DefaultMethodTest (MockT m) where
  defaultAction = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_defaultAction`.") $ findParam (Proxy :: Proxy "_defaultAction") defs
      !result = mockFn
    pure result

instance MonadIO m => AssocTypeTest (MockT m) where
  type ResultType (MockT m) = ResultType m
  produce = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_produce`.") $ findParam (Proxy :: Proxy "_produce") defs
      !result = mockFn
    pure result

instance MonadIO m => ParamThreeMonad Int Bool (MockT m) where
  fnParam3_1 a b = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fnParam3_1`.") $ findParam (Proxy :: Proxy "_fnParam3_1") defs
      !result = mockFn a b
    lift result
  fnParam3_2 = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fnParam3_2`.") $ findParam (Proxy :: Proxy "_fnParam3_2") defs
      !result = mockFn
    lift result
  fnParam3_3 = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_fnParam3_3`.") $ findParam (Proxy :: Proxy "_fnParam3_3") defs
      !result = mockFn
    lift result

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

_getBy ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m (String -> m Int)
_getBy p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_getBy" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_getBy") mockInstance NoVerification)
  pure mockInstance

_echo ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_echo p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_echo" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_echo") mockInstance NoVerification)
  pure mockInstance

_fn2_1Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fn2_1Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn2_1Sub" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fn2_1Sub") mockInstance NoVerification)
  pure mockInstance

_fn2_2Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fn2_2Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn2_2Sub" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fn2_2Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_1Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fn3_1Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_1Sub" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fn3_1Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_2Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fn3_2Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_2Sub" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fn3_2Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_3Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fn3_3Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_3Sub" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fn3_3Sub") mockInstance NoVerification)
  pure mockInstance

_getValueBy ::
  ( MockBuilder params (String -> m String) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m String) ~ Param String
  ) =>
  params ->
  MockT m (String -> m String)
_getValueBy p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_getValueBy" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_getValueBy") mockInstance NoVerification)
  pure mockInstance

_fnState ::
  ( MockBuilder params (Maybe s -> m s) (Param (Maybe s))
  , MonadIO m
  , Typeable m
  , Typeable s
  , Verify.ResolvableParamsOf (Maybe s -> m s) ~ Param (Maybe s)
  ) =>
  params ->
  MockT m (Maybe s -> m s)
_fnState p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fnState" p
  addDefinition (Definition (Proxy :: Proxy "_fnState") mockInstance NoVerification)
  pure mockInstance

_fnState2 ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_fnState2 p = MockT $ do
  BuiltMock { builtMockFn = mockInstance, builtMockRecorder = verifier } <- liftIO $ buildMock (Just "_fnState2") p
  registeredFn <- liftIO $ Registry.register (Just "_fnState2") verifier mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fnState2") registeredFn NoVerification)
  pure mockInstance

_fnParam3_1 ::
  ( MockBuilder params (Int -> Bool -> m String) (Param Int :> Param Bool)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (Int -> Bool -> m String) ~ (Param Int :> Param Bool)
  ) =>
  params ->
  MockT m (Int -> Bool -> m String)
_fnParam3_1 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fnParam3_1" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_1") mockInstance NoVerification)
  pure mockInstance

_fnParam3_2 ::
  ( MockBuilder params (m Int) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m Int) ~ ()
  ) =>
  params ->
  MockT m (m Int)
_fnParam3_2 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fnParam3_2" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_2") mockInstance NoVerification)
  pure mockInstance

_fnParam3_3 ::
  ( MockBuilder params (m Bool) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m Bool) ~ ()
  ) =>
  params ->
  MockT m (m Bool)
_fnParam3_3 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fnParam3_3" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_3") mockInstance NoVerification)
  pure mockInstance

_getByExplicit ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m (String -> m Int)
_getByExplicit p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_getByExplicit" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_getByExplicit") mockInstance NoVerification)
  pure mockInstance

_echoExplicit ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_echoExplicit p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_echoExplicit" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_echoExplicit") mockInstance NoVerification)
  pure mockInstance

_defaultAction ::
  ( MonadIO m
  , Verify.ResolvableParamsOf a ~ ()
  , Typeable a
  ) =>
  a ->
  MockT m a
_defaultAction value = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_defaultAction" (Head :> param value)
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_defaultAction") mockInstance NoVerification)
  pure mockInstance

_produce ::
  ( MonadIO m
  , Verify.ResolvableParamsOf a ~ ()
  , Typeable a
  ) =>
  a ->
  MockT m a
_produce value = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_produce" (Head :> param value)
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_produce") mockInstance NoVerification)
  pure mockInstance

stubPostFn ::
  ( MockBuilder params (Text -> ()) (Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
stubPostFn p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "stubPostFn" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "post") mockInstance NoVerification)

processFiles :: (MonadAsync m, FileOperation m) => [FilePath] -> m [Text]
processFiles = mapConcurrently readFile


echo2 :: Teletype m => m ()
echo2 = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo2

instance MonadIO m => Teletype (MockT m) where
  readTTY = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_readTTY`.") $ findParam (Proxy :: Proxy "_readTTY") defs
      !result = mockFn
    lift result

  writeTTY a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_writeTTY`.") $ findParam (Proxy :: Proxy "_writeTTY") defs
      !result = mockFn a
    lift result

_readTTY ::
  ( MockBuilder params (m String) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m String) ~ ()
  ) =>
  params ->
  MockT m (m String)
_readTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_readTTY" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_readTTY") mockInstance NoVerification)
  pure mockInstance

_writeTTY ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_writeTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_writeTTY" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "_writeTTY") mockInstance NoVerification)
  pure mockInstance
 
-- Concrete specializations to `MockT IO` for common spec helpers
_askIO :: Environment -> MockT IO Environment
_askIO = _ask

_readFileIO ::
  forall params.
  (MockBuilder params (FilePath -> Text) (Param FilePath)) =>
  params -> MockT IO (FilePath -> Text)
_readFileIO = _readFile

_writeFileIO ::
  forall params.
  (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) =>
  params -> MockT IO (FilePath -> Text -> ())
_writeFileIO = _writeFile

_postIO ::
  forall params.
  (MockBuilder params (Text -> ()) (Param Text)) =>
  params -> MockT IO (Text -> ())
_postIO = _post

_getByIO ::
  forall params.
  (MockBuilder params (String -> IO Int) (Param String)) =>
  params -> MockT IO (String -> IO Int)
_getByIO = _getBy

_echoIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_echoIO = _echo

_getValueByIO ::
  forall params.
  (MockBuilder params (String -> IO String) (Param String)) =>
  params -> MockT IO (String -> IO String)
_getValueByIO = _getValueBy

_fn2_1SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn2_1SubIO = _fn2_1Sub

_fn2_2SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn2_2SubIO = _fn2_2Sub

_fn3_1SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_1SubIO = _fn3_1Sub

_fn3_2SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_2SubIO = _fn3_2Sub

_fn3_3SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_3SubIO = _fn3_3Sub

_getByExplicitIO ::
  forall params.
  (MockBuilder params (String -> IO Int) (Param String)) =>
  params -> MockT IO (String -> IO Int)
_getByExplicitIO = _getByExplicit

_echoExplicitIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_echoExplicitIO = _echoExplicit

_defaultActionIO :: Int -> MockT IO Int
_defaultActionIO = _defaultAction

_produceIO :: Int -> MockT IO Int
_produceIO = _produce

_readTTYIO ::
  forall params.
  (MockBuilder params (IO String) ()) =>
  params -> MockT IO (IO String)
_readTTYIO = _readTTY

_writeTTYIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_writeTTYIO = _writeTTY

spec :: Spec
spec = do
  it "echo" do
    result <- runMockT do
      _readTTY $ casesIO [
        "a",
        ""
        ]
      _writeTTY $ "a" |> pure @IO ()
      echo2
    result `shouldBe` ()

  it "Read, edit, and output files" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _ask "environment"
      _readFile ("input.txt" |> pack "content")
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()
      _post $ pack "modifiedContent+environment" |> ()
      apiFileOperationProgram "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

  it "return monadic value test" do
    result <- runMockT do
      _getBy $ "s" |> pure @IO (10 :: Int)
      _echo $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()

  it "multi apply collects all results" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" |> pure @IO "ax"
        onCase $ "b" |> pure @IO "bx"
        onCase $ "c" |> pure @IO "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

  it "supports MonadVar2_1Sub pattern" do
    result <- runMockT do
      _fn2_1Sub $ "alpha" |> pure @IO ()
      fn2_1Sub "alpha"
    result `shouldBe` ()

  it "supports MonadVar2_2Sub pattern" do
    result <- runMockT do
      _fn2_2Sub $ "beta" |> pure @IO ()
      fn2_2Sub "beta"
    result `shouldBe` ()

  it "supports MonadVar3_1Sub pattern" do
    result <- runMockT do
      _fn3_1Sub $ "gamma" |> pure @IO ()
      fn3_1Sub "gamma"
    result `shouldBe` ()

  it "supports MonadVar3_2Sub pattern" do
    result <- runMockT do
      _fn3_2Sub $ "delta" |> pure @IO ()
      fn3_2Sub "delta"
    result `shouldBe` ()

  it "supports MonadVar3_3Sub pattern" do
    result <- runMockT do
      _fn3_3Sub $ "epsilon" |> pure @IO ()
      fn3_3Sub "epsilon"
    result `shouldBe` ()

  it "supports MonadStateSub pattern" do
    let action = runMockT $ do
          _fnState $ do
            onCase $ Just "current" |> pure @(StateT String IO) "next"
            onCase $ Nothing |> pure @(StateT String IO) "default"
          fnState (Just "current")
    result <- evalStateT action "seed"
    result `shouldBe` "next"

  it "supports MonadStateSub2 pattern" do
    let action = runMockT $ do
          _fnState2 $ "label" |> pure @(StateT String IO) ()
          fnState2 @String "label"
    result <- evalStateT action "initial"
    result `shouldBe` ()

  it "supports ParamThreeMonad functional dependencies" do
    result <- runMockT $ do
      _fnParam3_1 $ do
        onCase $ (1 :: Int) |> True |> pure @IO "combined"
      _fnParam3_2 $ casesIO [1 :: Int]
      _fnParam3_3 $ casesIO [True]
      r1 <- fnParam3_1 1 True
      r2 <- fnParam3_2
      r3 <- fnParam3_3
      pure (r1, r2, r3)
    result `shouldBe` ("combined", 1, True)

  it "supports ExplicitlyReturnMonadicValuesTest pattern" do
    result <- runMockT $ do
      _getByExplicit $ "key" |> pure @IO (42 :: Int)
      _echoExplicit $ "value" |> pure @IO ()
      v <- getByExplicit "key"
      echoExplicit "value"
      pure v
    result `shouldBe` 42

  it "supports DefaultMethodTest pattern" do
    result <- runMockT $ do
      _defaultAction (99 :: Int)
      defaultAction
    result `shouldBe` 99

  it "supports AssocTypeTest pattern" do
    value <- runMockT $ do
      _produce (321 :: Int)
      produce
    value `shouldBe` 321

  it "supports makeMockWithOptions prefix and suffix" do
    result <- runMockT $ do
      stubPostFn (pack "payload" |> ())
      post (pack "payload")
    result `shouldBe` ()

  it "supports MonadAsync pattern" do
    result <- runMockT $ do
      _readFile $ do
        onCase $ "file1.txt" |> pack "content1"
        onCase $ "file2.txt" |> pack "content2"
      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]

  describe "verification failures" do
    it "fails when _readFile is defined but readFile is never called" do
      (runMockT @IO do
        _ <- _readFile ("input.txt" |> pack "content")
          `expects` do
            called once
        -- readFile is never called
        pure ()) `shouldThrow` (missingCall "readFile")

    it "fails when _writeFile is defined but writeFile is never called" do
      (runMockT @IO do
        _ <- _writeFile ("output.txt" |> pack "content" |> ())
          `expects` do
            called once
        -- writeFile is never called
        pure ()) `shouldThrow` (missingCall "writeFile")

    it "fails when _ask is defined but ask is never called" do
      (runMockT @IO do
        _ <- _ask "environment"
          `expects` do
            called once
        -- ask is never called
        pure ()) `shouldThrow` (missingCall "ask")

    it "fails when _post is defined but post is never called" do
      (runMockT @IO do
        _ <- _post (pack "content" |> ())
          `expects` do
            called once
        -- post is never called
        pure ()) `shouldThrow` (missingCall "post")

    it "fails when _readFile is defined but only writeFile is called" do
      (runMockT @IO do
        _ <- _readFile ("input.txt" |> pack "content")
          `expects` do
            called once
        _writeFile ("output.txt" |> pack "content" |> ())
        -- readFile is never called, only writeFile is called
        do
          writeFile "output.txt" (pack "content")
          pure ()) `shouldThrow` (missingCall "readFile")

    it "fails when _writeFile is defined but only readFile is called" do
      (runMockT @IO do
        _readFile ("input.txt" |> pack "content")
        _ <- _writeFile ("output.txt" |> pack "content" |> ())
          `expects` do
            called once
        -- writeFile is never called, only readFile is called
        do
          readFile "input.txt") `shouldThrow` (missingCall "writeFile")

    it "fails when _fnState is defined but fnState is never called" do
      pendingWith "RegisterStub-based mocks require custom expectation handling"

    it "fails when _fnState2 is defined but fnState2 is never called" do
      pendingWith "RegisterStub-based mocks require custom expectation handling"

    it "fails when _getBy is defined but getBy is never called" do
      (runMockT @IO do
        _ <- _getBy ("s" |> pure @IO (10 :: Int))
          `expects` do
            called once
        -- getBy is never called
        pure ()) `shouldThrow` (missingCall "_getBy")

    it "fails when _echo is defined but echo is never called" do
      (runMockT @IO do
        _ <- _echo ("10" |> pure @IO ())
          `expects` do
            called once
        -- echo is never called
        pure ()) `shouldThrow` (missingCall "_echo")

    it "fails when _fn2_1Sub is defined but fn2_1Sub is never called" do
      (runMockT @IO do
        _ <- _fn2_1Sub ("alpha" |> pure @IO ())
          `expects` do
            called once
        -- fn2_1Sub is never called
        pure ()) `shouldThrow` (missingCall "_fn2_1Sub")

    it "fails when _fn2_2Sub is defined but fn2_2Sub is never called" do
      (runMockT @IO do
        _ <- _fn2_2Sub ("beta" |> pure @IO ())
          `expects` do
            called once
        -- fn2_2Sub is never called
        pure ()) `shouldThrow` (missingCall "_fn2_2Sub")

    it "fails when _fn3_1Sub is defined but fn3_1Sub is never called" do
      (runMockT @IO do
        _ <- _fn3_1Sub ("gamma" |> pure @IO ())
          `expects` do
            called once
        -- fn3_1Sub is never called
        pure ()) `shouldThrow` (missingCall "_fn3_1Sub")

    it "fails when _fn3_2Sub is defined but fn3_2Sub is never called" do
      (runMockT @IO do
        _ <- _fn3_2Sub ("delta" |> pure @IO ())
          `expects` do
            called once
        -- fn3_2Sub is never called
        pure ()) `shouldThrow` (missingCall "_fn3_2Sub")

    it "fails when _fn3_3Sub is defined but fn3_3Sub is never called" do
      (runMockT @IO do
        _ <- _fn3_3Sub ("epsilon" |> pure @IO ())
          `expects` do
            called once
        -- fn3_3Sub is never called
        pure ()) `shouldThrow` (missingCall "_fn3_3Sub")

    it "fails when _getValueBy is defined but getValueBy is never called" do
      (runMockT @IO do
        _ <- _getValueBy ("a" |> pure @IO "ax")
          `expects` do
            called once
        -- getValueBy is never called
        pure ()) `shouldThrow` (missingCall "_getValueBy")

    it "fails when _fnParam3_1 is defined but fnParam3_1 is never called" do
      (runMockT @IO do
        _ <- _fnParam3_1 (do
               onCase $ (1 :: Int) |> True |> pure @IO "combined")
          `expects` do
            called once
        -- fnParam3_1 is never called
        pure ()) `shouldThrow` (missingCall "_fnParam3_1")

    it "fails when _fnParam3_2 is defined but fnParam3_2 is never called" do
      (runMockT @IO do
        _ <- _fnParam3_2 (casesIO [1 :: Int])
          `expects` do
            called once
        -- fnParam3_2 is never called
        pure ()) `shouldThrow` (missingCall "_fnParam3_2")

    it "fails when _fnParam3_3 is defined but fnParam3_3 is never called" do
      (runMockT @IO do
        _ <- _fnParam3_3 (casesIO [True])
          `expects` do
            called once
        -- fnParam3_3 is never called
        pure ()) `shouldThrow` (missingCall "_fnParam3_3")

    it "fails when _getByExplicit is defined but getByExplicit is never called" do
      (runMockT @IO do
        _ <- _getByExplicit ("key" |> pure @IO (42 :: Int))
          `expects` do
            called once
        -- getByExplicit is never called
        pure ()) `shouldThrow` (missingCall "_getByExplicit")

    it "fails when _echoExplicit is defined but echoExplicit is never called" do
      (runMockT @IO do
        _ <- _echoExplicit ("value" |> pure @IO ())
          `expects` do
            called once
        -- echoExplicit is never called
        pure ()) `shouldThrow` (missingCall "_echoExplicit")

    it "fails when _defaultAction is defined but defaultAction is never called" do
      (runMockT @IO do
        _ <- _defaultAction (99 :: Int)
          `expects` do
            called once
        -- defaultAction is never called
        pure ()) `shouldThrow` (missingCall "_defaultAction")

    it "fails when _produce is defined but produce is never called" do
      (runMockT @IO do
        _ <- _produce (321 :: Int)
          `expects` do
            called once
        -- produce is never called
        pure ()) `shouldThrow` (missingCall "_produce")

    it "fails when _readTTY is defined but readTTY is never called" do
      (runMockT @IO do
        _ <- _readTTY (casesIO ["a", ""])
          `expects` do
            called once
        -- readTTY is never called
        pure ()) `shouldThrow` (missingCall "_readTTY")

    it "fails when _writeTTY is defined but writeTTY is never called" do
      (runMockT @IO do
        _ <- _writeTTY ("a" |> pure @IO ())
          `expects` do
            called once
        -- writeTTY is never called
        pure ()) `shouldThrow` (missingCall "_writeTTY")

  specEcho _readTTY _writeTTY
  specFileOperationApi _readFile _writeFile _post
  specApiRenaming _post
  specExplicitReturn _getByExplicit _echoExplicit
  specDefaultMethod _defaultAction
  specAssocType _produce
  specMonadAsync _readFile
  specMonadState _fnState _fnState2

  -- -- Verification Failures
  specVerifyFailureFileOp _readFile _writeFile
  specVerifyFailureApi _post
  specVerifyFailureReaderEnvironment _ask
  specVerifyFailureTestClass _getBy _echo
  specVerifyFailureSubVars _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub
  specVerifyFailureMultiApply _getValueBy
  -- specVerifyFailureParam3 _fnParam3_1 _fnParam3_2 _fnParam3_3
  specVerifyFailureExplicit _getByExplicit _echoExplicit
  specVerifyFailureDefaultAndAssoc _defaultAction _produce
  specVerifyFailureTTY _readTTY _writeTTY

  -- describe "verification failures (State - Pending)" do
  --   it "fails when _fnState is defined but fnState is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

  --   it "fails when _fnState2 is defined but fnState2 is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"