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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Internal.Types (Verifier (..))
import Test.MockCat.Internal.Message (mockNameLabel)
import Test.MockCat.SharedSpecDefs

verifyResolvedAny :: Verify.ResolvedMock params -> IO ()
verifyResolvedAny (Verify.ResolvedMock name (Verifier ref)) = do
  appliedParamsList <- Verify.readAppliedParamsList ref
  when (null appliedParamsList) $
    error $ "It has never been applied function" <> mockNameLabel name

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

instance MonadState s m => MonadState s (MockT m) where
  get = lift get
  put = lift . put
  state f = lift (state f)

_ask ::
  ( Typeable env
  , Verify.ResolvableParamsOf env ~ ()
  , MonadIO m
  ) =>
  env -> MockT m ()
_ask p = MockT $ do
  mockInstance <- liftIO $ createNamedConstantMockFn "ask" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "ask") mockInstance verifyStub)

_readFile ::
  ( MockBuilder params (FilePath -> Text) (Param FilePath)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "readFile" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance verifyStub)

_writeFile ::
  ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "writeFile" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance verifyStub)

_post ::
  ( MockBuilder params (Text -> ()) (Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
_post p = MockT $ do
  mockFn <- liftIO $ createNamedMockFn "post" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockFn
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "post") mockFn verifyStub)

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
  MockT m ()
_getBy p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_getBy" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_getBy") mockInstance verifyStub)

_echo ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_echo p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_echo" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_echo") mockInstance verifyStub)

_fn2_1Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fn2_1Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fn2_1Sub" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fn2_1Sub") mockInstance verifyStub)

_fn2_2Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fn2_2Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fn2_2Sub" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fn2_2Sub") mockInstance verifyStub)

_fn3_1Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fn3_1Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fn3_1Sub" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fn3_1Sub") mockInstance verifyStub)

_fn3_2Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fn3_2Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fn3_2Sub" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fn3_2Sub") mockInstance verifyStub)

_fn3_3Sub ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fn3_3Sub p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fn3_3Sub" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fn3_3Sub") mockInstance verifyStub)

_getValueBy ::
  ( MockBuilder params (String -> m String) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m String) ~ Param String
  ) =>
  params ->
  MockT m ()
_getValueBy p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_getValueBy" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_getValueBy") mockInstance verifyStub)

_fnState ::
  ( MockBuilder params (Maybe s -> m s) (Param (Maybe s))
  , MonadIO m
  , Typeable m
  , Typeable s
  , Verify.ResolvableParamsOf (Maybe s -> m s) ~ Param (Maybe s)
  ) =>
  params ->
  MockT m ()
_fnState p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fnState" p
  let verifyStub _ = pure ()
  addDefinition (Definition (Proxy :: Proxy "_fnState") mockInstance verifyStub)

_fnState2 ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_fnState2 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fnState2" p
  let verifyStub _ = pure ()
  addDefinition (Definition (Proxy :: Proxy "_fnState2") mockInstance verifyStub)

_fnParam3_1 ::
  ( MockBuilder params (Int -> Bool -> m String) (Param Int :> Param Bool)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (Int -> Bool -> m String) ~ (Param Int :> Param Bool)
  ) =>
  params ->
  MockT m ()
_fnParam3_1 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fnParam3_1" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_1") mockInstance verifyStub)

_fnParam3_2 ::
  ( MockBuilder params (m Int) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m Int) ~ ()
  ) =>
  params ->
  MockT m ()
_fnParam3_2 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fnParam3_2" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_2") mockInstance verifyStub)

_fnParam3_3 ::
  ( MockBuilder params (m Bool) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m Bool) ~ ()
  ) =>
  params ->
  MockT m ()
_fnParam3_3 p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_fnParam3_3" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_fnParam3_3") mockInstance verifyStub)

_getByExplicit ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m ()
_getByExplicit p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_getByExplicit" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_getByExplicit") mockInstance verifyStub)

_echoExplicit ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_echoExplicit p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_echoExplicit" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_echoExplicit") mockInstance verifyStub)

_defaultAction ::
  ( MonadIO m
  , Typeable a
  , Verify.ResolvableParamsOf a ~ ()
  ) =>
  a ->
  MockT m ()
_defaultAction value = MockT $ do
  mockInstance <- liftIO $ createNamedConstantMockFn "_defaultAction" value
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_defaultAction") mockInstance verifyStub)

_produce ::
  ( MonadIO m
  , Typeable a
  , Verify.ResolvableParamsOf a ~ ()
  ) =>
  a ->
  MockT m ()
_produce value = MockT $ do
  mockInstance <- liftIO $ createNamedConstantMockFn "_produce" value
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_produce") mockInstance verifyStub)

stubPostFn ::
  ( MockBuilder params (Text -> ()) (Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
stubPostFn p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "stubPostFn" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "post") mockInstance verifyStub)

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
  MockT m ()
_readTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_readTTY" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_readTTY") mockInstance verifyStub)

_writeTTY ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_writeTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFn "_writeTTY" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_writeTTY") mockInstance verifyStub)

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
    modifyContentStub <- createMockFn $ pack "content" |> pack "modifiedContent"

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