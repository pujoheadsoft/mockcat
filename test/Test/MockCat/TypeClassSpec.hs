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
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text)
import Test.Hspec (Spec)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (MonadState (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.TypeClassCommonSpec as SpecCommon
import Test.MockCat.Internal.Types (BuiltMock(..))
import qualified Test.MockCat.Internal.MockRegistry as Registry (register)

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

instance MonadIO m => MonadReader SpecCommon.Environment (MockT m) where
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

spec :: Spec
spec = do
  -- build SpecDeps and call aggregated spec entrypoint
  let deps = SpecCommon.SpecDeps
        { SpecCommon.basicDeps          = SpecCommon.BasicDeps _readFile _writeFile
        , SpecCommon.mixedDeps          = SpecCommon.MixedDeps _readFile _writeFile _post
        , SpecCommon.multipleDeps       = SpecCommon.MultipleDeps _ask _readFile _writeFile _post
        , SpecCommon.customNamingDeps   = SpecCommon.CustomNamingDeps _post
        , SpecCommon.readerContextDeps  = SpecCommon.ReaderContextDeps _ask _readFile _writeFile
        , SpecCommon.sequentialIODeps            = SpecCommon.SequentialIODeps _readTTY _writeTTY
        , SpecCommon.ttyDeps                      = SpecCommon.TtyDeps _readTTY _writeTTY
        , SpecCommon.implicitMonadicReturnDeps    = SpecCommon.ImplicitMonadicReturnDeps _getBy _echo
        , SpecCommon.testClassDeps                = SpecCommon.TestClassDeps _getBy _echo
        , SpecCommon.argumentPatternMatchingDeps  = SpecCommon.ArgumentPatternMatchingDeps _getValueBy
        , SpecCommon.multiApplyDeps               = SpecCommon.MultiApplyDeps _getValueBy
        , SpecCommon.monadStateTransformerDeps    = SpecCommon.MonadStateTransformerDeps _fnState _fnState2
        , SpecCommon.stateDeps                    = SpecCommon.StateDeps _fnState _fnState2
        , SpecCommon.multiParamTypeClassArityDeps = SpecCommon.MultiParamTypeClassArityDeps _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub
        , SpecCommon.multiParamDeps               = SpecCommon.MultiParamDeps _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub
        , SpecCommon.functionalDependenciesDeps   = SpecCommon.FunctionalDependenciesDeps _fnParam3_1 _fnParam3_2 _fnParam3_3
        , SpecCommon.funDeps                      = SpecCommon.FunDeps _fnParam3_1 _fnParam3_2 _fnParam3_3
        , SpecCommon.explicitMonadicReturnDeps    = SpecCommon.ExplicitMonadicReturnDeps _getByExplicit _echoExplicit
        , SpecCommon.explicitReturnDeps           = SpecCommon.ExplicitReturnDeps _getByExplicit _echoExplicit
        , SpecCommon.defaultMethodDeps            = SpecCommon.DefaultMethodDeps _defaultAction
        , SpecCommon.associatedTypeFamiliesDeps   = SpecCommon.AssociatedTypeFamiliesDeps _produce
        , SpecCommon.assocTypeDeps                = SpecCommon.AssocTypeDeps _produce
        , SpecCommon.concurrencyAndUnliftIODeps   = SpecCommon.ConcurrencyAndUnliftIODeps _readFile
        , SpecCommon.concurrencyDeps              = SpecCommon.ConcurrencyDeps _readFile
        }
  SpecCommon.spec deps

  -- describe "verification failures (State - Pending)" do
  --   it "fails when _fnState is defined but fnState is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

  --   it "fails when _fnState2 is defined but fnState2 is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"