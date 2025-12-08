{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (Text, pack, isInfixOf)
import Test.Hspec
import Test.MockCat
import Control.Monad.State
import Control.Monad.Reader (MonadReader, ask)
import Data.Proxy (Proxy(..))
import Control.Monad (unless)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Control.Concurrent.Async (async, wait)
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.TypeClassCommonSpec as SpecCommon
import qualified Test.MockCat.Verify as Verify

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  unless (pack "ngWord" `isInfixOf` content) $
    writeFile outputPath content

operationProgram2 ::
  (FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
operationProgram2 inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post modifiedContent

operationProgram3 ::
  MonadReader SpecCommon.Environment m =>
  FileOperation m =>
  m ()
operationProgram3 = do
  (SpecCommon.Environment inputPath outputPath) <- ask
  content <- readFile inputPath
  writeFile outputPath content


--makeMock [t|MonadReader Bool|]
makeMock [t|MonadReader SpecCommon.Environment|]
makeMockWithOptions [t|MonadVar2_1Sub|] options { implicitMonadicReturn = True }
makeMockWithOptions [t|MonadVar2_2Sub|] options { implicitMonadicReturn = True }
makeMockWithOptions [t|MonadVar3_1Sub|] options { implicitMonadicReturn = True }
makeMockWithOptions [t|MonadVar3_2Sub|] options { implicitMonadicReturn = True }
makeMockWithOptions [t|MonadVar3_3Sub|] options { implicitMonadicReturn = True }
makeMock [t|FileOperation|]
makeMock [t|ApiOperation|]

makeMockWithOptions [t|MultiApplyTest|] options { implicitMonadicReturn = False }

makeMockWithOptions [t|ParamThreeMonad Int Bool|] options { implicitMonadicReturn = False }

makeMockWithOptions [t|MonadStateSub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadStateSub2|] options { implicitMonadicReturn = False }

makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }

echoProgram :: ExplicitlyReturnMonadicValuesTest m => String -> m ()
echoProgram s = do
  v <- getByExplicit s
  echoExplicit $ show v

makeMockWithOptions [t|ExplicitlyReturnMonadicValuesTest|] options { implicitMonadicReturn = False }
makeMock [t|DefaultMethodTest|]
makeMock [t|AssocTypeTest|]
makeMockWithOptions [t|TestClass|] options { implicitMonadicReturn = False }

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

instance AssocTypeTest IO where
  type ResultType IO = Int
  produce = pure 0

processFiles :: MonadAsync m => FileOperation m => [FilePath] -> m [Text]
processFiles = mapConcurrently readFile

 

 

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

-- IO wrappers for subvars (TH generates pure-returning builders here)
_fn2_1SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn2_1SubIO p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn2_1Sub" p
  addDefinition (Definition (Proxy :: Proxy "_fn2_1Sub") mockInstance NoVerification)
  pure mockInstance

_fn2_2SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn2_2SubIO p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn2_2Sub" p
  addDefinition (Definition (Proxy :: Proxy "_fn2_2Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_1SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_1SubIO p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_1Sub" p
  addDefinition (Definition (Proxy :: Proxy "_fn3_1Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_2SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_2SubIO p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_2Sub" p
  addDefinition (Definition (Proxy :: Proxy "_fn3_2Sub") mockInstance NoVerification)
  pure mockInstance

_fn3_3SubIO ::
  forall params.
  (MockBuilder params (String -> IO ()) (Param String)) =>
  params -> MockT IO (String -> IO ())
_fn3_3SubIO p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_fn3_3Sub" p
  addDefinition (Definition (Proxy :: Proxy "_fn3_3Sub") mockInstance NoVerification)
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
        , SpecCommon.multiParamTypeClassArityDeps = SpecCommon.MultiParamTypeClassArityDeps _fn2_1SubIO _fn2_2SubIO _fn3_1SubIO _fn3_2SubIO _fn3_3SubIO
        , SpecCommon.multiParamDeps               = SpecCommon.MultiParamDeps _fn2_1SubIO _fn2_2SubIO _fn3_1SubIO _fn3_2SubIO _fn3_3SubIO
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

