{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{- HLINT ignore "Use newtype instead of data" -}

module Test.MockCat.TypeClassCommonSpec where

import Prelude hiding (readFile, writeFile, any)
import Test.Hspec
import Test.MockCat

import Data.Kind (Type)
import Data.Text (Text, pack, isInfixOf)
import Control.Exception (ErrorCall(..), displayException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Test.MockCat.SharedSpecDefs
import qualified Data.List as List
import Control.Concurrent.Async (async, wait)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad (unless)

-- Helpers

missingCall :: String -> Selector ErrorCall
missingCall name err =
  let needle1 = "function `" <> name <> "` was not called the expected number of times."
      needle2 = "function `_" <> name <> "` was not called the expected number of times."
   in (needle1 `List.isInfixOf` displayException err) || (needle2 `List.isInfixOf` displayException err)

-- Orphan Instances needed for testing

instance MonadState s m => MonadState s (MockT m) where
  get = lift get
  put = lift . put
  state f = lift (state f)

-- Programs under test

data Environment = Environment { inputPath :: String, outputPath :: String }
  deriving (Show, Eq)

apiFileOperationProgram ::
  (MonadReader Environment m, FileOperation m, ApiOperation m) =>
  (Text -> Text) ->
  m ()
apiFileOperationProgram modifyText = do
  e <- ask
  content <- readFile (inputPath e)
  let modifiedContent = modifyText content
  writeFile (outputPath e) modifiedContent
  post $ modifiedContent <> pack ("+" <> show e)

operationProgram ::
  (FileOperation m) =>
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
  MonadReader Environment m =>
  FileOperation m =>
  m ()
operationProgram3 = do
  (Environment inputPath outputPath) <- ask
  content <- readFile inputPath
  writeFile outputPath content

echoProgram :: (MonadIO m, TestClass m) => String -> m ()
echoProgram s = do
  v <- getBy s
  liftIO $ print v
  echo $ show v

echoProgramExplicit :: ExplicitlyReturnMonadicValuesTest m => String -> m ()
echoProgramExplicit s = do
  v <- getByExplicit s
  echoExplicit $ show v

processFiles :: (MonadAsync m, FileOperation m) => [FilePath] -> m [Text]
processFiles = mapConcurrently readFile

echo2 :: Teletype m => m ()
echo2 = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo2

-- Specs
-- Type-level utilities to derive MockBuilder arg-lists from function types
type family PrependParam (p :: Type) (rest :: Type) :: Type where
  PrependParam p () = p
  PrependParam p rest = p :> rest

type family ArgsOfF (f :: Type) :: Type where
  ArgsOfF (a -> b) = PrependParam (Param a) (ArgsOfF b)
  ArgsOfF r = ()

-- Generic Mock alias for a function type f
type MockFor f = forall params. (MockDispatch (IsMockSpec params) params (MockT IO) f) => params -> MockT IO f
-- Generic Mock alias for an arbitrary base monad m
type MockForM m f = forall params. (MockDispatch (IsMockSpec params) params (MockT m) f) => params -> MockT m f

-- Per-spec dependency records to group required builders/mocks
data BasicDeps = BasicDeps
  { _readFile  :: MockFor (FilePath -> Text)
  , _writeFile :: MockFor (FilePath -> Text -> ())
  }

data MixedDeps = MixedDeps
  { _readFile  :: MockFor (FilePath -> Text)
  , _writeFile :: MockFor (FilePath -> Text -> ())
  , _post      :: MockFor (Text -> ())
  }

data MultipleDeps = MultipleDeps
  { _ask      :: MockFor Environment
  , _readFile :: MockFor (FilePath -> Text)
  , _writeFile:: MockFor (FilePath -> Text -> ())
  , _post     :: MockFor (Text -> ())
  }



data ReaderContextDeps = ReaderContextDeps
  { _ask      :: MockFor Environment
  , _readFile :: MockFor (FilePath -> Text)
  , _writeFile:: MockFor (FilePath -> Text -> ())
  }

data SequentialIODeps = SequentialIODeps
  { _readTTY  :: MockFor (IO String)
  , _writeTTY :: MockFor (String -> IO ())
  }

data ImplicitMonadicReturnDeps = ImplicitMonadicReturnDeps
  { _getBy :: MockFor (String -> IO Int)
  , _echo  :: MockFor (String -> IO ())
  }

data ArgumentPatternMatchingDeps = ArgumentPatternMatchingDeps
  { _getValueBy :: MockFor (String -> IO String)
  }

data MonadStateTransformerDeps = MonadStateTransformerDeps
  { _fnState  :: MockForM (StateT String IO) (Maybe String -> StateT String IO String)
  , _fnState2 :: MockForM (StateT String IO) (String -> StateT String IO ())
  }

data MultiParamTypeClassArityDeps = MultiParamTypeClassArityDeps
  { _fn2_1Sub :: MockFor (String -> IO ())
  , _fn2_2Sub :: MockFor (String -> IO ())
  , _fn3_1Sub :: MockFor (String -> IO ())
  , _fn3_2Sub :: MockFor (String -> IO ())
  , _fn3_3Sub :: MockFor (String -> IO ())
  }

data FunctionalDependenciesDeps = FunctionalDependenciesDeps
  { _fnParam3_1 :: MockFor (Int -> Bool -> IO String)
  , _fnParam3_2 :: MockFor (IO Int)
  , _fnParam3_3 :: MockFor (IO Bool)
  }

data ExplicitMonadicReturnDeps = ExplicitMonadicReturnDeps
  { _getByExplicit :: MockFor (String -> IO Int)
  , _echoExplicit  :: MockFor (String -> IO ())
  }

data DefaultMethodDeps = DefaultMethodDeps
  { _defaultAction :: MockFor Int
  }

data AssociatedTypeFamiliesDeps = AssociatedTypeFamiliesDeps
  { _produce :: MockFor Int
  }

data ConcurrencyAndUnliftIODeps = ConcurrencyAndUnliftIODeps
  { _readFile :: MockFor (FilePath -> Text)
  }

data UserDefinedTypeDeps = UserDefinedTypeDeps
  { _processPost :: MockFor (Post -> Bool)
  }

-- backward-compatible constructor pattern synonyms (preserve old names used by test modules)
pattern TtyDeps :: MockFor (IO String) -> MockFor (String -> IO ()) -> SequentialIODeps
pattern TtyDeps r w <- SequentialIODeps { _readTTY = r, _writeTTY = w }
  where TtyDeps r w = SequentialIODeps { _readTTY = r, _writeTTY = w }

pattern TestClassDeps :: MockFor (String -> IO Int) -> MockFor (String -> IO ()) -> ImplicitMonadicReturnDeps
pattern TestClassDeps g e <- ImplicitMonadicReturnDeps { _getBy = g, _echo = e }
  where TestClassDeps g e = ImplicitMonadicReturnDeps { _getBy = g, _echo = e }

pattern MultiApplyDeps :: MockFor (String -> IO String) -> ArgumentPatternMatchingDeps
pattern MultiApplyDeps f <- ArgumentPatternMatchingDeps { _getValueBy = f }
  where MultiApplyDeps f = ArgumentPatternMatchingDeps { _getValueBy = f }

pattern StateDeps :: MockForM (StateT String IO) (Maybe String -> StateT String IO String) -> MockForM (StateT String IO) (String -> StateT String IO ()) -> MonadStateTransformerDeps
pattern StateDeps s s2 <- MonadStateTransformerDeps { _fnState = s, _fnState2 = s2 }
  where StateDeps s s2 = MonadStateTransformerDeps { _fnState = s, _fnState2 = s2 }

pattern MultiParamDeps :: MockFor (String -> IO ()) -> MockFor (String -> IO ()) -> MockFor (String -> IO ()) -> MockFor (String -> IO ()) -> MockFor (String -> IO ()) -> MultiParamTypeClassArityDeps
pattern MultiParamDeps a b c d e <- MultiParamTypeClassArityDeps { _fn2_1Sub = a, _fn2_2Sub = b, _fn3_1Sub = c, _fn3_2Sub = d, _fn3_3Sub = e }
  where MultiParamDeps a b c d e = MultiParamTypeClassArityDeps { _fn2_1Sub = a, _fn2_2Sub = b, _fn3_1Sub = c, _fn3_2Sub = d, _fn3_3Sub = e }

pattern FunDeps :: MockFor (Int -> Bool -> IO String) -> MockFor (IO Int) -> MockFor (IO Bool) -> FunctionalDependenciesDeps
pattern FunDeps a b c <- FunctionalDependenciesDeps { _fnParam3_1 = a, _fnParam3_2 = b, _fnParam3_3 = c }
  where FunDeps a b c = FunctionalDependenciesDeps { _fnParam3_1 = a, _fnParam3_2 = b, _fnParam3_3 = c }

pattern ExplicitReturnDeps :: MockFor (String -> IO Int) -> MockFor (String -> IO ()) -> ExplicitMonadicReturnDeps
pattern ExplicitReturnDeps a b <- ExplicitMonadicReturnDeps { _getByExplicit = a, _echoExplicit = b }
  where ExplicitReturnDeps a b = ExplicitMonadicReturnDeps { _getByExplicit = a, _echoExplicit = b }

pattern AssocTypeDeps :: MockFor Int -> AssociatedTypeFamiliesDeps
pattern AssocTypeDeps f <- AssociatedTypeFamiliesDeps { _produce = f }
  where AssocTypeDeps f = AssociatedTypeFamiliesDeps { _produce = f }

pattern ConcurrencyDeps :: MockFor (FilePath -> Text) -> ConcurrencyAndUnliftIODeps
pattern ConcurrencyDeps r <- ConcurrencyAndUnliftIODeps { _readFile = r }
  where ConcurrencyDeps r = ConcurrencyAndUnliftIODeps { _readFile = r }

-- Aggregate all per-spec dependency groups + standalone mocks into one record
data SpecDeps = SpecDeps
  { basicDeps                         :: BasicDeps
  , mixedDeps                         :: MixedDeps
  , multipleDeps                      :: MultipleDeps

  , readerContextDeps                 :: ReaderContextDeps
  , sequentialIODeps                  :: SequentialIODeps
  , ttyDeps                           :: SequentialIODeps
  , implicitMonadicReturnDeps         :: ImplicitMonadicReturnDeps
  , testClassDeps                     :: ImplicitMonadicReturnDeps
  , argumentPatternMatchingDeps       :: ArgumentPatternMatchingDeps
  , multiApplyDeps                    :: ArgumentPatternMatchingDeps
  , monadStateTransformerDeps         :: MonadStateTransformerDeps
  , stateDeps                         :: MonadStateTransformerDeps
  , multiParamTypeClassArityDeps      :: MultiParamTypeClassArityDeps
  , multiParamDeps                    :: MultiParamTypeClassArityDeps
  , functionalDependenciesDeps        :: FunctionalDependenciesDeps
  , funDeps                           :: FunctionalDependenciesDeps
  , explicitMonadicReturnDeps         :: ExplicitMonadicReturnDeps
  , explicitReturnDeps                :: ExplicitMonadicReturnDeps
  , defaultMethodDeps                 :: DefaultMethodDeps
  , assocTypeDeps                     :: AssociatedTypeFamiliesDeps
  , associatedTypeFamiliesDeps        :: AssociatedTypeFamiliesDeps
  , concurrencyAndUnliftIODeps        :: ConcurrencyAndUnliftIODeps
  , concurrencyDeps                   :: ConcurrencyAndUnliftIODeps
  , userDefinedTypeDeps               :: UserDefinedTypeDeps
  }

-- SpecDeps is defined above; test modules construct a `SpecDeps` and call the individual specs
-- Aggregated entry point: call all specs from a single SpecDeps
spec ::
  ( Teletype (MockT IO)
  , FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  , MonadReader Environment (MockT IO)
  , TestClass (MockT IO)
  , MultiApplyTest (MockT IO)
  , MonadVar2_1Sub (MockT IO) String
  , MonadVar2_2Sub String (MockT IO)
  , MonadVar3_1Sub (MockT IO) String String
  , MonadVar3_2Sub String (MockT IO) String
  , MonadVar3_3Sub String String (MockT IO)
  , MonadStateSub String (MockT (StateT String IO))
  , MonadStateSub2 String (MockT (StateT String IO))
  , ParamThreeMonad Int Bool (MockT IO)
  , ExplicitlyReturnMonadicValuesTest (MockT IO)
  , DefaultMethodTest (MockT IO)
  , AssocTypeTest (MockT IO)
  , ResultType (MockT IO) ~ Int
  , MonadAsync (MockT IO)
  , UserDefinedClass (MockT IO)
  ) =>
  SpecDeps ->
  Spec
spec deps = do
  specSequentialIOStubbing deps.sequentialIODeps
  specBasicStubbingAndVerification deps.basicDeps
  specMixedMockingStrategies deps.mixedDeps
  specMultipleTypeclassConstraints deps.multipleDeps

  specImplicitMonadicReturnValues deps.implicitMonadicReturnDeps
  specArgumentPatternMatching deps.argumentPatternMatchingDeps
  specMultiParamTypeClassArity deps.multiParamTypeClassArityDeps
  specMonadStateTransformerSupport deps.monadStateTransformerDeps
  specFunctionalDependenciesSupport deps.functionalDependenciesDeps
  specExplicitMonadicReturnValues deps.explicitMonadicReturnDeps
  specDefaultMethodMocking deps.defaultMethodDeps
  specAssociatedTypeFamiliesSupport deps.associatedTypeFamiliesDeps
  specConcurrencyAndUnliftIO deps.concurrencyAndUnliftIODeps
  specMonadReaderContextMocking deps.readerContextDeps
  specUserDefinedTypeSupport deps.userDefinedTypeDeps

  -- Verification failures
  specBasicVerificationFailureDetection deps.basicDeps

  specMonadReaderVerificationFailureDetection deps.readerContextDeps
  specImplicitReturnVerificationFailureDetection deps.implicitMonadicReturnDeps
  specMultiParamVerificationFailureDetection deps.multiParamTypeClassArityDeps
  specArgumentMatchingVerificationFailureDetection deps.argumentPatternMatchingDeps
  specFunDepsVerificationFailureDetection deps.functionalDependenciesDeps
  specExplicitReturnVerificationFailureDetection deps.explicitMonadicReturnDeps
  specAdvancedTypesVerificationFailureDetection deps.defaultMethodDeps deps.associatedTypeFamiliesDeps
  specSequentialStubbingVerificationFailureDetection deps.sequentialIODeps

specBasicStubbingAndVerification ::
  ( FileOperation (MockT IO)
  ) =>
  BasicDeps ->
  Spec
specBasicStubbingAndVerification (BasicDeps { _readFile, _writeFile }) = do
  it "Program reads content and writes it to output path" do
    result <- runMockT do
      _readFile $ "input.txt" ~> pack "content"
      _writeFile $ "output.txt" ~> pack "content" ~> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Program skips file write when input content contains 'ngWord'" do
    result <- runMockT do
      _readFile  ("input.txt" ~> pack "contains ngWord")
      _writeFile ("output.txt" ~> any ~> ())
        `expects` do
          called never
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

specMixedMockingStrategies ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  ) =>
  MixedDeps ->
  Spec
specMixedMockingStrategies (MixedDeps { _readFile, _writeFile, _post }) = do
  it "Program reads, modifies content via stub, writes, and posts result" do
    modifyContentStub <- mock $ pack "content" ~> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" ~> pack "content"
      _writeFile $ ("output.text" ~> pack "modifiedContent" ~> ())
      _post $ (pack "modifiedContent" ~> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

specMultipleTypeclassConstraints ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  , MonadReader Environment (MockT IO)
  ) =>
  MultipleDeps ->
  Spec
specMultipleTypeclassConstraints (MultipleDeps { _ask, _readFile, _writeFile, _post }) = do
  it "Composed program successfully reads environment, processes file, and posts combined result" do
    modifyContentStub <- mock $ pack "content" ~> pack "modifiedContent"
    let env = Environment "input.txt" "output.text"

    result <- runMockT do
      _ask env
      _readFile ("input.txt" ~> pack "content")
      _writeFile ("output.text" ~> pack "modifiedContent" ~> ())
      _post (pack "modifiedContent" <> pack ("+" <> show env) ~> ())
      apiFileOperationProgram modifyContentStub

    result `shouldBe` ()



specMonadReaderContextMocking ::
  ( MonadReader Environment (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  ReaderContextDeps ->
  Spec
specMonadReaderContextMocking (ReaderContextDeps { _ask, _readFile, _writeFile }) = do
  it "Program successfully uses MonadReader to find paths and executes FileOperation" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile  ("input.txt" ~> pack "content")
      _writeFile  ("output.txt" ~> pack "content" ~> ())
      operationProgram3
    r `shouldBe` ()

specSequentialIOStubbing ::
  ( Teletype (MockT IO)
  ) =>
  SequentialIODeps ->
  Spec
specSequentialIOStubbing (SequentialIODeps { _readTTY, _writeTTY }) = do
  it "Recursive program echo2 reads sequential input and writes output until empty string" do
    result <- runMockT do
      _readTTY $ casesIO ["a", ""]
      _writeTTY $ "a" ~> pure @IO ()
      echo2
    result `shouldBe` ()

specImplicitMonadicReturnValues ::
  ( TestClass (MockT IO)
  ) =>
  ImplicitMonadicReturnDeps ->
  Spec
specImplicitMonadicReturnValues (ImplicitMonadicReturnDeps { _getBy, _echo }) = do
  it "Program correctly uses and echoes the implicitly stubbed monadic return value" do
    result <- runMockT do
      _getBy $ "s" ~> pure @IO (10 :: Int)
      _echo $ "10" ~> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()

specArgumentPatternMatching ::
  ( MultiApplyTest (MockT IO)
  ) =>
  ArgumentPatternMatchingDeps ->
  Spec
specArgumentPatternMatching (ArgumentPatternMatchingDeps { _getValueBy }) = do
  it "Multi-case stubbing correctly dispatches and collects results for distinct arguments" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" ~> pure @IO "ax"
        onCase $ "b" ~> pure @IO "bx"
        onCase $ "c" ~> pure @IO "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

specMonadStateTransformerSupport ::
  ( MonadStateSub String (MockT (StateT String IO))
  , MonadStateSub2 String (MockT (StateT String IO))
  ) =>
  MonadStateTransformerDeps ->
  Spec
specMonadStateTransformerSupport (MonadStateTransformerDeps { _fnState, _fnState2 }) = do
  it "Mock with StateT correctly consumes input and returns next value" do
    let action = runMockT $ do
          _fnState $ do
            onCase $ Just "current" ~> pure @(StateT String IO) "next"
            onCase $ Nothing ~> pure @(StateT String IO) "default"
          fnState (Just "current")
    result <- evalStateT action "seed"
    result `shouldBe` "next"

  it "Mock in StateT correctly executes and leaves the state unchanged (unit return)" do
    let action = runMockT $ do
          _fnState2 $ "label" ~> pure @(StateT String IO) ()
          fnState2 @String "label"
    result <- evalStateT action "initial"
    result `shouldBe` ()

specMultiParamTypeClassArity ::
  ( MonadVar2_1Sub (MockT IO) String
  , MonadVar2_2Sub String (MockT IO)
  , MonadVar3_1Sub (MockT IO) String String
  , MonadVar3_2Sub String (MockT IO) String
  , MonadVar3_3Sub String String (MockT IO)
  ) =>
  MultiParamTypeClassArityDeps ->
  Spec
specMultiParamTypeClassArity (MultiParamTypeClassArityDeps { _fn2_1Sub, _fn2_2Sub, _fn3_1Sub, _fn3_2Sub, _fn3_3Sub }) = do
  it "Type variable MonadVar2_1Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn2_1Sub $ "alpha" ~> pure @IO ()
      fn2_1Sub @(MockT IO) @String "alpha"
    result `shouldBe` ()

  it "Type variable MonadVar2_2Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn2_2Sub $ "beta" ~> pure @IO ()
      fn2_2Sub @String @(MockT IO) "beta"
    result `shouldBe` ()

  it "Type variable MonadVar3_1Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_1Sub $ "gamma" ~> pure @IO ()
      fn3_1Sub @(MockT IO) @String @String "gamma"
    result `shouldBe` ()

  it "Type variable MonadVar3_2Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_2Sub $ "delta" ~> pure @IO ()
      fn3_2Sub @String @(MockT IO) @String "delta"
    result `shouldBe` ()

  it "Type variable MonadVar3_3Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_3Sub $ "epsilon" ~> pure @IO ()
      fn3_3Sub @String @String @(MockT IO) "epsilon"
    result `shouldBe` ()

specFunctionalDependenciesSupport ::
  ( ParamThreeMonad Int Bool (MockT IO)
  ) =>
  FunctionalDependenciesDeps ->
  Spec
specFunctionalDependenciesSupport (FunctionalDependenciesDeps { _fnParam3_1, _fnParam3_2, _fnParam3_3 }) = do
  it "FunDeps are correctly resolved allowing multiple actions to return values" do
    result <- runMockT $ do
      _fnParam3_1 $ do
        onCase $ (1 :: Int) ~> True ~> pure @IO "combined"
      _fnParam3_2 $ casesIO [1 :: Int]
      _fnParam3_3 $ casesIO [True]
      r1 <- fnParam3_1 (1 :: Int) True
      r2 <- fnParam3_2
      r3 <- fnParam3_3
      pure (r1, r2, r3)
    result `shouldBe` ("combined", 1, True)

specExplicitMonadicReturnValues ::
  ( ExplicitlyReturnMonadicValuesTest (MockT IO)
  ) =>
  ExplicitMonadicReturnDeps ->
  Spec
specExplicitMonadicReturnValues (ExplicitMonadicReturnDeps { _getByExplicit, _echoExplicit }) = do
  it "Explicitly stubbed function returns value and subsequent action is verified" do
    result <- runMockT do
      _getByExplicit $ "key" ~> pure @IO (42 :: Int)
      _echoExplicit $ "value" ~> pure @IO ()
      v <- getByExplicit "key"
      echoExplicit "value"
      pure v
    result `shouldBe` 42

  it "Helper program correctly uses and echoes the explicitly stubbed monadic return value" do
    result <- runMockT do
      _getByExplicit $ "s" ~> pure @IO (10 :: Int)
      _echoExplicit $ "10" ~> pure @IO ()
      echoProgramExplicit "s"
    result `shouldBe` ()

specDefaultMethodMocking ::
  ( DefaultMethodTest (MockT IO)
  ) =>
  DefaultMethodDeps ->
  Spec
specDefaultMethodMocking (DefaultMethodDeps { _defaultAction }) = do
  it "Default method is successfully overridden and stubbed value is returned" do
    result <- runMockT do
      _defaultAction (Head :> param (99 :: Int))
      defaultAction
    result `shouldBe` 99

specAssociatedTypeFamiliesSupport ::
  ( AssocTypeTest (MockT IO)
  , ResultType (MockT IO) ~ Int
  ) =>
  AssociatedTypeFamiliesDeps ->
  Spec
specAssociatedTypeFamiliesSupport (AssociatedTypeFamiliesDeps { _produce }) = do
  it "Associated Type family is correctly resolved and mocked stub value is returned" do
    v <- runMockT do
      _produce (Head :> param (321 :: Int))
      produce
    v `shouldBe` 321

specConcurrencyAndUnliftIO ::
  ( MonadAsync (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  ConcurrencyAndUnliftIODeps ->
  Spec
specConcurrencyAndUnliftIO (ConcurrencyAndUnliftIODeps { _readFile }) = do
  it "Concurrent execution (mapConcurrently) correctly calls and collects results from mocks" do
    result <- runMockT do
      _readFile $ do
        onCase $ "file1.txt" ~> pack "content1"
        onCase $ "file2.txt" ~> pack "content2"
      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]

  it "MonadUnliftIO correctly handles internal async operation and verification" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" ~> pack "content"

      content <- withRunInIO $ \runInIO -> do
        asyncAction <- async $ runInIO (readFile "test.txt")
        wait asyncAction

      liftIO $ content `shouldBe` pack "content"
      pure content

    result `shouldBe` pack "content"

  it "MonadUnliftIO basic runInIO functionality is verified correctly" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" ~> pack "test content"

      content <- withRunInIO $ \runInIO -> do
        runInIO (readFile "test.txt")

      liftIO $ content `shouldBe` pack "test content"
      pure content

    result `shouldBe` pack "test content"

specUserDefinedTypeSupport ::
  ( UserDefinedClass (MockT IO)
  ) =>
  UserDefinedTypeDeps ->
  Spec
specUserDefinedTypeSupport (UserDefinedTypeDeps { _processPost }) = do
  it "User defined class method handles shared user-defined type correctly" do
    let p = Post 1 "title"
    runMockT @IO $ do
      _processPost $ do
         -- verify that we can match exactly on the user defined type
         onCase $ p ~> True
      
      result <- processPost p 
      liftIO $ result `shouldBe` True

-- Verification Failure Tests

specBasicVerificationFailureDetection ::
  ( FileOperation (MockT IO)
  ) =>
  BasicDeps ->
  Spec
specBasicVerificationFailureDetection (BasicDeps { _readFile, _writeFile }) = describe "verification failures (FileOperation)" do
    it "Error when read stub is defined but target function readFile is never called" do
      (runMockT @IO do
        _readFile ("input.txt" ~> pack "content")
          `expects` do
            called once
        -- readFile is never called
        pure ()) `shouldThrow` missingCall "readFile"

    it "Error when write stub is defined but target function writeFile is never called" do
      (runMockT @IO do
        _writeFile ("output.txt" ~> pack "content" ~> ())
          `expects` do
            called once
        -- writeFile is never called
        pure ()) `shouldThrow` missingCall "writeFile"

    it "Error when read stub expects call but only writeFile is executed" do
      (runMockT @IO do
        _readFile ("input.txt" ~> pack "content")
          `expects` do
            called once
        _writeFile  ("output.txt" ~> pack "content" ~> ())
        -- readFile is never called, only writeFile is called
        do
          writeFile "output.txt" (pack "content")
          pure ()) `shouldThrow` missingCall "readFile"

    it "Error when write stub expects call but only readFile is executed" do
      (runMockT @IO do
        _readFile  ("input.txt" ~> pack "content")
        _writeFile ("output.txt" ~> pack "content" ~> ())
          `expects` do
            called once
        -- writeFile is never called, only readFile is called
        do
          readFile "input.txt") `shouldThrow` missingCall "writeFile"



specMonadReaderVerificationFailureDetection ::
  ReaderContextDeps ->
  Spec
specMonadReaderVerificationFailureDetection (ReaderContextDeps { _ask }) = describe "verification failures (Reader Environment)" do
    it "Error when MonadReader stub _ask is defined but target function ask is never called" do
      (runMockT @IO do
        _ask (Head :> param (Environment "input.txt" "output.txt"))
          `expects` do
            called once
        -- ask is never called
        pure ()) `shouldThrow` missingCall "ask"

specImplicitReturnVerificationFailureDetection ::
  ImplicitMonadicReturnDeps ->
  Spec
specImplicitReturnVerificationFailureDetection (ImplicitMonadicReturnDeps { _getBy, _echo }) = describe "verification failures (TestClass)" do
    it "Error when _getBy stub expects call but getBy is never executed" do
      (runMockT @IO do
        _getBy ("s" ~> pure @IO (10 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_getBy"

    it "Error when _echo stub expects call but echo is never executed" do
      (runMockT @IO do
        _echo ("10" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_echo"

specMultiParamVerificationFailureDetection ::
  MultiParamTypeClassArityDeps ->
  Spec
specMultiParamVerificationFailureDetection (MultiParamTypeClassArityDeps { _fn2_1Sub, _fn2_2Sub, _fn3_1Sub, _fn3_2Sub, _fn3_3Sub }) = describe "verification failures (SubVars)" do
    it "Error when _fn2_1Sub stub expects call but fn2_1Sub is never executed" do
      (runMockT @IO do
        _fn2_1Sub ("alpha" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fn2_1Sub"

    it "Error when _fn2_2Sub stub expects call but fn2_2Sub is never executed" do
      (runMockT @IO do
        _fn2_2Sub ("beta" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fn2_2Sub"

    it "Error when _fn3_1Sub stub expects call but fn3_1Sub is never executed" do
      (runMockT @IO do
        _fn3_1Sub ("gamma" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fn3_1Sub"

    it "Error when _fn3_2Sub stub expects call but fn3_2Sub is never executed" do
      (runMockT @IO do
        _fn3_2Sub ("delta" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fn3_2Sub"

    it "Error when _fn3_3Sub stub expects call but fn3_3Sub is never executed" do
      (runMockT @IO do
        _fn3_3Sub ("epsilon" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fn3_3Sub"

specArgumentMatchingVerificationFailureDetection ::
  ArgumentPatternMatchingDeps ->
  Spec
specArgumentMatchingVerificationFailureDetection (ArgumentPatternMatchingDeps { _getValueBy }) = describe "verification failures (MultiApply)" do
    it "Error when multi-case stub _getValueBy expects call but getValueBy is never executed" do
      (runMockT @IO do
        _getValueBy (do onCase $ "a" ~> pure @IO "ax")
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_getValueBy"

specFunDepsVerificationFailureDetection ::
  FunctionalDependenciesDeps ->
  Spec
specFunDepsVerificationFailureDetection (FunctionalDependenciesDeps { _fnParam3_1, _fnParam3_2, _fnParam3_3 }) = describe "verification failures (ParamThreeMonad)" do
    it "Error when FunDep stub _fnParam3_1 expects call but fnParam3_1 is never executed" do
      (runMockT @IO do
        _fnParam3_1 (do
               onCase $ (1 :: Int) ~> True ~> pure @IO "combined")
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fnParam3_1"

    it "Error when FunDep stub _fnParam3_2 expects call but fnParam3_2 is never executed" do
      (runMockT @IO do
        _fnParam3_2 (casesIO [1 :: Int])
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fnParam3_2"

    it "Error when FunDep stub _fnParam3_3 expects call but fnParam3_3 is never executed" do
      (runMockT @IO do
        _fnParam3_3 (casesIO [True])
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_fnParam3_3"

specExplicitReturnVerificationFailureDetection ::
  ExplicitMonadicReturnDeps ->
  Spec
specExplicitReturnVerificationFailureDetection (ExplicitMonadicReturnDeps { _getByExplicit, _echoExplicit }) = describe "verification failures (ExplicitReturn)" do
    it "Error when explicit stub _getByExplicit expects call but getByExplicit is never executed" do
      (runMockT @IO do
        _getByExplicit ("key" ~> pure @IO (42 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_getByExplicit"

    it "Error when explicit stub _echoExplicit expects call but echoExplicit is never executed" do
      (runMockT @IO do
        _echoExplicit ("value" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_echoExplicit"

specAdvancedTypesVerificationFailureDetection ::
  DefaultMethodDeps ->
  AssociatedTypeFamiliesDeps ->
  Spec
specAdvancedTypesVerificationFailureDetection (DefaultMethodDeps { _defaultAction }) (AssociatedTypeFamiliesDeps { _produce }) = describe "verification failures (Default/Assoc)" do
    it "Error when default method stub _defaultAction expects call but defaultAction is never executed" do
      (runMockT @IO do
        _defaultAction (Head :> param (99 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_defaultAction"

    it "Error when associated type stub _produce expects call but produce is never executed" do
      (runMockT @IO do
        _produce (Head :> param (321 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_produce"

specSequentialStubbingVerificationFailureDetection ::
  SequentialIODeps ->
  Spec
specSequentialStubbingVerificationFailureDetection (SequentialIODeps { _readTTY, _writeTTY }) = describe "verification failures (TTY)" do
    it "Error when sequential stub _readTTY expects call but readTTY is never executed" do
      (runMockT @IO do
        _readTTY (casesIO ["a", ""])
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_readTTY"

    it "Error when sequential stub _writeTTY expects call but writeTTY is never executed" do
      (runMockT @IO do
        _writeTTY ("a" ~> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` missingCall "_writeTTY"