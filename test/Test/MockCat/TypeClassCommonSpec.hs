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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MockCat.TypeClassCommonSpec where

import Prelude hiding (readFile, writeFile, any)
import Test.Hspec
import Test.MockCat
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
  let needle1 = "function `" <> name <> "` was not applied the expected number of times."
      needle2 = "function `_" <> name <> "` was not applied the expected number of times."
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
    _  -> writeTTY i >> echo2

-- Specs

specBasicStubbingAndVerification ::
  ( FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specBasicStubbingAndVerification _readFile _writeFile = do
  it "Program reads content and writes it to output path" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.txt" |> pack "content" |> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()
    
  it "Program skips file write when input content contains 'ngWord'" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      _writeFile ("output.txt" |> any |> ())
        `expects` do
          called never
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

specMixedMockingStrategies ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specMixedMockingStrategies _readFile _writeFile _post = do
  it "Program reads, modifies content via stub, writes, and posts result" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      _post (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

specMultipleTypeclassConstraints ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  , MonadReader Environment (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specMultipleTypeclassConstraints _ask _readFile _writeFile _post = do
  it "Composed program successfully reads environment, processes file, and posts combined result" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"
    let env = Environment "input.txt" "output.text"

    result <- runMockT do
      _ask env
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.text" |> pack "modifiedContent" |> ())
      _post ((pack "modifiedContent" <> pack ("+" <> show env)) |> ())
      apiFileOperationProgram modifyContentStub

    result `shouldBe` ()

specCustomMockNamingOptions ::
  ( ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specCustomMockNamingOptions _stubPost = do
  it "Mock names generated by makeMockWithOptions are correctly registered for verification" do
    result <- runMockT do
      _stubPost (pack "payload" |> ())
      post (pack "payload")
    result `shouldBe` ()

specMonadReaderContextMocking :: 
  ( MonadReader Environment (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specMonadReaderContextMocking _ask _readFile _writeFile = do
  it "Program successfully uses MonadReader to find paths and executes FileOperation" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram3
    r `shouldBe` ()

specSequentialIOStubbing ::
  ( Teletype (MockT IO)
  ) =>
  (forall params. (MockBuilder params (IO String) ()) => params -> MockT IO (IO String)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specSequentialIOStubbing _readTTY _writeTTY = do
  it "Recursive program echo2 reads sequential input and writes output until empty string" do
    result <- runMockT do
      _readTTY $ casesIO ["a", ""]
      _writeTTY $ "a" |> pure @IO ()
      echo2
    result `shouldBe` ()

specImplicitMonadicReturnValues ::
  ( TestClass (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specImplicitMonadicReturnValues _getBy _echo = do
  it "Program correctly uses and echoes the implicitly stubbed monadic return value" do
    result <- runMockT do
      _getBy $ "s" |> pure @IO (10 :: Int)
      _echo $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()

specArgumentPatternMatching ::
  ( MultiApplyTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO String) (Param String)) => params -> MockT IO (String -> IO String)) ->
  Spec
specArgumentPatternMatching _getValueBy = do
  it "Multi-case stubbing correctly dispatches and collects results for distinct arguments" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" |> pure @IO "ax"
        onCase $ "b" |> pure @IO "bx"
        onCase $ "c" |> pure @IO "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

specMonadStateTransformerSupport ::
  ( MonadStateSub String (MockT (StateT String IO))
  , MonadStateSub2 String (MockT (StateT String IO))
  ) =>
  (forall params. (MockBuilder params (Maybe String -> StateT String IO String) (Param (Maybe String))) => params -> MockT (StateT String IO) (Maybe String -> StateT String IO String)) ->
  (forall params. (MockBuilder params (String -> StateT String IO ()) (Param String)) => params -> MockT (StateT String IO) (String -> StateT String IO ())) ->
  Spec
specMonadStateTransformerSupport _fnState _fnState2 = do
  it "Mock with StateT correctly consumes input and returns next value" do
    let action = runMockT $ do
          _fnState $ do
            onCase $ Just "current" |> pure @(StateT String IO) "next"
            onCase $ Nothing |> pure @(StateT String IO) "default"
          fnState (Just "current")
    result <- evalStateT action "seed"
    result `shouldBe` "next"

  it "Mock in StateT correctly executes and leaves the state unchanged (unit return)" do
    let action = runMockT $ do
          _fnState2 $ "label" |> pure @(StateT String IO) ()
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
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specMultiParamTypeClassArity _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub = do
  it "Type variable MonadVar2_1Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn2_1Sub $ "alpha" |> pure @IO ()
      fn2_1Sub @(MockT IO) @String "alpha"
    result `shouldBe` ()

  it "Type variable MonadVar2_2Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn2_2Sub $ "beta" |> pure @IO ()
      fn2_2Sub @String @(MockT IO) "beta"
    result `shouldBe` ()

  it "Type variable MonadVar3_1Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_1Sub $ "gamma" |> pure @IO ()
      fn3_1Sub @(MockT IO) @String @String "gamma"
    result `shouldBe` ()

  it "Type variable MonadVar3_2Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_2Sub $ "delta" |> pure @IO ()
      fn3_2Sub @String @(MockT IO) @String "delta"
    result `shouldBe` ()

  it "Type variable MonadVar3_3Sub is correctly resolved and mocked" do
    result <- runMockT do
      _fn3_3Sub $ "epsilon" |> pure @IO ()
      fn3_3Sub @String @String @(MockT IO) "epsilon"
    result `shouldBe` ()

specFunctionalDependenciesSupport ::
  ( ParamThreeMonad Int Bool (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Int -> Bool -> IO String) (Param Int :> Param Bool)) => params -> MockT IO (Int -> Bool -> IO String)) ->
  (forall params. (MockBuilder params (IO Int) ()) => params -> MockT IO (IO Int)) ->
  (forall params. (MockBuilder params (IO Bool) ()) => params -> MockT IO (IO Bool)) ->
  Spec
specFunctionalDependenciesSupport _fnParam3_1 _fnParam3_2 _fnParam3_3 = do
  it "FunDeps are correctly resolved allowing multiple actions to return values" do
    result <- runMockT $ do
      _fnParam3_1 $ do
        onCase $ (1 :: Int) |> True |> pure @IO "combined"
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
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specExplicitMonadicReturnValues _getByExplicit _echoExplicit = do
  it "Explicitly stubbed function returns value and subsequent action is verified" do
    result <- runMockT do
      _getByExplicit $ "key" |> pure @IO (42 :: Int)
      _echoExplicit $ "value" |> pure @IO ()
      v <- getByExplicit "key"
      echoExplicit "value"
      pure v
    result `shouldBe` 42
  
  it "Helper program correctly uses and echoes the explicitly stubbed monadic return value" do
    result <- runMockT do
      _getByExplicit $ "s" |> pure @IO (10 :: Int)
      _echoExplicit $ "10" |> pure @IO ()
      echoProgramExplicit "s"
    result `shouldBe` ()

specDefaultMethodMocking ::
  ( DefaultMethodTest (MockT IO)
  ) =>
  (Int -> MockT IO Int) ->
  Spec
specDefaultMethodMocking _defaultAction = do
  it "Default method is successfully overridden and stubbed value is returned" do
    result <- runMockT do
      _defaultAction (99 :: Int)
      defaultAction
    result `shouldBe` 99

specAssociatedTypeFamiliesSupport ::
  ( AssocTypeTest (MockT IO)
  , ResultType (MockT IO) ~ Int
  ) =>
  (Int -> MockT IO Int) ->
  Spec
specAssociatedTypeFamiliesSupport _produce = do
  it "Associated Type family is correctly resolved and mocked stub value is returned" do
    value <- runMockT do
      _produce (321 :: Int)
      produce
    value `shouldBe` 321

specConcurrencyAndUnliftIO ::
  ( MonadAsync (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  Spec
specConcurrencyAndUnliftIO _readFile = do
  it "Concurrent execution (mapConcurrently) correctly calls and collects results from mocks" do
    result <- runMockT do
      _readFile $ do
        onCase $ "file1.txt" |> pack "content1"
        onCase $ "file2.txt" |> pack "content2"
      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]

  it "MonadUnliftIO correctly handles internal async operation and verification" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" |> pack "content"

      content <- withRunInIO $ \runInIO -> do
        asyncAction <- async $ runInIO (readFile "test.txt")
        wait asyncAction

      liftIO $ content `shouldBe` pack "content"
      pure content

    result `shouldBe` pack "content"

  it "MonadUnliftIO basic runInIO functionality is verified correctly" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" |> pack "test content"
      
      content <- withRunInIO $ \runInIO -> do
        runInIO (readFile "test.txt")
      
      liftIO $ content `shouldBe` pack "test content"
      pure content

    result `shouldBe` pack "test content"

-- Verification Failure Tests

specBasicVerificationFailureDetection ::
  ( FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specBasicVerificationFailureDetection _readFile _writeFile = describe "verification failures (FileOperation)" do
    it "Error when read stub is defined but target function readFile is never called" do
      (runMockT @IO do
        _ <- _readFile ("input.txt" |> pack "content")
          `expects` do
            called once
        -- readFile is never called
        pure ()) `shouldThrow` (missingCall "readFile")

    it "Error when write stub is defined but target function writeFile is never called" do
      (runMockT @IO do
        _ <- _writeFile ("output.txt" |> pack "content" |> ())
          `expects` do
            called once
        -- writeFile is never called
        pure ()) `shouldThrow` (missingCall "writeFile")

    it "Error when read stub expects call but only writeFile is executed" do
      (runMockT @IO do
        _ <- _readFile ("input.txt" |> pack "content")
          `expects` do
            called once
        _writeFile ("output.txt" |> pack "content" |> ())
        -- readFile is never called, only writeFile is called
        do
          writeFile "output.txt" (pack "content")
          pure ()) `shouldThrow` (missingCall "readFile")

    it "Error when write stub expects call but only readFile is executed" do
      (runMockT @IO do
        _readFile ("input.txt" |> pack "content")
        _ <- _writeFile ("output.txt" |> pack "content" |> ())
          `expects` do
            called once
        -- writeFile is never called, only readFile is called
        do
          readFile "input.txt") `shouldThrow` (missingCall "writeFile")

specCustomNamingVerificationFailureDetection ::
  ( ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specCustomNamingVerificationFailureDetection _post = describe "verification failures (Api)" do
    it "Error when custom-named stub _post is defined but target function post is never called" do
      (runMockT @IO do
        _ <- _post (pack "content" |> ())
          `expects` do
            called once
        -- post is never called
        pure ()) `shouldThrow` (missingCall "post")

specMonadReaderVerificationFailureDetection ::
  ( MonadReader Environment (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  Spec
specMonadReaderVerificationFailureDetection _ask = describe "verification failures (Reader Environment)" do
    it "Error when MonadReader stub _ask is defined but target function ask is never called" do
      (runMockT @IO do
        _ <- _ask (Environment "input.txt" "output.txt")
          `expects` do
            called once
        -- ask is never called
        pure ()) `shouldThrow` (missingCall "ask")

specImplicitReturnVerificationFailureDetection ::
  ( TestClass (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specImplicitReturnVerificationFailureDetection _getBy _echo = describe "verification failures (TestClass)" do
    it "Error when _getBy stub expects call but getBy is never executed" do
      (runMockT @IO do
        _getBy ("s" |> pure @IO (10 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getBy")

    it "Error when _echo stub expects call but echo is never executed" do
      (runMockT @IO do
        _echo ("10" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_echo")

specMultiParamVerificationFailureDetection ::
  ( MonadVar2_1Sub (MockT IO) String
  , MonadVar2_2Sub String (MockT IO)
  , MonadVar3_1Sub (MockT IO) String String
  , MonadVar3_2Sub String (MockT IO) String
  , MonadVar3_3Sub String String (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specMultiParamVerificationFailureDetection _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub = describe "verification failures (SubVars)" do
    it "Error when _fn2_1Sub stub expects call but fn2_1Sub is never executed" do
      (runMockT @IO do
        _fn2_1Sub ("alpha" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn2_1Sub")

    it "Error when _fn2_2Sub stub expects call but fn2_2Sub is never executed" do
      (runMockT @IO do
        _fn2_2Sub ("beta" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn2_2Sub")

    it "Error when _fn3_1Sub stub expects call but fn3_1Sub is never executed" do
      (runMockT @IO do
        _fn3_1Sub ("gamma" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_1Sub")

    it "Error when _fn3_2Sub stub expects call but fn3_2Sub is never executed" do
      (runMockT @IO do
        _fn3_2Sub ("delta" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_2Sub")

    it "Error when _fn3_3Sub stub expects call but fn3_3Sub is never executed" do
      (runMockT @IO do
        _fn3_3Sub ("epsilon" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_3Sub")

specArgumentMatchingVerificationFailureDetection ::
  ( MultiApplyTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO String) (Param String)) => params -> MockT IO (String -> IO String)) ->
  Spec
specArgumentMatchingVerificationFailureDetection _getValueBy = describe "verification failures (MultiApply)" do
    it "Error when multi-case stub _getValueBy expects call but getValueBy is never executed" do
      (runMockT @IO do
        _getValueBy (do onCase $ "a" |> pure @IO "ax")
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getValueBy")

specFunDepsVerificationFailureDetection ::
  ( ParamThreeMonad Int Bool (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Int -> Bool -> IO String) (Param Int :> Param Bool)) => params -> MockT IO (Int -> Bool -> IO String)) ->
  (forall params. (MockBuilder params (IO Int) ()) => params -> MockT IO (IO Int)) ->
  (forall params. (MockBuilder params (IO Bool) ()) => params -> MockT IO (IO Bool)) ->
  Spec
specFunDepsVerificationFailureDetection _fnParam3_1 _fnParam3_2 _fnParam3_3 = describe "verification failures (ParamThreeMonad)" do
    it "Error when FunDep stub _fnParam3_1 expects call but fnParam3_1 is never executed" do
      (runMockT @IO do
        _fnParam3_1 (do
               onCase $ (1 :: Int) |> True |> pure @IO "combined")
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_1")

    it "Error when FunDep stub _fnParam3_2 expects call but fnParam3_2 is never executed" do
      (runMockT @IO do
        _fnParam3_2 (casesIO [1 :: Int])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_2")

    it "Error when FunDep stub _fnParam3_3 expects call but fnParam3_3 is never executed" do
      (runMockT @IO do
        _fnParam3_3 (casesIO [True])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_3")

specExplicitReturnVerificationFailureDetection ::
  ( ExplicitlyReturnMonadicValuesTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specExplicitReturnVerificationFailureDetection _getByExplicit _echoExplicit = describe "verification failures (ExplicitReturn)" do
    it "Error when explicit stub _getByExplicit expects call but getByExplicit is never executed" do
      (runMockT @IO do
        _getByExplicit ("key" |> pure @IO (42 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getByExplicit")

    it "Error when explicit stub _echoExplicit expects call but echoExplicit is never executed" do
      (runMockT @IO do
        _echoExplicit ("value" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_echoExplicit")

specAdvancedTypesVerificationFailureDetection ::
  ( DefaultMethodTest (MockT IO)
  , AssocTypeTest (MockT IO)
  ) =>
  (Int -> MockT IO Int) ->
  (Int -> MockT IO Int) ->
  Spec
specAdvancedTypesVerificationFailureDetection _defaultAction _produce = describe "verification failures (Default/Assoc)" do
    it "Error when default method stub _defaultAction expects call but defaultAction is never executed" do
      (runMockT @IO do
        _defaultAction (99 :: Int)
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_defaultAction")

    it "Error when associated type stub _produce expects call but produce is never executed" do
      (runMockT @IO do
        _produce (321 :: Int)
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_produce")

specSequentialStubbingVerificationFailureDetection ::
  ( Teletype (MockT IO)
  ) =>
  (forall params. (MockBuilder params (IO String) ()) => params -> MockT IO (IO String)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specSequentialStubbingVerificationFailureDetection _readTTY _writeTTY = describe "verification failures (TTY)" do
    it "Error when sequential stub _readTTY expects call but readTTY is never executed" do
      (runMockT @IO do
        _readTTY (casesIO ["a", ""])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_readTTY")

    it "Error when sequential stub _writeTTY expects call but writeTTY is never executed" do
      (runMockT @IO do
        _writeTTY ("a" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_writeTTY")