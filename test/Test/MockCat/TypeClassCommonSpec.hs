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
import Data.Typeable (Typeable)
import Test.Hspec
import Test.MockCat
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Internal.Types (Verifier)
import Test.MockCat.Cons (Head(..), (:>)(..))
import Test.MockCat.Param (param)
import Data.Text (Text, pack, isInfixOf)
import Control.Exception (ErrorCall(..), displayException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState(..), StateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Test.MockCat.SharedSpecDefs
import qualified Data.List as List
import Control.Concurrent.Async (async, wait)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Control.Monad (unless)

-- Helpers

missingCall :: String -> Selector ErrorCall
missingCall name err =
  let needle = "function `" <> name <> "` was not applied the expected number of times."
   in needle `List.isInfixOf` displayException err

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

specFileOperation ::
  ( FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specFileOperation _readFile _writeFile = do
  it "Read, and output files (operationProgram)" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.txt" |> pack "content" |> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()
    
  it "Read, and output files (contain ng word)" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      _writeFile ("output.txt" |> any |> ())
        `expects` do
          called never
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

specFileOperationApi ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specFileOperationApi _readFile _writeFile _post = do
  it "Read, edit, and output files2 (operationProgram2)" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      _post (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

specFileOperationReaderEnvironment ::
  ( FileOperation (MockT IO)
  , ApiOperation (MockT IO)
  , MonadReader Environment (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specFileOperationReaderEnvironment _ask _readFile _writeFile _post = do
  it "Read, edit, and output files (apiFileOperationProgram)" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"
    let env = Environment "input.txt" "output.text"

    result <- runMockT do
      _ask env
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.text" |> pack "modifiedContent" |> ())
      _post ((pack "modifiedContent" <> pack ("+" <> show env)) |> ())
      apiFileOperationProgram modifyContentStub

    result `shouldBe` ()

specApiRenaming ::
  ( ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specApiRenaming _stubPost = do
  it "supports makeMockWithOptions prefix and suffix" do
    result <- runMockT do
      _stubPost (pack "payload" |> ())
      post (pack "payload")
    result `shouldBe` ()

specMonadReaderEnvironment :: 
  ( MonadReader Environment (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specMonadReaderEnvironment _ask _readFile _writeFile = do
  it "Read, and output files (with MonadReader Environment)" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram3
    r `shouldBe` ()

specEcho ::
  ( Teletype (MockT IO)
  ) =>
  (forall params. (MockBuilder params (IO String) ()) => params -> MockT IO (IO String)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specEcho _readTTY _writeTTY = do
  it "echo (readTTY/writeTTY)" do
    result <- runMockT do
      _readTTY $ casesIO ["a", ""]
      _writeTTY $ "a" |> pure @IO ()
      echo2
    result `shouldBe` ()

specTestClass ::
  ( TestClass (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> Int) (Param String)) => params -> MockT IO (String -> Int)) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  Spec
specTestClass _getBy _echo = do
  it "return monadic value test" do
    result <- runMockT do
      _getBy $ "s" |> (10 :: Int)
      _echo $ "10" |> ()
      echoProgram "s"

    result `shouldBe` ()

specMultiApply ::
  ( MultiApplyTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> String) (Param String)) => params -> MockT IO (String -> String)) ->
  Spec
specMultiApply _getValueBy = do
  it "multi apply collects all results" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" |> "ax"
        onCase $ "b" |> "bx"
        onCase $ "c" |> "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

specMonadState ::
  ( MonadStateSub String (MockT (StateT String IO))
  , MonadStateSub2 String (MockT (StateT String IO))
  ) =>
  (forall params. (MockBuilder params (Maybe String -> StateT String IO String) (Param (Maybe String))) => params -> MockT (StateT String IO) (Maybe String -> StateT String IO String)) ->
  (forall params. (MockBuilder params (String -> StateT String IO ()) (Param String)) => params -> MockT (StateT String IO) (String -> StateT String IO ())) ->
  Spec
specMonadState _fnState _fnState2 = do
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

specSubVars ::
  ( MonadVar2_1Sub (MockT IO) String
  , MonadVar2_2Sub String (MockT IO)
  , MonadVar3_1Sub (MockT IO) String String
  , MonadVar3_2Sub String (MockT IO) String
  , MonadVar3_3Sub String String (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  Spec
specSubVars _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub = do
  it "supports MonadVar2_1Sub pattern" do
    result <- runMockT do
      _fn2_1Sub $ "alpha" |> ()
      fn2_1Sub @(MockT IO) @String "alpha"
    result `shouldBe` ()

  it "supports MonadVar2_2Sub pattern" do
    result <- runMockT do
      _fn2_2Sub $ "beta" |> ()
      fn2_2Sub @String @(MockT IO) "beta"
    result `shouldBe` ()

  it "supports MonadVar3_1Sub pattern" do
    result <- runMockT do
      _fn3_1Sub $ "gamma" |> ()
      fn3_1Sub @(MockT IO) @String @String "gamma"
    result `shouldBe` ()

  it "supports MonadVar3_2Sub pattern" do
    result <- runMockT do
      _fn3_2Sub $ "delta" |> ()
      fn3_2Sub @String @(MockT IO) @String "delta"
    result `shouldBe` ()

  it "supports MonadVar3_3Sub pattern" do
    result <- runMockT do
      _fn3_3Sub $ "epsilon" |> ()
      fn3_3Sub @String @String @(MockT IO) "epsilon"
    result `shouldBe` ()

specParamThreeMonad ::
  ( ParamThreeMonad Int Bool (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Int -> Bool -> String) (Param Int :> Param Bool)) => params -> MockT IO (Int -> Bool -> String)) ->
  (forall params. (MockBuilder params (IO Int) ()) => params -> MockT IO (IO Int)) ->
  (forall params. (MockBuilder params (IO Bool) ()) => params -> MockT IO (IO Bool)) ->
  Spec
specParamThreeMonad _fnParam3_1 _fnParam3_2 _fnParam3_3 = do
  it "supports ParamThreeMonad functional dependencies" do
    result <- runMockT $ do
      _fnParam3_1 $ do
        onCase $ (1 :: Int) |> True |> "combined"
      _fnParam3_2 $ casesIO [1 :: Int]
      _fnParam3_3 $ casesIO [True]
      r1 <- fnParam3_1 (1 :: Int) True
      r2 <- fnParam3_2
      r3 <- fnParam3_3
      pure (r1, r2, r3)
    result `shouldBe` ("combined", 1, True)

specExplicitReturn ::
  ( ExplicitlyReturnMonadicValuesTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specExplicitReturn _getByExplicit _echoExplicit = do
  it "supports ExplicitlyReturnMonadicValuesTest pattern" do
    result <- runMockT do
      _getByExplicit $ "key" |> pure @IO (42 :: Int)
      _echoExplicit $ "value" |> pure @IO ()
      v <- getByExplicit "key"
      echoExplicit "value"
      pure v
    result `shouldBe` 42
  
  it "Return monadic value test (using helper)" do
    result <- runMockT do
      _getByExplicit $ "s" |> pure @IO (10 :: Int)
      _echoExplicit $ "10" |> pure @IO ()
      echoProgramExplicit "s"
    result `shouldBe` ()

specDefaultMethod ::
  ( DefaultMethodTest (MockT IO)
  ) =>
  (Int -> MockT IO Int) ->
  Spec
specDefaultMethod _defaultAction = do
  it "supports DefaultMethodTest pattern" do
    result <- runMockT do
      _defaultAction (99 :: Int)
      defaultAction
    result `shouldBe` 99

specAssocType ::
  ( AssocTypeTest (MockT IO)
  , ResultType (MockT IO) ~ Int
  ) =>
  (Int -> MockT IO Int) ->
  Spec
specAssocType _produce = do
  it "supports AssocTypeTest pattern" do
    value <- runMockT do
      _produce (321 :: Int)
      produce
    value `shouldBe` 321

specMonadAsync ::
  ( MonadAsync (MockT IO)
  , FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  Spec
specMonadAsync _readFile = do
  it "supports MonadAsync pattern (processFiles)" do
    result <- runMockT do
      _readFile $ do
        onCase $ "file1.txt" |> pack "content1"
        onCase $ "file2.txt" |> pack "content2"
      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]

  it "MonadUnliftIO instance works correctly" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" |> pack "content"

      content <- withRunInIO $ \runInIO -> do
        asyncAction <- async $ runInIO (readFile "test.txt")
        wait asyncAction

      liftIO $ content `shouldBe` pack "content"
      pure content

    result `shouldBe` pack "content"

  it "MonadUnliftIO basic functionality" do
    result <- runMockT do
      _readFile $ do
        onCase $ "test.txt" |> pack "test content"
      
      content <- withRunInIO $ \runInIO -> do
        runInIO (readFile "test.txt")
      
      liftIO $ content `shouldBe` pack "test content"
      pure content

    result `shouldBe` pack "test content"

-- Verification Failure Tests

specVerifyFailureFileOp ::
  ( FileOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (FilePath -> Text) (Param FilePath)) => params -> MockT IO (FilePath -> Text)) ->
  (forall params. (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)) => params -> MockT IO (FilePath -> Text -> ())) ->
  Spec
specVerifyFailureFileOp _readFile _writeFile = describe "verification failures (FileOperation)" do
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

specVerifyFailureApi ::
  ( ApiOperation (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Text -> ()) (Param Text)) => params -> MockT IO (Text -> ())) ->
  Spec
specVerifyFailureApi _post = describe "verification failures (Api)" do
    it "fails when _post is defined but post is never called" do
      (runMockT @IO do
        _ <- _post (pack "content" |> ())
          `expects` do
            called once
        -- post is never called
        pure ()) `shouldThrow` (missingCall "post")

specVerifyFailureReaderEnvironment ::
  ( MonadReader Environment (MockT IO)
  ) =>
  (Environment -> MockT IO Environment) ->
  Spec
specVerifyFailureReaderEnvironment _ask = describe "verification failures (Reader Environment)" do
    it "fails when _ask is defined but ask is never called" do
      (runMockT @IO do
        _ <- _ask (Environment "input" "output")
          `expects` do
            called once
        -- ask is never called
        pure ()) `shouldThrow` (missingCall "ask")

specVerifyFailureTestClass ::
  ( TestClass (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> Int) (Param String)) => params -> MockT IO (String -> Int)) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  Spec
specVerifyFailureTestClass _getBy _echo = describe "verification failures (TestClass)" do
    it "fails when _getBy is defined but getBy is never called" do
      (runMockT @IO do
        _getBy ("s" |> (10 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getBy")

    it "fails when _echo is defined but echo is never called" do
      (runMockT @IO do
        _echo ("10" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_echo")

specVerifyFailureSubVars ::
  ( MonadVar2_1Sub (MockT IO) String
  , MonadVar2_2Sub String (MockT IO)
  , MonadVar3_1Sub (MockT IO) String String
  , MonadVar3_2Sub String (MockT IO) String
  , MonadVar3_3Sub String String (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  (forall params. (MockBuilder params (String -> ()) (Param String)) => params -> MockT IO (String -> ())) ->
  Spec
specVerifyFailureSubVars _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub = describe "verification failures (SubVars)" do
    it "fails when _fn2_1Sub is defined but fn2_1Sub is never called" do
      (runMockT @IO do
        _fn2_1Sub ("alpha" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn2_1Sub")

    it "fails when _fn2_2Sub is defined but fn2_2Sub is never called" do
      (runMockT @IO do
        _fn2_2Sub ("beta" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn2_2Sub")

    it "fails when _fn3_1Sub is defined but fn3_1Sub is never called" do
      (runMockT @IO do
        _fn3_1Sub ("gamma" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_1Sub")

    it "fails when _fn3_2Sub is defined but fn3_2Sub is never called" do
      (runMockT @IO do
        _fn3_2Sub ("delta" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_2Sub")

    it "fails when _fn3_3Sub is defined but fn3_3Sub is never called" do
      (runMockT @IO do
        _fn3_3Sub ("epsilon" |> ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fn3_3Sub")

specVerifyFailureMultiApply ::
  ( MultiApplyTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> String) (Param String)) => params -> MockT IO (String -> String)) ->
  Spec
specVerifyFailureMultiApply _getValueBy = describe "verification failures (MultiApply)" do
    it "fails when _getValueBy is defined but getValueBy is never called" do
      (runMockT @IO do
        _getValueBy (do onCase $ "a" |> "ax")
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getValueBy")

specVerifyFailureParam3 ::
  ( ParamThreeMonad Int Bool (MockT IO)
  ) =>
  (forall params. (MockBuilder params (Int -> Bool -> String) (Param Int :> Param Bool)) => params -> MockT IO (Int -> Bool -> String)) ->
  (forall params. (MockBuilder params (IO Int) ()) => params -> MockT IO (IO Int)) ->
  (forall params. (MockBuilder params (IO Bool) ()) => params -> MockT IO (IO Bool)) ->
  Spec
specVerifyFailureParam3 _fnParam3_1 _fnParam3_2 _fnParam3_3 = describe "verification failures (ParamThreeMonad)" do
    it "fails when _fnParam3_1 is defined but fnParam3_1 is never called" do
      (runMockT @IO do
        _fnParam3_1 (do
               onCase $ (1 :: Int) |> True |> "combined")
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_1")

    it "fails when _fnParam3_2 is defined but fnParam3_2 is never called" do
      (runMockT @IO do
        _fnParam3_2 (casesIO [1 :: Int])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_2")

    it "fails when _fnParam3_3 is defined but fnParam3_3 is never called" do
      (runMockT @IO do
        _fnParam3_3 (casesIO [True])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_fnParam3_3")

specVerifyFailureExplicit ::
  ( ExplicitlyReturnMonadicValuesTest (MockT IO)
  ) =>
  (forall params. (MockBuilder params (String -> IO Int) (Param String)) => params -> MockT IO (String -> IO Int)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specVerifyFailureExplicit _getByExplicit _echoExplicit = describe "verification failures (ExplicitReturn)" do
    it "fails when _getByExplicit is defined but getByExplicit is never called" do
      (runMockT @IO do
        _getByExplicit ("key" |> pure @IO (42 :: Int))
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_getByExplicit")

    it "fails when _echoExplicit is defined but echoExplicit is never called" do
      (runMockT @IO do
        _echoExplicit ("value" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_echoExplicit")

specVerifyFailureDefaultAndAssoc ::
  ( DefaultMethodTest (MockT IO)
  , AssocTypeTest (MockT IO)
  ) =>
  (Int -> MockT IO Int) ->
  (Int -> MockT IO Int) ->
  Spec
specVerifyFailureDefaultAndAssoc _defaultAction _produce = describe "verification failures (Default/Assoc)" do
    it "fails when _defaultAction is defined but defaultAction is never called" do
      (runMockT @IO do
        _defaultAction (99 :: Int)
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_defaultAction")

    it "fails when _produce is defined but produce is never called" do
      (runMockT @IO do
        _produce (321 :: Int)
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_produce")

specVerifyFailureTTY ::
  ( Teletype (MockT IO)
  ) =>
  (forall params. (MockBuilder params (IO String) ()) => params -> MockT IO (IO String)) ->
  (forall params. (MockBuilder params (String -> IO ()) (Param String)) => params -> MockT IO (String -> IO ())) ->
  Spec
specVerifyFailureTTY _readTTY _writeTTY = describe "verification failures (TTY)" do
    it "fails when _readTTY is defined but readTTY is never called" do
      (runMockT @IO do
        _readTTY (casesIO ["a", ""])
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_readTTY")

    it "fails when _writeTTY is defined but writeTTY is never called" do
      (runMockT @IO do
        _writeTTY ("a" |> pure @IO ())
          `expects` do
            called once
        pure ()) `shouldThrow` (missingCall "_writeTTY")
