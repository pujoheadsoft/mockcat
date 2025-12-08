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
  it "Read, and output files" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.txt" |> pack "content" |> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files" do
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

  it "Read, and output files (contain ng word)2" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      _writeFile ("output.txt" |> any |> ())
        `expects` do
          called never
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "does not auto verify TH mocks without expects" do
    ( runMockT do
        _writeFile ("output.txt" |> pack "content" |> ())
        pure ()
      )
      `shouldReturn` ()

  it "Read, and output files (with MonadReader)" do
    r <- runMockT do
      _ask (SpecCommon.Environment "input.txt" "output.text")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.text" |> pack "content" |> ())
      operationProgram3
    r `shouldBe` ()


  it "Read, edit, and output files2" do
    modifyContentStub <- mock $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      _post (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
  
  
  -- it "Multi apply" do
  --   result <- runMockT do
  --     _getValueBy $ do
  --       onCase $ "a" |> "ax"
  --       onCase $ "b" |> "bx"
  --       onCase $ "c" |> "cx"
  --     getValues ["a", "b", "c"]
  --   result `shouldBe` ["ax", "bx", "cx"]

  it "Return monadic value test" do
    result <- runMockT do
      _getByExplicit $ "s" |> pure @IO (10 :: Int)
      _echoExplicit $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()

  it "Default method can be stubbed" do
    result <- runMockT do
      _defaultAction (99 :: Int)
      defaultAction
    result `shouldBe` 99

  it "Associated type can be stubbed" do
    result <- runMockT do
      _produce (321 :: Int)
      produce
    result `shouldBe` 321

  it "MonadUnliftIO instance works correctly" do
    result <- runMockT do
      _readFile ("test.txt" |> pack "content")

      content <- withRunInIO $ \runInIO -> do
        asyncAction <- async $ runInIO (readFile "test.txt")
        wait asyncAction

      liftIO $ content `shouldBe` pack "content"
      pure content

    result `shouldBe` pack "content"

  it "MonadUnliftIO basic functionality" do
    result <- runMockT do
      _readFile ("test.txt" |> pack "test content")
      
      content <- withRunInIO $ \runInIO -> do
        runInIO (readFile "test.txt")
      
      liftIO $ content `shouldBe` pack "test content"
      pure content

    result `shouldBe` pack "test content"
  
  it "MonadAsync type class can be instantiated for MockT" do
    result <- runMockT do
      _readFile $ do
        onCase $ "file1.txt" |> pack "content1"
        onCase $ "file2.txt" |> pack "content2"

      processFiles ["file1.txt", "file2.txt"]
    result `shouldBe` [pack "content1", pack "content2"]



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



  SpecCommon.specSequentialIOStubbing _readTTY _writeTTY
  SpecCommon.specBasicStubbingAndVerification (SpecCommon.BasicDeps _readFile _writeFile )
  SpecCommon.specMixedMockingStrategies (SpecCommon.MixedDeps _readFile _writeFile _post)
  SpecCommon.specMultipleTypeclassConstraints (SpecCommon.MultipleDeps _ask _readFile _writeFile _post)
  SpecCommon.specCustomMockNamingOptions _post
  SpecCommon.specImplicitMonadicReturnValues _getBy _echo
  SpecCommon.specArgumentPatternMatching _getValueBy
  SpecCommon.specMultiParamTypeClassArity _fn2_1SubIO _fn2_2SubIO _fn3_1SubIO _fn3_2SubIO _fn3_3SubIO
  SpecCommon.specMonadStateTransformerSupport _fnState _fnState2
  SpecCommon.specFunctionalDependenciesSupport _fnParam3_1 _fnParam3_2 _fnParam3_3
  SpecCommon.specExplicitMonadicReturnValues _getByExplicit _echoExplicit
  SpecCommon.specDefaultMethodMocking _defaultAction
  SpecCommon.specAssociatedTypeFamiliesSupport _produce
  SpecCommon.specConcurrencyAndUnliftIO _readFile
  SpecCommon.specMonadReaderContextMocking _ask _readFile _writeFile

  -- -- Verification Failures
  SpecCommon.specBasicVerificationFailureDetection _readFile _writeFile
  SpecCommon.specCustomNamingVerificationFailureDetection _post
  SpecCommon.specMonadReaderVerificationFailureDetection _ask
  SpecCommon.specImplicitReturnVerificationFailureDetection _getBy _echo
  SpecCommon.specMultiParamVerificationFailureDetection _fn2_1SubIO _fn2_2SubIO _fn3_1SubIO _fn3_2SubIO _fn3_3SubIO
  SpecCommon.specArgumentMatchingVerificationFailureDetection _getValueBy
  SpecCommon.specFunDepsVerificationFailureDetection _fnParam3_1 _fnParam3_2 _fnParam3_3
  SpecCommon.specExplicitReturnVerificationFailureDetection _getByExplicit _echoExplicit
  SpecCommon.specAdvancedTypesVerificationFailureDetection _defaultAction _produce
  SpecCommon.specSequentialStubbingVerificationFailureDetection _readTTY _writeTTY

  -- describe "verification failures (State - Pending)" do
  --   it "fails when _fnState is defined but fnState is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

  --   it "fails when _fnState2 is defined but fnState2 is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

