{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (Text, pack, isInfixOf)
import Test.Hspec
import Test.MockCat
import Control.Monad.State
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad (unless)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Control.Concurrent.Async (async, wait)
import Test.MockCat.SharedSpecDefs

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

data Environment = Environment { inputPath :: String, outputPath :: String }

operationProgram3 ::
  MonadReader Environment m =>
  FileOperation m =>
  m ()
operationProgram3 = do
  (Environment inputPath outputPath) <- ask
  content <- readFile inputPath
  writeFile outputPath content


--makeMock [t|MonadReader Bool|]
makeMock [t|MonadReader Environment|]
makeMock [t|MonadVar2_1Sub|]
makeMock [t|MonadVar2_2Sub|]
makeMock [t|MonadVar3_1Sub|]
makeMock [t|MonadVar3_2Sub|]
makeMock [t|MonadVar3_3Sub|]
makeMock [t|FileOperation|]
makeMockWithOptions [t|ApiOperation|] options { prefix = "stub_", suffix = "_fn" }

makeMock [t|MultiApplyTest|]
  
echoProgram :: ExplicitlyReturnMonadicValuesTest m => String -> m ()
echoProgram s = do
  v <- getByExplicit s
  echoExplicit $ show v

makeMockWithOptions [t|ExplicitlyReturnMonadicValuesTest|] options { implicitMonadicReturn = False }
makeMock [t|DefaultMethodTest|]
makeMock [t|AssocTypeTest|]

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

instance AssocTypeTest IO where
  type ResultType IO = Int
  produce = pure 0

processFiles :: MonadAsync m => FileOperation m => [FilePath] -> m [Text]
processFiles = mapConcurrently readFile

spec :: Spec
spec = do
  it "Read, and output files" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.txt" |> pack "content" |> ()
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (contain ng word)" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      _writeFile ("output.txt" |> any |> ()) `applyTimesIs` 0
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (contain ng word)2" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "contains ngWord")
      neverApply $ _writeFile ("output.txt" |> any |> ())
      operationProgram "input.txt" "output.txt"

    result `shouldBe` ()

  it "Read, and output files (with MonadReader)" do
    r <- runMockT do
      _ask (Environment "input.txt" "output.txt")
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
      operationProgram3
    r `shouldBe` ()


  it "Read, edit, and output files2" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      stub_post_fn (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
  
  
  it "Multi apply" do
    result <- runMockT do
      _getValueBy $ do
        onCase $ "a" |> "ax"
        onCase $ "b" |> "bx"
        onCase $ "c" |> "cx"
      getValues ["a", "b", "c"]
    result `shouldBe` ["ax", "bx", "cx"]

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

