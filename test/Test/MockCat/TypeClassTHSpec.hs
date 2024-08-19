{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (Text, pack, isInfixOf)
import Test.Hspec
import Test.MockCat (makeMock, makeMockWithOptions, createStubFn, options, MockOptions(..), runMockT, (|>), any, applyTimesIs, neverApply)
import Control.Monad.State
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad (unless)

class Monad m => FileOperation m where
  writeFile :: FilePath -> Text -> m ()
  readFile :: FilePath -> m Text

class Monad m => ApiOperation m where
  post :: Text -> m ()

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

class (Eq s, Show s, MonadState s m) => MonadStateSub s m where
  fn_state :: Maybe s -> m s

class (MonadState String m) => MonadStateSub2 s m where
  fn_state2 :: String -> m ()

class Monad m => MonadVar2_1 m a where
class MonadVar2_1 m a => MonadVar2_1Sub m a where
  fn2_1Sub :: String -> m ()

class Monad m => MonadVar2_2 a m where
class MonadVar2_2 a m => MonadVar2_2Sub a m where
  fn2_2Sub :: String -> m ()

class Monad m => MonadVar3_1 m a b where
class MonadVar3_1 m a b => MonadVar3_1Sub m a b where
  fn3_1Sub :: String -> m ()

class Monad m => MonadVar3_2 a m b where
class MonadVar3_2 a m b => MonadVar3_2Sub a m b where
  fn3_2Sub :: String -> m ()

class Monad m => MonadVar3_3 a b m where
class MonadVar3_3 a b m => MonadVar3_3Sub a b m where
  fn3_3Sub :: String -> m ()

--makeMock [t|MonadReader Bool|]
makeMock [t|MonadReader Environment|]
makeMock [t|MonadState String|]
makeMock [t|MonadStateSub|]
makeMock [t|MonadStateSub2|]
makeMock [t|MonadVar2_1Sub|]
makeMock [t|MonadVar2_2Sub|]
makeMock [t|MonadVar3_1Sub|]
makeMock [t|MonadVar3_2Sub|]
makeMock [t|MonadVar3_3Sub|]
makeMock [t|FileOperation|]
makeMockWithOptions [t|ApiOperation|] options { prefix = "stub_", suffix = "_fn" }

class Monad m => ParamThreeMonad a b m | m -> a, m -> b where
  fnParam3_1 :: a -> b -> m String
  fnParam3_2 :: m a
  fnParam3_3 :: m b

makeMock [t|ParamThreeMonad String Bool|]

monadStateSubExec :: MonadStateSub String m => String -> m String
monadStateSubExec s = do
  r <- fn_state (pure "X")
  pure $ s <> r

threeParamMonadExec :: ParamThreeMonad String Bool m => m String
threeParamMonadExec = do
  v1 <- fnParam3_1 "foo" True
  v2 <- fnParam3_2
  v3 <- fnParam3_3
  pure $ v1 <> v2 <> show v3

spec :: Spec
spec = do
  it "Read, and output files" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.txt" |> pack "content" |> ())
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
  
  it "MonadState subclass mock" do
    r <- runMockT do
      _fn_state $ Just "X" |> "Y"
      monadStateSubExec "foo"
    r `shouldBe` "fooY"

  it "3 param Moand mock" do
    r <- runMockT do
      _fnParam3_1 $ "foo" |> True |> "Result1"
      _fnParam3_2 "Result2"
      _fnParam3_3 False
      threeParamMonadExec
    r `shouldBe` "Result1Result2False"