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

module Test.MockCat.TyprClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack)
import Test.Hspec
import Test.MockCat
import Control.Monad.State
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad (when)

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

monadReaderProgram :: MonadReader Bool m => m String
monadReaderProgram = do
  v <- ask
  pure $ "Environment:" <> show v

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

makeMock [t|MonadReader Bool|]
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
  it "Read, edit, and output files" do
    result <- runMockT do
      _readFile ("input.txt" |> pack "content")
      _writeFile ("output.text" |> pack "content" |> ()) `appliedTimesIs` 0
      operationProgram "input.txt" "output.text"

    result `shouldBe` ()

  it "Read, edit, and output files2" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile ("output.text" |> pack "modifiedContent" |> ()) 
      stub_post_fn (pack "modifiedContent" |> ())
      operationProgram2 "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
  
  it "MonadReader mock" do
    r <- runMockT do
      _ask True
      monadReaderProgram
    r `shouldBe` "Environment:True"

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