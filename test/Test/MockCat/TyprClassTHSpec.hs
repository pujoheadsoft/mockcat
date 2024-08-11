{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.MockCat.TyprClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack)
import Test.Hspec
import Test.MockCat
import Control.Monad.State

class (Monad m) => FileOperation m where
  writeFile :: FilePath -> Text -> m ()
  readFile :: FilePath -> m Text

class (Monad m) => ApiOperation m where
  post :: Text -> m ()

program ::
  (FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post modifiedContent

makeMockWithOptions [t|FileOperation|] options { prefix = "_" }
makeMock [t|ApiOperation|]

spec :: Spec
spec = it "Read, edit, and output files" do
  modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

  result <- runMockT do
    _readFile [
      "input.txt" |> pack "content",
      "hoge.txt" |> pack "content"
      ]
    _writeFile $ "output.text" |> pack "modifiedContent" |> ()
    _post $ pack "modifiedContent" |> ()
    program "input.txt" "output.text" modifyContentStub

  result `shouldBe` ()

class (MonadIO m, MonadState String n) => MonadX m n where
  xxx :: String -> m ()

class (Eq s, Show s, MonadState s m) => MonadStateSub s m where
  fn_state :: String -> m ()

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

makeMock [t|MonadStateSub|]
makeMock [t|MonadStateSub2|]
makeMock [t|MonadVar2_1Sub|]
makeMock [t|MonadVar2_2Sub|]
makeMock [t|MonadVar3_1Sub|]
makeMock [t|MonadVar3_2Sub|]
makeMock [t|MonadVar3_3Sub|]