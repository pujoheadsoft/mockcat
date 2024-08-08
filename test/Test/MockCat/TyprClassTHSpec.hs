{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module Test.MockCat.TyprClassTHSpec where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack)
import Test.MockCat.TH (makeMock)
import Test.Hspec
import Test.MockCat
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.RWS (MonadReader)

class Monad m => Monad2 m where
class (Monad2 m, Eq n) => Monad1 m n where
class Monad1 m n => Monad0 m a n where
class (Eq a, Monad0 m a n) => Clazz m a n where
  xxx :: String

--class (Monad0 m, MonadIO n, Eq a) => Moge m n a where

-- class (Monad0 m, Eq a, MonadReader a m) => Hoge m a where
--   hoge :: String -> m ()
--   hage :: a -> m String
--   moge :: String -> a

-- instance (MonadIO m, Eq a, MonadReader a (MockT m)) => Hoge (MockT m) a where
--   hoge = undefined
--   hage = undefined
--   moge = undefined

class (Monad m) => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

class (Monad m) => ApiOperation m where
  post :: Text -> m ()

-- program2 :: Hoge m String => String -> m ()
-- program2 s = do 
--   h <- hage s
--   hoge h

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

makeMock [t|Clazz|]
-- makeMock [t|FileOperation|]
-- makeMock [t|ApiOperation|]


spec :: Spec
spec = it "Read, edit, and output files" do
  "" `shouldBe` ""
  -- modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

  -- result <- runMockT do
  --   _readFile [
  --     "input.txt" |> pack "content",
  --     "hoge.txt" |> pack "content"
  --     ]
  --   _writeFile $ "output.text" |> pack "modifiedContent" |> ()
  --   _post $ pack "modifiedContent" |> ()
  --   program "input.txt" "output.text" modifyContentStub

  -- result `shouldBe` ()