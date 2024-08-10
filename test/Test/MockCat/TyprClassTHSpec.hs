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
import Data.Data
import Data.Maybe
import GHC.TypeLits
import Data.List
import Unsafe.Coerce

class Monad m => Monad2 m where
class (Monad2 m, Eq n) => Monad1 m n where
class Monad1 m n => Monad0 m a n where
-- class (Eq a, Monad0 m a n) => Clazz m a n where
-- --class (Eq a) => Clazz a where
--   xxx :: String -> m String

-- instance (Eq a, Monad0 (MockT m) a n) => Clazz (MockT m) a n where
--   xxx = undefined
class (MonadState String m, Eq a) => Moge m a where
  xxx :: String -> m ()

-- instance (MonadState String (MockT m), Eq a, Monad m) => Moge (MockT m) a where
--   xxx path = MockT do
--     defs <- get
--     let
--       mock = fromMaybe (error "no answer found stub function `readFile`.") $ findParam (Proxy :: Proxy "readFile") defs
--       !result = stubFn mock path
--     pure result

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition
-- class (Monad0 m a n, Eq a, MonadReader a m) => Hoge m a n where
--   hoge :: String -> m ()
--   --hage :: a -> m String
--   moge :: String -> m ()

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

makeMock [t|Moge|]
makeMock [t|FileOperation|]
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