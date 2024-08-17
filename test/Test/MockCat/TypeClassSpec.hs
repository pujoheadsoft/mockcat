{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.State

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

class Monad m => ApiOperation m where
  post :: Text -> m ()

program ::
  (MonadReader String m, FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  e <- ask
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post $ modifiedContent <> pack ("+" <> e)

instance (Monad m) => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- get
    let
      mock = fromMaybe (error "no answer found stub function `readFile`.") $ findParam (Proxy :: Proxy "readFile") defs
      !result = stubFn mock path
    pure result

  writeFile path content = MockT do
    defs <- get
    let
      mock = fromMaybe (error "no answer found stub function `writeFile`.") $ findParam (Proxy :: Proxy "writeFile") defs
      !result = stubFn mock path content
    pure result

instance Monad m => ApiOperation (MockT m) where
  post content = MockT do
    defs <- get
    let
      mock = fromMaybe (error "no answer found stub function `post`.") $ findParam (Proxy :: Proxy "post") defs
      !result = stubFn mock content
    pure result

instance Monad m => MonadReader String (MockT m) where
  ask = MockT do
    defs <- get
    let
      mock = fromMaybe (error "no answer found stub function `ask`.") $ findParam (Proxy :: Proxy "ask") defs
      !result = stubFn mock
    pure result
  local = undefined

_ask :: Monad m => params -> MockT m ()
_ask p = MockT $ do
  modify (++ [Definition
    (Proxy :: Proxy "ask")
    (unsafePerformIO $ createNamedConstantMock "ask" p)
    shouldApplyAnythingTo])

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), Monad m) => params -> MockT m ()
_readFile p = MockT $ do
  modify (++ [Definition
    (Proxy :: Proxy "readFile")
    (unsafePerformIO $ createNamedMock "readFile" p)
    shouldApplyAnythingTo])

_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), Monad m) => params -> MockT m ()
_writeFile p = MockT $ modify (++ [Definition
  (Proxy :: Proxy "writeFile")
  (unsafePerformIO $ createNamedMock "writeFile" p)
  shouldApplyAnythingTo])

_post :: (MockBuilder params (Text -> ()) (Param Text), Monad m) => params -> MockT m ()
_post p = MockT $ modify (++ [Definition
  (Proxy :: Proxy "post")
  (unsafePerformIO $ createNamedMock "post" p)
  shouldApplyAnythingTo])

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

spec :: Spec
spec = it "Read, edit, and output files" do
  modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

  result <- runMockT do
    _ask "environment"
    _readFile ("input.txt" |> pack "content")
    _writeFile $ "output.text" |> pack "modifiedContent" |> ()
    _post $ pack "modifiedContent+environment" |> ()
    program "input.txt" "output.text" modifyContentStub

  result `shouldBe` ()
