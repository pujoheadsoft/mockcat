{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.PartialMockSpec where

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
import Control.Monad.Reader (MonadReader, runReaderT, ask, local)
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader hiding (ask)

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

program2 ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program2 inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content

instance FileOperation IO where
  readFile _ = pure $ pack "IO content"
  writeFile _ _ = pure ()

instance Monad m => FileOperation (MaybeT m) where
  readFile _ = pure $ pack "MaybeT content"
  writeFile _ _ = undefined

instance Monad m => FileOperation (ReaderT String m) where
  readFile _ = do
    e <- ask
    pure $ pack "ReaderT content " <> pack e
  writeFile _ _ = undefined

instance (Monad m, FileOperation m) => FileOperation (MockT m) where
  readFile path = lift $ readFile path

  writeFile path content = MockT do
    defs <- get
    let
      mock = fromMaybe (error "no answer found stub function `writeFile`.") $ findParam (Proxy :: Proxy "writeFile") defs
      !result = stubFn mock path content
    pure result

_readFile :: (MockBuilder params (FilePath -> Text) (Param FilePath), Monad m) => params -> MockT m ()
_readFile p = MockT $ do
  modify (++ [Definition
    (Proxy :: Proxy "readFile")
    (unsafePerformIO $ createNamedMock "readFile" p)
    shouldApplyToAnything])

_writeFile :: (MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text), Monad m) => params -> MockT m ()
_writeFile p = MockT $ modify (++ [Definition
  (Proxy :: Proxy "writeFile")
  (unsafePerformIO $ createNamedMock "writeFile" p)
  shouldApplyToAnything])

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

spec :: Spec
spec = do
  it "2" do
    result <- runMaybeT do
      runMockT do
        _writeFile $ "output.text" |> pack "MaybeT content" |> ()
        program2 "input.txt" "output.text"

    result `shouldBe` Just ()

  it "3" do
    result <- runMockT do
      _writeFile $ "output.text" |> pack "IO content" |> ()
      program2 "input.txt" "output.text"

    result `shouldBe` ()

  it "4" do
    result <- flip runReaderT "foo" do
      runMockT do
        _writeFile $ "output.text" |> pack "ReaderT content foo" |> ()
        program2 "input.txt" "output.text"

    result `shouldBe` ()