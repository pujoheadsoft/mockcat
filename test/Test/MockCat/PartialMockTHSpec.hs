{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockCat.PartialMockTHSpec where

import Data.Text (pack)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat
import Test.MockCat.Definition
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Reader (ReaderT(..))

class Monad m => Finder a b m | a -> b, b -> a where
  findIds :: m [a]
  findById :: a -> m b

findValue :: Finder a b m => m [b]
findValue = do
  ids <- findIds
  mapM findById ids

makePartialMock [t|Finder|]
makePartialMock [t|FileOperation|]

instance Finder Int String IO where
  findIds = pure [1, 2, 3]
  findById id = pure $ "{id: " <> show id <> "}"

instance Finder String Bool IO where
  findIds = pure ["1", "2", "3"]
  findById id = pure $ id == "2"

spec :: Spec
spec = do
  describe "MultiParamType" do
    it "all real function" do
      values <- runMockT findValue
      values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

    -- it "partial 1" do
    --   values <- runMockT  do
    --     _findIds [1, 2, 3]
    --     findValue
    --   values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

    it "partial 1" do
      values <- runMockT  do
        _findById [
          (1 :: Int) |> "id1",
          (2 :: Int) |> "id2",
          (3 :: Int) |> "id3"
          ]
        findValue
      values `shouldBe` ["id1", "id2", "id3"]

  describe "Partial Mock Test (TH)" do
    it "MaybeT" do
      result <- runMaybeT do
        runMockT do
          _writeFile $ "output.text" |> pack "MaybeT content" |> ()
          program "input.txt" "output.text"

      result `shouldBe` Just ()

    it "IO" do
      result <- runMockT do
        _writeFile $ "output.text" |> pack "IO content" |> ()
        program "input.txt" "output.text"

      result `shouldBe` ()

    it "ReaderT" do
      result <- flip runReaderT "foo" do
        runMockT do
          _writeFile $ "output.text" |> pack "ReaderT content foo" |> ()
          program "input.txt" "output.text"

      result `shouldBe` ()