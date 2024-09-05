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

module Test.MockCat.PartialMockTHSpec (spec) where

import Data.Text (pack)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat
import Test.MockCat.Definition
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Reader (ReaderT(..))

data UserInput = UserInput String deriving (Show, Eq)

class Monad m => UserInputGetter m where
  getInput :: m String
  toUserInput :: String -> m (Maybe UserInput)

instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a

getUserInput :: UserInputGetter m => m (Maybe UserInput)
getUserInput = do
  i <- getInput
  toUserInput i

makePartialMock [t|UserInputGetter|]
makePartialMock [t|Finder|]
makePartialMock [t|FileOperation|]

spec :: Spec
spec = do
  it "" do
    a <- runMockT do
      _getInput (Just (Just "value"))
      getUserInput
    a `shouldBe` Just (UserInput "value")

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

    describe "MultiParamType" do
      it "all real function" do
        values <- runMockT findValue
        values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

      it "partial findIds" do
        values <- runMockT  do
          _findIds [1 :: Int, 2]
          findValue
        values `shouldBe` ["{id: 1}", "{id: 2}"]

      it "partial findById" do
        values <- runMockT  do
          _findById [
            (1 :: Int) |> "id1",
            (2 :: Int) |> "id2",
            (3 :: Int) |> "id3"
            ]
          findValue
        values `shouldBe` ["id1", "id2", "id3"]
