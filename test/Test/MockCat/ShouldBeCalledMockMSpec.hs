{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test.MockCat.ShouldBeCalledMockMSpec (spec) where

import Control.Exception (ErrorCall(..), try)
import Control.Monad (replicateM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = describe "shouldBeCalled API (mockM)" $ do
  describe "Simple verification" do
    it "records arguments and results" do
      f <- mockM $ "a" |> True
      result <- f "a"
      result `shouldBe` True
      f `shouldBeCalled` "a"

    it "fails when called with unexpected argument" do
      f <- mockM $ "a" |> True
      void $ f "a"
      f `shouldBeCalled` "b" `shouldThrow` anyErrorCall

    it "anything expectation succeeds" do
      f <- mockM $ any @String |> True
      void $ f "x"
      f `shouldBeCalled` anything

    it "anything expectation fails when never called" do
      f <- mockM $ any @String |> True
      f `shouldBeCalled` anything `shouldThrow` anyErrorCall

    it "never expectation" do
      f <- mockM $ any @String |> True
      f `shouldBeCalled` (never `withArgs` "z")
      void $ f "a"
      f `shouldBeCalled` (never `withArgs` "a") `shouldThrow` anyErrorCall

  describe "Count verification" do
    it "times succeeds" do
      f <- mockM $ "a" |> True
      replicateM_ 3 (void $ f "a")
      f `shouldBeCalled` (times 3 `withArgs` "a")

    it "times fails" do
      f <- mockM $ "a" |> True
      replicateM_ 2 (void $ f "a")
      f `shouldBeCalled` (times 3 `withArgs` "a") `shouldThrow` anyErrorCall

    it "atLeast / atMost" do
      f <- mockM $ "a" |> True
      replicateM_ 3 (void $ f "a")
      f `shouldBeCalled` (atLeast 2 `withArgs` "a")
      f `shouldBeCalled` (atMost 3 `withArgs` "a")
      f `shouldBeCalled` (atMost 2 `withArgs` "a") `shouldThrow` anyErrorCall

    it "greaterThan / lessThan" do
      f <- mockM $ "a" |> True
      replicateM_ 3 (void $ f "a")
      f `shouldBeCalled` (greaterThan 2 `withArgs` "a")
      f `shouldBeCalled` (lessThan 4 `withArgs` "a")
      f `shouldBeCalled` (lessThan 3 `withArgs` "a") `shouldThrow` anyErrorCall

  describe "Multiple arguments" do
    it "works with Param combinators" do
      f <- mockM $ any @String |> any @String |> True
      void $ f "x" "y"
      f `shouldBeCalled` (any @String |> any @String)

    it "supports explicit Param values" do
      f <- mockM $ param "x" |> param "y" |> True
      void $ f "x" "y"
      f `shouldBeCalled` ("x" |> "y")
      f `shouldBeCalled` ("y" |> "x") `shouldThrow` anyErrorCall

    it "accepts cases blocks" do
      f <- mockM $ cases
        [ "hello" |> True
        , "world" |> False
        ]
      void $ f "hello"
      result <- f "world"
      result `shouldBe` False
      f `shouldBeCalled` ("hello" :: String)
      f `shouldBeCalled` ("world" :: String)

  describe "Order verification" do
    it "validates inOrderWith" do
      f <- mockM $ any @String |> ()
      void $ f "first"
      void $ f "second"
      f `shouldBeCalled` inOrderWith ["first", "second"]
      f `shouldBeCalled` inOrderWith ["second", "first"] `shouldThrow` anyErrorCall

    it "validates inPartialOrderWith" do
      f <- mockM $ any @String |> ()
      void $ f "alpha"
      void $ f "beta"
      void $ f "gamma"
      f `shouldBeCalled` inPartialOrderWith ["alpha", "gamma"]
      f `shouldBeCalled` inPartialOrderWith ["gamma", "alpha"] `shouldThrow` anyErrorCall

  describe "named mocks and errors" do
    it "reports names in error messages" do
      f <- mockM (label "named" :: Label) $ "a" |> True
      result <- f "a"
      result `shouldBe` True
      e <- try (f `shouldBeCalled` "b") :: IO (Either ErrorCall ())
      case e of
        Left (ErrorCall msg) -> msg `shouldContain` "named"
        Right _ -> expectationFailure "expected an error"

  describe "integration with transformer stacks" do
    it "works inside ReaderT IO" do
      runReaderT @() @IO
        (do
          f <- mockM $ any @String |> True
          void $ f "lifted"
          void $ f "lifted"
          liftIO $ f `shouldBeCalled` (times 2 `withArgs` "lifted"))
        ()

