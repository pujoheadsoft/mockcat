{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.MockCat.ShouldBeCalledSpec (spec) where

import GHC.IO (evaluate)
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "shouldBeCalled API" do
    describe "Simple verification (arguments only, at least once)" do
      it "single argument" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` "a"

      it "multiple arguments" do
        f <- mock $ "a" |> "b" |> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` ("a" |> "b")

      it "failure case" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` "b" `shouldThrow` anyErrorCall

      it "single argument with param" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` param "a"

      it "single argument with any" do
        f <- mock $ any |> True
        evaluate $ f "a"
        f `shouldBeCalled` any

      it "single argument with expect" do
        f <- mock $ any |> True
        evaluate $ f "a"
        f `shouldBeCalled` expect (const True) "always true"

    describe "Simple verification without arguments (anything)" do
      it "success" do
        f <- mock $ any |> True
        evaluate $ f "a"
        f `shouldBeCalled` anything

      it "failure (never called)" do
        f <- mock $ "a" |> True
        f `shouldBeCalled` anything `shouldThrow` anyErrorCall

    describe "Count verification with arguments" do
      it "times - exact count" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` "a")

      it "times - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "atLeast - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a")

      it "atLeast - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "atMost - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 3 `withArgs` "a")

      it "atMost - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 2 `withArgs` "a") `shouldThrow` anyErrorCall

      it "greaterThan - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a")

      it "greaterThan - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a") `shouldThrow` anyErrorCall

      it "lessThan - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 4 `withArgs` "a")

      it "lessThan - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "once - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (once `withArgs` "a")

      it "once - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (once `withArgs` "a") `shouldThrow` anyErrorCall

      it "never - success" do
        f <- mock $ "a" |> True
        f `shouldBeCalled` (never `withArgs` "a")

      it "never - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (never `withArgs` "a") `shouldThrow` anyErrorCall

    describe "Count verification with Param arguments" do
      it "times with param" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` param "a")

      it "times with any" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (times 3 `withArgs` any)

      it "times with expect" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (times 3 `withArgs` expect (const True) "always true")

    describe "Count verification without arguments (times only)" do
      it "times - success" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` times 3

      it "times - failure" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        f `shouldBeCalled` times 3 `shouldThrow` anyErrorCall

    describe "Order verification" do
      it "inOrderWith - success" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inOrderWith ["a", "b", "c"]

      it "inOrderWith - failure" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inOrderWith ["a", "b", "b"] `shouldThrow` anyErrorCall

      it "inPartialOrderWith - success" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inPartialOrderWith ["a", "c"]

      it "inPartialOrderWith - failure" do
        f <- mock $ any |> ()
        evaluate $ f "b"
        evaluate $ f "a"
        f `shouldBeCalled` inPartialOrderWith ["a", "b"] `shouldThrow` anyErrorCall

    describe "Monadic mocks" do
      it "shouldBeCalled with IO mock" do
        f <- mock $ "a" |> (1 :: Int) |> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` ("a" |> (1 :: Int))

      it "shouldBeCalled times with IO mock" do
        f <- mock $ "a" |> (1 :: Int) |> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (times 3 `withArgs` ("a" |> (1 :: Int)))

      it "shouldBeCalled inOrderWith with IO mock" do
        f <- mock $ any |> any |> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inOrderWith ["a" |> (1 :: Int), "b" |> (2 :: Int), "c" |> (3 :: Int)]

