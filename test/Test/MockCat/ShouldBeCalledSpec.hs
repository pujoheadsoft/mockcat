{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.MockCat.ShouldBeCalledSpec (spec) where

import qualified Control.Exception as E
import Data.Function ((&))
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
        f `shouldBeCalled` (calledWith (param "a"))

      it "multiple arguments" do
        f <- mock $ "a" |> "b" |> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (calledWith ("a" |> "b"))

      it "failure case" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (calledWith (param "b")) `shouldThrow` anyErrorCall

    describe "Simple verification with calledWith" do
      it "single argument" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (calledWith (param "a"))

      it "multiple arguments" do
        f <- mock $ "a" |> "b" |> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (calledWith ("a" |> "b"))

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
        f `shouldBeCalled` (times 3 `with` (param "a"))

      it "times - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `with` (param "a")) `shouldThrow` anyErrorCall

      it "atLeast - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `with` (param "a"))

      it "atLeast - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `with` (param "a")) `shouldThrow` anyErrorCall

      it "atMost - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 3 `with` (param "a"))

      it "atMost - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 2 `with` (param "a")) `shouldThrow` anyErrorCall

      it "greaterThan - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `with` (param "a"))

      it "greaterThan - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `with` (param "a")) `shouldThrow` anyErrorCall

      it "lessThan - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 4 `with` (param "a"))

      it "lessThan - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 3 `with` (param "a")) `shouldThrow` anyErrorCall

      it "once - success" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (once `with` (param "a"))

      it "once - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (once `with` (param "a")) `shouldThrow` anyErrorCall

      it "never - success" do
        f <- mock $ "a" |> True
        f `shouldBeCalled` (never `with` (param "a"))

      it "never - failure" do
        f <- mock $ "a" |> True
        evaluate $ f "a"
        f `shouldBeCalled` (never `with` (param "a")) `shouldThrow` anyErrorCall

    describe "Count verification without arguments (times only)" do
      it "times - success" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (times 3)

      it "times - failure" do
        f <- mock $ any |> True
        evaluate $ f "a"
        evaluate $ f "b"
        f `shouldBeCalled` (times 3) `shouldThrow` anyErrorCall

    describe "Order verification" do
      it "inOrderWith - success" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (inOrderWith [param "a", param "b", param "c"])

      it "inOrderWith - failure" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (inOrderWith [param "a", param "b", param "b"]) `shouldThrow` anyErrorCall

      it "inPartialOrderWith - success" do
        f <- mock $ any |> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (inPartialOrderWith [param "a", param "c"])

      it "inPartialOrderWith - failure" do
        f <- mock $ any |> ()
        evaluate $ f "b"
        evaluate $ f "a"
        f `shouldBeCalled` (inPartialOrderWith [param "a", param "b"]) `shouldThrow` anyErrorCall

    describe "Monadic mocks" do
      it "shouldBeCalled with IO mock" do
        f <- mock $ "a" |> (1 :: Int) |> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (calledWith ("a" |> (1 :: Int)))

      it "shouldBeCalled times with IO mock" do
        f <- mock $ "a" |> (1 :: Int) |> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (times 3 `with` ("a" |> (1 :: Int)))

      it "shouldBeCalled inOrderWith with IO mock" do
        f <- mock $ any |> any |> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` (inOrderWith ["a" |> (1 :: Int), "b" |> (2 :: Int), "c" |> (3 :: Int)])

