{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-hpc #-}

module Test.MockCat.ShouldBeCalledSpec (spec) where

import Control.Exception (ErrorCall(..), evaluate)
import Data.List (isInfixOf)
import Test.Hspec
import Test.MockCat
import Test.MockCat.Verify (verificationFailureMessage)
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "shouldBeCalled API" do
    describe "Simple verification (arguments only, at least once)" do
      it "single argument" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        f `shouldBeCalled` "a"

      it "multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` ("a" ~> "b")

      it "failure case" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        f `shouldBeCalled` "b" `shouldThrow` anyErrorCall

      it "single argument with param" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        f `shouldBeCalled` param "a"

      it "single argument with any" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        f `shouldBeCalled` any

      it "single argument with expect" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        f `shouldBeCalled` expect (const True) "always true"

    describe "Simple verification without arguments (anything)" do
      it "success" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        f `shouldBeCalled` anything

      it "failure (never called)" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` anything `shouldThrow` anyErrorCall

    describe "Count verification with arguments" do
      it "times - exact count" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` "a")

      it "times - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "atLeast - success" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a")

      it "atLeast - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "atMost - success" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 3 `withArgs` "a")

      it "atMost - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 2 `withArgs` "a") `shouldThrow` anyErrorCall

      it "greaterThan - success" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a")

      it "greaterThan - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a") `shouldThrow` anyErrorCall

      it "lessThan - success" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 4 `withArgs` "a")

      it "lessThan - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 3 `withArgs` "a") `shouldThrow` anyErrorCall

      it "once - success" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        f `shouldBeCalled` (once `withArgs` "a")

      it "once - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (once `withArgs` "a") `shouldThrow` anyErrorCall

      it "never - success" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` (never `withArgs` "a")

      it "never - failure" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        f `shouldBeCalled` (never `withArgs` "a") `shouldThrow` anyErrorCall

    describe "Count verification with Param arguments" do
      it "times with param" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (times 3 `withArgs` param "a")

      it "times with any" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (times 3 `withArgs` any)

      it "times with expect" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` (times 3 `withArgs` expect (const True) "always true")

    describe "Count verification without arguments (times only)" do
      it "times - success" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` times 3

      it "times - failure" do
        f <- mock $ any ~> True
        evaluate $ f "a"
        evaluate $ f "b"
        f `shouldBeCalled` times 3 `shouldThrow` anyErrorCall

    describe "Order verification" do
      it "inOrderWith - success" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inOrderWith ["a", "b", "c"]

      it "inOrderWith - failure" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inOrderWith ["a", "b", "b"] `shouldThrow` anyErrorCall

      it "inPartialOrderWith - success" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        evaluate $ f "b"
        evaluate $ f "c"
        f `shouldBeCalled` inPartialOrderWith ["a", "c"]

      it "inPartialOrderWith - failure" do
        f <- mock $ any ~> ()
        evaluate $ f "b"
        evaluate $ f "a"
        f `shouldBeCalled` inPartialOrderWith ["a", "b"] `shouldThrow` anyErrorCall

    describe "Monadic mocks" do
      it "shouldBeCalled with IO mock" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` ("a" ~> (1 :: Int))

      it "shouldBeCalled times with IO mock" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (times 3 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled inOrderWith with IO mock" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inOrderWith ["a" ~> (1 :: Int), "b" ~> (2 :: Int), "c" ~> (3 :: Int)]

      it "shouldBeCalled inPartialOrderWith with IO mock" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inPartialOrderWith ["a" ~> (1 :: Int), "c" ~> (3 :: Int)]

      it "shouldBeCalled anything with IO mock" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` anything

      it "shouldBeCalled times without args with IO mock" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` times 3

    describe "Named mocks (error messages)" do
      it "shouldBeCalled with name in error message" do
        f <- mock (label "named mock") $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` ("b" ~> (1 :: Int)) `shouldThrow` anyErrorCall

      it "shouldBeCalled times with name in error message" do
        f <- mock (label "named mock") $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        let e =
              "function `named mock` was not called the expected number of times with the expected arguments.\n\
              \  expected: 3\n\
              \   but got: 2"
        f `shouldBeCalled` (times 3 `withArgs` ("a" ~> (1 :: Int))) `shouldThrow` errorCall e

      it "shouldBeCalled inOrderWith with name in error message" do
        f <- mock (label "named mock") $ any ~> any ~> pure @IO True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inOrderWith ["a" ~> (1 :: Int), "b" ~> (2 :: Int), "c" ~> (3 :: Int)] `shouldThrow` errorContains "expected 1st call:"

      it "shouldBeCalled inPartialOrderWith with name in error message" do
        f <- mock (label "named mock") $ any ~> any ~> pure @IO True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        let e =
              "function `named mock` was not called with the expected arguments in the expected order.\n\
              \  expected order:\n\
              \    \"a\",1\n\
              \    \"c\",3\n\
              \  but got:\n\
              \    \"b\",2\n\
              \    \"a\",1"
        f `shouldBeCalled` inPartialOrderWith ["a" ~> (1 :: Int), "c" ~> (3 :: Int)] `shouldThrow` errorCall e

      it "shouldBeCalled anything with name in error message" do
        f <- mock (label "named mock") $ "a" ~> (1 :: Int) ~> pure @IO True
        f `shouldBeCalled` anything `shouldThrow` errorCall "Function `named mock` was never called"

    describe "Non-Eq/Show support" do
      it "can verify calls with NoEq argument using anything" do
        f <- mock $ any @NoEq ~> "result"
        f (NoEq "val") `shouldBe` "result"
        f `shouldBeCalled` anything

    describe "Error messages" do
      it "shouldBeCalled failure with detailed error message" do
        f <- mock $ any ~> True
        evaluate $ f "A"
        -- The error message format uses showForMessage which may quote the value
        f `shouldBeCalled` "X" `shouldThrow` anyErrorCall

      it "shouldBeCalled times failure with detailed error message" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        let e =
              "function was not called the expected number of times with the expected arguments.\n\
              \  expected: 3\n\
              \   but got: 2"
        f `shouldBeCalled` (times 3 `withArgs` ("a" ~> (1 :: Int))) `shouldThrow` errorCall e

      it "shouldBeCalled with non-mock function shows guidance message" do
        let f :: Int -> Int
            f x = x
        (f `shouldBeCalled` times 1) `shouldThrow` errorCall verificationFailureMessage

    describe "Multiple arguments with typed values" do
      it "shouldBeCalled with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` ("a" ~> (1 :: Int))

      it "shouldBeCalled times with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (times 3 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled atLeast with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (atLeast 3 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled atMost with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (atMost 3 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled greaterThan with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (greaterThan 2 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled lessThan with multiple typed arguments" do
        f <- mock $ "a" ~> (1 :: Int) ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldBeCalled` (lessThan 4 `withArgs` ("a" ~> (1 :: Int)))

      it "shouldBeCalled inOrderWith with multiple typed arguments" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inOrderWith ["a" ~> (1 :: Int), "b" ~> (2 :: Int), "c" ~> (3 :: Int)]

      it "shouldBeCalled inPartialOrderWith with multiple typed arguments" do
        f <- mock $ any ~> any ~> pure @IO True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldBeCalled` inPartialOrderWith ["a" ~> (1 :: Int), "c" ~> (3 :: Int)]

    describe "High arity mocks" do
      it "shouldBeCalled with arity 2" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` ("a" ~> "b")

      it "shouldBeCalled with arity 3" do
        f <- mock $ "a" ~> "b" ~> "c" ~> False
        evaluate $ f "a" "b" "c"
        f `shouldBeCalled` ("a" ~> "b" ~> "c")

      it "shouldBeCalled times with arity 2" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (times 3 `withArgs` ("a" ~> "b"))

      it "shouldBeCalled times with arity 3" do
        f <- mock $ "a" ~> "b" ~> "c" ~> False
        evaluate $ f "a" "b" "c"
        evaluate $ f "a" "b" "c"
        f `shouldBeCalled` (times 2 `withArgs` ("a" ~> "b" ~> "c"))

      it "shouldBeCalled inOrderWith with arity 2" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        f `shouldBeCalled` inOrderWith ["a" ~> "b", "c" ~> "d"]

    describe "Multi-case mocks" do
      it "shouldBeCalled with multiple cases (arity 1)" do
        f <- mock $ do
          onCase $ "1" ~> True
          onCase $ "2" ~> False
        evaluate $ f "1"
        evaluate $ f "2"
        f `shouldBeCalled` "1"
        f `shouldBeCalled` "2"


      it "shouldBeCalled with multiple cases (arity 2)" do
        f <- mock $ do
          onCase $ "1" ~> "2" ~> True
          onCase $ "2" ~> "3" ~> False
        evaluate $ f "1" "2"
        evaluate $ f "2" "3"
        f `shouldBeCalled` ("1" ~> "2")
        f `shouldBeCalled` ("2" ~> "3")

      it "shouldBeCalled times with multiple cases (arity 2)" do
        f <- mock $ do
          onCase $ "1" ~> "2" ~> True
          onCase $ "2" ~> "3" ~> False
        evaluate $ f "1" "2"
        evaluate $ f "1" "2"
        evaluate $ f "2" "3"
        evaluate $ f "2" "3"
        f `shouldBeCalled` (times 2 `withArgs` ("1" ~> "2"))
        f `shouldBeCalled` (times 2 `withArgs` ("2" ~> "3"))

    describe "Edge cases and boundary conditions" do
      it "times 0 (never called)" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` (times 0 `withArgs` "a")

      it "atLeast 0 (always succeeds)" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` (atLeast 0 `withArgs` "a")
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 0 `withArgs` "a")

      it "atLeast boundary (exact count)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a")

      it "atMost boundary (exact count)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 3 `withArgs` "a")

      it "greaterThan boundary (one more)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a")

      it "lessThan boundary (one less)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 3 `withArgs` "a")

    describe "Multiple arguments with Param combinators" do
      it "shouldBeCalled with multiple arguments using any" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (any ~> any)

      it "shouldBeCalled times with multiple arguments using any" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        evaluate $ f "e" "f"
        f `shouldBeCalled` (times 3 `withArgs` (any ~> any))

      it "shouldBeCalled with multiple arguments using expect" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (expect (const True) "always true" ~> expect (const True) "always true")

      it "shouldBeCalled times with multiple arguments using expect" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        f `shouldBeCalled` (times 2 `withArgs` (expect (const True) "always true" ~> expect (const True) "always true"))

      it "shouldBeCalled anything with multiple arguments" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` anything

    describe "High arity mocks (continued)" do
      it "shouldBeCalled with arity 9" do
        f <- mock $ any ~> any ~> any ~> any ~> any ~> any ~> any ~> any ~> ()
        evaluate $ f "1" "2" "3" "4" "5" "6" "7" "8"
        f `shouldBeCalled` ("1" ~> "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8")

      it "shouldBeCalled inOrderWith with arity 9" do
        f <- mock $ any ~> any ~> any ~> any ~> any ~> any ~> any ~> any ~> ()
        evaluate $ f "1" "2" "3" "4" "5" "6" "7" "8"
        evaluate $ f "2" "3" "4" "5" "6" "7" "8" "9"
        evaluate $ f "3" "4" "5" "6" "7" "8" "9" "0"
        f `shouldBeCalled` inOrderWith
          [ "1" ~> "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8",
            "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8" ~> "9",
            "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8" ~> "9" ~> "0"
          ]

    describe "Order verification edge cases" do
      it "inOrderWith with single element" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        f `shouldBeCalled` inOrderWith ["a"]

      it "inPartialOrderWith with single element" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        f `shouldBeCalled` inPartialOrderWith ["a"]

      it "inOrderWith with multiple arguments (failure case)" do
        f <- mock $ any ~> any ~> ()
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        f `shouldBeCalled` inOrderWith ["a" ~> "b", "d" ~> "c"] `shouldThrow` anyErrorCall

      it "inPartialOrderWith with multiple arguments (failure case)" do
        f <- mock $ any ~> any ~> ()
        evaluate $ f "b" "a"
        evaluate $ f "c" "d"
        f `shouldBeCalled` inPartialOrderWith ["a" ~> "b", "c" ~> "d"] `shouldThrow` anyErrorCall

    describe "Count verification with multiple arguments" do
      it "once with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (once `withArgs` ("a" ~> "b"))

      it "once with multiple arguments (failure)" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (once `withArgs` ("a" ~> "b")) `shouldThrow` anyErrorCall

      it "never with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        f `shouldBeCalled` (never `withArgs` ("a" ~> "b"))

      it "never with multiple arguments (failure)" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (never `withArgs` ("a" ~> "b")) `shouldThrow` anyErrorCall

      it "atLeast with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (atLeast 2 `withArgs` ("a" ~> "b"))

      it "atMost with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (atMost 2 `withArgs` ("a" ~> "b"))

      it "greaterThan with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (greaterThan 1 `withArgs` ("a" ~> "b"))

      it "lessThan with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (lessThan 3 `withArgs` ("a" ~> "b"))

  describe "mockM" do
    it "records calls for monadic mocks" do
      f <- mockM $ "a" ~> True
      result <- f "a"
      result `shouldBe` True
      f `shouldBeCalled` "a"

    describe "Edge cases and boundary conditions" do
      it "times 0 (never called)" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` (times 0 `withArgs` "a")

      it "atLeast 0 (always succeeds)" do
        f <- mock $ "a" ~> True
        f `shouldBeCalled` (atLeast 0 `withArgs` "a")
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 0 `withArgs` "a")

      it "atLeast boundary (exact count)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atLeast 3 `withArgs` "a")

      it "atMost boundary (exact count)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (atMost 3 `withArgs` "a")

      it "greaterThan boundary (one more)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (greaterThan 2 `withArgs` "a")

      it "lessThan boundary (one less)" do
        f <- mock $ "a" ~> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldBeCalled` (lessThan 3 `withArgs` "a")

    describe "Multiple arguments with Param combinators" do
      it "shouldBeCalled with multiple arguments using any" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (any ~> any)

      it "shouldBeCalled times with multiple arguments using any" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        evaluate $ f "e" "f"
        f `shouldBeCalled` (times 3 `withArgs` (any ~> any))

      it "shouldBeCalled with multiple arguments using expect" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (expect (const True) "always true" ~> expect (const True) "always true")

      it "shouldBeCalled times with multiple arguments using expect" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        f `shouldBeCalled` (times 2 `withArgs` (expect (const True) "always true" ~> expect (const True) "always true"))

      it "shouldBeCalled anything with multiple arguments" do
        f <- mock $ any ~> any ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` anything

    describe "High arity mocks (continued)" do
      it "shouldBeCalled with arity 9" do
        f <- mock $ any ~> any ~> any ~> any ~> any ~> any ~> any ~> any ~> ()
        evaluate $ f "1" "2" "3" "4" "5" "6" "7" "8"
        f `shouldBeCalled` inOrderWith ["1" ~> "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8"]

      it "shouldBeCalled inOrderWith with arity 9" do
        f <- mock $ any ~> any ~> any ~> any ~> any ~> any ~> any ~> any ~> ()
        evaluate $ f "1" "2" "3" "4" "5" "6" "7" "8"
        evaluate $ f "2" "3" "4" "5" "6" "7" "8" "9"
        evaluate $ f "3" "4" "5" "6" "7" "8" "9" "0"
        f `shouldBeCalled` inOrderWith
          [ "1" ~> "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8",
            "2" ~> "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8" ~> "9",
            "3" ~> "4" ~> "5" ~> "6" ~> "7" ~> "8" ~> "9" ~> "0"
          ]

    describe "Order verification edge cases" do
      it "inOrderWith with single element" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        f `shouldBeCalled` inOrderWith ["a"]

      it "inPartialOrderWith with single element" do
        f <- mock $ any ~> ()
        evaluate $ f "a"
        f `shouldBeCalled` inPartialOrderWith ["a"]

      it "inOrderWith with multiple arguments (failure case)" do
        f <- mock $ any ~> any ~> ()
        evaluate $ f "a" "b"
        evaluate $ f "c" "d"
        f `shouldBeCalled` inOrderWith ["a" ~> "b", "d" ~> "c"] `shouldThrow` anyErrorCall

      it "inPartialOrderWith with multiple arguments (failure case)" do
        f <- mock $ any ~> any ~> ()
        evaluate $ f "b" "a"
        evaluate $ f "c" "d"
        f `shouldBeCalled` inPartialOrderWith ["a" ~> "b", "c" ~> "d"] `shouldThrow` anyErrorCall

    describe "Count verification with multiple arguments" do
      it "once with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (once `withArgs` ("a" ~> "b"))

      it "once with multiple arguments (failure)" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (once `withArgs` ("a" ~> "b")) `shouldThrow` anyErrorCall

      it "never with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        f `shouldBeCalled` (never `withArgs` ("a" ~> "b"))

      it "never with multiple arguments (failure)" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        f `shouldBeCalled` (never `withArgs` ("a" ~> "b")) `shouldThrow` anyErrorCall

      it "atLeast with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (atLeast 2 `withArgs` ("a" ~> "b"))

      it "atMost with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (atMost 2 `withArgs` ("a" ~> "b"))

      it "greaterThan with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (greaterThan 1 `withArgs` ("a" ~> "b"))

      it "lessThan with multiple arguments" do
        f <- mock $ "a" ~> "b" ~> True
        evaluate $ f "a" "b"
        evaluate $ f "a" "b"
        f `shouldBeCalled` (lessThan 3 `withArgs` ("a" ~> "b"))

errorContains :: String -> Selector ErrorCall
errorContains sub (ErrorCall msg) = sub `isInfixOf` msg

data NoEq = NoEq String deriving (Show, Eq)

instance WrapArg NoEq where wrapArg v = ExpectValue v (show v)

