{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}

module Test.MockCat.MockSpec (spec) where

import qualified Control.Exception as E
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)
import Data.List (isInfixOf)

spec :: Spec
spec = do
  describe "Test of Mock" do
    describe "combination test" do
      it "arity = 1" do
        f <- mock $ True |> False
        f True `shouldBe` False

      it "arity = 2" do
        f <- mock $ True |> False |> True
        f True False `shouldBe` True

      it "arity = 3" do
        f <- mock $ True |> "False" |> True |> "False"
        f True "False" True `shouldBe` "False"

      it "arity = 4" do
        f <- mock $ True |> "False" |> True |> "False" |> True
        f True "False" True "False" `shouldBe` True

      it "Param |> a" do
        f <- mock $ any |> False
        f True `shouldBe` False

      it "Param |> (a |> b)" do
        f <- mock $ any |> False |> True
        f True False `shouldBe` True

      it "a     |> (Param |> b)" do
        f <- mock $ True |> any |> True
        f True False `shouldBe` True

      it "Param |> (Param |> a)" do
        f <- mock $ any |> any |> True
        f True False `shouldBe` True

      it "a     |> (Param |> (Param |> a))" do
        f <- mock $ "any" |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

      it "param |> (Param |> (Param |> a))" do
        f <- mock $ any |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

  describe "Monad" do
    it "Return IO Monad." do
      f <- mock $ "Article Id" |> pure @IO "Article Title"

      result <- f "Article Id"

      result `shouldBe` "Article Title"

  describe "Appropriate message when a test fails." do
    describe "anonymous mock" do
      describe "apply" do
        it "simple mock" do
          f <- mock $ "a" |> pure @IO True
          f "b"
            `shouldThrow` errorContains "expected: \"a\"\n   but got: \"b\"\n             ^^"

        it "multi mock" do
          f <-
            mock $ do
              onCase $ "aaa" |> (100 :: Int) |> pure @IO True
              onCase $ "bbb" |> (200 :: Int) |> pure @IO False

          f "aaa" 200
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected one of the following:\n\
              \    \"aaa\",100\n\
              \    \"bbb\",200\n\
              \  but got:\n\
              \    \"aaa\",200"

    describe "named mock" do
      describe "aply" do
        it "simple mock" do
          f <- mock (label "mock function") $ "a" |> pure @IO ()
          f "b" `shouldThrow` errorContains "expected: \"a\"\n   but got: \"b\"\n             ^^"

        it "multi mock" do
          f <-
            mock (label "mock function")
              do 
                onCase $ "aaa" |> True |> pure @IO True
                onCase $ "bbb" |> False |> pure @IO False
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected one of the following:\n\
                \    \"aaa\",True\n\
                \    \"bbb\",False\n\
                \  but got:\n\
                \    \"aaa\",False"
          f "aaa" False `shouldThrow` errorCall e

  describe "use expectation" do
    it "expectByExpr" do
      f <- mock $ $(expectByExpr [|\x -> x == "y" || x == "z"|]) |> True
      f "y" `shouldBe` True

  describe "repeatable" do
    it "arity = 1" do
      f <- mock $ do
        onCase $ "a" |> True
        onCase $ "b" |> False
        onCase $ "a" |> False
        onCase $ "b" |> True
        
      v1 <- evaluate $ f "a"
      v2 <- evaluate $ f "a"
      v3 <- evaluate $ f "b"
      v4 <- evaluate $ f "b"
      v1 `shouldBe` True
      v2 `shouldBe` False
      v3 `shouldBe` False
      v4 `shouldBe` True

    it "arity = 2" do
      f <- mock $ do
        onCase $ "a" |> "b" |> (0 :: Int)
        onCase $ "a" |> "c" |> (1 :: Int)
        onCase $ "a" |> "b" |> (2 :: Int)
        onCase $ "a" |> "c" |> (3 :: Int)

      v1 <- evaluate $ f "a" "b"
      v2 <- evaluate $ f "a" "b"
      v3 <- evaluate $ f "a" "c"
      v4 <- evaluate $ f "a" "c"
      v5 <- evaluate $ f "a" "b"
      v1 `shouldBe` (0 :: Int)
      v2 `shouldBe` (2 :: Int)
      v3 `shouldBe` (1 :: Int)
      v4 `shouldBe` (3 :: Int)
      v5 `shouldBe` (2 :: Int)

  describe "constant" do
    it "createConstantMock" do
      f <- mock "foo"
      f `shouldBe` "foo"
    
    it "createNamedConstantMock" do
      f <- mock (label "const") "foo"
      f `shouldBe` "foo"

    it "verify constant IO mock" do
      f <- mock $ pure @IO "foo"
      f `shouldReturn` "foo"
      f `shouldReturn` "foo"
      f `shouldReturn` "foo"

    it "verify constant multi IO mock" do
      f <- mock $ do
        onCase $ pure @IO "foo"
        onCase $ pure @IO "bar"
        onCase $ pure @IO "baz"

      f `shouldReturn` "foo"
      f `shouldReturn` "bar"
      f `shouldReturn` "baz"

class Eval a where
  evaluate :: a -> IO a

instance Eval [a] where
  evaluate v = do
    mapM_ (\x -> E.evaluate (x `seq` ())) v
    pure v

instance {-# OVERLAPPABLE #-} Eval a where
  evaluate = E.evaluate

errorContains :: String -> Selector E.ErrorCall
errorContains sub (E.ErrorCall msg) = sub `isInfixOf` msg
