{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.MockCat.ParamSpec (spec) where

import Prelude hiding (and, or)
import Test.Hspec
import Test.MockCat.Cons
import Test.MockCat.Param as P

spec :: Spec
spec = do
  describe "Param" do
    describe "|>" do
      it "a |> b" do
        True |> False `shouldBe` param True :> param False
      it "Param a |> b" do
        param True |> False `shouldBe` param True :> param False
      it "a |> Param b" do
        True |> param False `shouldBe` param True :> param False
      it "Param a |> Param b" do
        param True |> param False `shouldBe` param True :> param False
      it "a |> b |> c" do
        True |> False |> True `shouldBe` param True :> (param False :> param True)
      it "Param a |> b |> c" do
        param True |> False |> True `shouldBe` param True :> (param False :> param True)

    describe "Show" do
      it "String" do
        show (param "X") `shouldBe` "X"
      it "Integer" do
        show (param 100) `shouldBe` "100"
      it "Bool" do
        show (param False) `shouldBe` "False"

    describe "Eq" do
      it "param (eq)" do
        param 10 == param 10 `shouldBe` True
      it "param (not eq)" do
        param 10 == param 11 `shouldBe` False

    describe "Returns True if the expected value condition is met." do
      it "any" do
        (P.any :: Param Int) == param 10 `shouldBe` True
      it "not equal" do
        P.not "v" == param "x" `shouldBe` True
      it "a `or` b" do
        let orParam = "x" `or` "y"
        orParam == param "x" `shouldBe` True
        orParam == param "y" `shouldBe` True
      it "expect_ `or` b" do
        let orParam = expect_ (> (0 :: Int)) `or` (10 :: Int)
        orParam == param (1 :: Int) `shouldBe` True
        orParam == param (10 :: Int) `shouldBe` True
      it "a `or` expect_" do
        let orParam = (10 :: Int) `or` expect_ (> (0 :: Int))
        orParam == param (10 :: Int) `shouldBe` True
        orParam == param (5 :: Int) `shouldBe` True
      it "expect_ `or` expect_" do
        let orParam = expect_ (< (0 :: Int)) `or` expect_ (> (0 :: Int))
        orParam == param (10 :: Int) `shouldBe` True
        orParam == param (-1 :: Int) `shouldBe` True

      it "expect_ `and` b" do
        let andParam = expect_ (> (0 :: Int)) `and` (10 :: Int)
        andParam == param (1 :: Int) `shouldBe` False
        andParam == param (10 :: Int) `shouldBe` True
      it "a `and` expect_" do
        let andParam = (10 :: Int) `and` expect_ (> (0 :: Int))
        andParam == param (10 :: Int) `shouldBe` True
        andParam == param (5 :: Int) `shouldBe` False
      it "expect_ `and` expect_" do
        let andParam = expect_ (> (3 :: Int)) `and` expect_ (< (5 :: Int))
        andParam == param (4 :: Int) `shouldBe` True
        andParam == param (3 :: Int) `shouldBe` False

    describe "show expectation" do
      it "any" do
        show (P.any :: Param Int) `shouldBe` "any"

      it "expect" do
        show (expect (> 4) "> 4") `shouldBe` "> 4"

      it "expect_" do
        show (expect_ (> 4)) `shouldBe` "[some condition]"

      it "or" do
        show (expect (> (4 :: Int)) "> 4" `or` expect (== (10 :: Int)) "== 10") `shouldBe` "> 4 || == 10"

      it "and" do
        show (expect (> (3 :: Int)) "> 3" `and` expect (< (5 :: Int)) "< 5") `shouldBe` "> 3 && < 5"
    
    describe "show expectation by Exp" do
      it "(> 3)" do
        show $(expectByExpr [|(> 3)|]) `shouldBe` "(> 3)"

      it "lambda" do
        show $(expectByExpr [|\x -> x == 3 || x == 5|]) `shouldBe` "(\\x -> ((x == 3) || (x == 5)))"
      
      it "use data" do
        show $(expectByExpr [|\x -> x == Foo "foo"|]) `shouldBe` "(\\x -> (x == (Foo \"foo\")))"

data TestData = Foo String | Bar deriving (Eq, Show)