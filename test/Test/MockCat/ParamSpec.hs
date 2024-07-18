{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.MockCat.ParamSpec (spec) where

import Prelude hiding (and, or)
import Test.Hspec
import Test.MockCat.Cons
import Test.MockCat.Param as P
import Control.Applicative (Alternative(empty))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Test.MockCat.Param

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

    describe "Matcher" do
      it "any" do
        (P.any :: Param Int) == param 10 `shouldBe` True
      -- it "custom matcher" do
      --   let m = [|matcher (== "x")|]
      --   m == param "x" `shouldBe` True
      it "not equal" do
        notEqual "v" == param "x" `shouldBe` True
      -- it "and" do
      --   matcher ((0 :: Int) <) `and` matcher (< (3 :: Int)) == param (2 :: Int) `shouldBe` True
      it "a `or` b" do
        let orParam = "x" `or` "y"
        orParam == param "x" `shouldBe` True
        orParam == param "y" `shouldBe` True
      -- it "matcher `or` b" do
      --   let orParam = matcher ((0 :: Int) <) `or` (10 :: Int)
      --   orParam == param (1 :: Int) `shouldBe` True
      --   orParam == param (10 :: Int) `shouldBe` True
      -- it "a `or` matcher" do
      --   let orParam = (10 :: Int) `or` matcher ((0 :: Int) <)
      --   orParam == param (10 :: Int) `shouldBe` True
      --   orParam == param (5 :: Int) `shouldBe` True
      -- it "matcher `or` matcher" do
      --   let orParam = matcher (< (0 :: Int)) `or` matcher ((0 :: Int) <)
      --   orParam == param (10 :: Int) `shouldBe` True
      --   orParam == param (-1 :: Int) `shouldBe` True
    
    describe "Matcher (show)" do
      it "any" do
        show (P.any :: Param Int) `shouldBe` "any"

      it "expect" do
        show (expect (> 4) "> 4") `shouldBe` "> 4"

      it "expect_" do
        show (expect_ (> 4)) `shouldBe` "[some condition]"

      it "expectByExpr" do
        show $(expectByExpr [|(> 3)|]) `shouldBe` "(GHC.Classes.> 3)"

      -- it "or" do
      --   show ((expect (> (4 :: Int)) "> 4") `or` (expect (> (4 :: Int)) "> 4")) `shouldBe` "> 4"
data Hoge = Foo String | Bar deriving (Eq, Show)