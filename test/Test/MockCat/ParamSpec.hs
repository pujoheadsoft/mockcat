{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCat.ParamSpec (spec) where

import Test.Hspec
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.Param as P

spec :: Spec
spec = do
  describe "Param" do
    describe "|>" do
      it "a |> b" do
        True |> False `shouldBe` Cons (param True) (param False)
      it "Param a |> b" do
        param True |> False `shouldBe` Cons (param True) (param False)
      it "a |> Param b" do
        True |> param False `shouldBe` Cons (param True) (param False)
      it "Param a |> Param b" do
        param True |> param False `shouldBe` Cons (param True) (param False)
      it "a |> b |> c" do
        True |> False |> True `shouldBe` Cons (param True) (Cons (param False) (param True))
      it "Param a |> b |> c" do
        param True |> False |> True `shouldBe` Cons (param True) (Cons (param False) (param True))

    describe "Show" do
      it "Integer" do
        show (param 100) `shouldBe` "100"
      it "Bool" do
        show (param False) `shouldBe` "False"

    describe "Eq" do
      it "param (eq)" do
        param 10 == param 10 `shouldBe` True
      it "param (not eq)" do
        param 10 == param 11 `shouldBe` False
      it "any (Left)" do
        (P.any :: Param Int) == param 10 `shouldBe` True
      it "any (Right)" do
         param 10 == (P.any :: Param Int) `shouldBe` True
