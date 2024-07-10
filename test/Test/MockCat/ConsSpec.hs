{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCat.ConsSpec (spec) where

import Test.Hspec
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.Param as P

spec :: Spec
spec = do
  describe "Cons" do
    describe "Show" do
      it "2 arguments" do
        show ((10 :: Int) |> True) `shouldBe` "10,True"
      it "3 arguments" do
        show ("1" |> False |> [3, 4]) `shouldBe` "\"1\",False,[3,4]"
    describe "Eq" do
      it "2 arguments" do
        (1 |> "2") `shouldBe` (1 |> "2")
      it "3 arguments" do
        ("1" |> False |> [3, 4]) `shouldBe` ("1" |> False |> [3, 4])
    describe "Param" do
      it "param (eq)" do
        param 10 == param 10 `shouldBe` True
      it "param (not eq)" do
        param 10 == param 11 `shouldBe` False
      it "any (Left)" do
        (P.any :: Param Int) == param 10 `shouldBe` True
      it "any (Right)" do
         param 10 == (P.any :: Param Int) `shouldBe` True
      it "any (Left)" do
        ((P.any :: Param String) |> True) == ("x" |> True) `shouldBe` True
