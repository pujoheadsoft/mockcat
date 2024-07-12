{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCatSpec (spec) where

import Test.Hspec
import Test.MockCat
import Test.MockCat.Param
import Test.MockCat


spec :: Spec
spec = do
  describe "mock" do
    it "fn" do
      m <- mock $ "a" |> "x"
      let 
        f = fun m
        v = f "c"
      v `shouldBe` "x"
      -- verify m "a"
