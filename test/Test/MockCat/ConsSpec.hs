{-# LANGUAGE BlockArguments #-}
module Test.MockCat.ConsSpec (spec) where

import Test.Hspec

spec :: Spec 
spec = do 
  describe "Cons" do
    describe "Show" do
      it "2 arguments" do
        show (10 :> true) `shouldEqual` "10,true"
      it "3 arguments" do
        show ("1" :> false :> [3, 4]) `shouldEqual` "\"1\",false,[3,4]"
    describe "Eq" do
      it "2 arguments" do
        (1 :> "2") `shouldEqual` (1 :> "2")
      it "3 arguments" do
        ("1" :> false :> [3, 4]) `shouldEqual` ("1" :> false :> [3, 4])
