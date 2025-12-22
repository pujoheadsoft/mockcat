{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MockCat.StubSpec where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate, ErrorCall(..))
import Data.List (isInfixOf)

spec :: Spec
spec = do
  it "stub" do
    let f = stub $ "value" |> True
    f "value" `shouldBe` True

  it "stub with multiple arguments" do
    let f = stub $ "value1" |> "value2" |> True
    f "value1" "value2" `shouldBe` True

  it "stub with multiple arguments and return value" do
    let f = stub $ "value1" |> "value2" |> "value3" |> True
    f "value1" "value2" "value3" `shouldBe` True
    
  it "stub with multiple arguments and return value" do
    let f = stub $ "value1" |> "value2" |> "value3" |> "value4" |> True
    f "value1" "value2" "value3" "value4" `shouldBe` True
  
  it "stub with multiple arguments and return value" do
    let 
      f = stub do
        onCase $ "value1" |> "value2" |> True
        onCase $ "value2" |> "value3" |> False
    
    f "value1" "value2" `shouldBe` True
    f "value2" "value3" `shouldBe` False
  
  it "throws on unexpected argument" do
    let f = stub $ "a" |> True
    evaluate (f "b") `shouldThrow` errorContains "expected: \"a\"\n   but got: \"b\"\n             ^^"

  describe "named stub" do
    it "stub with label" do
      let f = stub (label "stub function") $ "value" |> True
      f "value" `shouldBe` True

    it "stub with label and multiple arguments" do
      let f = stub (label "stub function") $ "value1" |> "value2" |> True
      f "value1" "value2" `shouldBe` True

    it "stub with label throws on unexpected argument with name in error message" do
      let f = stub (label "stub function") $ "a" |> True
      evaluate (f "b") `shouldThrow` errorContains "expected: \"a\"\n   but got: \"b\"\n             ^^"

    it "stub with label and cases" do
      let f = stub (label "stub function") $ do
            onCase $ "value1" |> "value2" |> True
            onCase $ "value2" |> "value3" |> False
      f "value1" "value2" `shouldBe` True
      f "value2" "value3" `shouldBe` False

errorContains :: String -> Selector ErrorCall
errorContains sub (ErrorCall msg) = sub `isInfixOf` msg