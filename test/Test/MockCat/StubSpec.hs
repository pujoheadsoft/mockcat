{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MockCat.StubSpec where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate)

spec :: Spec
spec = do
  it "createStubFn" do
    let f = createStubFn $ "value" |> True
    f "value" `shouldBe` True

  it "createStubFn with multiple arguments" do
    let f = createStubFn $ "value1" |> "value2" |> True
    f "value1" "value2" `shouldBe` True

  it "createStubFn with multiple arguments and return value" do
    let f = createStubFn $ "value1" |> "value2" |> "value3" |> True
    f "value1" "value2" "value3" `shouldBe` True
    
  it "createStubFn with multiple arguments and return value" do
    let f = createStubFn $ "value1" |> "value2" |> "value3" |> "value4" |> True
    f "value1" "value2" "value3" "value4" `shouldBe` True
  
  it "createStubFn with multiple arguments and return value" do
    let 
      f = createStubFn do
        onCase $ "value1" |> "value2" |> True
        onCase $ "value2" |> "value3" |> False
    
    f "value1" "value2" `shouldBe` True
    f "value2" "value3" `shouldBe` False
  
  it "throws on unexpected argument" do
    let f = createStubFn $ "value1" |> True
    let e =
          "function was not applied to the expected arguments.\n\
          \  expected: \"value1\"\n\
          \   but got: \"value2\""
    evaluate (f "value2") `shouldThrow` errorCall e