{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "stub & verify" do
    -- make a mock
    m <- mock $ "value" |> True
    -- stub function
    let f = fun m
    -- assert
    f "value" `shouldBe` True
    -- verify
    m `shouldApplyTo` "value"

  it "how to use" do
    f <- mockFun $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()
  
  it "named mock" do
    m <- namedMock "mock" $ "value" |> True
    fun m "value" `shouldBe` True
  
  it "stub function" do
    f <- mockFun $ "value" |> True
    f "value" `shouldBe` True
  
  it "verify to applied times" do
    m <- mock $ "value" |> True
    print $ fun m "value"
    print $ fun m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value" 

  it "verify order of apply" do
    m <- mock $ any |> True |> ()
    print $ fun m "a" True
    print $ fun m "b" True
    m `shouldApplyInOrder` ["a" |> True, "b" |> True]


