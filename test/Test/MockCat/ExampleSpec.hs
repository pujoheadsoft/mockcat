{-# LANGUAGE BlockArguments #-}
module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat.Mock

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
    m `hasBeenCalledWith` "value"
  
  it "named mock" do
    m <- namedMock "mock" $ "value" |> True
    fun m "value" `shouldBe` True
  
  it "stub function" do
    f <- mockFun $ "value" |> True
    f "value" `shouldBe` True