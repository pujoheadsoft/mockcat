{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "stub & verify" do
    -- create a mock
    mock <- createMock $ "value" |> True
    -- stub function
    let stubFunction = stubFn mock
    -- assert
    stubFunction "value" `shouldBe` True
    -- verify
    mock `shouldApplyTo` "value"

  it "how to use" do
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()
  
  it "named mock" do
    m <- createNamedMock "mock" $ "value" |> "a" |> True
    stubFn m "value" "a" `shouldBe` True
  
  it "stub function" do
    f <- createStubFn $ "value" |> True
    f "value" `shouldBe` True
  
  it "verify to applied times" do
    m <- createMock $ "value" |> True
    print $ stubFn m "value"
    print $ stubFn m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value" 

  it "verify order of apply" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    m `shouldApplyInOrder` ["a" |> True, "b" |> True]
  
  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"


