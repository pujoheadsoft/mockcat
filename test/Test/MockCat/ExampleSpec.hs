{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (and, any, not, or)

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

  it "named stub" do
    f <- createNamedStubFn "named stub" $ "x" |> "y" |> True
    f "x" "y" `shouldBe` True

  it "named mock" do
    m <- createNamedMock "mock" $ "value" |> "a" |> True
    stubFn m "value" "a" `shouldBe` True

  it "stub function" do
    f <- createStubFn $ "value" |> True
    f "value" `shouldBe` True

  it "shouldApplyTimes" do
    m <- createMock $ "value" |> True
    print $ stubFn m "value"
    print $ stubFn m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value"

  it "shouldApplyInOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    m
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]

  it "shouldApplyInPartialOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    print $ stubFn m "c" True
    m
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]

  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"

  it "expect" do
    f <- createStubFn $ expect (> 5) "> 5" |> "return value"
    f 6 `shouldBe` "return value"

  it "expect_" do
    f <- createStubFn $ expect_ (> 5) |> "return value"
    f 6 `shouldBe` "return value"

  it "expectByExpr" do
    f <- createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
    f 6 `shouldBe` "return value"

  it "multi" do
    f <-
      createStubFn
        [ "a" |> "return x",
          "b" |> "return y"
        ]
    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"

  it "multi2" do
    f <- createNamedStubFn "multi2" [
        "a" |> "x",
        "a" |> "y"
      ]
    let
      -- Do not allow optimization to remove duplicates.
      x = notInlineMap f ["a", "a"]
    x `shouldBe` ["x", "y"]

{-# NOINLINE notInlineMap #-}
notInlineMap :: Functor f => (a -> b) -> f a -> f b
notInlineMap f v = f <$> v
