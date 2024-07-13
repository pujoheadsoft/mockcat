{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test.MockCatSpec (spec) where

import Prelude hiding (any)
import Test.Hspec
import Test.MockCat (mock, fun, hasBeenCalledWith, hasBeenCalledTimes, with, hasBeenCalledInOrder)
import Test.MockCat.Param (any, (|>))
import Control.Exception (evaluate)
import Data.Function ((&))

data Fixture mock r = Fixture {
  name :: String,
  create :: IO mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> IO (),
  verifyFailed :: mock -> IO (),
  verifyCount :: mock -> Int -> IO ()
}

data VerifyOrderFixture mock r = VerifyOrderFixture {
  name :: String,
  create :: IO mock,
  execute :: mock -> IO r,
  verifyMock :: mock -> IO (),
  verifyFailed :: mock -> IO ()
}

-- mock test template
mockTest :: (Eq r, Show r) => Fixture mock r -> SpecWith (Arg Expectation)
mockTest f = describe f.name do
  it "Expected argument is applied, the expected value is returned." do
    m <- f.create
    f.execute m `shouldBe` f.expected

  it "Unexpected argument is applied, an exception is thrown." do
    f.executeFailed & maybe mempty \func -> do
      m <- f.create
      evaluate (func m) `shouldThrow` anyErrorCall

  it "Expected arguments are applied, the verification succeeds." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyMock m

  it "Unexpected arguments are applied, the verification fails." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyFailed m `shouldThrow` anyErrorCall

  it "The number of times a function has been applied can be verification (0 times)." do
    m <- f.create
    f.verifyCount m 0

  it "The number of times a function has been applied can be verification (3 times)." do
    m <- f.create
    evaluate $ f.execute m
    evaluate $ f.execute m
    evaluate $ f.execute m
    f.verifyCount m 3

  it "Fails to verification the number of times it has been applied, an exception is thrown." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyCount m 3 `shouldThrow` anyErrorCall

mockOrderTest :: VerifyOrderFixture mock r -> SpecWith (Arg Expectation)
mockOrderTest f = describe f.name do
  it "If the functions are applied in the expected order, the verification succeeds." do
    m <- f.create
    f.execute m
    f.verifyMock m

  it "If the functions are not applied in the expected order, verification fails." do
    m <- f.create
    f.execute m
    f.verifyFailed m `shouldThrow` anyErrorCall

spec :: Spec
spec = do
  mockTest Fixture {
    name = "Test of Mock (arity = 1)",
    create = mock $ "a" |> False,
    execute = (`fun` "a"),
    executeFailed = Just (`fun` "x"),
    expected = False,
    verifyMock = (`hasBeenCalledWith` "a"),
    verifyFailed = (`hasBeenCalledWith` "2"),
    verifyCount = \m c -> m `hasBeenCalledTimes` c `with` "a"
  }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest VerifyOrderFixture {
        name = "1 Arguments", 
        create = mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          evaluate $ fun m "b"
          evaluate $ fun m "c",
        verifyMock = \m -> m `hasBeenCalledInOrder` [
          "a",
          "b",
          "c"
        ],
        verifyFailed = \m -> m `hasBeenCalledInOrder` [
          "a",
          "b",
          "b"
        ]
      }

  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
      --verify m "a"
