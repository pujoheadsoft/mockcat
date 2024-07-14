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

spec :: Spec
spec = do
  describe "Test of Mock" do
    mockTest Fixture {
      name = "arity = 1",
      create = mock $ "a" |> False,
      execute = (`fun` "a"),
      executeFailed = Just (`fun` "x"),
      expected = False,
      verifyMock = (`hasBeenCalledWith` "a"),
      verifyFailed = (`hasBeenCalledWith` "2"),
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` "a"
    }

    mockTest Fixture {
      name = "arity = 2",
      create = mock $ "a" |> "b" |> True,
      execute = \m -> fun m "a" "b",
      executeFailed = Just (\m -> fun m "a" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b",
      verifyFailed = \m -> hasBeenCalledWith m $ "2" |> "b",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b")
    }

    mockTest Fixture {
      name = "arity = 3",
      create = mock $ (100 :: Int) |> "1" |> True |> (11.1 :: Float),
      expected = 11.1 :: Float,
      execute = \m -> fun m (100 :: Int) "1" True,
      executeFailed = Just \m -> fun m (100 :: Int) "1" False,
      verifyMock = \m -> m `hasBeenCalledWith` ((100 :: Int) |> "1" |> True),
      verifyFailed = \m -> m `hasBeenCalledWith` ((100 :: Int) |> "1" |> False),
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ((100 :: Int) |> "1" |> True)
    }

    mockTest Fixture {
      name = "arity = 4",
      create = mock $ "a" |> "b" |> "c" |> False,
      execute = \m -> fun m "a" "b" "c",
      executeFailed = Just (\m -> fun m "a" "b" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "d",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c")
    }

    mockTest Fixture {
      name = "arity = 5",
      create = mock $ "a" |> "b" |> "c" |> "d" |> True,
      execute = \m -> fun m "a" "b" "c" "d",
      executeFailed = Just (\m -> fun m "a" "b" "c" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d")
    }

    mockTest Fixture {
      name = "arity = 6",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e")
    }

    mockTest Fixture {
      name = "arity = 7",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> True,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f")
    }

    mockTest Fixture {
      name = "arity = 8",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f" "g",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "f" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "y",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g")
    }

    mockTest Fixture {
      name = "arity = 9",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f" "g" "h",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "f" "g" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h")
    }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest VerifyOrderFixture {
        name = "arity = 1",
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

      mockOrderTest VerifyOrderFixture {
        name = "arity = 9",
        create = mock $ any |> any |> any |> any |> any |> any |> any |> any |> (),
        execute = \m -> do
          evaluate $ fun m "1" "2" "3" "4" "5" "6" "7" "8"
          evaluate $ fun m "2" "3" "4" "5" "6" "7" "8" "9"
          evaluate $ fun m "3" "4" "5" "6" "7" "8" "9" "0",
        verifyMock = \m -> m `hasBeenCalledInOrder` [
          "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8",
          "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9",
          "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "0"
        ],
        verifyFailed = \m -> m `hasBeenCalledInOrder` [
          "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x",
          "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "y",
          "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "z"
        ]
      }

  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
  --     m `hasBeenCalledWith` "a"


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