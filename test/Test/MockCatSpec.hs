{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test.MockCatSpec (spec) where

import Test.Hspec
import Test.MockCat (mock, fun, hasBeenCalledWith, hasBeenCalledTimes, with)
import Test.MockCat.Param
import Control.Exception (evaluate)
import Data.Function ((&))
import Control.Monad (replicateM_)
import Data.Foldable (forM_)
import Data.Traversable (for)

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

-- data VerifyOrderFixture mock r m = VerifyOrderFixture {
--   name :: String,
--   create :: () -> m mock,
--   execute :: mock -> r,
--   verifyMock :: mock -> m (),
--   verifyFailed :: mock -> m ()
-- }

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

  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
      --verify m "a"
