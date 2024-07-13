{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test.MockCatSpec (spec) where

import Test.Hspec
import Test.MockCat
import Test.MockCat.Param
import Control.Exception (evaluate)

data Fixture mock r = Fixture {
  name :: String,
  create :: IO mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> IO (),
  verifyFailed :: mock -> IO ()
  -- verifyCount :: mock -> Int -> IO (),
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
    case f.executeFailed of
      Just func -> do
        m <- f.create
        evaluate (func m) `shouldThrow` anyErrorCall
      Nothing -> pure ()

  it "Expected arguments are applied, the verification succeeds." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyMock m

  it "Unexpected arguments are applied, the verification fails." do
    m <- f.create
    evaluate $ f.execute m 
    f.verifyFailed m `shouldThrow` anyErrorCall

  -- it "the number of times it has been called with the set arguments (0 times)." do
  --   m <- f.create unit
  --   f.verifyCount m 0

  -- it "the number of times it has been called with the set arguments (3 times)." do
  --   m <- f.create unit
  --   let
  --     _ = f.execute m
  --     _ = f.execute m
  --     _ = f.execute m
  --   f.verifyCount m 3

spec :: Spec
spec = do
  mockTest Fixture {
    name = "",
    create = mock $ "a" |> False,
    execute = (`fun` "a"),
    executeFailed = Just (\m -> fun m "x"),
    expected = False,
    verifyMock = (`hasBeenCalledWith` "a"),
    verifyFailed = (`hasBeenCalledWith` "2")
  }

  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
      --verify m "a"
