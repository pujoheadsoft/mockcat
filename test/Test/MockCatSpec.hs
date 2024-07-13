{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Test.MockCatSpec (spec) where

import Test.Hspec
import Test.MockCat
import Test.MockCat.Param
import Test.MockCat
import Control.Exception (evaluate)

data Fixture mock r = Fixture {
  name :: String,
  create :: () -> IO mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r
  -- verifyMock :: mock -> IO (),
  -- verifyCount :: mock -> Int -> IO (),
  -- verifyFailed :: mock -> IO ()
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
  it "Returns a set value when called with a set argument." do
    m <- f.create ()
    f.execute m `shouldBe` f.expected

  it "Failure to call with set arguments." do
    case f.executeFailed of
      Just func -> do
        m <- f.create ()
        evaluate (func m) `shouldThrow` anyErrorCall
      Nothing -> pure ()

  -- it "that the call was made with the set arguments." do
  --   m <- f.create unit
  --   let _ = f.execute m
  --   f.verifyMock m

  -- it "fails if the call is made with arguments different from those set." do
  --   m <- f.create unit
  --   let _ = f.execute m
  --   expectError $ f.verifyFailed m

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
    create = \_ -> mock $ "a" |> False,
    execute = (`fun` "a"),
    executeFailed = Just (\m -> fun m "x"),
    expected = False
  }

  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
      --verify m "a"
