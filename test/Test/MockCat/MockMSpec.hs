{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}

module Test.MockCat.MockMSpec (spec) where

import Test.Hspec
import Test.MockCat
import Test.MockCat.Mock (createMockSession, runMockSession, createStubFnM, createMockM, stubFnM)
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = describe "MockM API" do
  it "createStubFnM returns expected value" do
    session <- createMockSession
    runMockSession session do
      f <- createStubFnM $ "a1" |> True
      r <- f "a1"
      liftIO $ r `shouldBe` True

  it "createMockM and stubFnM usage" do
    session <- createMockSession
    runMockSession session do
      m <- createMockM $ "a2" |> "b2" |> True
      let f = stubFnM m
      r <- f "a2" "b2"
      liftIO $ r `shouldBe` True


