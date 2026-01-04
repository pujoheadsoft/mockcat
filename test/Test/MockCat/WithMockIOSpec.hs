{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.WithMockIOSpec (spec) where

import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  describe "withMockIO" $ do
    it "can run IO actions directly without liftIO" $ do
      withMockIO $ do
        f <- mock $ "hello" ~> "world"
        f "hello" `shouldBe` "world"
        f `shouldBeCalled` "hello"
