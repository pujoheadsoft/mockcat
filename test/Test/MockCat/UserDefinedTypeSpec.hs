{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.MockCat.UserDefinedTypeSpec (spec) where

import Test.Hspec
import Test.MockCat

data Post = Post { postId :: Int, title :: String }
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "user-defined type comparison" $ do
    it "should be able to compare user-defined types with Eq and Show" $ do
      let post = Post 1 "title"
      f <- mock (post ~> True)
      f post `shouldBe` True
