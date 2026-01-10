{-# LANGUAGE TypeApplications #-}
module Test.MockCat.Readme.MatcherSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (any)
import Data.List (isPrefixOf)

spec :: Spec
spec = do
  it "Matcher Examples" $ do
    -- 任意の文字列 (param any)
    f <- mock (any @String ~> True)
    f "foo" `shouldBe` True

    -- 条件式 (when)
    g <- mock (when (> (5 :: Int)) "> 5" ~> True)
    g 6 `shouldBe` True
  
  it "When Example" $ do
    -- 引数が "error" で始まる場合のみ False を返す
    f <- mock $ do
      onCase $ when (\s -> "error" `isPrefixOf` s) "start with error" ~> False
      onCase $ any ~> True

    f "error message" `shouldBe` False
    f "success" `shouldBe` True
    f "other" `shouldBe` True