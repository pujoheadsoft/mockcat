module Test.MockCat.Readme.QuickStartDemoSpec (spec) where

import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Quick Start Demo" $ do
    withMockIO $ do
      -- 1. モックを作成 ("Hello" を受け取ったら 42 を返す)
      --    同時に「1回呼ばれるはずだ」と期待を宣言
      f <- mock ("Hello" ~> (42 :: Int))
        `expects` called once

      -- 2. f "Hello" を呼び出し、42 を得る
      f "Hello" `shouldBe` 42

      -- 3. スコープを抜ける際、宣言した期待に対する検証が自動で行われる
