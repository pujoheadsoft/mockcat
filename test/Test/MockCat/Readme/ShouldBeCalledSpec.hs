module Test.MockCat.Readme.ShouldBeCalledSpec (spec) where

import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Function Mocking" $ do
    -- "Hello" に対して 1 を返す関数を定義 (expects は書かない)
    f <- mock ("Hello" ~> True)
    
    -- 実行
    f "Hello" `shouldBe` True

    -- 事後検証 (shouldBeCalled)
    f `shouldBeCalled` "Hello"
