module Test.MockCat.Readme.WithMockIOSpec (spec) where 

import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "User Guide (withMockIO)" $ do
    withMockIO $ do
      f <- mock ("Hello" ~> True)
        `expects` called once

      let result = f "Hello"

      result `shouldBe` True
