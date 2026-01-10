module Test.MockCat.Readme.WithMockSpec (spec) where 

import Test.Hspec
import Test.MockCat
import Control.Monad.IO.Class (MonadIO(liftIO))

spec :: Spec
spec = do
  it "User Guide (withMock)" $ do
    withMock $ do
      -- "Hello" に対して True を返すモックを定義
      f <- mock ("Hello" ~> True)
        `expects` called once

      -- 実行
      let result = f "Hello"

      liftIO $ result `shouldBe` True

