module Test.MockCat.StubSpec where

import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "createPureStubFn" $ do
    let f = createPureStubFn $ "value" |> True
    f "value" `shouldBe` True

  it "createPureStubFn with multiple arguments" $ do
    let f = createPureStubFn $ "value1" |> "value2" |> True
    f "value1" "value2" `shouldBe` True

  it "createPureStubFn with multiple arguments and return value" $ do
    let f = createPureStubFn $ "value1" |> "value2" |> "value3" |> True
    f "value1" "value2" "value3" `shouldBe` True
    
  it "createPureStubFn with multiple arguments and return value" $ do
    let f = createPureStubFn $ "value1" |> "value2" |> "value3" |> "value4" |> True
    f "value1" "value2" "value3" "value4" `shouldBe` True