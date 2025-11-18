{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THFunctionTypeSpec (spec) where

import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.FunctionType (isNotConstantFunctionType)

spec :: Spec
spec = describe "isNotConstantFunctionType" $ do
  it "矢印を含む型は True" $ do
    let ty = AppT (AppT ArrowT (ConT ''Int)) (ConT ''Bool)
    isNotConstantFunctionType ty `shouldBe` True

  it "タプルは False" $ do
    isNotConstantFunctionType (TupleT 0) `shouldBe` False

  it "forall で包まれていても判定できる" $ do
    let a = mkName "a"
        body = AppT (AppT ArrowT (VarT a)) (ConT ''Int)
        ty = ForallT [PlainTV a SpecifiedSpec] [] body
    isNotConstantFunctionType ty `shouldBe` True


