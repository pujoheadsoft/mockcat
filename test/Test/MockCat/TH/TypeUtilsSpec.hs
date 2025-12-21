{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.MockCat.TH.TypeUtilsSpec (spec) where

import qualified Data.Map.Strict as Map
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.TypeUtils

spec :: Spec
spec = do
  describe "splitApps" $ do
    it "expands function type arguments in order" $ do
      let a = mkName "a"
          ty = AppT (AppT ArrowT (ConT ''Int)) (VarT a)
      splitApps ty `shouldBe` (ArrowT, [ConT ''Int, VarT a])

    it "decomposes nested type applications into top-level and argument list" $ do
      let a = mkName "a"
          b = mkName "b"
          ty =
            AppT
              (ConT ''Maybe)
              (AppT (AppT (ConT ''Either) (VarT a)) (VarT b))
      splitApps ty
        `shouldBe` ( ConT ''Maybe
                   , [AppT (AppT (ConT ''Either) (VarT a)) (VarT b)]
                   )

    it "returns types without application as they are" $ do
      splitApps (ConT ''Bool) `shouldBe` (ConT ''Bool, [])

  describe "substituteType" $ do
    it "replaces VarT with types from the map" $ do
      let a = mkName "a"
          b = mkName "b"
          ty = AppT (VarT a) (VarT b)
          subMap =
            Map.fromList
              [ (a, ConT ''Int)
              , (b, ConT ''Bool)
              ]
      substituteType subMap ty `shouldBe` AppT (ConT ''Int) (ConT ''Bool)

    it "recursively applies to constraints and body within ForallT" $ do
      let a = mkName "a"
          b = mkName "b"
          ty =
            ForallT
              [PlainTV a SpecifiedSpec]
              [AppT (ConT ''Eq) (VarT a)]
              (AppT (VarT b) (VarT a))
          subMap =
            Map.fromList
              [ (a, ConT ''Char)
              , (b, ConT ''Maybe)
              ]
          expected =
            ForallT
              [PlainTV a SpecifiedSpec]
              [AppT (ConT ''Eq) (ConT ''Char)]
              (AppT (ConT ''Maybe) (ConT ''Char))
      substituteType subMap ty `shouldBe` expected

    it "leaves type variables not in the map as they are" $ do
      let a = mkName "a"
          ty = VarT a
      substituteType Map.empty ty `shouldBe` VarT a

  describe "isNotConstantFunctionType" $ do
    it "is True for types containing arrows" $ do
      let ty = AppT (AppT ArrowT (ConT ''Int)) (ConT ''Bool)
      isNotConstantFunctionType ty `shouldBe` True

    it "is False for tuples" $ do
      isNotConstantFunctionType (TupleT 0) `shouldBe` False

    it "can judge even when wrapped in forall" $ do
      let a = mkName "a"
          body = AppT (AppT ArrowT (VarT a)) (ConT ''Int)
          ty = ForallT [PlainTV a SpecifiedSpec] [] body
      isNotConstantFunctionType ty `shouldBe` True


