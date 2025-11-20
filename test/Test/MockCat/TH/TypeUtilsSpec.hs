{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.TypeUtilsSpec (spec) where

import qualified Data.Map.Strict as Map
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.TypeUtils

spec :: Spec
spec = do
  describe "splitApps" $ do
    it "関数型の引数を順に展開する" $ do
      let a = mkName "a"
          ty = AppT (AppT ArrowT (ConT ''Int)) (VarT a)
      splitApps ty `shouldBe` (ArrowT, [ConT ''Int, VarT a])

    it "入れ子の型適用を最上位と引数列に分解する" $ do
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

    it "適用の無い型はそのまま返す" $ do
      splitApps (ConT ''Bool) `shouldBe` (ConT ''Bool, [])

  describe "substituteType" $ do
    it "VarTをマップの型で置き換える" $ do
      let a = mkName "a"
          b = mkName "b"
          ty = AppT (VarT a) (VarT b)
          subMap =
            Map.fromList
              [ (a, ConT ''Int)
              , (b, ConT ''Bool)
              ]
      substituteType subMap ty `shouldBe` AppT (ConT ''Int) (ConT ''Bool)

    it "ForallT内の制約と本体にも再帰的に適用される" $ do
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

    it "マップに無い型変数はそのまま残す" $ do
      let a = mkName "a"
          ty = VarT a
      substituteType Map.empty ty `shouldBe` VarT a

  describe "isNotConstantFunctionType" $ do
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


