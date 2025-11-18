{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THVarAppliedSpec (spec) where

import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.VarApplied
import Test.MockCat.MockT (MockT)

spec :: Spec
spec = do
  describe "applyVarAppliedTypes" $ do
    it "変換対象の型変数をクラス名に置き換える" $ do
      let m = mkName "m"
          a = mkName "a"
          mapping =
            [ VarAppliedType m (Just ''Maybe)
            , VarAppliedType a Nothing
            ]
          ty = AppT (AppT ArrowT (VarT m)) (VarT a)
          expected = AppT (AppT ArrowT (ConT ''Maybe)) (VarT a)
      applyVarAppliedTypes mapping ty `shouldBe` expected

    it "Forall 内の制約や型にも再帰的に適用する" $ do
      let m = mkName "m"
          b = mkName "b"
          mapping = [VarAppliedType m (Just ''Maybe)]
          inner = AppT (VarT m) (VarT b)
          ty = ForallT [] [VarT m] inner
          expected = ForallT [] [ConT ''Maybe] (AppT (ConT ''Maybe) (VarT b))
      applyVarAppliedTypes mapping ty `shouldBe` expected

  describe "updateType" $ do
    it "VarTの組み合わせをクラス名へ差し替える" $ do
      let m = mkName "m"
          a = mkName "a"
          mapping =
            [ VarAppliedType m (Just ''MockT)
            , VarAppliedType a (Just ''Int)
            ]
          ty = AppT (VarT m) (VarT a)
          expected = AppT (ConT ''MockT) (ConT ''Int)
      updateType ty mapping `shouldBe` expected

    it "対応するマッピングが無ければ元の型を返す" $ do
      let m = mkName "m"
          ty = AppT (VarT m) (VarT m)
      updateType ty [] `shouldBe` ty

  describe "findClass/hasClass" $ do
    it "マッピングされたクラス名を取得できる" $ do
      let a = mkName "a"
          cls = ''Maybe
          mapping = [VarAppliedType a (Just cls)]
      hasClass a mapping `shouldBe` True
      findClass a mapping `shouldBe` Just cls

    it "マッピングが無ければ Nothing" $ do
      let a = mkName "a"
      hasClass a [] `shouldBe` False
      findClass a [] `shouldBe` Nothing

