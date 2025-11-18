{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THMonadVarSpec (spec) where

import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.MockT (MockT)
import Test.MockCat.TH.MonadVar

spec :: Spec
spec = describe "MonadVar helpers" $ do
  describe "mockTType" $ do
    it "指定した変数名で MockT 型を構築する" $ do
      let m = mkName "m"
      mockTType m `shouldBe` AppT (ConT ''MockT) (VarT m)

  describe "tyVarBndrToType" $ do
    it "モナド変数に一致する PlainTV は MockT に変換する" $ do
      let m = mkName "m"
          binder = PlainTV m SpecifiedSpec
      tyVarBndrToType m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "異なる変数は VarT のまま" $ do
      let m = mkName "m"
          a = mkName "a"
          binder = PlainTV a SpecifiedSpec
      tyVarBndrToType m binder `shouldBe` VarT a

  describe "applyFamilyArg" $ do
    it "KindedTV でもモナド変数を MockT で包む" $ do
      let m = mkName "m"
          binder = KindedTV m SpecifiedSpec StarT
      applyFamilyArg m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "モナド変数以外は VarT で返す" $ do
      let m = mkName "m"
          b = mkName "b"
          binder = KindedTV b SpecifiedSpec StarT
      applyFamilyArg m binder `shouldBe` VarT b

