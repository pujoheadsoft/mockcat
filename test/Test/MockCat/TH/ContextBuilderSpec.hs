{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.MockCat.TH.ContextBuilderSpec (spec) where

import Language.Haskell.TH
import Test.Hspec
import Control.Monad.IO.Class (MonadIO)
import Test.MockCat.MockT (MockT)
import Test.MockCat.TH.ContextBuilder

spec :: Spec
spec = describe "ContextBuilder helpers" $ do
  describe "liftConstraint" $ do
    it "Monad 制約はそのまま維持する" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''Monad) (VarT m)
      liftConstraint m constraint `shouldBe` constraint

    it "モナド型変数に MockT を差し込む" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''MonadIO) (VarT m)
          expected = AppT (ConT ''MonadIO) (AppT (ConT ''MockT) (VarT m))
      liftConstraint m constraint `shouldBe` expected

  describe "mockTType / tyVarBndrToType / applyFamilyArg" $ do
    it "mockTType は MockT を構築する" $ do
      let m = mkName "m"
      mockTType m `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "tyVarBndrToType はモナド変数に一致する PlainTV を MockT に変換する" $ do
      let m = mkName "m"
          binder = PlainTV m SpecifiedSpec
      tyVarBndrToType m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "applyFamilyArg は KindedTV でも動作する" $ do
      let m = mkName "m"
          binder = KindedTV m SpecifiedSpec StarT
      applyFamilyArg m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

  describe "buildContext" $ do
    it "Total モックでは Monad constraint を取り除き MonadIO を追加する" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''Monad) (VarT m)
      buildContext [constraint] Total (mkName "Cls") m [] []
        `shouldBe` [AppT (ConT ''MonadIO) (VarT m)]

    it "Partial モックではクラス自身の制約が追加される" $ do
      let m = mkName "m"
          a = mkName "a"
          cls = mkName "Cls"
          tyVars =
            [ PlainTV m SpecifiedSpec
            , PlainTV a SpecifiedSpec
            ]
          expectedClassConstraint =
            AppT (AppT (ConT cls) (VarT m)) (VarT a)
      buildContext [] Partial cls m tyVars []
        `shouldBe`
          [ AppT (ConT ''MonadIO) (VarT m)
          , expectedClassConstraint
          ]


