{-# LANGUAGE TemplateHaskellQuotes #-}
module Test.MockCat.TH.ClassAnalysisSpec (spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.ClassAnalysis
import Test.MockCat.MockT (MockT)

spec :: Spec
spec = describe "ClassAnalysis helpers" $ do
  describe "toClassInfo" $ do
    it "単一引数のクラス制約から変数名を収集する" $ do
      let a = mkName "a"
          predicate = AppT (ConT ''Monad) (VarT a)
          ClassName2VarNames name vars = toClassInfo predicate
      name `shouldBe` ''Monad
      vars `shouldBe` [a]

    it "ネストした制約でも順に変数を集める" $ do
      let env = mkName "env"
          m = mkName "m"
          predicate = AppT (AppT (ConT ''MonadReader) (VarT env)) (VarT m)
          ClassName2VarNames name vars = toClassInfo predicate
      name `shouldBe` ''MonadReader
      vars `shouldBe` [env, m]

  describe "filterClassInfo" $ do
    it "対象の型変数を含むクラスのみ残す" $ do
      let a = mkName "a"
          b = mkName "b"
          infos =
            [ ClassName2VarNames ''Monad [a]
            , ClassName2VarNames ''Applicative [b]
            ]
      fmap (\(ClassName2VarNames name vars) -> (name, vars)) (filterClassInfo a infos)
        `shouldBe` [(''Monad, [a])]

  describe "filterMonadicVarInfos" $ do
    it "Monad 制約を持つ変数のみ残す" $ do
      let vars =
            [ VarName2ClassNames (mkName "m") [''Monad, ''MonadIO]
            , VarName2ClassNames (mkName "x") [''Applicative]
            ]
      fmap (\(VarName2ClassNames name classes) -> (name, classes)) (filterMonadicVarInfos vars)
        `shouldBe` [(mkName "m", [''Monad, ''MonadIO])]

  describe "getClassName / getClassNames" $ do
    it "最上位のクラス名を取得する" $ do
      getClassName (AppT (ConT ''MonadReader) (VarT (mkName "env"))) `shouldBe` ''MonadReader

    it "ネストしたクラス適用から全ての名前を取得する" $ do
      let a = mkName "a"
          ty = AppT (AppT (ConT ''Either) (ConT ''String)) (VarT a)
      getClassNames ty `shouldBe` [''Either, ''String]

  describe "VarApplied helpers" $ do
    it "applyVarAppliedTypes は型変数をクラス名に置き換える" $ do
      let m = mkName "m"
          a = mkName "a"
          mapping =
            [ VarAppliedType m (Just ''Maybe)
            , VarAppliedType a Nothing
            ]
          ty = AppT (AppT ArrowT (VarT m)) (VarT a)
          expected = AppT (AppT ArrowT (ConT ''Maybe)) (VarT a)
      applyVarAppliedTypes mapping ty `shouldBe` expected

    it "updateType は VarT の組み合わせをクラス名へ差し替える" $ do
      let m = mkName "m"
          a = mkName "a"
          mapping =
            [ VarAppliedType m (Just ''MockT)
            , VarAppliedType a (Just ''Int)
            ]
          ty = AppT (VarT m) (VarT a)
          expected = AppT (ConT ''MockT) (ConT ''Int)
      updateType ty mapping `shouldBe` expected


