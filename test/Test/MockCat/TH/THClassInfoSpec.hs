{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THClassInfoSpec (spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.List (isInfixOf)
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.TH.ClassInfo

spec :: Spec
spec = describe "ClassInfo helpers" $ do
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

  describe "Show instances" $ do
    it "ClassName2VarNames の表示にクラス名が含まれる" $ do
      show (ClassName2VarNames ''Monad [mkName "m"])
        `shouldSatisfy` ("Monad" `isInfixOf`)

    it "VarName2ClassNames の表示にクラス名一覧が含まれる" $ do
      show (VarName2ClassNames (mkName "m") [''Monad, ''MonadIO])
        `shouldSatisfy` ("Monad" `isInfixOf`)

