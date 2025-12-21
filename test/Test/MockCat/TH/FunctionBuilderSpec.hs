{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.MockCat.TH.FunctionBuilderSpec (spec) where

import qualified Data.Text as T
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.Cons ((:>))
import Test.MockCat.Param (Param)
import Test.MockCat.TH.FunctionBuilder
import Test.MockCat.SharedSpecDefs (UserInput(..))

spec :: Spec
spec = do
  describe "createMockBuilderVerifyParams" $ do
    it "単一引数の関数では Param が1つだけ付く" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (VarT m) (TupleT 0))
      createMockBuilderVerifyParams funType
        `shouldBe` AppT (ConT ''Param) (ConT ''String)

    it "複数引数の関数では Param が :> で連結される" $ do
      let m = mkName "m"
          rest = AppT (AppT ArrowT (ConT ''Int)) (AppT (VarT m) (TupleT 0))
          funType = AppT (AppT ArrowT (ConT ''String)) rest
          expected =
            AppT
              (AppT (ConT ''(:>)) (AppT (ConT ''Param) (ConT ''String)))
              (AppT (ConT ''Param) (ConT ''Int))
      createMockBuilderVerifyParams funType `shouldBe` expected

    it "引数が無い場合は () を返す" $ do
      let m = mkName "m"
      createMockBuilderVerifyParams (AppT (VarT m) (TupleT 0))
        `shouldBe` TupleT 0

  describe "createMockBuilderFnType" $ do
    it "モナド引数を取り除き純粋な関数型にする" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (VarT m) (ConT ''Int))
          expected = AppT (AppT ArrowT (ConT ''String)) (ConT ''Int)
      createMockBuilderFnType m funType `shouldBe` expected

    it "モナド変数以外の戻り値は変更しない" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (ConT ''IO) (ConT ''Int))
      createMockBuilderFnType m funType `shouldBe` funType

    it "forall を含む型でもモナド変数を取り除く" $ do
      let m = mkName "m"
          a = mkName "a"
          body = AppT (VarT m) (VarT a)
          funType = ForallT [PlainTV a SpecifiedSpec] [] body
      createMockBuilderFnType m funType `shouldBe` VarT a

  describe "partialAdditionalPredicates (migrated from THMockFnContextSpec)" $ do
    it "ポリモーフィックな関数では検証用パラメータのTypeableと等式制約を付与する" $ do
      let a = mkName "a"
          b = mkName "b"
          funType = AppT (AppT ArrowT (VarT a)) (VarT b)
          verifyParams = AppT (ConT ''Param) (VarT a)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "Typeable (Param a)"
        , "ResolvableParamsOf (a -> b) ~ Param a"
        ]

    it "戻り値にのみ型変数が含まれる場合、検証用パラメータのTypeableは付与されない (funTypeのTypeableは呼び出し側で付与される)" $ do
      let a = mkName "a"
          funType = AppT (AppT ArrowT (ConT ''String)) (VarT a)
          verifyParams = AppT (ConT ''Param) (ConT ''String)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "ResolvableParamsOf (String -> a) ~ Param String"
        ]

    it "同じ型変数が複数回現れてもTypeableは重複しない" $ do
      let a = mkName "a"
          funType = AppT (AppT ArrowT (VarT a)) (VarT a)
          verifyParams = AppT (ConT ''Param) (VarT a)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "Typeable (Param a)"
        , "ResolvableParamsOf (a -> a) ~ Param a"
        ]

    it "具象型の関数では冗長な制約を付与しない" $ do
      let funType =
            AppT
              (AppT ArrowT (ConT ''String))
              (AppT (ConT ''Maybe) (ConT ''UserInput))
          verifyParams = AppT (ConT ''Param) (ConT ''String)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldBe` []

normalize :: [Pred] -> [String]
normalize = fmap (cleanup . pprint)
  where
    cleanup =
      T.unpack
        . T.replace (T.pack "\n") (T.pack " ")
        . T.replace (T.pack "Data.Typeable.Internal.") (T.pack "")
        . T.replace (T.pack "GHC.Internal.") (T.pack "")
        . T.replace (T.pack "GHC.Base.") (T.pack "")
        . T.replace (T.pack "GHC.Types.") (T.pack "")
        . T.replace (T.pack "Test.MockCat.Param.") (T.pack "")
        . T.replace (T.pack "Test.MockCat.Verify.") (T.pack "")
        . T.pack
