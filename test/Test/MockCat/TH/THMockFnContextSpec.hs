{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THMockFnContextSpec (spec) where

import qualified Data.Text as T
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.Param (Param)
import Test.MockCat.SharedSpecDefs (UserInput (..))
import Test.MockCat.TH.MockFnContext (partialAdditionalPredicates)

spec :: Spec
spec = describe "partialAdditionalPredicates" $ do
  it "ポリモーフィックな関数では型変数分のTypeableと等式制約を付与する" $ do
    let a = mkName "a"
        b = mkName "b"
        funType = AppT (AppT ArrowT (VarT a)) (VarT b)
        verifyParams = AppT (ConT ''Param) (VarT a)
        preds = partialAdditionalPredicates funType verifyParams
    normalize preds `shouldMatchList`
      [ "Typeable a"
      , "Typeable b"
      , "Test.MockCat.Verify.ResolvableParamsOf (a -> b) ~ Test.MockCat.Param.Param a"
      ]

  it "同じ型変数が複数回現れてもTypeableは重複しない" $ do
    let a = mkName "a"
        funType = AppT (AppT ArrowT (VarT a)) (VarT a)
        verifyParams = AppT (ConT ''Param) (VarT a)
        preds = partialAdditionalPredicates funType verifyParams
    normalize preds `shouldMatchList`
      [ "Typeable a"
      , "Test.MockCat.Verify.ResolvableParamsOf (a -> a) ~ Test.MockCat.Param.Param a"
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
normalize = fmap cleanup . fmap pprint
  where
    cleanup =
      T.unpack
        . T.replace (T.pack "\n") (T.pack " ")
        . T.replace (T.pack "Data.Typeable.Internal.") (T.pack "")
        . T.pack

