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
    it "returns a single Param for a function with a single argument" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (VarT m) (TupleT 0))
      createMockBuilderVerifyParams funType
        `shouldBe` AppT (ConT ''Param) (ConT ''String)

    it "connects Params with :> for functions with multiple arguments" $ do
      let m = mkName "m"
          rest = AppT (AppT ArrowT (ConT ''Int)) (AppT (VarT m) (TupleT 0))
          funType = AppT (AppT ArrowT (ConT ''String)) rest
          expected =
            AppT
              (AppT (ConT ''(:>)) (AppT (ConT ''Param) (ConT ''String)))
              (AppT (ConT ''Param) (ConT ''Int))
      createMockBuilderVerifyParams funType `shouldBe` expected

    it "returns () when there are no arguments" $ do
      let m = mkName "m"
      createMockBuilderVerifyParams (AppT (VarT m) (TupleT 0))
        `shouldBe` TupleT 0

  describe "createMockBuilderFnType" $ do
    it "removes monad arguments to make a pure function type" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (VarT m) (ConT ''Int))
          expected = AppT (AppT ArrowT (ConT ''String)) (ConT ''Int)
      createMockBuilderFnType m funType `shouldBe` expected

    it "does not change return values other than the monad variable" $ do
      let m = mkName "m"
          funType = AppT (AppT ArrowT (ConT ''String)) (AppT (ConT ''IO) (ConT ''Int))
      createMockBuilderFnType m funType `shouldBe` funType

    it "removes the monad variable even in types containing forall" $ do
      let m = mkName "m"
          a = mkName "a"
          body = AppT (VarT m) (VarT a)
          funType = ForallT [PlainTV a SpecifiedSpec] [] body
      createMockBuilderFnType m funType `shouldBe` VarT a

  describe "partialAdditionalPredicates (migrated from THMockFnContextSpec)" $ do
    it "adds Typeable and equality constraints for verification parameters in polymorphic functions" $ do
      let a = mkName "a"
          b = mkName "b"
          funType = AppT (AppT ArrowT (VarT a)) (VarT b)
          verifyParams = AppT (ConT ''Param) (VarT a)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "ResolvableParamsOf (a -> b) ~ Param a"
        ]

    it "does not add Typeable for verification parameters when type variables only appear in the return value" $ do
      let a = mkName "a"
          funType = AppT (AppT ArrowT (ConT ''String)) (VarT a)
          verifyParams = AppT (ConT ''Param) (ConT ''String)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "ResolvableParamsOf (String -> a) ~ Param String"
        ]

    it "does not duplicate Typeable even if the same type variable appears multiple times" $ do
      let a = mkName "a"
          funType = AppT (AppT ArrowT (VarT a)) (VarT a)
          verifyParams = AppT (ConT ''Param) (VarT a)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldMatchList`
        [ "ResolvableParamsOf (a -> a) ~ Param a"
        ]

    it "does not add redundant constraints for functions with concrete types" $ do
      let funType =
            AppT
              (AppT ArrowT (ConT ''String))
              (AppT (ConT ''Maybe) (ConT ''UserInput))
          verifyParams = AppT (ConT ''Param) (ConT ''String)
          preds = partialAdditionalPredicates funType verifyParams
      normalize preds `shouldBe` []

  describe "createTypeablePreds" $ do
    it "decomposes and extracts types containing type variables and removes duplicates" $ do
      let a = mkName "a"
          b = mkName "b"
          funType = AppT (AppT ArrowT (VarT a)) (VarT b)
          verifyParams = AppT (ConT ''Param) (VarT a)
          preds = createTypeablePreds [funType, verifyParams]
      normalize preds `shouldMatchList`
        [ "Typeable (a -> b)"
        , "Typeable a"
        , "Typeable b"
        , "Typeable (Param a)"
        ]

    it "extracts type families without decomposing them" $ do
      let m = mkName "m"
          assoc = AppT (ConT (mkName "ResultType")) (VarT m)
          preds = createTypeablePreds [assoc]
      normalize preds `shouldMatchList`
        [ "Typeable (ResultType m)"
        , "Typeable m"
        ]

    it "generates nothing for concrete types only" $ do
      let ty = AppT (ConT ''Maybe) (ConT ''Int)
          preds = createTypeablePreds [ty]
      normalize preds `shouldBe` []

normalize :: [Pred] -> [String]
normalize = fmap (cleanup . pprint)
  where
    cleanup =
      T.unpack
        . T.unwords
        . T.words
        . T.replace (T.pack "\n") (T.pack " ")
        . T.replace (T.pack "Data.Typeable.Internal.") (T.pack "")
        . T.replace (T.pack "GHC.Internal.") (T.pack "")
        . T.replace (T.pack "Base.") (T.pack "")
        . T.replace (T.pack "GHC.Base.") (T.pack "")
        . T.replace (T.pack "GHC.Types.") (T.pack "")
        . T.replace (T.pack "Test.MockCat.Param.") (T.pack "")
        . T.replace (T.pack "Test.MockCat.Verify.") (T.pack "")
        . T.pack
