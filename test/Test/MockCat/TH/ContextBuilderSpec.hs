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
    it "preserves Monad constraints" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''Monad) (VarT m)
      liftConstraint m constraint `shouldBe` constraint

    it "injects MockT into the monad type variable" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''MonadIO) (VarT m)
          expected = AppT (ConT ''MonadIO) (AppT (ConT ''MockT) (VarT m))
      liftConstraint m constraint `shouldBe` expected

  describe "mockTType / tyVarBndrToType / applyFamilyArg" $ do
    it "mockTType constructs MockT" $ do
      let m = mkName "m"
      mockTType m `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "tyVarBndrToType converts a PlainTV matching the monad variable to MockT" $ do
      let m = mkName "m"
          binder = PlainTV m SpecifiedSpec
      tyVarBndrToType m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

    it "applyFamilyArg works with KindedTV" $ do
      let m = mkName "m"
          binder = KindedTV m SpecifiedSpec StarT
      applyFamilyArg m binder `shouldBe` AppT (ConT ''MockT) (VarT m)

  describe "buildContext" $ do
    it "removes Monad constraints and adds MonadIO in Total mocks" $ do
      let m = mkName "m"
          constraint = AppT (ConT ''Monad) (VarT m)
      buildContext [constraint] Total (mkName "Cls") m [] []
        `shouldBe` [AppT (ConT ''MonadIO) (VarT m)]

    it "adds the class itself as a constraint in Partial mocks" $ do
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


