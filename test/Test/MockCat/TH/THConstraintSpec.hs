{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THConstraintSpec (spec) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.MockT (MockT)
import Test.MockCat.TH.Constraint (liftConstraint)

spec :: Spec
spec = describe "liftConstraint" $ do
  it "Monad 制約はそのまま維持する" $ do
    let m = mkName "m"
        constraint = AppT (ConT ''Monad) (VarT m)
    liftConstraint m constraint `shouldBe` constraint

  it "モナド型変数に MockT を差し込む" $ do
    let m = mkName "m"
        constraint = AppT (ConT ''MonadIO) (VarT m)
        expected = AppT (ConT ''MonadIO) (AppT (ConT ''MockT) (VarT m))
    liftConstraint m constraint `shouldBe` expected

  it "ネストした制約でモナド変数が引数側に現れる場合はそのまま" $ do
    let m = mkName "m"
        a = mkName "a"
        constraint =
          AppT
            (AppT (ConT ''MonadReader) (VarT a))
            (AppT (VarT m) (ConT ''Int))
    liftConstraint m constraint `shouldBe` constraint

