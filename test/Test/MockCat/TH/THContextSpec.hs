{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.THContextSpec (spec) where

import Control.Monad.IO.Class (MonadIO)
import Language.Haskell.TH
import Test.Hspec
import Test.MockCat.MockT (MockT)
import Test.MockCat.TH.Context

spec :: Spec
spec = describe "buildContext" $ do
  it "Total モックでは Monad constraint を取り除き MonadIO を追加する" $ do
    let m = mkName "m"
        constraint = AppT (ConT ''Monad) (VarT m)
    buildContext [constraint] Total (mkName "Cls") m [] []
      `shouldBe` [AppT (ConT ''MonadIO) (VarT m)]

  it "既に MonadIO が存在する場合は重複させない" $ do
    let m = mkName "m"
        monadIOConstraint = AppT (ConT ''MonadIO) (VarT m)
    buildContext [monadIOConstraint] Total (mkName "Cls") m [] []
      `shouldBe` [AppT (ConT ''MonadIO) (AppT (ConT ''MockT) (VarT m))]

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

