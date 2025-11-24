{-# LANGUAGE GADTs #-}
module Property.ParamSpecRangeMergeRandomProp (spec) where

import Test.Hspec
import Test.QuickCheck
import Support.ParamSpec (ParamSpec(..))
import Support.ParamSpecNormalize

-- ランダムに Range / Exact を生成し normaliseManyInt の受理集合と元集合の OR が一致するか検証
spec :: Spec
spec = describe "ParamSpec Int range merge randomized" $ do
  it "acceptance equivalence on sampled domain" $ property prop_range_merge_acceptance

newtype PSList = PSList [ParamSpec Int]
instance Show PSList where show _ = "<ParamSpec Int list>"

prop_range_merge_acceptance :: Property
prop_range_merge_acceptance = forAll genPSList $ \(PSList ps) ->
  forAll sampleDomain $ \xs ->
    let n = normaliseManyInt ps
    in conjoin [ acceptsNormal n x === any (`acceptsParamSpec` x) ps | x <- xs ]
  where
    genPSList = PSList <$> genParamSpecs
    genParamSpecs = sized $ \sz -> do
      k <- choose (0, min 8 (sz+2))
      vectorOf k genOne
    genOne = frequency
      [ (4, do l <- small; PSRangeInt l <$> small;)
      , (1, PSExact <$> small)
      ]
    small = choose (-20,20)
    sampleDomain = vectorOf 25 (choose (-25,25))
