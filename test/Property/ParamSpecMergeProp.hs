{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
module Property.ParamSpecMergeProp (spec) where

import Test.Hspec
import Test.QuickCheck
import Support.ParamSpec (ParamSpec(..))
import Support.ParamSpecNormalize

spec :: Spec
spec = describe "ParamSpec Int merge (normaliseManyInt)" $ do
  it "overlapping ranges merge" $ property prop_overlap_merge
  it "adjacent ranges merge" $ property prop_adjacent_merge
  it "separate ranges stay separate" $ property prop_separate_no_merge

prop_overlap_merge :: Property
prop_overlap_merge =
  let r = normaliseManyInt [PSRangeInt 1 5, PSRangeInt 3 10]
  in case r of
       NRangeInt 1 10 -> property True
       _              -> counterexample (show r) False

prop_adjacent_merge :: Property
prop_adjacent_merge =
  let r = normaliseManyInt [PSRangeInt 1 5, PSRangeInt 6 8]
  in case r of
       NRangeInt 1 8 -> property True
       _             -> counterexample (show r) False

prop_separate_no_merge :: Property
prop_separate_no_merge =
  let r = normaliseManyInt [PSRangeInt 1 3, PSRangeInt 5 7]
  in case r of
       NUnion [NRangeInt 1 3, NRangeInt 5 7] -> property True
       _ -> counterexample (show r) False
