{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
module Property.ParamSpecNormalizeProp (spec, prop_enum_duplicate_equivalence) where

import Test.Hspec
import Test.QuickCheck
import Data.List (nub)
import "mockcat" Test.MockCat.ParamSpec (ParamSpec(..))
import Support.ParamSpecNormalize
import Data.Hashable (Hashable)

-- Generator for small ParamSpec Int
-- Avoid empty enum (library contract), but include duplicates.
genParamSpecInt :: Gen (ParamSpec Int)
genParamSpecInt = frequency
  [ (3, PSExact <$> smallInt)
  , (3, sizedEnum)
  , (2, rangeSpec)
  , (1, pure PSAny)
  , (1, pure (PSPredicate even "even"))
  ]
  where
    smallInt = choose (-5, 5)
    sizedEnum = do
      n <- choose (1,5)
      xs <- vectorOf n smallInt
      pure (PSEnum xs)
    rangeSpec = do
      a <- smallInt; b <- smallInt
      pure (PSRangeInt a b)

pretty :: ParamSpec Int -> String
pretty ps = case ps of
  PSExact a -> "Exact " ++ show a
  PSEnum xs -> "Enum " ++ show xs
  PSRangeInt l h -> "Range " ++ show (l,h)
  PSAny -> "Any"
  PSPredicate _ l -> "Pred " ++ l

-- Property: normalise . toParamSpec . normalise ≡ normalise (idempotence via roundtrip)
prop_normalise_idempotent :: Property
prop_normalise_idempotent = forAll (Blind <$> genParamSpecInt) $ \(Blind ps) ->
  let n1 = normalise ps
      n2 = normalise (toParamSpec n1)
  in counterexample ("ps=" ++ pretty ps) (n1 == n2)

-- Property: acceptance equivalence on a finite sample domain
prop_acceptance_equivalence :: Property
prop_acceptance_equivalence = forAll (Blind <$> genParamSpecInt) $ \(Blind ps) ->
  forAll sampleDomain $ \xs ->
    let n = normalise ps
        ok = all (\x -> acceptsParamSpec ps x == acceptsNormal n x) xs
    in counterexample ("ps=" ++ pretty ps ++ "; xs sample mismatch") ok
  where
    sampleDomain = vectorOf 20 (choose (-7,7))

spec :: Spec
spec = do
  describe "ParamSpec Normalisation" $ do
    it "idempotent (roundtrip)" $ prop_normalise_idempotent
    it "acceptance equivalence on sample domain" $ prop_acceptance_equivalence
    it "enum duplicate equivalence" $ prop_enum_duplicate_equivalence

-- Property A: Enum に重複があっても正規化後の形が期待通り (単一点→NExact / 複数→NEnum uniq)
prop_enum_duplicate_equivalence :: Property
prop_enum_duplicate_equivalence = forAll genDupEnum $ \(xs :: [Int]) ->
  let n = normalise (PSEnum xs)
      uniq = nub xs
  in case uniq of
       []    -> counterexample "unexpected empty after nub" False
       [x]   -> case n of
                  NExact y -> counterexample (show n) (y == x)
                  _        -> counterexample (show n) False
       _     -> case n of
                  NEnum ys -> counterexample (show n) (ys == uniq)
                  _        -> counterexample (show n ++ " expected NEnum") False
  where
    genDupEnum :: Gen [Int]
    genDupEnum = do
      baseLen <- choose (1,5)
      base <- vectorOf baseLen (choose (-5,5))
      dupFactor <- choose (0,3)
      extras <- concat <$> mapM (\v -> vectorOf dupFactor (pure v)) base
      shuffle (base ++ extras)
