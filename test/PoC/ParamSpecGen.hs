{-# LANGUAGE ScopedTypeVariables #-}
module PoC.ParamSpecGen
  ( genFromParamSpec
  , prop_exact
  , prop_rangeIntWithin
  , prop_enumOnly
  ) where

import Test.QuickCheck
import Test.MockCat (ParamSpec(..))

-- | Derive a QuickCheck generator from a ParamSpec.
-- This is a *test-side* PoC; not yet part of the public API contract.
-- Fallback strategies:
--   PSPredicate -> arbitrary `suchThat` p (may shrink poorly)
--   PSAny       -> arbitrary
-- NOTE: PSEnum [] is treated as failing generator (using 'discard') for now.
-- Range invariants (low <= high) are assumed; if violated we swap to be forgiving.

genFromParamSpec :: forall a. (Arbitrary a) => ParamSpec a -> Gen a
genFromParamSpec (PSExact a)      = pure a
genFromParamSpec (PSEnum [])      = discard
genFromParamSpec (PSEnum xs)      = elements xs
genFromParamSpec (PSRangeInt l h) = let (lo, hi) = if l <= h then (l,h) else (h,l) in chooseInt (lo,hi)
genFromParamSpec (PSPredicate p _) = arbitrary `suchThat` p
genFromParamSpec PSAny            = arbitrary

-- Properties ---------------------------------------------------------------

prop_exact :: Int -> Property
prop_exact n = forAll (genFromParamSpec (PSExact n)) (== n)

prop_rangeIntWithin :: Int -> Int -> Property
prop_rangeIntWithin a b = a /= b ==> forAll (genFromParamSpec (PSRangeInt a b)) $ \v ->
  let lo = min a b; hi = max a b in v >= lo && v <= hi

prop_enumOnly :: NonEmptyList Int -> Property
prop_enumOnly (NonEmpty xs) = let spec = PSEnum xs in
  forAll (genFromParamSpec spec) (`elem` xs)
