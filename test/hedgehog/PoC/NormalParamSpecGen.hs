module PoC.NormalParamSpecGen (genParamSpecIntH, prop_genFromNormalInt_sound) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.MockCat.ParamSpec (ParamSpec(..))
import Test.MockCat.Internal.ParamSpec.Normalize

-- Hedgehog generator for ParamSpec Int (subset avoiding predicates sometimes)
genParamSpecIntH :: Gen (ParamSpec Int)
genParamSpecIntH = Gen.choice
  [ PSExact <$> small
  , do l <- small; h <- small; pure (PSRangeInt l h)
  , do n <- Gen.int (Range.linear 1 5)
       xs <- Gen.list (Range.singleton n) small
       pure (PSEnum xs)
  , pure PSAny
  ]
  where
    small = Gen.int (Range.linear (-5) 5)

prop_genFromNormalInt_sound :: Property
prop_genFromNormalInt_sound = property $ do
  ps <- forAllWith (const "<ParamSpec>") genParamSpecIntH
  let n = normalise ps
  case genFromNormalInt n of
    Nothing -> success
    Just f  -> do
      let seeds = [0..199]
          vals  = map f seeds
      assert (all (acceptsNormal n) vals)
  where
    -- Avoid Show constraint by not collecting the value; Hedgehog only needs Show for failure rendering.
    _suppress :: ParamSpec Int -> ()
    _suppress _ = ()
