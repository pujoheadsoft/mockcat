{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
module PoC.ParamSpecIntGenBridge (prop_paramSpec_bridge_sound) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.MockCat.ParamSpec (ParamSpec(..))
import Test.MockCat.Internal.ParamSpec.Normalize
import Test.MockCat.Hedgehog.ParamGen

-- | property: Bridge Gen から得た値は元 ParamSpec を受理する
prop_paramSpec_bridge_sound :: Property
prop_paramSpec_bridge_sound = property $ do
  ps <- forAllWith pp genSpec
  case paramSpecIntGenMaybe ps of
    Nothing -> success
    Just g  -> do
      v <- forAll g
      assert (acceptsParamSpec ps v && acceptsNormal (normalise ps) v)
  where
    genSpec = Gen.choice
        [ PSExact <$> small
        , do l <- small; h <- small; pure (PSRangeInt l h)
        , do n <- Gen.int (Range.linear 1 5)
             k <- Gen.int (Range.linear 1 n) -- ensure <= n elements
             xs <- Gen.list (Range.singleton k) small
             pure (PSEnum xs)
        , pure PSAny
        ]
    small = Gen.int (Range.linear (-7) 7)
    pp p = case p of
      PSExact a -> "Exact " ++ show a
      PSRangeInt l h -> "RangeInt(" ++ show l ++ "," ++ show h ++ ")"
      PSEnum xs -> "Enum" ++ show xs
      PSAny -> "Any"
      PSPredicate _ l -> "Predicate(" ++ l ++ ")"
