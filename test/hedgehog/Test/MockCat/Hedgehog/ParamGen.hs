module Test.MockCat.Hedgehog.ParamGen
  ( paramSpecIntGenMaybe
  , normalParamIntGenMaybe
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.MockCat.ParamSpec (ParamSpec(..))
import Test.MockCat.Internal.ParamSpec.Normalize

-- | ParamSpec Int から Hedgehog Gen を (可能なら) 構築 (テスト専用の PoC)
paramSpecIntGenMaybe :: ParamSpec Int -> Maybe (Gen Int)
paramSpecIntGenMaybe ps = normalParamIntGenMaybe (normalise ps)

-- | NormalParamSpec Int からの対応付け (述語や Any は生成不可)
normalParamIntGenMaybe :: NormalParamSpec Int -> Maybe (Gen Int)
normalParamIntGenMaybe n = case n of
  NExact a      -> Just (pure a)
  NEnum []      -> Nothing
  NEnum xs      -> Just (Gen.element xs)
  NRangeInt l h -> Just (Gen.int (Range.linear l h))
  NUnion parts  -> do
    gens <- traverse normalParamIntGenMaybe parts
    if null gens then Nothing else Just (Gen.choice gens)
  NAny          -> Nothing
  NOpaquePred _ _ -> Nothing
