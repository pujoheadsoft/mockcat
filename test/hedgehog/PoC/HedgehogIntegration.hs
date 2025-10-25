{-# LANGUAGE ScopedTypeVariables #-}
-- | Hedgehog PoC integration (experimental, not exported by library).
-- Mirrors the QuickCheck PoC to validate shrink & generation path.
module PoC.HedgehogIntegration
  ( prop_singleCall_hh
  , prop_multiCallCount_hh
  , prop_order_preserved_hh
  , prop_order_shrink_example_hh
  ) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.MockCat hiding (any)
import qualified Test.MockCat as MC (any)

--------------------------------------------------------------------------------
-- Single call property
--------------------------------------------------------------------------------
prop_singleCall_hh :: Property
prop_singleCall_hh = property $ do
  x <- forAll $ Gen.int (Range.linear (-50) 50)
  m <- evalIO $ createMock (param x |> True)
  let f = stubFn m
  evalIO $ f x `seq` pure ()
  evalIO $ m `shouldApplyTo` x
  evalIO $ m `shouldApplyTimes` (1 :: Int) `to` x

--------------------------------------------------------------------------------
-- Multi call count property
--------------------------------------------------------------------------------
prop_multiCallCount_hh :: Property
prop_multiCallCount_hh = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 40) (Gen.int (Range.linear 0 100))
  m <- evalIO $ createMock (MC.any |> True)
  let f = stubFn m
  evalIO $ mapM_ (\a -> f a `seq` pure ()) xs
  evalIO $ m `shouldApplyTimesToAnything` (length xs :: Int)

--------------------------------------------------------------------------------
-- Order preserved property
--------------------------------------------------------------------------------
prop_order_preserved_hh :: Property
prop_order_preserved_hh = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 15) (Gen.int (Range.linear 0 20))
  assert (length xs <= 15)
  if null xs
     then success
     else do
       m <- evalIO $ createMock $ cases [ param a |> True | a <- xs ]
       let f = stubFn m
       evalIO $ mapM_ (\a -> f a `seq` pure ()) xs
       evalIO $ m `shouldApplyInOrder` (param <$> xs)

--------------------------------------------------------------------------------
-- Intentional failing property to observe shrink behaviour (NOT run by default)
--------------------------------------------------------------------------------
-- | 並び順をシャッフルして意図的に失敗させ、Hedgehog の最小化で
--   どの程度単純なケース(最小の逆転)へ縮小されるかを手動観察するための property。
--   通常の CI / デフォルト実行では失敗させたくないので Main では呼びません。
--   実行方法: 環境変数 HEDGEHOG_FAILING=1 を付けて runner がこの失敗を
--             「期待通りの失敗」とみなし green にします。
prop_order_shrink_example_hh :: Property
prop_order_shrink_example_hh = property $ do
  xs <- forAll $ Gen.list (Range.linear 2 12) (Gen.int (Range.linear 0 30))
  shuffled <- forAll $ Gen.shuffle xs
  -- まれに shuffle 結果が同一になるため除外
  annotateShow (length xs, take 6 xs)
  assert (xs /= shuffled)
  let mismatch = length $ filter id $ zipWith (/=) xs shuffled
  annotateShow mismatch
  m <- evalIO $ createMock $ cases [ param a |> True | a <- xs ]
  let f = stubFn m
  evalIO $ mapM_ (\a -> f a `seq` pure ()) shuffled
  -- ここで本来は順序不一致で失敗することを期待
  evalIO $ m `shouldApplyInOrder` (param <$> xs)
