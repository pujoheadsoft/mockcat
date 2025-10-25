{-# LANGUAGE ScopedTypeVariables #-}
-- | Hedgehog PoC integration (experimental, not exported by library).
-- Mirrors the QuickCheck PoC to validate shrink & generation path.
module PoC.HedgehogIntegration
  ( prop_singleCall_hh
  , prop_multiCallCount_hh
  , prop_order_preserved_hh
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
