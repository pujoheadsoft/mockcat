{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-hpc #-}
module Property.OrderProps
  ( prop_inorder_succeeds
  , prop_adjacent_swap_fails
  , prop_partial_order_subset_succeeds
  , prop_partial_order_reversed_pair_fails
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (try, SomeException)
import Data.Maybe (listToMaybe)
import Data.List (nub)
import Test.MockCat (shouldBeCalled, inOrderWith, inPartialOrderWith, param)
import Property.Generators

-- | Property: executing a non-empty script yields exact order success.
prop_inorder_succeeds :: Property
prop_inorder_succeeds = forAll scriptGen $ \scr@(Script xs) -> not (null xs) ==> monadicIO $ do
  f <- run $ buildUnaryMock scr
  run $ runScript f scr
  run $ f `shouldBeCalled` inOrderWith (param <$> xs)
  assert True

-- | Property: a single adjacent swap causes order verification failure.
prop_adjacent_swap_fails :: Property
prop_adjacent_swap_fails = forAll scriptGen $ \(Script xs) -> length xs >= 2 ==> monadicIO $ do
  let distinct = nub xs
  if length distinct /= length xs
    then assert True  -- discard scripts with duplicates; they can mask order errors
    else do
      i <- run $ generate $ chooseInt (0, length xs - 2)
      let swapped = take i xs ++ [xs !! (i+1), xs !! i] ++ drop (i+2) xs
      f <- run $ buildUnaryMock (Script xs)
      run $ runScript f (Script xs)
      e <- run $ (try (f `shouldBeCalled` inOrderWith (param <$> swapped)) :: IO (Either SomeException ()))
      case e of
        Left _ -> assert True
        Right _ -> assert False

-- | Helper: produce a non-empty in-order subsequence.
chooseSubsequence :: [a] -> Gen [a]
chooseSubsequence [] = pure []
chooseSubsequence xs = do
  bools <- vectorOf (length xs) arbitrary
  let picked = [ x | (x,b) <- zip xs bools, b ]
  if null picked then chooseSubsequence xs else pure picked

-- | Property: any non-empty subsequence (order-preserving) passes partial order check.
prop_partial_order_subset_succeeds :: Property
prop_partial_order_subset_succeeds = forAll scriptGen $ \scr@(Script xs) -> not (null xs) ==> monadicIO $ do
  subset <- run $ generate $ chooseSubsequence xs
  f <- run $ buildUnaryMock scr
  run $ runScript f scr
  run $ f `shouldBeCalled` inPartialOrderWith (param <$> subset)
  assert True

-- | Property: selecting two distinct values and reversing them causes partial order failure.
prop_partial_order_reversed_pair_fails :: Property
prop_partial_order_reversed_pair_fails = forAll scriptGen $ \scr@(Script xs) -> length xs >= 2 ==> monadicIO $ do
  if length (nub xs) /= length xs
    then assert True -- discard non-unique scripts to avoid accidental subsequences
    else do
      let pairs = [ (i,j) | i <- [0..length xs -2], j <- [i+1..length xs -1] ]
      case listToMaybe pairs of
        Nothing -> assert True
        Just (i,j) -> do
          f <- run $ buildUnaryMock scr
          run $ runScript f scr
          let reversed = [xs!!j, xs!!i]
          e <- run $ (try (f `shouldBeCalled` inPartialOrderWith (param <$> reversed)) :: IO (Either SomeException ()))
          case e of
            Left _ -> assert True
            Right _ -> assert False
