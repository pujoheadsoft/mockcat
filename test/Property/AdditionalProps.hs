{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
module Property.AdditionalProps
  ( prop_predicate_param_match_counts
  , prop_multicase_progression
  , prop_runMockT_isolation
  , prop_neverApply_unused
  , prop_partial_order_duplicates
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (replicateM, replicateM_)
import Data.List (nub)
import Data.Proxy (Proxy(..))
import Test.MockCat
import Control.Monad.IO.Class (liftIO)
import Test.MockCat.MockT (MockT(..), Definition(..), runMockT, MonadMockDefs(..))

--------------------------------------------------------------------------------
-- 21. PredicateParam property
--------------------------------------------------------------------------------

-- | Property: A predicate Param (expect even) accepts only matching values and
-- the total recorded applications equals count of even inputs attempted.
prop_predicate_param_match_counts :: Property
prop_predicate_param_match_counts = forAll genVals $ \xs -> monadicIO $ do
  -- build predicate mock: (Int -> Bool) returning True for all evens
  m <- run $ createMock (expect even "even" |> True)
  let f = stubFn m
  results <- run $ mapM (\x -> try (evaluate (f x)) :: IO (Either SomeException Bool)) xs
  let successes = length [ () | Right _ <- results ]
      expected  = length (filter even xs)
  -- sanity: every success came from an even value
  assert (all even [ x | (x, Right _) <- zip xs results ])
  -- counts should match
  run $ m `shouldApplyTimesToAnything` expected
  assert (successes == expected)
  where
    genVals = resize 40 $ listOf (arbitrary :: Gen Int)

--------------------------------------------------------------------------------
-- 22. Multi-case progression property
--------------------------------------------------------------------------------

-- | For identical argument, successive cases yield successive results then
-- saturate at the last value.
prop_multicase_progression :: Property
prop_multicase_progression = forAll genSeq $ \(arg, rs, extra) -> monadicIO $ do
  m <- run $ createMock $ cases [ param arg |> r | r <- rs ]
  let f = stubFn m
      totalCalls = length rs + extra
  vals <- run $ replicateM totalCalls (evaluate (f arg))
  let (prefix, suffix) = splitAt (length rs) vals
  assert (prefix == rs)
  assert (all (== last rs) suffix)
  run $ m `shouldApplyTimesToAnything` totalCalls
  where
    genSeq = do
      arg <- arbitrary :: Gen Int
      Positive len <- arbitrary :: Gen (Positive Int)
      let capped = 1 + (len `mod` 5) -- keep small (1..5)
      rs <- vectorOf capped (arbitrary :: Gen Int)
      extra <- chooseInt (0,3)
      pure (arg, rs, extra)

--------------------------------------------------------------------------------
-- 23. runMockT isolation property
--------------------------------------------------------------------------------

-- | Two independent runMockT invocations must not leak application counts.
-- First run uses expectApplyTimes 1; second run expects 0 for a freshly built mock.
prop_runMockT_isolation :: Property
prop_runMockT_isolation = monadicIO $ do
  -- Run 1: expect one application
  r1 <- run $ try $ runMockT $ do
    m <- liftIO $ createMock (param (1 :: Int) |> True)
    addDefinition Definition { symbol = Proxy @"iso", mock = m, verify = \m' -> m' `shouldApplyTimesToAnything` 1 }
    let f = stubFn m
    liftIO $ f 1 `seq` pure ()
  case r1 of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True
  -- Run 2: expect zero (if leaked, would see 1 and fail)
  r2 <- run $ try $ runMockT $ do
    m <- liftIO $ createMock (param (1 :: Int) |> True)
    addDefinition Definition { symbol = Proxy @"iso", mock = m, verify = \m' -> m' `shouldApplyTimesToAnything` 0 }
    pure ()
  case r2 of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True

--------------------------------------------------------------------------------
-- 24. neverApply unused property
--------------------------------------------------------------------------------

-- | A definition not invoked verifies zero applications while another is used.
prop_neverApply_unused :: Property
prop_neverApply_unused = forAll (chooseInt (0,5)) $ \n -> monadicIO $ do
  r <- run $ try $ runMockT $ do
    -- used mock
    mUsed <- liftIO $ createMock (param (0 :: Int) |> True)
    addDefinition Definition { symbol = Proxy @"used", mock = mUsed, verify = \m' -> m' `shouldApplyTimesToAnything` n }
    -- unused mock
    mUnused <- liftIO $ createMock (param (42 :: Int) |> True)
    addDefinition Definition { symbol = Proxy @"unused", mock = mUnused, verify = \m' -> m' `shouldApplyTimesToAnything` 0 }
    let f = stubFn mUsed
    liftIO $ replicateM_ n (evaluate (f 0))
  case r of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True

--------------------------------------------------------------------------------
-- 25. Partial order duplicates property
--------------------------------------------------------------------------------

-- | For a sequence with duplicates, the partial order check succeeds with the
-- list of first occurrences; reversing that list (when length >=2) fails.
prop_partial_order_duplicates :: Property
prop_partial_order_duplicates = forAll genDupScript $ \xs -> length xs >= 2 ==> monadicIO $ do
  -- build mock over sequence
  m <- run $ createMock $ cases [ param x |> True | x <- xs ]
  let f = stubFn m
  run $ sequence_ [ f x `seq` pure () | x <- xs ]
  let uniques = nub xs
  -- success case
  run $ m `shouldApplyInPartialOrder` (param <$> uniques)
  assert True
  -- failure case (if we have at least two unique values)
  case uniques of
    (_:_:_) | isClusterOrdered uniques xs -> do
      let reversed = reverse uniques
      e <- run $ (try (m `shouldApplyInPartialOrder` (param <$> reversed)) :: IO (Either SomeException ()))
      case e of
        Left _ -> assert True
        Right _ -> assert False
    _ -> assert True
  where
    genDupScript = do
      Positive len <- arbitrary :: Gen (Positive Int)
      let size = 2 + (len `mod` 6) -- 2..7
      base <- vectorOf size (arbitrary :: Gen Int)
      -- ensure at least one duplicate by forcing first element copy if all distinct
      pure $ ensureDup base
    ensureDup ys = if length (nub ys) == length ys && not (null ys) then head ys : ys else ys
    -- Check that for each earlier unique value all its indices precede all indices of later unique values.
    isClusterOrdered us seqVals = all pairOrdered pairs
      where
        idxs v = [ i | (i,a) <- zip [0..] seqVals, a == v ]
        pairs = [ (u1,u2) | (i,u1) <- zip [0..] us, u2 <- drop (i+1) us ]
        pairOrdered (u1,u2) = case (idxs u1, idxs u2) of
          ([],_) -> True
          (_,[]) -> True
          (is1,is2) -> maximum is1 < minimum is2
