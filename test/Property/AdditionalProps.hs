{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-hpc #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Property.AdditionalProps
  ( prop_predicate_param_match_counts
  , prop_multicase_progression
  , prop_runMockT_isolation
  , prop_partial_order_duplicates
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (forM, forM_)
import Data.List (nub)
import Data.Proxy (Proxy(..))
import Test.MockCat
import Control.Monad.IO.Class (liftIO)
import Property.Generators (resetMockHistory)

perCall :: Int -> a -> a
perCall i x = case i of
  _ -> x

--------------------------------------------------------------------------------
-- 21. PredicateParam property
--------------------------------------------------------------------------------

-- | Property: A predicate Param (expect even) accepts only matching values and
-- the total recorded calls equals count of even inputs attempted.
prop_predicate_param_match_counts :: Property
prop_predicate_param_match_counts = forAll genVals $ \xs -> monadicIO $ do
  run resetMockHistory
  -- build predicate mock: (Int -> Bool) returning True for all evens
  f <- run $ mock (expect even "even" ~> True)
  results <- run $ mapM (\x -> try (evaluate (f x)) :: IO (Either SomeException Bool)) xs
  let successes = length [ () | Right _ <- results ]
      expected  = length (filter even xs)
  -- sanity: every success came from an even value
  assert (all even [ x | (x, Right _) <- zip xs results ])
  -- counts should match
  run $ f `shouldBeCalled` times expected
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
  run resetMockHistory
  f <- run $ mock $ cases [ param arg ~> r | r <- rs ]
  let totalCalls = length rs + extra
  -- NOTE [GHC9.4 duplicate-call counting]
  -- On GHC 9.4 we observed that using @replicateM totalCalls (evaluate (f arg))@
  -- can result in only a single side‑effect (call recording) when all
  -- of (argument,result) pairs are identical. The optimizer (or full laziness
  -- even under -O0) may float the pure expression @f arg@ and share it.
  -- We intentionally inject the loop index via a seq so each iteration has a
  -- syntactically distinct thunk, ensuring the unsafePerformIO body runs per call.
  vals <- run $
    forM [1 .. totalCalls] $ \i ->
      evaluate (perCall i (f arg))
  let (prefix, suffix) = splitAt (length rs) vals
  assert (prefix == rs)
  assert (all (== last rs) suffix)
  run $ f `shouldBeCalled` times totalCalls
  where
    genSeq = do
      arg <- arbitrary :: Gen Int
      Positive len <- arbitrary :: Gen (Positive Int)
      let capped = 1 + len `mod` 5 -- keep small (1..5)
      rs <- vectorOf capped (arbitrary :: Gen Int)
      extra <- chooseInt (0,3)
      pure (arg, rs, extra)

--------------------------------------------------------------------------------
-- 23. runMockT isolation property
--------------------------------------------------------------------------------

-- | Two independent runMockT invocations must not leak call counts.
-- The first run enforces a single invocation; the second expects zero for a fresh mock.
prop_runMockT_isolation :: Property
prop_runMockT_isolation = monadicIO $ do
  run resetMockHistory
  -- Run 1: expect one call
  r1 <- run $ try $ runMockT $ do
    f <- liftIO $ mock (param (1 :: Int) ~> True)
    addDefinition Definition { symbol = Proxy @"iso", mockFunction = f, verification = Verification (\m' -> m' `shouldBeCalled` times 1) }
    liftIO $ f 1 `seq` pure ()
  case r1 of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True
  -- Run 2: expect zero (if leaked, would see 1 and fail)
  r2 <- run $ try $ runMockT $ do
    f <- liftIO $ mock (param (1 :: Int) ~> True)
    addDefinition Definition { symbol = Proxy @"iso", mockFunction = f, verification = Verification (\m' -> m' `shouldBeCalled` times 0) }
    pure ()
  case r2 of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True


--------------------------------------------------------------------------------
-- 25. Partial order duplicates property
--------------------------------------------------------------------------------

-- | For a sequence with duplicates, the partial order check succeeds with the
-- list of first occurrences; reversing that list (when length >=2) fails.
prop_partial_order_duplicates :: Property
prop_partial_order_duplicates = forAll genDupScript $ \xs -> length xs >= 2 ==> monadicIO $ do
  run resetMockHistory
  -- build mock over sequence
  f <- run $ mock $ cases [ param x ~> True | x <- xs ]
  run $ forM_ xs $ \x -> f x `seq` pure ()
  let uniques = nub xs
  -- success case
  run $ f `shouldBeCalled` inPartialOrderWith (param <$> uniques)
  assert True
  -- failure case (if we have at least two unique values)
  case uniques of
    (_:_:_) | isClusterOrdered uniques xs -> do
      let reversed = reverse uniques
      e <- run (try (f `shouldBeCalled` inPartialOrderWith (param <$> reversed)) :: IO (Either SomeException ()))
      case e of
        Left _ -> assert True
        Right _ -> assert False
    _ -> assert True
  where
    genDupScript = do
      Positive len <- arbitrary :: Gen (Positive Int)
      let size = 2 + len `mod` 6 -- 2..7
      base <- vectorOf size (arbitrary :: Gen Int)
      -- ensure at least one duplicate by forcing first element copy if all distinct
      pure $ ensureDup base
    ensureDup ys = if length (nub ys) == length ys && not (null ys) then (case ys of (h:_) -> h : ys; [] -> ys) else ys
    -- Check that for each earlier unique value all its indices precede all indices of later unique values.
    isClusterOrdered us seqVals = all pairOrdered pairs
      where
        idxs v = [ i | (i,a) <- zip [0..] seqVals, a == v ]
        pairs = [ (u1,u2) | (i,u1) <- zip [0..] us, u2 <- drop (i+1) us ]
        pairOrdered (u1,u2) = case (idxs u1, idxs u2) of
          ([],_) -> True
          (_,[]) -> True
          (is1,is2) -> maximum is1 < minimum is2
