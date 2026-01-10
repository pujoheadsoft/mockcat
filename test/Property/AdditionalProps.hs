{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-hpc #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Property.AdditionalProps
  ( spec
  , prop_predicate_param_match_counts
  , prop_multicase_progression
  , prop_runMockT_isolation
  , prop_partial_order_duplicates
  ) where

import Test.QuickCheck hiding (once)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (try, SomeException, evaluate)
import Control.Monad (forM, forM_)
import Data.List (nub)

import Test.MockCat
import Control.Monad.IO.Class (liftIO)
import Property.Generators (resetMockHistory)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
    describe "Property Additional (Predicate / Multi-case / Isolation / Duplicates)" $ do
      it "predicate param counts match" $ property prop_predicate_param_match_counts
      it "multi-case progression saturates" $ property prop_multicase_progression
      it "runMockT isolation of counts" $ property prop_runMockT_isolation
      it "partial order with duplicates behaves" $ property prop_partial_order_duplicates

perCall :: Int -> a -> a
perCall i x = case i of
  _ -> x

--------------------------------------------------------------------------------
-- 21. PredicateParam property
--------------------------------------------------------------------------------

-- | Property: A predicate Param (expect even) accepts only matching values and
-- the total recorded calls equals count of even inputs attempted.
-- | Property: A predicate Param (expect even) accepts only matching values and
-- the total recorded calls equals count of even inputs attempted.
prop_predicate_param_match_counts :: Property
prop_predicate_param_match_counts = forAll genVals $ \xs -> monadicIO $ do
  -- Use withMock
  let expected  = length (filter even xs)
  run $ withMock $ do
     -- build predicate mock
     f <- mock (expect even "even" ~> True)
            `expects` called (times expected)
            
     results <- liftIO $ mapM (\x -> try (evaluate (f x)) :: IO (Either SomeException Bool)) xs
     
     let successes = length [ () | Right _ <- results ]
     -- sanity checks (optional, but runMockT handles main verification)
     liftIO $ do
       if successes /= expected 
         then error $ "Success count mismatch: " ++ show successes ++ " vs " ++ show expected
         else pure ()
  assert True
  where
    genVals = resize 40 $ listOf (arbitrary :: Gen Int)

--------------------------------------------------------------------------------
-- 22. Multi-case progression property
--------------------------------------------------------------------------------

-- | For identical argument, successive cases yield successive results then
-- saturate at the last value.
prop_multicase_progression :: Property
prop_multicase_progression = forAll genSeq $ \(arg, rs, extra) -> monadicIO $ do
  let totalCalls = length rs + extra
  run $ withMock $ do
    f <- mock (cases [ param arg ~> r | r <- rs ])
           `expects` called (times totalCalls)
           
    -- Run loop
    vals <- liftIO $
      forM [1 .. totalCalls] $ \i ->
        evaluate (perCall i (f arg))
        
    liftIO $ do
      let (prefix, suffix) = splitAt (length rs) vals
      if prefix /= rs || not (all (== last rs) suffix)
         then error "Result sequence mismatch"
         else pure ()
         
  assert True
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
  r1 <- run ((try $ runMockT $ do
    f <- mock (param (1 :: Int) ~> True)
          `expects` called once
    liftIO $ f 1 `seq` pure ()
    ) :: IO (Either SomeException ()))

  case r1 of
    Left (_ :: SomeException) -> assert False
    Right () -> assert True
    
  -- Run 2: expect zero (if leaked, would see 1 and fail)
  r2 <- run ((try $ runMockT $ do
    _f <- mock (param (1 :: Int) ~> True)
          `expects` called never
    pure ()
    ) :: IO (Either SomeException ()))
    
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
  let uniques = nub xs
  -- success case
  run $ withMock $ do
      f <- mock (cases [ param x ~> True | x <- xs ])
             `expects` calledInSequence uniques
      liftIO $ forM_ xs $ \x -> f x `seq` pure ()
  assert True
  
  -- failure case (if we have at least two unique values)
  case uniques of
    (_:_:_) | isClusterOrdered uniques xs -> do
      let reversed = reverse uniques
      e <- run ((try $ withMock $ do
          f <- mock (cases [ param x ~> True | x <- xs ])
                `expects` calledInSequence reversed
          liftIO $ forM_ xs $ \x -> f x `seq` pure ()
          ) :: IO (Either SomeException ()))
          
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
