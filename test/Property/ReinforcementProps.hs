{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Property.ReinforcementProps
  where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Exception (try, SomeException, evaluate)
import Control.Concurrent.Async (forConcurrently_)
import UnliftIO (withRunInIO)
import Test.MockCat hiding (any)

--------------------------------------------------------------------------------
-- 1. Predicate negative: failing inputs raise & are not counted
--------------------------------------------------------------------------------

prop_predicate_negative_not_counted :: Property
prop_predicate_negative_not_counted = forAll genVals $ \xs -> monadicIO $ do
  f <- run $ createStubFn (expect even "even" |> True)
  outcomes <- run $ mapM (\x -> (try (evaluate (f x)) :: IO (Either SomeException Bool))) xs
  let evens = length (filter even xs)
      successes = length [ () | (x, Right _) <- zip xs outcomes, even x ]
      oddSuccesses = [ x | (x, Right _) <- zip xs outcomes, odd x ]
      failures = length [ () | (_, Left _) <- zip xs outcomes ]
      odds = length (filter odd xs)
  -- No odd value should succeed
  assert (null oddSuccesses)
  -- Failures count equals number of odd inputs
  assert (failures == odds)
  -- Successes count equals even inputs
  assert (successes == evens)
  -- Applied count equals successes (only even)
  run $ f `shouldApplyTimesToAnything` evens
  assert True
  where
    genVals = resize 60 $ listOf (arbitrary :: Gen Int)

--------------------------------------------------------------------------------
-- 2. Lazy partial force with concurrency
--------------------------------------------------------------------------------

-- We reuse a unary typeclass mock to get laziness semantics (same pattern as LazyEvalProp).
class Monad m => ParLazyAction m where
  parLazy :: Int -> m Int

makeMock [t|ParLazyAction|]

prop_lazy_partial_force_concurrency :: Property
prop_lazy_partial_force_concurrency = forAll genPlan $ \(arg, mask) -> monadicIO $ do
  let forcedCount = length (filter id mask)
  run $ runMockT $ do
    -- expectation: arg -> arg; count only forced executions
    _parLazy (param arg |> arg) `applyTimesIs` forcedCount
    -- prepare thunks (NOT executed yet)
    let thunks = replicate (length mask) (parLazy arg)
    withRunInIO $ \runIn -> do
      forConcurrently_ (zip mask thunks) $ \(forceIt, action) ->
        if forceIt then do
          v <- runIn action
          v `seq` pure ()
        else pure () -- skip executing -> not counted
  assert True
  where
    genPlan = do
      size <- chooseInt (1,40)
      arg <- arbitrary :: Gen Int
      mask <- vectorOf size arbitrary
      pure (arg, mask)

--------------------------------------------------------------------------------
-- 3. Interleaved duplicate partial-order spec (A B A pattern)
--------------------------------------------------------------------------------

prop_partial_order_interleaved_duplicates :: Property
prop_partial_order_interleaved_duplicates = forAll genPair $ \(a,b) -> a /= b ==> monadicIO $ do
  -- Pattern a a b : [a,b] subsequence succeeds, [b,a] fails.
  f <- run $ createStubFn $ cases [ param a |> True
                                , param a |> True
                                , param b |> True ]
  run $ f a `seq` f a `seq` f b `seq` pure ()
  run $ f `shouldApplyInPartialOrder` [param a, param b]
  e <- run $ (try (f `shouldApplyInPartialOrder` [param b, param a]) :: IO (Either SomeException ()))
  case e of
    Left _  -> assert True
    Right _ -> assert False
  assert True
  where
    genPair = do
      a <- arbitrary :: Gen Int
      b <- arbitrary :: Gen Int
      pure (a,b)
