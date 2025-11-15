{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Property.LazyEvalProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.MockCat hiding (any)

-- | Unary action so we can register a concrete expected argument & return value.
--   We test that constructing (but not forcing) the application does not count.
class Monad m => LazyUnaryAction m where
  lazyUnaryAction :: Int -> m Int

makeMock [t|LazyUnaryAction|]

-- | Property: if we declare an expectation but never force (execute) the action,
--   the recorded application count remains 0.
prop_lazy_unforced_not_counted :: Property
prop_lazy_unforced_not_counted = monadicIO $ do
  run $ runMockT $ do
    _lazyUnaryAction (param (10 :: Int) |> (42 :: Int)) `applyTimesIs` 0
    -- Do NOT force the call; only build a thunk.
    let _thunk :: MockT IO Int
        _thunk = lazyUnaryAction 10
    pure ()
  assert True

-- | Property: once we actually execute (force) the action, it is counted exactly once.
prop_lazy_forced_counted :: Property
prop_lazy_forced_counted = monadicIO $ do
  run $ runMockT $ do
    _lazyUnaryAction (param (10 :: Int) |> (7 :: Int)) `applyTimesIs` 1
    v <- lazyUnaryAction 10   -- forcing the monadic action executes the mock
    v `seq` pure ()           -- ensure result is evaluated (WHNF for Int)
  assert True
