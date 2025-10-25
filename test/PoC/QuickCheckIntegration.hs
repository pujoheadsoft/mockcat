{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module PoC.QuickCheckIntegration
  ( forAllMock    -- minimal wrapper: single call expectation
  , forAllMockTimes -- wrapper: list of calls, verifies count
  , prop_singleCall_example
  , prop_multiCallCount_example
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.MockCat hiding (any)
import qualified Test.MockCat as MC (any)

-- | Minimal PoC wrapper: generate an argument, build a mock expecting that exact
-- argument once, invoke user runner, then verify it was applied to that value.
-- NOTE: This is intentionally narrow (one call) to keep PoC surface tiny.
forAllMock :: (Arbitrary a, Show a, Eq a)
           => ( (a -> Bool) -> a -> IO () )  -- ^ runner gets stub + generated arg
           -> Property
forAllMock runner = property $ \(a :: a) -> monadicIO $ do
  m <- run $ createMock (param a |> True)
  let f = stubFn m
  run $ runner f a
  -- verify exactly one application to the generated arg
  run $ m `shouldApplyTo` a
  run $ m `shouldApplyTimes` (1 :: Int) `to` a
  assert True

-- | PoC wrapper for multiple calls: generate a list of arguments, build a mock
-- that always returns True, apply stub to every element, then verify total count.
-- This does NOT yet coalesce identical args; it just checks each element was used.
forAllMockTimes :: (Arbitrary a, Show a, Eq a)
                => ([a] -> IO ())                 -- ^ precondition? (unused for now)
                -> ( (a -> Bool) -> [a] -> IO () )  -- ^ runner applying stub over list
                -> Property
forAllMockTimes _prep runner = property $ \(xs :: [a]) -> monadicIO $ do
  -- simple cap to avoid very large lists skewing test time
  let limited = take 40 xs
  m <- run $ createMock (MC.any |> True)
  let f = stubFn m
  run $ runner f limited
  -- verify aggregate count to anything (using provided DSL)
  run $ m `shouldApplyTimesToAnything` (length limited :: Int)
  assert True

-- | Example property: single call path
prop_singleCall_example :: Property
prop_singleCall_example = forAllMock @Int $ \stub a -> do
  stub a `seq` pure ()

-- | Example property: multi-call count path
prop_multiCallCount_example :: Property
prop_multiCallCount_example = forAllMockTimes @Int (const (pure ())) $ \stub xs -> do
  mapM_ (\x -> stub x `seq` pure ()) xs
  pure ()
