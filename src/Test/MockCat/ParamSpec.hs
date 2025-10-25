{-# LANGUAGE GADTs #-}
-- | Minimal reversible specification for parameters.
--   This is an experimental PoC layer to allow deriving / documenting
--   generation strategies from expectation descriptions.
--   (Intended to stay small & dependency–free; QuickCheck integration lives in test code.)
module Test.MockCat.ParamSpec
  (
    ParamSpec(..)
  , fromParam
  , exact
  , enum
  , rangeInt
  , anySpec
  , predicate
  ) where

import Test.MockCat.Param (Param(..))

-- | A small, closed specification set.
--   NOTE: 'PSPredicate' keeps the original predicate for fallback generation / validation.
--   For now we do not attempt to decompose arbitrary predicates.
--   'PSRangeInt' is specialised so we can show a concrete ergonomic win early.
--   Future: add textual pretty printer / merging / normalisation if needed.
data ParamSpec a where
  PSExact     :: a -> ParamSpec a
  PSEnum      :: [a] -> ParamSpec a            -- ^ Non-empty expected; empty is treated as unsatisfiable elsewhere.
  PSRangeInt  :: Int -> Int -> ParamSpec Int   -- ^ Inclusive bounds (low <= high assumed; caller responsible)
  PSPredicate :: (a -> Bool) -> String -> ParamSpec a
  PSAny       :: ParamSpec a

-- | Construct exact.
exact :: a -> ParamSpec a
exact = PSExact

-- | Construct enumeration (duplicates allowed; kept verbatim so that later we could weight frequency if we wished).
enum :: [a] -> ParamSpec a
enum = PSEnum

-- | Inclusive integer range.
rangeInt :: Int -> Int -> ParamSpec Int
rangeInt = PSRangeInt

-- | Wildcard (accept any value of the domain).
anySpec :: ParamSpec a
anySpec = PSAny

-- | Explicit predicate (kept for validation / fallback generation filtering).
predicate :: (a -> Bool) -> String -> ParamSpec a
predicate = PSPredicate

-- | Derive a coarse specification from an existing 'Param'.
--   * ExpectValue v      -> PSExact v
--   * ExpectCondition f l-> PSPredicate f l
fromParam :: Param a -> ParamSpec a
fromParam (ExpectValue v)      = PSExact v
fromParam (ExpectCondition f l) = PSPredicate f l
