{-# LANGUAGE GADTs #-}
-- |
-- = ParamSpec (Experimental)
--
-- Minimal reversible specification for parameters.
--
-- This module is /experimental/ and its data constructors or semantics may
-- change between minor releases until the Scenario / Hedgehog integration is
-- stabilized.  It intentionally stays tiny and dependency–free; integration
-- layers (QuickCheck / Hedgehog) live outside the core library for now.
--
-- Stability: experimental
--
-- Design notes:
-- * `PSPredicate` keeps the original predicate for fallback generation /
--   validation. We do not attempt predicate decomposition.
-- * `PSRangeInt` focuses on a common ergonomic win first; more numeric ranges
--   can be added when needed.
-- * Future additions may introduce pretty-printing or normalisation helpers
--   (e.g. enum duplicate collapse) without breaking existing pattern matches.
--
-- When in doubt prefer using smart constructors (`exact`, `enum`, ...) instead
-- of pattern matching directly so future internal tweaks are easier.
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
  -- | Exact value expectation.
  PSExact     :: a -> ParamSpec a
  -- | Enumeration of acceptable values (duplicates preserved for now).
  PSEnum      :: [a] -> ParamSpec a            -- ^ Non-empty expected; empty is treated as unsatisfiable elsewhere.
  -- | Inclusive integer range (caller responsible for low <= high; library may normalise in future).
  PSRangeInt  :: Int -> Int -> ParamSpec Int
  -- | Arbitrary predicate with a human readable label.
  PSPredicate :: (a -> Bool) -> String -> ParamSpec a
  -- | Wildcard (accept any value of the domain).
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
