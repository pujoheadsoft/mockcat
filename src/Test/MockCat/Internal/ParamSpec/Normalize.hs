{-# LANGUAGE GADTs #-}
-- | Internal normalization for ParamSpec (experimental).
-- NOT exported to end users; subject to change.
module Test.MockCat.Internal.ParamSpec.Normalize
  ( NormalParamSpec(..)
  , normalise
  , normaliseMany
  , normaliseManyInt
  , combine
  , toParamSpec
  , acceptsParamSpec
  , acceptsNormal
  , genFromNormalInt
  ) where

import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Test.MockCat.ParamSpec
import Data.List (sortOn)

-- | Canonical internal form.
-- NOTE: We intentionally keep opaque predicate only by its function + label.
-- Eq instance treats predicate functions as unequal unless labels match (best effort).
-- For test equivalence we only rely on behavioural acceptance, not structural Eq of predicates.
data NormalParamSpec a where
  NExact      :: a -> NormalParamSpec a
  NEnum       :: [a] -> NormalParamSpec a            -- ^ de-duplicated, stable order, length >= 2
  NRangeInt   :: Int -> Int -> NormalParamSpec Int   -- ^ low <= high, low < high (else collapsed to NExact)
  NUnion      :: [NormalParamSpec a] -> NormalParamSpec a -- ^ flattened, length >= 2
  NAny        :: NormalParamSpec a
  NOpaquePred :: (a -> Bool) -> String -> NormalParamSpec a

instance Show a => Show (NormalParamSpec a) where
  show (NExact a) = "NExact " ++ show a
  show (NEnum xs) = "NEnum " ++ show xs
  show (NRangeInt l h) = "NRangeInt " ++ show (l,h)
  show (NUnion xs) = "NUnion " ++ show (map show xs)
  show NAny = "NAny"
  show (NOpaquePred _ l) = "NOpaquePred <fn> " ++ show l

instance Eq a => Eq (NormalParamSpec a) where
  NExact a == NExact b = a == b
  NEnum xs == NEnum ys = xs == ys
  NRangeInt a b == NRangeInt c d = a == c && b == d
  NUnion xs == NUnion ys = xs == ys
  NAny == NAny = True
  NOpaquePred _ la == NOpaquePred _ lb = la == lb
  _ == _ = False

-- | Convert public ParamSpec to internal canonical form (minimal subset of rules).
normalise :: Hashable a => ParamSpec a -> NormalParamSpec a
normalise ps = case ps of
  PSExact a      -> NExact a
  PSAny          -> NAny
  PSRangeInt l h -> range l h
  PSPredicate f l -> NOpaquePred f l
  PSEnum xs      -> enumNorm xs
 where
  range l h
    | l == h    = NExact l
    | l < h     = NRangeInt l h
    | otherwise = NRangeInt h l
  enumNorm [] = NEnum []  -- unreachable by current API (documented non-empty), but keep safe
  enumNorm xs =
    let uniq = dedupStable xs
    in case uniq of
         []       -> NEnum []
         [x]      -> NExact x
         (x:y:rs) -> NEnum (x:y:rs)

normaliseMany :: Hashable a => [ParamSpec a] -> NormalParamSpec a
normaliseMany [] = NEnum []
normaliseMany [p] = normalise p
normaliseMany ps = combine (map normalise ps)

-- | Combine already normalised specs (flatten union, absorb Any). More advanced merging TBD.
combine :: [NormalParamSpec a] -> NormalParamSpec a
combine specs
  | any isAny specs = NAny
  | otherwise = case flatten specs of
      []  -> NEnum []
      [x] -> x
      xs  -> NUnion xs
  where
    isAny NAny = True; isAny _ = False
    flatten [] = []
    flatten (NUnion xs:ys) = flatten xs ++ flatten ys
    flatten (x:xs) = x : flatten xs

normaliseManyInt :: [ParamSpec Int] -> NormalParamSpec Int
normaliseManyInt ps = finalize merged others
  where
    ns = map normalise ps
    flat = flatten ns
    (ranges, others) = partitionInt flat
    merged = mergeRanges ranges

    flatten [] = []
    flatten (NUnion xs:ys) = flatten xs ++ flatten ys
    flatten (x:xs) = x:flatten xs

    partitionInt [] = ([],[])
    partitionInt (n:ns') = case n of
      NRangeInt l h -> let (rs,os) = partitionInt ns' in ( (l,h):rs, os)
      NExact v      -> let (rs,os) = partitionInt ns' in ( (v,v):rs, os)
      _             -> let (rs,os) = partitionInt ns' in ( rs, n:os)

    mergeRanges [] = []
    mergeRanges rs = go (sortOn fst (map norm rs))
      where
        norm (l,h) = if l<=h then (l,h) else (h,l)
        go [] = []
        go [x] = [x]
        go ((l1,h1):(l2,h2):rest)
          | h1 + 1 < l2 = (l1,h1) : go ((l2,h2):rest)
          | otherwise   = go ((l1, max h1 h2):rest)

    finalize [] [] = NEnum []
    finalize [(l,h)] [] | l==h = NExact l
    finalize [(l,h)] []        = NRangeInt l h
    finalize rs os =
      let rangeNodes = map (\(l,h) -> if l==h then NExact l else NRangeInt l h) rs
          parts = rangeNodes ++ os
      in case parts of
           []  -> NEnum []
           [x] -> x
           xs  -> NUnion xs

-- | Convert back to a (not necessarily original shape) ParamSpec.
toParamSpec :: NormalParamSpec a -> ParamSpec a
toParamSpec n = case n of
  NExact a        -> PSExact a
  NEnum xs        -> PSEnum xs
  -- Union does not have a faithful representation in simple ParamSpec; fall back to PSAny.
  NUnion _        -> PSAny
  NAny            -> PSAny
  NOpaquePred f l -> PSPredicate f l
  NRangeInt l h   -> PSRangeInt l h

-- | Semantics: does a value satisfy the (non normalised) ParamSpec?
acceptsParamSpec :: Eq a => ParamSpec a -> a -> Bool
acceptsParamSpec ps x = case ps of
  PSExact a        -> x == a
  PSEnum as        -> any (== x) as
  PSRangeInt l h   -> let (lo,hi) = if l <= h then (l,h) else (h,l) in lo <= x && x <= hi
  PSAny            -> True
  PSPredicate f _  -> f x
 where
  -- (旧 between 削除) intentionally empty

-- | Semantics for NormalParamSpec.
acceptsNormal :: Eq a => NormalParamSpec a -> a -> Bool
acceptsNormal n x = case n of
  NExact a        -> x == a
  NEnum as        -> any (== x) as
  NUnion as       -> any (\s -> acceptsNormal s x) as
  NAny            -> True
  NOpaquePred f _ -> f x
  NRangeInt l h   -> l <= x && x <= h

-- | Stable dedup preserving first occurrence.
dedupStable :: Hashable a => [a] -> [a]
dedupStable = go HS.empty
  where
    go _ [] = []
    go seen (z:zs)
      | HS.member z seen = go seen zs
      | otherwise        = z : go (HS.insert z seen) zs

-- | Simple Int sampler helper; returns a deterministic function from seed -> value.
genFromNormalInt :: NormalParamSpec Int -> Maybe (Int -> Int)
genFromNormalInt n = case n of
  NExact a -> Just (\_ -> a)
  NEnum [] -> Nothing
  NEnum xs -> Just (\k -> xs !! (k `mod` length xs))
  NRangeInt l h -> Just (\k -> l + (k `mod` (h - l + 1)))
  NUnion xs -> case mapM genFromNormalInt xs of
                 Just fs | not (null fs) -> Just (\k -> let (q,r) = k `divMod` length fs
                                                       in (fs !! r) q)
                 _ -> Nothing
  NAny -> Nothing
  NOpaquePred _ _ -> Nothing
