{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
-- | Test-only normalization utilities migrated from former
-- Test.MockCat.Internal.ParamSpec.Normalize. These are not part of the
-- public API and may change freely. Copied here because only the
-- property tests rely on the canonical form & merging behaviour.
module Support.ParamSpecNormalize
  ( NormalParamSpec(..)
  , normalise
  , normaliseMany
  , normaliseManyInt
  , combine
  , toParamSpec
  , acceptsParamSpec
  , acceptsNormal
  ) where

import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import "mockcat" Test.MockCat.ParamSpec
import Data.List (sortOn)

-- | Canonical internal form (test-only).
data NormalParamSpec a where
  NExact      :: a -> NormalParamSpec a
  NEnum       :: [a] -> NormalParamSpec a
  NRangeInt   :: Int -> Int -> NormalParamSpec Int
  NUnion      :: [NormalParamSpec a] -> NormalParamSpec a
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
  enumNorm [] = NEnum []
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

toParamSpec :: NormalParamSpec a -> ParamSpec a
toParamSpec n = case n of
  NExact a        -> PSExact a
  NEnum xs        -> PSEnum xs
  NUnion _        -> PSAny
  NAny            -> PSAny
  NOpaquePred f l -> PSPredicate f l
  NRangeInt l h   -> PSRangeInt l h

acceptsParamSpec :: Eq a => ParamSpec a -> a -> Bool
acceptsParamSpec ps x = case ps of
  PSExact a        -> x == a
  PSEnum as        -> any (== x) as
  PSRangeInt l h   -> let (lo,hi) = if l <= h then (l,h) else (h,l) in lo <= x && x <= hi
  PSAny            -> True
  PSPredicate f _  -> f x

acceptsNormal :: Eq a => NormalParamSpec a -> a -> Bool
acceptsNormal n x = case n of
  NExact a        -> x == a
  NEnum as        -> any (== x) as
  NUnion as       -> any (\s -> acceptsNormal s x) as
  NAny            -> True
  NOpaquePred f _ -> f x
  NRangeInt l h   -> l <= x && x <= h

dedupStable :: Hashable a => [a] -> [a]
dedupStable = go HS.empty
  where
    go _ [] = []
    go seen (z:zs)
      | HS.member z seen = go seen zs
      | otherwise        = z : go (HS.insert z seen) zs
