# Param Reversible Extension Design (Draft / EXPERIMENTAL)

Status: draft
Target version: 0.5.x (non-breaking additions) / potential 0.6.0 (if constructor reshaping required)
Author: internal design aid

## 1. Goals

Provide a *reversible*, *normalisable* parameter specification layer that can:

1. Represent user expectations succinctly (exact / enum / range / predicate / wildcard).
2. Normalise semantically equivalent specs to a canonical form for:
   - Equality / dedup / caching keys
   - Shrink & generation reuse (Hedgehog / QuickCheck bridge)
3. Support partial inversion: given `ParamSpec a`, produce a `Gen a` (where feasible) that samples uniformly or with configurable weighting.
4. Allow future Scenario DSL to derive call scripts & coverage metrics from a normalised space.
5. Keep core library free of heavy dependencies (generators live in integration layer).

## 2. Current State Summary

`ParamSpec` (in `Test.MockCat.ParamSpec`):
- `PSExact a`
- `PSEnum [a]` (duplicates preserved)
- `PSRangeInt Int Int`
- `PSPredicate (a -> Bool) String`
- `PSAny`

Limitations:
- No normalisation utilities exported.
- Duplicates in `PSEnum` not collapsed (cannot compare easily for equality / hashing).
- No weighting / frequency semantics.
- `PSPredicate` opaque (cannot be reversed into a safe finite generator without user hint).
- No composite forms (e.g. union / intersection / difference) to refine a spec structurally.

## 3. Proposed Extended Surface

Introduce a *normalized internal form* plus *smart constructors* that normalise eagerly when safe.

### 3.1 New Data Types

```haskell
-- Internal canonical form (not exported directly initially)
-- Invariant: each constructor is normalised (see rules below).
data NormalParamSpec a
  = NExact a
  | NEnum !(NonEmpty a)          -- duplicates removed, stable order (first occurrence wins)
  | NRangeInt !Int !Int          -- low <= high
  | NAny
  | NOpaquePredicate (a -> Bool) String  -- non-reversible
  | NUnion !(NonEmpty (NormalParamSpec a)) -- flattened, size >=2, members not unions themselves
  deriving (Functor)
```

Rationale:
- `NUnion` covers logical OR composition (replaces nested enums of different shapes) and enables future weighting.
- For now restrict to *disjunction* because it is the minimal structure we need for merging heterogeneous specs.

### 3.2 Smart Constructor Layer (Public)

Keep existing `ParamSpec` GADT for backward compatibility; add helper functions:

```haskell
normalise :: (Eq a, Hashable a) => ParamSpec a -> NormalParamSpec a
combine    :: (Eq a, Hashable a) => [ParamSpec a] -> NormalParamSpec a  -- union w/ flatten
```

For `Int` specialised helpers:

```haskell
mergeRanges :: NormalParamSpec Int -> NormalParamSpec Int -> NormalParamSpec Int
-- merges overlapping / adjacent NRangeInt in a union context
```

### 3.3 Normalisation Rules

1. `PSExact x` -> `NExact x`.
2. `PSEnum xs` ->
   - If empty: represent as `NUnion` of zero? (Instead treat as logically unsatisfiable; we'll store as `NUnion []` is invalid -> choose sentinel) => Adopt a dedicated *unsatisfiable* form? For simplicity: reject empty earlier (current doc: "non-empty expected").
   - Remove duplicates preserving first occurrence.
   - Single element => `NExact`.
   - Otherwise => `NEnum`.
3. Multiple specs combined by `combine`:
   - Flatten nested unions.
   - Collapse exact duplicates.
   - Merge compatible `NRangeInt` segments: if intervals overlap or touch (`a2 + 1 == b1`) ⇒ new merged range.
   - If after merge only one child remains => return that child (idempotence).
4. `PSRangeInt l h`:
   - If `l > h` swap (or treat as invalid; choose swap for resilience) then produce `NRangeInt l' h'`.
   - If `l == h` => `NExact l`.
5. `PSPredicate` => `NOpaquePredicate` (no further structural reasoning).
6. `PSAny` in a union causes entire union to reduce to `NAny`.
7. Union containing a mix of single-value exacts that cover an existing small range entirely MAY be compacted into `NRangeInt` if:
   - Type is `Int`
   - Values form a dense closed interval and count > 3 (heuristic to reduce enumeration size). (Heuristic rule optional behind flag.)

### 3.4 Reversibility & Generation Strategy

We classify constructors by reversibility level:

| Constructor          | Finite? | Safe Gen direct? | Notes |
|----------------------|---------|------------------|-------|
| NExact               | Yes     | Yes              | Singleton
| NEnum                | Yes     | Yes              | Uniform or weighted
| NRangeInt            | Yes     | Yes              | Range uniform
| NAny                 | Infinite| Need typeclass   | Use `Arbitrary` / Hedgehog `Range` fallback
| NOpaquePredicate     | Unknown | Filter or reject | Potentially infinite & non-productive
| NUnion               | Depends | If all finite    | Weighted sum; else must treat infinite members carefully

Generation API (hedgehog side) concept:

```haskell
class FiniteSpec a where
  cardinality :: NormalParamSpec a -> Maybe Integer  -- Nothing = infinite / unknown

-- Provided helper:
genFromSpec :: NormalParamSpec a -> Gen a
```

Policies:
- If any branch infinite + another finite => choose one uniformly each trial (risk bias). Later: adaptive weighting.
- `NOpaquePredicate`: attempt fallback: choose base generator from `TypeRep` dictionary (provided by integration) then `filter`. Guard with max discard threshold; if exceeded produce a warning (label) and shrink to first encountered satisfying value.

### 3.5 Shrinking

- For `NExact` → no shrink.
- For `NEnum` → shrink to earlier element (list prefix strategy) then maybe to `NExact` shortest printed form (optional heuristic).
- For `NRangeInt l h` → midpoint + boundaries binary shrinking (standard Hedgehog Range semantics re-used by converting to a `Range`).
- For `NUnion`:
  1. Shrink within selected branch (standard shrink) keeping union shape.
  2. Secondary phase (optional): shrink to alternative branch with smaller cardinality.
- For `NOpaquePredicate` / `NAny` rely on integration’s generic shrink (if any). If none, no shrink.

### 3.6 Predicate Fallback Strategy

Provide user escape hatch to *approximate* a predicate as a finite spec for generation consistency:

```haskell
-- User supplies a bounding universe (e.g. an enum list or int range) to try enumerating.
approximate :: NormalParamSpec a            -- a bounding superset (finite)
            -> (a -> Bool)
            -> NormalParamSpec a            -- filtered, normalised
```

If result becomes empty ⇒ signal via an Either:

```haskell
approximateEither :: (Eq a, Hashable a) => NormalParamSpec a -> (a -> Bool) -> Either ApproximationError (NormalParamSpec a)
```

### 3.7 API Additions (Public Roadmap)

Phase 1 (0.5.x minor):
- `normalise`, `combine` (pure functions)
- Internal `NormalParamSpec` (not exported) + pattern synonyms (optional) for safe matching.

Phase 2 (after Hedgehog integration stabilises):
- Hedgehog helper: `genParam :: NormalParamSpec a -> Gen a`
- QuickCheck helper: `arbitraryFrom :: NormalParamSpec a -> Gen a`
- Optional shrink wrappers.

Phase 3 (Scenario DSL alignment):
- Coverage metric: measure fraction of branches / intervals hit.
- Script generator exploring cross-product of `NormalParamSpec` lists with pruning.

## 4. Backward Compatibility & Migration

- Existing pattern matches on `ParamSpec` stay valid.
- Normalisation helpers are additive; no constructors removed.
- Potential breaking change candidates deferred to 0.6.0:
  - Converting `PSEnum` to automatically dedup (currently documented behaviour would change). For now: *do not change runtime shape*; leave dedup only in `normalise` path.

## 5. Edge Cases & Decisions

| Case | Policy |
|------|--------|
| Empty enum list passed by user | Reject early (document); `enum []` considered programmer error. |
| Range with l > h | Swap & normalise; log (via debug trace in test) OR keep silent for ergonomics. |
| Opaque predicate always False | During approximation → returns empty spec error. |
| Infinite + Finite union | Generation chooses branch uniformly; doc warns about bias. |
| Large dense int enum (> 64 elems) | Heuristic: treat as range if dense; else keep enum. |

## 6. Implementation Plan Breakdown

1. Introduce internal module `Test.MockCat.Internal.ParamSpec.Normalize` (not exposed)
2. Define `NormalParamSpec` + invariants + smart builder functions.
3. Implement `normalise :: (Eq a, Hashable a) => ParamSpec a -> NormalParamSpec a`.
4. Implement `combine :: (Eq a, Hashable a) => [ParamSpec a] -> NormalParamSpec a`.
5. Provide Hedgehog-side prototype: `toHedgehogGen` (in test space first).
6. Add property tests:
   - Idempotence: `normalise . normalise == normalise` (via an arbitrary ParamSpec generator)
   - Semantic equality preserved: value accepted by original spec iff accepted by normalised form (requires interpreter `accepts :: ParamSpec a -> a -> Bool`).
   - Range merge correctness.
7. Document in Haddock and link from README experimental section.

## 7. Open Questions / Future

- Weighting / frequency DSL (e.g. 70% exact A, 30% other) — postpone until real usage patterns observed.
- Intersection (AND) for Scenario coverage constraints — likely needed for advanced script reduction; not required yet.
- Deriving `Show` for opaque predicate gracefully (currently only have label). Possibly add structured `PredicateMeta` with classification tags.

## 8. Risks

- Over-normalisation could hide user intent (esp. dense enum → range) making failure messages less explicit. Mitigate with: store *original* form alongside canonical when needed for pretty-print.
- Predicate approximation naive filtering may cause high discard rates; we must surface a counter / label to inform the user.

## 9. Minimal Viable Slice

Deliver in first PR:
- `NormalParamSpec`
- `normalise` (no union or range merging beyond simple cases)
- Tests: idempotence + acceptance equivalence for exact/enum/range

Then iterate with merging & union flattening in subsequent PR.

---
(End of draft)
