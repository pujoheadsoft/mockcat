# Hedgehog Integration Design (Experimental)

Status: draft / experimental (not exposed yet)
Target Version: tentative 0.5.x (internal) -> stabilize in 0.6.0
Stability Tag: DO NOT EXPORT until shrink semantics validated

## Goals
- Provide first-class Hedgehog property helpers similar to existing QuickCheck PoC wrappers.
- Leverage Hedgehog shrinking to minimize failing call sequences (order / partial-order / count mismatches).
- Reuse existing `ParamSpec` (experimental) for generator derivation; avoid duplicating spec logic.
- Keep core library free from mandatory Hedgehog dependency (initially test-only); later allow optional package or cabal flag.

## Non-Goals (initial phase)
- No multi-argument auto-derivation beyond what Mock already supports manually.
- No higher-level Scenario branching (fork/branch/parallel) — linear sequences only.
- No automatic coverage metrics persistence (just return values for user assertions).

## High-Level Flow
```
ParamSpec a  ──►  Hedgehog Gen a (with shrink strat) ──► sequence generator ─► run stub on each ─► verify & shrink failing sequence
```

## Core Types (proposed)
```haskell
module Test.MockCat.Hedgehog ( -- not yet implemented / exported
  forAllMockH,           -- single call expectation wrapper
  forAllCallsH,          -- list of calls, verify total count
  forAllOrderH,          -- generate sequence, verify order
  genFromParamSpecH,     -- ParamSpec -> Gen
  callScriptGenH         -- sequence generator (length & element shrink)
) where
```

### Function Signatures (draft)
```haskell
forAllMockH :: (Show a, Eq a, MonadIO m)
            => Gen a
            -> ( (a -> IO b) -> a -> IO () )   -- runner (receives stub & generated value)
            -> Property

forAllCallsH :: (Show a, Eq a)
             => Range Int              -- length range for sequence
             -> Gen a                  -- element generator (maybe from ParamSpec)
             -> ( (a -> IO b) -> [a] -> IO () )
             -> Property

forAllOrderH :: (Show a, Eq a)
             => Range Int
             -> Gen a
             -> ( (a -> IO b) -> [a] -> IO () )
             -> Property
             -- Verifies exact order (shouldApplyInOrder) & total count.

genFromParamSpecH :: ParamSpec a -> Gen a

callScriptGenH :: Range Int -> Gen a -> Gen [a]
```

### Potential Extensions (not in first commit)
```haskell
forAllPartialOrderH :: (Show a, Eq a) => ...
forAllWithExpectationsH :: ExpectScript a -> Property  -- higher-level script wrapper
```

## Shrink Strategy

### Sequence Shrinking (callScriptGenH)
1. Length reduction first (drop suffix) — ensures minimal failing prefix.
2. Element shrinking (Hedgehog's element shrink) for each position after length minimized.
3. (Future) Duplicate cluster compaction: if order property only needs subsequence, remove redundant interior duplicates.

Implementation approach: start with simple list `Gen` using `Hedgehog.Gen.list (Range.linear lo hi) elemGen`. Default list shrink already performs length & element shrink (OK for phase 1). Later customize if cluster compaction needed.

### Element Shrinking (genFromParamSpecH)
| ParamSpec Variant | Shrink Plan |
|-------------------|-------------|
| `PSExact v`       | No shrink (singleton). |
| `PSEnum xs`       | Shrink toward first element index (preserve ordering semantics). Could map to indices and shrink Int. |
| `PSRangeInt l h`  | Hedgehog `int (linear l h)` already provides numeric shrink toward bounds (favor l). |
| `PSPredicate p l` | Generate via base arbitrary-like fallback then filter. Shrink: attempt underlying shrink steps; discard those failing predicate. (Note: may produce bigger shrink search; acceptable for initial PoC.) |
| `PSAny`           | Delegate to generic Hedgehog arbitrary-like (e.g. `intRange`, `text`, depending on instantiation). |

### Order Fail Minimization
Fail case example: expected exact order of generated sequence; by shrinking length first, failure reduces to shortest prefix still violating order (often 2 elements swapped). This matches core debugging ergonomics.

## Verification Hooks
- Single call: `shouldApplyTo`, `shouldApplyTimes` 1.
- Multiple calls count: `shouldApplyTimesToAnything` (exact length).
- Order: `shouldApplyInOrder` with `param <$> xs`.
- Partial order: optional later via `shouldApplyInPartialOrder` (subsequence).

## Property Failure Messages
Hedgehog includes diffing; we wrap library failures which already print expectation vs actual. Accept double-layer messaging for now; optional improvement: catch exception, parse result counts, attach as Hedgehog annotation.

## Error Modes
| Error | Source | Handling |
|-------|--------|----------|
| Argument mismatch | Mock lookup | Exception (prop fails) |
| Order mismatch | `shouldApplyInOrder` | Exception |
| Predicate rejection | Param predicate | Exception; counts excluded |
| Unexpected extra calls | Verification after run | Exception |

## Dependency Strategy (Phase 0 Decision)
- Do NOT add `hedgehog` to library build-dep yet.
- Add dependency only to test-suite; keep module either uncompiled (commented) or placed under `test/` until stable.
- Future: split `mockcat-hedgehog` package or cabal flag once API proves stable.

## Minimal Implementation Plan
1. Add test dependency: `hedgehog >= 1.4 && < 1.5` (verify current latest compatible with GHC matrix).
2. Create `test/PoC/HedgehogIntegration.hs` implementing:
   - `genFromParamSpecH` (mirror QuickCheck version).
   - `prop_singleCall_hh`
   - `prop_multiCallCount_hh`
   - `prop_order_shrinks` (deliberate adjacent swap; assert shrunk sequence length == 2).
3. (Optional) Draft `src/Test/MockCat/Hedgehog.hs` with only types re-exporting internal helpers guarded by CPP `#ifdef MOCKCAT_INTERNAL_HEDGEHOG` until dependency strategy decided.

## Open Questions
- Provide unified wrapper that auto-extracts ParamSpec from mock definition? (Would require storing spec alongside mock – intrusive.)
- Introduce a Scenario newtype now or after ParamSpec normalization? (Leaning after normalization.)
- Should predicate-based generators attempt smart shrinking (e.g., keep original value, then attempt ±1)? Possibly later.

## Future: Scenario DSL Alignment
Scenario DSL will likely produce a richer structure (maybe typed steps). Hedgehog layer should accept both `[a]` and a `Scenario a` convertible to flattened `[a]` for reuse of order & count verification.

## Risks & Mitigations
| Risk | Mitigation |
|------|------------|
| Predicate shrink starvation | Accept initial cost; document; later add custom shrink.
| Overfitting sequence shrink (custom) vs built-in list shrink | Start with built-in; only optimize if failure diffs are noisy.
| API churn after publication | Keep experimental; no export + doc disclaimer until stable.

## Example (sketch)
```haskell
prop_order_shrinks :: Property
prop_order_shrinks = Hedgehog.property $ do
  xs <- Hedgehog.forAll $ callScriptGenH (Range.linear 2 8) (genFromParamSpecH (PSRangeInt 0 5))
  Hedgehog.assert (length xs >= 2)
  let swapped = case xs of (a:b:rest) -> b:a:rest; _ -> xs
  m <- Hedgehog.evalIO $ buildMock xs
  Hedgehog.evalIO $ runScript swapped m  -- will fail order
  -- failure triggers shrink; we could post-check via annotation (phase 2)
```

## Acceptance Criteria (Phase 1)
- 3 green Hedgehog properties exercising single, multi, order failure shrink.
- No changes required in core library modules to support Hedgehog.
- Design doc (this file) committed.

---
END OF DESIGN DRAFT
