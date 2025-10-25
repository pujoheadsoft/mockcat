# ParamSpec Union Design (Draft)

Status: draft / internal exploration

## Motivation
Current `ParamSpec` lacks an explicit union form. Composite specifications appear in tests as lists which we normalise internally (`NUnion`). Providing a first-class union could:
- enable richer shrinking (branch elimination)
- allow precise round-trip of normalization (avoid fallback to `PSAny`)
- support future scenario fuzzing where choice points are explicit for coverage accounting

## Proposed Surface (incremental)
```haskell
-- Experimental; behind an internal module or CPP flag initially
data ParamSpec a
  = PSExact a
  | PSEnum [a]
  | PSRangeInt Int Int
  | PSPredicate (a -> Bool) String
  | PSAny
  | PSOr [ParamSpec a]           -- NEW: semantic union (logical OR)
```
### Invariants for `PSOr`
- Flattened: no nested `PSOr` inside direct list (enforced by smart constructor)
- Deduplicated by structural equality after normalization
- Empty list forbidden (would be uninhabited); singleton reduced to that element

## Normalization Interaction
`normalise` would map:
- existing forms to current `NormalParamSpec`
- `PSOr parts` -> `NUnion (map normalise parts)` followed by union simplification rules

### Union Simplification Rules (future)
1. `NAny` absorbs: `NUnion [..., NAny, ...] => NAny`
2. Range merging (already implemented for Int via `normaliseManyInt`) generalized inside union
3. Enum promotion: singletons pulled into exacts; exacts collected into enum when cardinality ≥ 2 and value type is hashable & Eq
4. Subsumption: if an `NRangeInt l h` fully covers `NExact v` or another smaller range, drop the subsumed node
5. Predicate non-determinism: Opaque predicates cannot be reasoned for subsumption—left as-is and block some rewrites

## Generation Strategy
`NUnion` -> weighted uniform choice of branch gens. Future enhancement: dynamic weighting guided by coverage (e.g., number of uncovered script transitions referencing each branch).

## Shrinking Strategy (sketch)
1. Shrink within selected branch (standard structural shrink)
2. Attempt branch elimination (replace chosen branch value with value from simpler branch) if still satisfies failing predicate
3. For Int range branches, attempt narrowing range endpoints
4. For enums, shrink element list before value shrink (prefer smaller enumerations)

## Migration Plan
Phase 1 (done): internal `NUnion` representation only.
Phase 2: expose `PSOr` under `Test.MockCat.ParamSpec.Experimental` with smart constructors.
Phase 3: integrate Hedgehog gen + shrink heuristics.
Phase 4: coverage accounting (branch hit counts) for Scenario DSL.

## Open Questions
- Should `PSAny` appear inside `PSOr` or be short-circuited at construction? (Leaning: short-circuit to `PSAny`.)
- Interaction with predicate: can we tag predicate complexity to prioritize shrinking choices? Perhaps measure sampling acceptance rate.
- Need typeclass-based merging for non-Int numeric domains? (Potential `class NormalisableNumeric a` with bounds & successor/pred operations.)

## Risk & Safety
Keeping union experimental avoids breaking existing API; `toParamSpec` fallback to `PSAny` remains until `PSOr` is introduced. Range merging restricted to Int mitigates premature generalization.

## Next Steps
1. Implement `PSOr` behind internal module + tests mirroring current union properties.
2. Replace `toParamSpec NUnion -> PSAny` with reconstruction when all children reifiable.
3. Introduce coverage counters keyed by union branch index.

-- end --
