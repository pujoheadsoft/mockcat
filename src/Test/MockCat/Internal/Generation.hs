-- | Internal generation helpers (placeholder).
--
-- This module is reserved for backend-agnostic helpers that may be reused by
-- future QuickCheck / Hedgehog / Scenario integrations (e.g. length heuristics,
-- normalisation utilities).  Keeping a stable façade here reduces churn when
-- property backends are split into separate packages.
--
-- NOTE: Currently empty – populated during Hedgehog PoC convergence.
module Test.MockCat.Internal.Generation where

-- Future sketch:
-- shrinkListLike :: [a] -> [[a]]
-- normaliseEnum  :: (Ord a) => [a] -> [a]
