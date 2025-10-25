{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module PoC.ScenarioDSLPoC
  ( Scenario(..)
  , Step(..)
  , buildMockFor
  , runScenario
  , prop_scenario_order
  , prop_scenario_count
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.MockCat hiding (any)
import qualified Test.MockCat as MC (any)

-- | Minimal Step GADT (拡張余地: Fork / Barrier / ExpectCount ...)
newtype Step a = Call a deriving (Show, Eq)

newtype Scenario a = Scenario { unScenario :: [Step a] } deriving (Show, Eq)

-- | Build a single-argument mock whose allowed arguments are exactly the scenario steps (each returning True).
-- Implemented via 'cases' so each expected argument is validated per call.
buildMockFor :: [Int] -> IO (Mock (Int -> Bool) (Param Int))
buildMockFor [] = createMock (param 0 |> True)  -- degenerate, never invoked
buildMockFor xs = createMock $ cases [ param a |> True | a <- xs ]

-- | Execute a scenario against a given stub.
runScenario :: (Int -> Bool) -> Scenario Int -> IO ()
runScenario f (Scenario steps) = mapM_ (\(Call a) -> f a `seq` pure ()) steps

-- Generators ---------------------------------------------------------------

scenarioGen :: Gen (Scenario Int)
scenarioGen = do
  -- limited to 9 due to MockBuilder arity constraints
  sz <- chooseInt (0,9)
  xs <- vectorOf sz arbitrary
  pure $ Scenario (Call <$> xs)

-- Properties ---------------------------------------------------------------

-- | Order verification property (skips empty scenario to avoid meaningless order check).
prop_scenario_order :: Property
prop_scenario_order = forAll scenarioGen $ \sc@(Scenario steps) ->
  not (null steps) ==> monadicIO $ do
    let xs = [ a | Call a <- steps ]
    m <- run $ buildMockFor xs
    let f = stubFn m
    run $ runScenario f sc
    -- verify order
    run $ m `shouldApplyInOrder` (param <$> xs)
    assert True

-- | Total count property (includes empty). Uses 'shouldApplyTimesToAnything'.
prop_scenario_count :: Property
prop_scenario_count = forAll scenarioGen $ \(Scenario steps) -> monadicIO $ do
  let xs = [ a | Call a <- steps ]
  m <- run $ buildMockFor xs
  let f = stubFn m
  run $ runScenario f (Scenario steps)
  run $ m `shouldApplyTimesToAnything` (length xs :: Int)
  assert True
