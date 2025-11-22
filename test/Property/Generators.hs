{-# LANGUAGE ScopedTypeVariables #-}
module Property.Generators
  ( Script(..)
  , scriptGen
  , buildUnaryMock
  , runScript
  ) where

import Test.QuickCheck
import Test.MockCat

-- | A simple script: sequence of argument values for a unary function.
newtype Script a = Script { unScript :: [a] } deriving (Show, Eq)

-- | Generator for a script of Ints (length 0..9 to respect current arity limits if later extended to multi-arg).
scriptGen :: Gen (Script Int)
scriptGen = do
  n <- chooseInt (0,9)
  xs <- vectorOf n arbitrary
  pure (Script xs)

-- | Build a mock (Int -> Bool) whose allowed arguments are exactly the script values, each returning True.
buildUnaryMock :: Script Int -> IO (Int -> Bool)
buildUnaryMock (Script []) = createMockFn (param 0 |> True) -- degenerate, never invoked
buildUnaryMock (Script xs) = createMockFn $ cases [ param a |> True | a <- xs ]

-- | Execute the script against the provided stub function, forcing each Bool.
runScript :: (Int -> Bool) -> Script Int -> IO ()
runScript f (Script xs) = mapM_ (\a -> f a `seq` pure ()) xs
