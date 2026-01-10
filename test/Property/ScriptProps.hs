{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-hpc #-}
module Property.ScriptProps
  ( spec
  , prop_script_count_matches
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.MockCat
import Property.Generators
import Control.Monad.IO.Class (liftIO)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
    describe "Property Script Generator" $ do
      it "script count matches recorded calls" $ property prop_script_count_matches

-- | Property: executing a generated script produces exactly that many recorded calls.
prop_script_count_matches :: Property
prop_script_count_matches = forAll scriptGen $ \scr@(Script xs) -> monadicIO $ do
  -- Use withMock for safe verification
  run $ withMock $ do
    f <- mock (cases [ param a ~> True | a <- xs ])
          `expects` called (times (length xs))
    
    liftIO $ runScript f scr
  assert True
