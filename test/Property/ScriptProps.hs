{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-hpc #-}
module Property.ScriptProps
  ( prop_script_count_matches
  ) where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Test.MockCat (shouldBeCalled, times)
import Property.Generators

-- | Property: executing a generated script produces exactly that many recorded calls.
prop_script_count_matches :: Property
prop_script_count_matches = forAll scriptGen $ \scr@(Script xs) -> monadicIO $ do
  run resetMockHistory
  f <- run $ buildUnaryMock scr
  run $ runScript f scr
  run $ f `shouldBeCalled` times (length xs)
  assert True
