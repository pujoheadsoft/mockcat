{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.MockCat.HPCFallbackSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate)
import Prelude hiding (readFile, writeFile)

spec :: Spec
spec = do
  describe "HPC Fallback Scenarios (Multiple Mocks)" do
    
    it "distinguishes mocks of different types" do
      -- Case 1: Different types should be distinguishable by type check in fallback
      f <- mock $ "a" ~> (1 :: Int)
      g <- mock ((1 :: Int) ~> "b")
      
      -- Verify f (String -> Int)
      f "a" `shouldBe` 1
      f `shouldBeCalled` "a"
      
      -- Verify g (Int -> String)
      g 1 `shouldBe` "b"
      g `shouldBeCalled` (1 :: Int)

    it "behavior with multiple mocks of SAME type (Unnamed)" do
      -- Case 2: Same type mocks. 
      -- In HPC environment (StableName broken), fallback picks based on history.
      -- Expected behavior: It might pick the wrong recorder if implementation blindly takes the first/last match.
      
      f1 <- mock $ "1" ~> (1 :: Int)
      f2 <- mock $ "2" ~> (2 :: Int)

      -- Execute both
      evaluate $ f1 "1"
      evaluate $ f2 "2"

      -- Verify f1. 
      -- If fallback picks f2 (latest or first), verification might fail or see wrong calls.
      f1 `shouldBeCalled` "1" 
      
      -- Verify f2
      f2 `shouldBeCalled` "2"

    it "behavior with multiple mocks of SAME type (Named)" do
      -- Case 3: Named mocks of same type.
      -- Verification should ideally use the name to disambiguate.
      
      fA <- mock (label "mockA") $ "A" ~> (1 :: Int)
      fB <- mock (label "mockB") $ "B" ~> (2 :: Int)

      evaluate $ fA "A"
      evaluate $ fB "B"

      fA `shouldBeCalled` "A"
      fB `shouldBeCalled` "B"
