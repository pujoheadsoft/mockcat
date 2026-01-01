{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.MockCat.MultipleMocksSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate, try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Prelude hiding (any)

spec :: Spec
spec = describe "Multiple Mocks Isolation" do
  it "expects attaches only to the target mock, not others of the same type" do
    -- This test ensures that `expects` is tightly bound to the specific mock instance
    -- it is chained from, even when multiple mocks share the same type signature.
    withMock do
      -- Define two mocks of the same type (String -> Int)
      -- f expects "A" once
      -- f expects matchers to be satisfied once
      f <- mock ("A" ~> (1 :: Int)) `expects` do
        called once
      
      -- g expects matchers to be satisfied once
      g <- mock ("B" ~> (2 :: Int)) `expects` do
        called once

      -- Call them to satisfy expectations
      liftIO $ evaluate $ f "A"
      liftIO $ evaluate $ g "B"
      
      pure ()

  it "fails when calls are crossed between identical mocks (expects isolation)" do
    -- Verify that calling f with "B" (g's expectation) does not count for g,
    -- and violates f's expectation (if strictly checked).
    -- Here we verify that f is NOT g.
    result :: Either SomeException () <- try $ withMock do
       f <- mock ("A" ~> (1 :: Int)) `expects` do
         called once
       
       mock ("B" ~> (2 :: Int)) `expects` do
         called once
       
       -- We call f with "B". 
       -- f expects "A", so f should fail (unexpected arg).
       -- g expects "B", but g was not called, so g should fail (count mismatch).
       liftIO $ evaluate $ f "B"
       pure ()

    -- We expect failure because expectations were not met.
    result `shouldSatisfy` isLeft


