{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}

module Test.MockCat.DeferredTypeErrorsSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (any)

spec :: Spec
spec = describe "Compile-time restrictions (Deferred Type Errors)" do
  it "expects throws type error when applied directly to MockSpec instead of mock result" do
    -- This expression should fail to typecheck because `expects` requires `m fn` (monadic action),
    -- but here it is applied to `MockSpec` (pure value).
    -- With -fdefer-type-errors, this becomes a runtime error.
    let expression = (any ~> True) `expects` do
          called once
    
    evaluate expression `shouldThrow` anyException

  it "expects throws type error when applied to instantiated mock function (f)" do
    -- f <- mock ... returns a function `f`.
    -- expects expects `m fn`, not `fn`.
    -- This verifies that we cannot "attach" expectations to an existing function variable.
    -- We type-annotate the block to ensure 'mock' has a concreter context (MockT IO),
    -- so that the error is specifically about 'f' not matching 'm fn'.
    let expression :: MockT IO ()
        expression = do
          f <- mock (any ~> (1 :: Int))
          let val = f `expects` do
                called once
          liftIO $ evaluate val
          pure ()
    
    -- Since the type error is deferred inside the IO action logic, we must run it to trigger the exception.
    runMockT expression `shouldThrow` anyException

