{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}

module Test.MockCat.DeferredTypeErrorsSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List
import Prelude hiding (any)

class Monad m => BadNoop m where
  bad :: m Int

deriveNoopInstance [t|BadNoop|]

class Monad m => UnsupportedClass m where
  type family UnsupportedType m :: *
  unsupportedMethod :: m ()

deriveMockInstances [t|UnsupportedClass|]

spec :: Spec
spec = describe "Compile-time restrictions (Deferred Type Errors)" do
  it "expects throws type error when applied directly to MockSpec instead of mock result" do
    let expression = (any ~> True) `expects` do
          called once
    evaluate expression `shouldThrow` anyException

  it "expects throws type error when applied to instantiated mock function (f)" do
    let expression :: MockT IO ()
        expression = do
          f <- mock (any ~> (1 :: Int))
          let val = f `expects` do
                called once
          liftIO $ evaluate val
          pure ()
    runMockT expression `shouldThrow` anyException

  it "deriveNoopInstance throws type error when a method does not return m ()" do
    -- BadNoop.bad returns m Int, which is not supported by deriveNoopInstance.
    -- This should trigger the generated TypeError.
    let expression :: MockT IO Int
        expression = bad

    runMockT expression `shouldThrow` (\(e :: SomeException) ->
      let s = show e in
      (("deriveNoopInstance: Function `bad` does not return `m ()`" `Data.List.isInfixOf` s) || error ("Actual exception: " ++ s)))

  it "deriveMockInstances throws type error for unsupported declarations" do
    -- UnsupportedClass contains a type family, which is not supported by deriveMockInstances.
    -- The macro should generate a TypeError.
    let expression :: MockT IO ()
        expression = unsupportedMethod

    runMockT expression `shouldThrow` (\(e :: SomeException) ->
      "deriveMockInstances: Unsupported declaration in class: " `Data.List.isInfixOf` show e)
