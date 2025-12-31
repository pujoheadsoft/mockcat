{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | withMock: Declarative mock expectations DSL
-}
module Test.MockCat.WithMock
  ( withMock
  , expects
  , called
  , with
  , calledInOrder
  , calledInSequence
  , times
  , once
  , never
  , atLeast
  , atMost
  , greaterThan
  , lessThan
  , anything
  , WithMockContext(..)
  , MonadWithMockContext(..)
  , Expectation(..)
  , Expectations(..)
  , verifyExpectationDirect
  ) where

import Control.Monad.IO.Class ()
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad.State (get, put, modify)
import Test.MockCat.Verify (VerificationSpec(..), TimesSpec(..), times, once, never, atLeast, atMost, greaterThan, lessThan, anything)
import Test.MockCat.Internal.Verify
  ( verifyExpectationDirect
  )
import Test.MockCat.Internal.Types
  ( VerifyOrderMethod(..)
  , WithMockContext(..)
  , MonadWithMockContext(..)
  , Expectation(..)
  , Expectations(..)
  , runExpectations
  , addExpectation
  )

import Test.MockCat.Param (Param(..), param, MockSpec(..), ArgsOf)


-- | Run a block with mock expectations that are automatically verified
withMock :: ReaderT WithMockContext IO a -> IO a
withMock action = do
  ctxVar <- newTVarIO []
  let ctx = WithMockContext ctxVar
  result <- runReaderT action ctx
  -- Verify all registered verification actions
  actions <- readTVarIO ctxVar
  sequence_ actions
  pure result

-- | Attach expectations to a mock function
--   Supports both single expectation and multiple expectations in a do block
infixr 0 `expects`



-- | Type class for building expectations from various expression types.
-- This is used to convert the expectation DSL into a list of Expectation values.
class BuildExpectationsForParams exp params | exp -> params where
  buildExpectationsForParams :: exp -> [Expectation params]

-- | Instance for Expectations monad
instance BuildExpectationsForParams (Expectations params ()) params where
  buildExpectationsForParams = runExpectations

-- | Instance for VerificationSpec
instance BuildExpectationsForParams (VerificationSpec params) params where
  buildExpectationsForParams spec =
    case spec of
      CountVerification method args -> [CountExpectation method args]
      CountAnyVerification method -> [CountAnyExpectation method]
      OrderVerification method argsList -> [OrderExpectation method argsList]
      SimpleVerification args -> [SimpleExpectation args]
      AnyVerification -> [AnyExpectation]

-- | Attach expectations to paramerters.
--   This creates a MockSpec which can be passed to 'mock' or typeclass mock methods.
--   
--   > mock $ any ~> True `expects` do ...
expects ::
  forall params exp.
  ( BuildExpectationsForParams exp (ArgsOf params)
  ) =>
  params ->
  exp ->
  MockSpec params [Expectation (ArgsOf params)]
expects params exp = MockSpec params (buildExpectationsForParams exp)

-- | Create a count expectation builder
--   The params type is inferred from the mock function in expects
--   Use type application to specify params when needed: called @(Param String) once
-- | Class-based called builder so that the `params` type can be resolved
--   via instance selection in the `expects` context.
--   This version uses a type class to help type inference by allowing
--   the params type to be inferred from the context where it's used.
class Called params where
  called :: TimesSpec -> Expectations params ()

-- | Default instance that works for any params type
instance {-# OVERLAPPABLE #-} Called params where
  called (TimesSpec method) = do
    addExpectation (CountAnyExpectation method)

-- | Combine expectations with arguments
--   Accepts both raw values (like "a") and Param values (like param "a")
class WithArgs exp args params | exp args -> params where
  with :: exp -> args -> Expectations params ()

instance {-# OVERLAPPING #-}
  WithArgs (Expectations params ()) params params
  where
  with expM args = do
    expM
    -- Extract the last expectation (last in list, since addExpectation appends) and modify it to include args
    Expectations $ do
      exps <- get
      case reverse exps of
        [] -> error "with: no expectation to add arguments to"
        (CountAnyExpectation method : rest) -> do
          put (reverse rest)
          modify (++ [CountExpectation method args])
        _ -> error "with: can only add arguments to count-only expectations"

instance {-# OVERLAPPABLE #-}
  (params ~ Param a, Show a, Eq a) =>
  WithArgs (Expectations params ()) a params
  where
  with expM rawValue = do
    expM
    -- Extract the last expectation (last in list, since addExpectation appends) and modify it to include args
    Expectations $ do
      exps <- get
      case reverse exps of
        [] -> error "with: no expectation to add arguments to"
        (CountAnyExpectation method : rest) -> do
          put (reverse rest)
          modify (++ [CountExpectation method (param rawValue)])
        _ -> error "with: can only add arguments to count-only expectations"




-- | Create an order expectation
--   Accepts both Param values and raw values
class CalledInOrder args params | args -> params where
  calledInOrder :: args -> Expectations params ()

-- | Convenience instance: infer params from function argument type @a@
instance
  (params ~ Param a, Show a, Eq a) =>
  CalledInOrder [a] params
  where
  calledInOrder args =
    addExpectation (OrderExpectation ExactlySequence (map param args))

-- | Create a partial order expectation
--   Accepts both Param values and raw values
class CalledInSequence args params | args -> params where
  calledInSequence :: args -> Expectations params ()

-- | Convenience instance: infer params from function argument type @a@
instance
  (params ~ Param a, Show a, Eq a) =>
  CalledInSequence [a] params
  where
  calledInSequence args =
    addExpectation (OrderExpectation PartiallySequence (map param args))
