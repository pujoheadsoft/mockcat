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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | withMock: Declarative mock expectations DSL

This module provides a declarative way to define mock functions with expectations
that are automatically verified when the 'withMock' block completes.

Example:

@
withMock $ do
  mockFn <- mock (any |> True)
    `expects` do
      called once `with` "a"
  
  evaluate $ mockFn "a"
@
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
  , anything
  , WithMockContext(..)
  , MonadWithMockContext(..)
  , Expectation(..)
  , Expectations(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), runReaderT, MonadReader(..), ask)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Monad.State (State, get, put, modify, execState)
import Test.MockCat.Verify
  ( ResolvableParamsOf
  , ResolvableMock

  , requireResolved
  , verifyCount
  , verifyOrder
  , verifyResolvedAny
  , verifyAppliedCount
  , ResolvedMock(..)
  , TimesSpec(..)
  , times
  , once
  , never
  , atLeast
  , anything

  )
import Test.MockCat.Internal.Types
  ( CountVerifyMethod(..)
  , VerifyOrderMethod(..)
  )
import Test.MockCat.Param (Param(..), param)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

-- | Mock expectation context holds verification actions to run at the end
--   of the `withMock` block. Storing `IO ()` avoids forcing concrete param
--   types at registration time.
newtype WithMockContext = WithMockContext (TVar [IO ()])

class Monad m => MonadWithMockContext m where
  askWithMockContext :: m WithMockContext

instance {-# OVERLAPPABLE #-} (Monad m, MonadReader WithMockContext m) => MonadWithMockContext m where
  askWithMockContext = ask

-- | Expectation specification
data Expectation params where
  -- | Count expectation with specific arguments
  CountExpectation :: CountVerifyMethod -> params -> Expectation params
  -- | Count expectation without arguments (any arguments)
  CountAnyExpectation :: CountVerifyMethod -> Expectation params
  -- | Order expectation
  OrderExpectation :: VerifyOrderMethod -> [params] -> Expectation params
  -- | Simple expectation (at least once) with arguments
  SimpleExpectation :: params -> Expectation params
  -- | Simple expectation (at least once) without arguments
  AnyExpectation :: Expectation params

-- | Expectations builder (Monad instance for do syntax)
newtype Expectations params a = Expectations (State [Expectation params] a)
  deriving (Functor, Applicative, Monad)

-- | Run Expectations to get a list of expectations
runExpectations :: Expectations params a -> [Expectation params]
runExpectations (Expectations s) = execState s []

-- | Add an expectation to the builder
addExpectation :: Expectation params -> Expectations params ()
addExpectation exp = Expectations $ modify (++ [exp])

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

-- | Verify a single expectation
verifyExpectation ::
  ( ResolvableMock m
  , ResolvableParamsOf m ~ params
  , Show params
  , Eq params
  ) =>
  m ->
  Expectation params ->
  IO ()
verifyExpectation mockFn expectation = do
  resolved <- requireResolved mockFn
  case expectation of
    CountExpectation method args ->
      verifyCount mockFn args method
    CountAnyExpectation count ->
      verifyAppliedCount (resolvedMockName resolved) (resolvedMockRecorder resolved) count
    OrderExpectation method argsList ->
      verifyOrder method mockFn argsList
    SimpleExpectation _ ->
      verifyResolvedAny resolved
    AnyExpectation ->
      verifyResolvedAny resolved

-- | Attach expectations to a mock function
--   Supports both single expectation and multiple expectations in a do block
infixl 0 `expects`

-- | Type class to extract params type from an expectation expression
class ExtractParams exp where
  type ExpParams exp :: Type
  extractParams :: exp -> Proxy (ExpParams exp)

instance ExtractParams (Expectations params ()) where
  type ExpParams (Expectations params ()) = params
  extractParams _ = Proxy

instance ExtractParams (fn -> Expectations params ()) where
  type ExpParams (fn -> Expectations params ()) = params
  extractParams _ = Proxy

-- | Register expectations for a mock function
--   Accepts an Expectations builder
--   The params type is inferred from the mock function type or the expectation
class BuildExpectations fn exp where
  buildExpectations :: fn -> exp -> [Expectation (ResolvableParamsOf fn)]

-- | Instance for direct Expectations value
--   The params type must match ResolvableParamsOf fn
instance forall fn params. (ResolvableParamsOf fn ~ params) => BuildExpectations fn (Expectations params ()) where
  buildExpectations _ = runExpectations

-- | Instance for function form (fn -> Expectations params ())
--   This allows passing a function that receives the mock function
instance forall fn params. (ResolvableParamsOf fn ~ params) => BuildExpectations fn (fn -> Expectations params ()) where
  buildExpectations fn f = runExpectations (f fn)

expects ::
  forall m fn exp params.
  ( MonadIO m
  , MonadWithMockContext m
  , ResolvableMock fn
  , ResolvableParamsOf fn ~ params
  , ExtractParams exp
  , ExpParams exp ~ params
  , BuildExpectations fn exp
  , Show params
  , Eq params
  ) =>
  m fn ->
  exp ->
  m fn
expects mockFnM exp = do
  (WithMockContext ctxVar) <- askWithMockContext
  -- Try to help type inference by using exp first
  let _ = extractParams exp :: Proxy params
  mockFn <- mockFnM
  let expectations = buildExpectations mockFn exp
  let actions = map (verifyExpectation mockFn) expectations
  liftIO $ atomically $ modifyTVar' ctxVar (++ actions)
  pure mockFn

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
  (params ~ Param a) =>
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
  (params ~ Param a) =>
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
  (params ~ Param a) =>
  CalledInSequence [a] params
  where
  calledInSequence args =
    addExpectation (OrderExpectation PartiallySequence (map param args))

