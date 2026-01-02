{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | withMock: Declarative mock expectations DSL
-}
module Test.MockCat.WithMock
  ( withMock
  , expects
  , MockResult(..)
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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Concurrent.STM (newTVarIO, readTVarIO, atomically, modifyTVar')
import Control.Monad.State (get, put, modify)

import Test.MockCat.Verify (TimesSpec(..), times, once, never, atLeast, atMost, greaterThan, lessThan, anything)
import Test.MockCat.Verify (ResolvableMock, ResolvableParamsOf)
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
  , InvocationRecorder(..)
  , ResolvedMock(..)
  )
import qualified Test.MockCat.Internal.Registry.Core as MockRegistry
import Unsafe.Coerce (unsafeCoerce)

import Test.MockCat.Param (Param(..), param, EqParams(..))
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

-- | A specialized Unit type that carries the parameter type information.
--   This is used to improve type inference for unit-returning mock helpers.
newtype MockResult params = MockResult ()
  deriving (Show, Eq)



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
--   The params type is usually inferred from the mock function, but can be overridden
class BuildExpectations fn exp params | fn exp -> params where
  buildExpectations :: fn -> exp -> [Expectation params]

-- | Instance for direct Expectations value
instance {-# OVERLAPPABLE #-} forall fn params. (ResolvableParamsOf fn ~ params) => BuildExpectations fn (Expectations params ()) params where
  buildExpectations _ = runExpectations

-- | Instance for function form when fn is MockResult
instance {-# OVERLAPPING #-} forall params. BuildExpectations (MockResult params) ((MockResult params) -> Expectations params ()) params where
  buildExpectations _ f = runExpectations (f (MockResult ()))

-- | Instance for direct Expectations value when fn is MockResult
instance {-# OVERLAPPING #-} forall params. BuildExpectations (MockResult params) (Expectations params ()) params where
  buildExpectations _ = runExpectations

-- | Instance for direct Expectations value when fn is ()
instance {-# OVERLAPPING #-} forall params. BuildExpectations () (Expectations params ()) params where
  buildExpectations _ = runExpectations

-- | Instance for function form (fn -> Expectations params ())
instance {-# OVERLAPPABLE #-} forall fn params. (ResolvableParamsOf fn ~ params) => BuildExpectations fn (fn -> Expectations params ()) params where
  buildExpectations fn f = runExpectations (f fn)

-- | Type class for dispatching expectations based on mock function type
class ExpectsDispatch fn exp m where
  expects :: m fn -> exp -> m fn

-- | Specialized instance for MockResult helpers (handles type inference)
instance {-# OVERLAPPING #-} (exp ~ Expectations params (), ExpectsDispatchImpl 'True (MockResult params) (Expectations params ()) m) => ExpectsDispatch (MockResult params) exp m where
  expects = expectsDispatchImpl @'True

-- | Specialized instance for Unit helpers (fallback)
instance {-# OVERLAPPING #-} (exp ~ Expectations params (), ExpectsDispatchImpl 'True () (Expectations params ()) m) => ExpectsDispatch () exp m where
  expects = expectsDispatchImpl @'True

-- | Generic instance for normal mocks
instance {-# OVERLAPPABLE #-} (ExpectsDispatchImpl 'False fn exp m) => ExpectsDispatch fn exp m where
  expects = expectsDispatchImpl @'False

-- | Internal class for implementation dispatch
class ExpectsDispatchImpl (flag :: Bool) fn exp m where
  expectsDispatchImpl :: m fn -> exp -> m fn

-- | Instance for normal mocks (flag ~ 'False)
--   Strict matching of params
instance
  ( MonadIO m
  , MonadWithMockContext m
  , ResolvableMock fn
  , ResolvableParamsOf fn ~ params
  , ExtractParams exp
  , ExpParams exp ~ params
  , BuildExpectations fn exp params
  , Show params
  , EqParams params
  ) =>
  ExpectsDispatchImpl 'False fn exp m
  where
  expectsDispatchImpl mockFnM exp = do
    (WithMockContext ctxVar) <- askWithMockContext
    -- Try to help type inference by using exp first
    let _ = extractParams exp :: Proxy params
    mockFn <- mockFnM
    -- Get the recorder from the thread-local store (set by mock/register)
    -- This avoids StableName lookup and is HPC-safe
    (mockName, mRecorder) <- liftIO $ MockRegistry.getLastRecorder @(InvocationRecorder params)
    
    let resolved = case mRecorder of
          Just recorder -> ResolvedMock mockName recorder
          Nothing -> errorWithoutStackTrace "expects: mock recorder not found. Use mock inside withMock/runMockT."
  
    let expectations = buildExpectations mockFn exp
    let actions = map (verifyExpectationDirect resolved) expectations
    liftIO $ atomically $ modifyTVar' ctxVar (++ actions)
    pure mockFn

-- | Instance for MockResult mocks (flag ~ 'True)
--   Dynamic resolution using expectation params
instance
  ( MonadIO m
  , MonadWithMockContext m
  , BuildExpectations (MockResult params) (Expectations params ()) params
  , Show params
  , EqParams params
  ) =>
  ExpectsDispatchImpl 'True (MockResult params) (Expectations params ()) m
  where
  expectsDispatchImpl mockFnM exp = do
    (WithMockContext ctxVar) <- askWithMockContext
    _ <- mockFnM
    (mockName, mRecorder) <- liftIO $ MockRegistry.getLastRecorderRaw
    resolved <- case mRecorder of
      Just raw -> do
         let recorder = unsafeCoerce raw :: InvocationRecorder params
         pure $ ResolvedMock mockName recorder
      Nothing -> errorWithoutStackTrace "expects: mock recorder not found (Dynamic Resolution Failed). Ensure the mock helper function was called."
    -- Use the expectations directly since we know the context
    let expectations = runExpectations exp
    let actions = map (verifyExpectationDirect resolved) expectations
    liftIO $ atomically $ modifyTVar' ctxVar (++ actions)
    pure (MockResult ())

-- | Instance for Unit mocks (flag ~ 'True)
--   Dynamic resolution using expectation params
instance
  ( MonadIO m
  , MonadWithMockContext m
  , BuildExpectations () (Expectations params ()) params
  , Show params
  , EqParams params
  ) =>
  ExpectsDispatchImpl 'True () (Expectations params ()) m
  where
  expectsDispatchImpl mockFnM exp = do
    (WithMockContext ctxVar) <- askWithMockContext
    _ <- mockFnM
    (mockName, mRecorder) <- liftIO $ MockRegistry.getLastRecorderRaw
    resolved <- case mRecorder of
      Just raw -> do
         let recorder = unsafeCoerce raw :: InvocationRecorder params
         pure $ ResolvedMock mockName recorder
      Nothing -> errorWithoutStackTrace "expects: mock recorder not found (Dynamic Resolution Failed). Ensure the mock helper function was called."
    let expectations = buildExpectations () exp
    let actions = map (verifyExpectationDirect resolved) expectations
    liftIO $ atomically $ modifyTVar' ctxVar (++ actions)
    pure ()



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
