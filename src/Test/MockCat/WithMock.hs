{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
  , withAnything
  , calledInOrder
  , calledInSequence
  , times
  , once
  , never
  , atLeast
  , anything
  , WithMockContext(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..), runReaderT, MonadReader(..), ask)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Data.Foldable (for_)
import Test.MockCat.Mock (mock)
import Test.MockCat.Verify
  ( ResolvableParamsOf
  , ResolvableMock
  , ResolvableMockWithParams
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
  , VerificationSpec(..)
  )
import Test.MockCat.Internal.Types
  ( Verifier
  , AppliedRecord
  , appliedParamsList
  , CountVerifyMethod(..)
  , VerifyOrderMethod(..)
  )
import Test.MockCat.Internal.Registry (lookupVerifierForFn, withAllUnitGuards)
import Test.MockCat.Param (Param(..), param, (|>))
import Test.MockCat.Cons ((:>))
import Data.Typeable (Typeable)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (eqT)
import Data.Dynamic (fromDynamic)
import Unsafe.Coerce (unsafeCoerce)

-- | Mock expectation context
newtype WithMockContext = WithMockContext (TVar [MockExpectation])

-- | Mock expectation data
data MockExpectation where
  MockExpectation ::
    ( ResolvableMock m
    , ResolvableParamsOf m ~ params
    , Typeable params
    , Typeable (Verifier params)
    , Show params
    , Eq params
    ) =>
    m ->
    Expectation params ->
    MockExpectation

-- | Expectation specification
data Expectation params where
  -- | Count expectation with specific arguments
  CountExpectation :: CountVerifyMethod -> params -> Expectation params
  -- | Count expectation without arguments (any arguments)
  CountAnyExpectation :: Int -> Expectation params
  -- | Order expectation
  OrderExpectation :: VerifyOrderMethod -> [params] -> Expectation params
  -- | Simple expectation (at least once) with arguments
  SimpleExpectation :: params -> Expectation params
  -- | Simple expectation (at least once) without arguments
  AnyExpectation :: Expectation params

-- | Run a block with mock expectations that are automatically verified
withMock :: ReaderT WithMockContext IO a -> IO a
withMock action = do
  ctxVar <- newTVarIO []
  let ctx = WithMockContext ctxVar
  result <- runReaderT action ctx
  -- Verify all expectations
  expectations <- readTVarIO ctxVar
  for_ expectations $ \(MockExpectation mockFn expectation) -> do
    verifyExpectation mockFn expectation
  pure result

-- | Verify a single expectation
verifyExpectation ::
  ( ResolvableMock m
  , ResolvableParamsOf m ~ params
  , Typeable params
  , Typeable (Verifier params)
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
      verifyAppliedCount (resolvedMockName resolved) (resolvedMockVerifier resolved) count
    OrderExpectation method argsList ->
      verifyOrder method mockFn argsList
    SimpleExpectation args ->
      verifyResolvedAny resolved
    AnyExpectation ->
      verifyResolvedAny resolved

-- | Attach expectations to a mock function
expects ::
  forall m fn params.
  ( MonadIO m
  , MonadReader WithMockContext m
  , ResolvableMock fn
  , ResolvableParamsOf fn ~ params
  , Typeable params
  , Typeable (Verifier params)
  , Show params
  , Eq params
  ) =>
  m fn ->
  Expectation params ->
  m fn
expects mockFnM expectation = do
  ctx@(WithMockContext ctxVar) <- ask
  mockFn <- mockFnM
  liftIO $ atomically $ modifyTVar' ctxVar (++ [MockExpectation mockFn expectation])
  pure mockFn

-- | Create a count expectation
called :: TimesSpec -> CalledSpec
called (TimesSpec method) = CalledSpec method

-- | Called specification
newtype CalledSpec = CalledSpec CountVerifyMethod

-- | Combine called spec with arguments
--   Accepts both raw values (like "a") and Param values (like param "a")
class WithArgs spec args result where
  with :: spec -> args -> result

instance {-# OVERLAPPING #-}
  ( Eq params
  , Show params
  , Typeable params
  ) =>
  WithArgs CalledSpec params (Expectation params)
  where
  with (CalledSpec method) args = CountExpectation method args

instance {-# OVERLAPPABLE #-}
  ( Eq (Param a)
  , Show (Param a)
  , Typeable (Param a)
  , Typeable a
  , params ~ Param a
  ) =>
  WithArgs CalledSpec a (Expectation params)
  where
  with (CalledSpec method) rawValue = CountExpectation method (param rawValue)

-- | Combine called spec with 'anything' (accepts any arguments)
--   Note: This returns a polymorphic Expectation, which will be resolved
--   based on the mock function's parameter type.
withAnything ::
  forall params.
  CalledSpec ->
  Expectation params
withAnything (CalledSpec method) = 
  case method of
    Equal n -> CountAnyExpectation n
    _ -> error "withAnything only supports Equal (times)"

-- | Create an order expectation
calledInOrder ::
  ( Eq params
  , Show params
  , Typeable params
  ) =>
  [params] ->
  Expectation params
calledInOrder args = OrderExpectation ExactlySequence args

-- | Create a partial order expectation
calledInSequence ::
  ( Eq params
  , Show params
  , Typeable params
  ) =>
  [params] ->
  Expectation params
calledInSequence args = OrderExpectation PartiallySequence args

