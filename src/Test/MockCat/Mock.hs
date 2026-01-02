{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- | Utilities for constructing verifiable stub functions.
     This module provides the core functions for creating mocks and stubs.

     = Key Functions
     * 'mock': Create a verifiable mock function (records calls).
     * 'stub': Create a pure stub function (no recording).
     * 'mockM': Create a monadic mock function (allows explicit side effects).
-}
module Test.MockCat.Mock
  ( MockBuilder
  , buildMock
  , mock
  , mockM
  , createNamedMockFnWithParams
  , CreateMockFn(..)
  , stub
  , shouldBeCalled
  , times
  , atLeast
  , atMost
  , greaterThan
  , lessThan
  , once
  , never
  , inOrder
  , inPartialOrder
  , inOrderWith
  , inPartialOrderWith
  , calledWith
  , anything
  , withArgs
  , onCase
  , cases
  , casesIO
  , label
  , Label
  , MockDispatch(..)
  , IsMockSpec
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (get, put)
import Control.Concurrent.STM (atomically, modifyTVar')
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Prelude hiding (lookup)
import Test.MockCat.Internal.Builder
import Test.MockCat.Internal.Verify (verifyExpectationDirect)
import qualified Test.MockCat.Internal.MockRegistry as MockRegistry ( register )
import Test.MockCat.Internal.Types
import Test.MockCat.Param
import Test.MockCat.Verify
import Test.MockCat.Cons (Head(..), (:>)(..))


-- | Type family to convert raw values to mock parameters.
--   Raw values (like "foo") are converted to Head :> Param a,
--   while existing Param chains remain unchanged.
type family ToMockParams p where
  ToMockParams (Param a :> rest) = Param a :> rest  -- Already a Param chain, keep as is
  ToMockParams (Param a) = Param a                  -- Single Param, keep as is
  ToMockParams (Cases a b) = Cases a b              -- Cases, keep as is
  ToMockParams (IO a) = IO a                        -- IO, keep as is
  ToMockParams (Head :> a) = Head :> a              -- Already has Head, keep as is
  ToMockParams a = Head :> Param a                  -- Raw value, wrap with Head :> Param

-- | Type class for converting values to mock parameters.
class CreateMock p where
  toParams :: p -> ToMockParams p

-- Instance for Param chains (most specific - should match first)
instance {-# OVERLAPPING #-} CreateMock (Param a :> rest) where
  toParams = id

-- Instance for single Param
instance {-# OVERLAPPING #-} CreateMock (Param a) where
  toParams = id

-- Instance for Cases
instance {-# OVERLAPPING #-} CreateMock (Cases a b) where
  toParams = id

-- Instance for IO
instance {-# OVERLAPPING #-} CreateMock (IO a) where
  toParams = id

-- Instance for Head :> Param r (constant values)
instance {-# OVERLAPPING #-} CreateMock (Head :> Param r) where
  toParams = id

-- Instance for raw values (fallback)
-- This handles raw values by wrapping them with Head :> Param
-- We need to ensure this doesn't match Param chains, Cases, IO, or Head types
instance {-# OVERLAPPABLE #-}
  ( ToMockParams b ~ (Head :> Param b)
  , Normalize b ~ Param b
  , Typeable b
  , WrapResult b
  ) => CreateMock b where
    toParams value = Head :> wrapResult value

-- | Label type for naming mock functions.
newtype Label = Label MockName

-- | Label function for naming mock functions.
--   Use it with 'mock' to provide a name for the mock function.
--   
--   @
--   f <- mock (label "mockName") $ "a" ~> "b"
--   @
label :: MockName -> Label
label = Label

-- | Type class for creating mock functions with optional name.
class CreateMockFn a where
  mockImpl :: a

-- | Type class for creating stub functions with optional name.
class CreateStubFn a where
  stubImpl :: a

-- | Type class for extracting result or unit.






-- | Type family to distinguish MockSpec from other parameters.
type family IsMockSpec p :: Bool where
  IsMockSpec (MockSpec p e) = 'True
  IsMockSpec other = 'False

-- | Internal class for dispatching named mocks based on parameter type.
--   This resolves overlapping instances between generic params and MockSpec.
--   The 'flag' parameter avoids instance overlap.
class MockDispatch (flag :: Bool) p m fn | flag p m -> fn where
  mockDispatchImpl :: Label -> p -> m fn

-- Generic instance for named mocks (flag ~ 'False)
instance
  ( MonadIO m
  , CreateMock p
  , MockBuilder (ToMockParams p) fn verifyParams
  , Show verifyParams
  , EqParams verifyParams
  , Typeable verifyParams
  , Typeable fn
  , IsMockSpec p ~ 'False
  ) =>
  MockDispatch 'False p m fn
  where
  mockDispatchImpl (Label name) p = do
    let params = toParams p
    BuiltMock { builtMockFn = fn, builtMockRecorder = recorder } <- buildMock (Just name) params :: m (BuiltMock fn verifyParams)
    _ <- liftIO $ MockRegistry.register (Just name) recorder fn
    pure fn

-- Specific instance for MockSpec (flag ~ 'True)
instance
  ( MonadIO m
  , MonadWithMockContext m
  , CreateMock params
  , MockBuilder (ToMockParams params) fn verifyParams
  , Show verifyParams
  , EqParams verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  MockDispatch 'True (MockSpec params [Expectation verifyParams]) m fn
  where
  mockDispatchImpl (Label name) (MockSpec params exps) = do
    BuiltMock { builtMockFn = fn, builtMockRecorder = recorder } <- buildMock (Just name) (toParams params) :: m (BuiltMock fn verifyParams)
    _ <- liftIO $ MockRegistry.register (Just name) recorder fn
    
    WithMockContext ctxRef <- askWithMockContext
    let resolved = ResolvedMock (Just name) recorder
    let verifyAction = mapM_ (verifyExpectationDirect resolved) exps
    liftIO $ atomically $ modifyTVar' ctxRef (++ [verifyAction])

    liftIO $ atomically $ modifyTVar' ctxRef (++ [verifyAction])

    pure fn

-- | Create a mock function with verification hooks attached (unnamed version).
--   The returned function mimics a pure function (via 'unsafePerformIO') but records its calls for later verification.
--
--   > f <- mock $ "a" ~> "b"
--   > f "a" `shouldBe` "b"
--   > f `shouldBeCalled` "a"
instance {-# OVERLAPPABLE #-}
  ( MonadIO m
  , CreateMock p
  , MockBuilder (ToMockParams p) fn verifyParams
  , Show verifyParams
  , EqParams verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  CreateMockFn (p -> m fn)
  where
  mockImpl p = do
    let params = toParams p
    BuiltMock { builtMockFn = fn, builtMockRecorder = recorder } <- buildMock Nothing params :: m (BuiltMock fn verifyParams)
    _ <- liftIO $ MockRegistry.register Nothing recorder fn
    pure fn

-- | Create a mock function from MockSpec.
--   MockSpec can optionally contain expectations.
--   If expectations are present, they are automatically registered.
--
--   > f <- mock $ any ~> True
--   > f <- mock $ any ~> True `expects` do called once
instance {-# OVERLAPPING #-}
  ( MonadIO m
  , MonadWithMockContext m
  , CreateMock params
  , MockBuilder (ToMockParams params) fn verifyParams
  , Show verifyParams
  , EqParams verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  CreateMockFn (MockSpec params [Expectation verifyParams] -> m fn)
  where
  mockImpl (MockSpec params exps) = do
    BuiltMock { builtMockFn = fn, builtMockRecorder = recorder } <- buildMock Nothing (toParams params) :: m (BuiltMock fn verifyParams)
    _ <- liftIO $ MockRegistry.register Nothing recorder fn
    
    WithMockContext ctxRef <- askWithMockContext
    let resolved = ResolvedMock Nothing recorder
    let verifyAction = mapM_ (verifyExpectationDirect resolved) exps
    liftIO $ atomically $ modifyTVar' ctxRef (++ [verifyAction])

    pure fn

-- | Create a named mock function.
--   The name is used in error messages to help you identify which mock failed.
--
--   > f <- mock (label "MyAPI") $ "a" ~> "b"
instance {-# OVERLAPPING #-} forall p m fn.
  ( MockDispatch (IsMockSpec p) p m fn
  ) =>
  CreateMockFn (Label -> p -> m fn)
  where
  mockImpl = mockDispatchImpl @(IsMockSpec p)

-- | Create a mock function with verification hooks attached.
--
-- This function can be used in two ways:
--
-- 1. Without a name:
--    @
--    f <- mock $ "a" ~> "b"
--    @
--
-- 2. With a name (using 'label'):
--    @
--    f <- mock (label "mockName") $ "a" ~> "b"
--    @
--
-- The function creates a verifiable stub that records calls
-- and can be verified via the unified 'shouldBeCalled' API.
-- The function internally uses 'unsafePerformIO' to make the returned function
-- appear pure, but it requires 'MonadIO' for creation.
mock :: CreateMockFn a => a
mock = mockImpl

-- | Type class for creating monadic mock functions without unsafePerformIO.
class CreateMockFnM a where
  mockMImpl :: a

instance
  ( MonadIO m
  , CreateMock p
  , MockIOBuilder (ToMockParams p) fn verifyParams
  , LiftFunTo fn fnM m
  , Typeable verifyParams
  , Typeable fnM
  ) =>
  CreateMockFnM (p -> m fnM)
  where
  mockMImpl p = do
    let params = toParams p
    BuiltMock { builtMockFn = fnIO, builtMockRecorder = verifier } <- buildIO Nothing params
    let lifted = liftFunTo (Proxy :: Proxy m) fnIO
    liftIO $ MockRegistry.register Nothing verifier lifted

instance {-# OVERLAPPING #-}
  ( MonadIO m
  , CreateMock p
  , MockIOBuilder (ToMockParams p) fn verifyParams
  , LiftFunTo fn fnM m
  , Typeable verifyParams
  , Typeable fnM
  ) =>
  CreateMockFnM (Label -> p -> m fnM)
  where
  mockMImpl (Label name) p = do
    let params = toParams p
    BuiltMock { builtMockFn = fnIO, builtMockRecorder = verifier } <- buildIO (Just name) params
    let lifted = liftFunTo (Proxy :: Proxy m) fnIO
    liftIO $ MockRegistry.register (Just name) verifier lifted

mockM :: CreateMockFnM a => a
mockM = mockMImpl

-- | Internal function for TH code that already has MockBuilder constraint.
--   This avoids CreateNamedMock instance resolution issues in generated code.
createNamedMockFnWithParams ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  MockName ->
  params ->
  m fn
createNamedMockFnWithParams name params = do
  BuiltMock { builtMockFn = fn, builtMockRecorder = recorder } <- buildMock (Just name) params
  liftIO $ MockRegistry.register (Just name) recorder fn


-- | Create a pure stub function without verification hooks.
--   Useful when you only need to return values and don't care about verification.
--   This is completely pure and safe.
--
--   > let f = stub $ "a" ~> "b"
instance StubBuilder params fn => CreateStubFn (params -> fn) where
  stubImpl = buildStub Nothing

-- | Create a named pure stub function without verification hooks (named version).
--
-- The provided name is used in failure messages.
-- This function creates a simple stub that returns values based on the provided
-- parameters, but does not support verification. Use 'mock' if you need
-- verification capabilities.
--
-- @
-- let f = stub (label "stubName") $ "a" ~> "b"
-- @
instance {-# OVERLAPPING #-} StubBuilder params fn => CreateStubFn (Label -> params -> fn) where
  stubImpl (Label name) = buildStub (Just name)

{- | Create a pure stub function without verification hooks.

This function can be used in two ways:

1. Without a name:
   @
   let f = stub $ "a" ~> "b"
   @

2. With a name (using 'label'):
   @
   let f = stub (label "stubName") $ "a" ~> "b"
   @

This function creates a simple stub that returns values based on the provided
parameters, but does not support verification. Use 'mock' if you need
verification capabilities.
-}
stub :: CreateStubFn a => a
stub = stubImpl

{- | Register a stub case within a 'Cases' builder. -}
onCase :: a -> Cases a ()
onCase a = Cases $ do
  st <- get
  put (st ++ [a])

{- | Define stub cases from a list of patterns. -}
cases :: [a] -> Cases a ()
cases a = Cases $ put a

{- | IO variant of 'cases'. -}
casesIO :: [a] -> Cases (IO a) ()
casesIO = Cases . (put . map pure)

class LiftFunTo funIO funM (m :: Type -> Type) | funIO m -> funM where
  liftFunTo :: Proxy m -> funIO -> funM

instance MonadIO m => LiftFunTo (IO r) (m r) m where
  liftFunTo _ = liftIO

instance LiftFunTo restIO restM m => LiftFunTo (a -> restIO) (a -> restM) m where
  liftFunTo proxy f a = liftFunTo proxy (f a)


