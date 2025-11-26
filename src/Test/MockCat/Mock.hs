{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}

{- | Utilities for constructing verifiable stub functions.

Each stub produced by this module records argument applications and can be
verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
-}
module Test.MockCat.Mock
  ( MockBuilder
  , build
  , mock
  , createNamedMockFnWithParams
  , stub
  , shouldApplyTo
  , shouldApplyTimes
  , shouldApplyInOrder
  , shouldApplyInPartialOrder
  , shouldApplyTimesGreaterThanEqual
  , shouldApplyTimesLessThanEqual
  , shouldApplyTimesGreaterThan
  , shouldApplyTimesLessThan
  , MockResolvable (ResolvableParams)
  , shouldApplyToAnything
  , shouldApplyTimesToAnything
  , to
  , onCase
  , cases
  , casesIO
  , label
  , Label
  ) where

import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (get, put)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import GHC.IO (evaluate)
import Prelude hiding (lookup)
import Test.MockCat.Internal.Builder
import Test.MockCat.Internal.Registry
  ( UnitMeta
  , attachVerifierToFn
  , isGuardActive
  , markUnitUsed
  , registerUnitMeta
  , withUnitGuard
  )
import Test.MockCat.Internal.Types
import Test.MockCat.Param
import Test.MockCat.Verify
import Test.MockCat.Cons (Head(..), (:>)(..))
import Type.Reflection (TyCon, splitApps, typeRep, typeRepTyCon)

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
  , Typeable b
  ) => CreateMock b where
  toParams value = Head :> param value

-- | Label type for naming mock functions.
newtype Label = Label MockName

-- | Label function for naming mock functions.
--   Use it with 'mock' to provide a name for the mock function.
--   
--   @
--   f <- mock (label "mockName") $ "a" |> "b"
--   @
label :: MockName -> Label
label = Label

-- | Type class for creating mock functions with optional name.
class CreateMockFn a where
  mockImpl :: a

-- | Type class for creating stub functions with optional name.
class CreateStubFn a where
  stubImpl :: a

-- | Create a mock function with verification hooks attached (unnamed version).
--
-- This function creates a verifiable stub that records argument applications
-- and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
-- The function internally uses 'unsafePerformIO' to make the returned function
-- appear pure, but it requires 'MonadIO' for creation.
--
-- @
-- f <- mock $ "a" |> "b"
-- @
instance
  ( MonadIO m
  , CreateMock p
  , MockBuilder (ToMockParams p) fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  CreateMockFn (p -> m fn)
  where
  mockImpl p = do
    let params = toParams p
    (fn, verifier) <- build Nothing params
    registerStub Nothing verifier fn

-- | Create a named mock function (named version).
--
-- The provided name is used in failure messages.
-- This function creates a verifiable stub that records argument applications
-- and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
-- The function internally uses 'unsafePerformIO' to make the returned function
-- appear pure, but it requires 'MonadIO' for creation.
--
-- @
-- f <- mock (label "mockName") $ "a" |> "b"
-- @
instance {-# OVERLAPPING #-}
  ( MonadIO m
  , CreateMock p
  , MockBuilder (ToMockParams p) fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  CreateMockFn (Label -> p -> m fn)
  where
  mockImpl (Label name) p = do
    let params = toParams p
    (fn, verifier) <- build (Just name) params
    registerStub (Just name) verifier fn

-- | Create a mock function with verification hooks attached.
--
-- This function can be used in two ways:
--
-- 1. Without a name:
--    @
--    f <- mock $ "a" |> "b"
--    @
--
-- 2. With a name (using 'label'):
--    @
--    f <- mock (label "mockName") $ "a" |> "b"
--    @
--
-- The function creates a verifiable stub that records argument applications
-- and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
-- The function internally uses 'unsafePerformIO' to make the returned function
-- appear pure, but it requires 'MonadIO' for creation.
mock :: CreateMockFn a => a
mock = mockImpl

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
  (fn, verifier) <- build (Just name) params
  registerStub (Just name) verifier fn


-- | Create a pure stub function without verification hooks (unnamed version).
--
-- This function creates a simple stub that returns values based on the provided
-- parameters, but does not support verification. Use 'mock' if you need
-- verification capabilities.
--
-- @
-- let f = stub $ "a" |> "b"
-- @
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
-- let f = stub (label "stubName") $ "a" |> "b"
-- @
instance {-# OVERLAPPING #-} StubBuilder params fn => CreateStubFn (Label -> params -> fn) where
  stubImpl (Label name) = buildStub (Just name)

{- | Create a pure stub function without verification hooks.

This function can be used in two ways:

1. Without a name:
   @
   let f = stub $ "a" |> "b"
   @

2. With a name (using 'label'):
   @
   let f = stub (label "stubName") $ "a" |> "b"
   @

This function creates a simple stub that returns values based on the provided
parameters, but does not support verification. Use 'mock' if you need
verification capabilities.
-}
stub :: CreateStubFn a => a
stub = stubImpl

to :: (a -> IO ()) -> a -> IO ()
to f = f

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

registerStub ::
  forall m params fn.
  ( MonadIO m
  , Typeable params
  , Typeable (Verifier params)
  , Typeable fn
  ) =>
  Maybe MockName ->
  Verifier params ->
  fn ->
  m fn
registerStub name verifier@(Verifier ref) fn = do
  baseValue <- liftIO $ evaluate fn
  case eqT :: Maybe (params :~: ()) of
    Just Refl -> do
      meta <- liftIO $ registerUnitMeta ref
      liftIO $ atomically $ writeTVar ref appliedRecord
      let trackedValue = wrapUnitStub ref meta baseValue
      liftIO $
        withUnitGuard meta $ do
          attachVerifierToFn trackedValue (name, verifier)
          attachVerifierToFn baseValue (name, verifier)
      pure trackedValue
    Nothing -> do
      liftIO $ attachVerifierToFn baseValue (name, verifier)
      pure baseValue

ioTyCon :: TyCon
ioTyCon = typeRepTyCon (typeRep @(IO ()))

wrapUnitStub ::
  forall fn.
  Typeable fn =>
  TVar (AppliedRecord ()) ->
  UnitMeta ->
  fn ->
  fn
wrapUnitStub ref meta value =
  let trackedValue = perform $ do
        guardActive <- isGuardActive meta
        if guardActive || isIOType (Proxy :: Proxy fn)
          then pure value
          else do
            markUnitUsed meta
            appendAppliedParams ref ()
            pure value
   in value `seq` trackedValue

isIOType :: forall a. Typeable a => Proxy a -> Bool
isIOType _ =
  case splitApps (typeRep @a) of
    (tc, _) -> tc == ioTyCon