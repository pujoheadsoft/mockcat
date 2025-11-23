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

{- | Utilities for constructing verifiable stub functions.

Each stub produced by this module records argument applications and can be
verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
-}
module Test.MockCat.Mock
  ( MockBuilder
  , build
  , MockIOBuilder
  , buildIO
  , createMockFn
  , createNamedMockFn
  , createMockFnIO
  , createStubFn
  , createConstantMockFn
  , createNamedConstantMockFn
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
  , LiftFunTo(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (get, put)
import Data.IORef (IORef, writeIORef)
import Data.Kind (Type)
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
import Type.Reflection (TyCon, splitApps, typeRep, typeRepTyCon)

{- | Create a mock function with verification hooks attached.

This function creates a verifiable stub that records argument applications
and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
The function internally uses 'unsafePerformIO' to make the returned function
appear pure, but it requires 'MonadIO' for creation.
-}
createMockFn ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  params ->
  m fn
createMockFn params = do
  (fn, verifier) <- build Nothing params
  registerStub Nothing verifier fn

{- | Create a named mock function. The provided name is used in failure messages.

This function creates a verifiable stub that records argument applications
and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
The function internally uses 'unsafePerformIO' to make the returned function
appear pure, but it requires 'MonadIO' for creation.
-}
createNamedMockFn ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  MockName ->
  params ->
  m fn
createNamedMockFn name params = do
  (fn, verifier) <- build (Just name) params
  registerStub (Just name) verifier fn

{- | Create a constant mock function with verification hooks attached. -}
createConstantMockFn ::
  ( MonadIO m
  , MockBuilder (Param b) b ()
  , Typeable b
  ) =>
  b ->
  m b
createConstantMockFn value = createMockFn (param value)

{- | Create a named constant mock function. -}
createNamedConstantMockFn ::
  ( MonadIO m
  , MockBuilder (Param b) b ()
  , Typeable b
  ) =>
  MockName ->
  b ->
  m b
createNamedConstantMockFn name value = createNamedMockFn name (param value)

{- | Create a mock function whose result lives in another monad.

This function creates a verifiable stub that records argument applications
and can be verified via helpers such as 'shouldApplyTo' and 'shouldApplyTimes'.
The returned function's result type is in IO or can be lifted to other monads.
-}
createMockFnIO ::
  forall params fn verifyParams m fnM.
  ( MockIOBuilder params fn verifyParams
  , MonadIO m
  , LiftFunTo fn fnM m
  , Typeable verifyParams
  , Typeable fnM
  ) =>
  params ->
  m fnM
createMockFnIO params = do
  (fnIO, verifier) <- liftIO $ buildIO Nothing params
  let lifted = liftFunTo (Proxy :: Proxy m) fnIO
  registerStub Nothing verifier lifted

{- | Create a pure stub function without verification hooks.

This function creates a simple stub that returns values based on the provided
parameters, but does not support verification. Use 'createMockFn' if you need
verification capabilities.
-}
createStubFn ::
  StubBuilder params fn =>
  params ->
  fn
createStubFn = buildStub Nothing

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

class LiftFunTo funIO funM (m :: Type -> Type) | funIO m -> funM where
  liftFunTo :: Proxy m -> funIO -> funM

instance MonadIO m => LiftFunTo (IO r) (m r) m where
  liftFunTo _ = liftIO

instance LiftFunTo restIO restM m => LiftFunTo (a -> restIO) (a -> restM) m where
  liftFunTo p f a = liftFunTo p (f a)

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
      liftIO $ writeIORef ref appliedRecord
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
  IORef (AppliedRecord ()) ->
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