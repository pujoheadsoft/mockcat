{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Test.MockCat.Internal.MockRegistry
  ( attachVerifierToFn
  , lookupVerifierForFn
  , register
  , registerUnitMeta
  , lookupUnitMeta
  , UnitMeta
  , withUnitGuard
  , withAllUnitGuards
  , markUnitUsed
  , isGuardActive
  , getLastRecorder
  , resetMockHistory
  ) where

import Test.MockCat.Internal.Registry.Core
  ( attachVerifierToFn
  , lookupVerifierForFn
  , registerUnitMeta
  , lookupUnitMeta
  , UnitMeta
  , withUnitGuard
  , withAllUnitGuards
  , markUnitUsed
  , isGuardActive
  , getLastRecorder
  , resetMockHistory
  )
import GHC.IO (evaluate)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Test.MockCat.Internal.Types (MockName, InvocationRecorder(..), InvocationRecord, perform)
import Data.Proxy (Proxy(..))
import Data.Dynamic
import Test.MockCat.Internal.Builder (invocationRecord, appendCalledParams)
import Type.Reflection (TyCon, splitApps, typeRep, typeRepTyCon)
import Data.Typeable (eqT)
import Data.Type.Equality ((:~:) (Refl))

ioTyCon :: TyCon
ioTyCon = typeRepTyCon (typeRep @(IO ()))

isIOType :: forall a. Typeable a => Proxy a -> Bool
isIOType _ =
  case splitApps (typeRep @a) of
    (tc, _) -> tc == ioTyCon

-- | Wrap a function value for unit-typed stubs so that calls are tracked.
-- This uses the UnitMeta guard to avoid double-counting when both the tracked
-- and base values are registered. The wrapper will mark the unit meta used and
-- append an invocation to the recorder's TVar when appropriate.
wrapUnitStub ::
  forall fn.
  Typeable fn =>
  TVar (InvocationRecord ()) ->
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
            appendCalledParams ref ()
            pure value
  in
    trackedValue


-- | Register a recorder for a function in the global mock registry.
-- This handles the special '()' (unit) case by creating a tracked wrapper
-- and registering both the tracked and base values so StableName lookup
-- succeeds regardless of which closure is later passed for verification.
register ::
  forall fn params.
  ( Typeable params
  , Typeable (InvocationRecorder params)
  , Typeable fn
  ) =>
  Maybe MockName ->
  InvocationRecorder params ->
  fn ->
  IO fn
register name recorder@(InvocationRecorder {invocationRef = ref}) fn = do
  baseValue <- evaluate fn
  case eqT :: Maybe (params :~: ()) of
    Just Refl -> do
      meta <- registerUnitMeta ref
      atomically $ writeTVar ref invocationRecord
      let trackedValue = wrapUnitStub ref meta baseValue
      withUnitGuard meta $ do
        attachVerifierToFn trackedValue (name, recorder)
        attachVerifierToFn baseValue (name, recorder)
      pure trackedValue
    Nothing -> do
      attachVerifierToFn baseValue (name, recorder)
      pure baseValue

