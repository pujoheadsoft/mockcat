{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Test.MockCat.Internal.Registry.Core
  ( attachVerifierToFn
  , lookupVerifierForFn
  , attachDynamicVerifierToFn
  , registerUnitMeta
  , lookupUnitMeta
  , UnitMeta
  , withUnitGuard
  , withAllUnitGuards
  , markUnitUsed
  , isGuardActive
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Exception (bracket_)
import Control.Monad (forM_)
import Data.Dynamic
import Data.Typeable (eqT)
import Data.Type.Equality ((:~:) (Refl))
import Data.Proxy (Proxy(..))
import Data.IntMap.Strict (IntMap, alter, empty, insert, lookup, elems)
import System.IO.Unsafe (unsafePerformIO)
import Test.MockCat.Internal.Types (MockName, InvocationRecorder(..), InvocationRecord, perform)
import Unsafe.Coerce (unsafeCoerce)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Prelude hiding (lookup)
import GHC.IO (evaluate)
import Test.MockCat.Internal.Builder (invocationRecord, appendAppliedParams)
import Type.Reflection (TyCon, splitApps, typeRep, typeRepTyCon)

data FnTag

type FnStableName = StableName FnTag

data Entry
  = Entry !FnStableName !Dynamic
  | NamedEntry !FnStableName !MockName !Dynamic

stableFnName :: Entry -> FnStableName
stableFnName (Entry fn _) = fn
stableFnName (NamedEntry fn _ _) = fn

mockName :: Entry -> Maybe MockName
mockName (NamedEntry _ name _) = Just name
mockName _ = Nothing

entryPayload :: Entry -> Dynamic
entryPayload (Entry _ payload) = payload
entryPayload (NamedEntry _ _ payload) = payload

toFnStable :: forall a. StableName a -> FnStableName
toFnStable = unsafeCoerce

fromFnStable :: forall a.FnStableName -> StableName a
fromFnStable = unsafeCoerce

sameFnStable :: FnStableName -> FnStableName -> Bool
sameFnStable a b = eqStableName (fromFnStable a) (fromFnStable b)

type Registry = IntMap [Entry]

{-# NOINLINE registry #-}
registry :: TVar Registry
registry = unsafePerformIO $ newTVarIO empty

attachVerifierToFn ::
  forall fn params.
  (Typeable (InvocationRecorder params)) =>
  fn ->
  (Maybe MockName, InvocationRecorder params) ->
  IO ()
attachVerifierToFn fn (name, payload) = attachDynamicVerifierToFn fn (name, toDyn payload)

lookupVerifierForFn ::
  forall fn.
  fn ->
  IO (Maybe (Maybe MockName, Dynamic))
lookupVerifierForFn fn = do
  stable <- makeStableName fn
  let
    key = hashStableName stable
    stableFn = toFnStable stable
  store <- readTVarIO registry
  pure $ lookup key store >>= findMatch stableFn

attachDynamicVerifierToFn :: forall fn. fn -> (Maybe MockName, Dynamic) -> IO ()
attachDynamicVerifierToFn fn (name, payload) = do
  stable <- makeStableName fn
  let
    stableFn = toFnStable stable
    key = hashStableName stable
    entry = toEntry name stableFn payload
  atomically $
    modifyTVar' registry $ \m -> alter (updateEntries entry stableFn) key m

toEntry :: Maybe MockName -> FnStableName -> Dynamic -> Entry
toEntry (Just n) stableFn p = NamedEntry stableFn n p
toEntry Nothing stableFn p = Entry stableFn p

updateEntries :: Entry -> FnStableName -> Maybe [Entry] -> Maybe [Entry]
updateEntries entry stableFn (Just entries) = Just $ entry : filterSameFnStable stableFn entries
updateEntries entry _        Nothing        = Just [entry]

filterSameFnStable :: FnStableName -> [Entry] -> [Entry]
filterSameFnStable stableFn = filter (not . sameFnStable stableFn . stableFnName)

findMatch :: FnStableName -> [Entry] -> Maybe (Maybe MockName, Dynamic)
findMatch _ [] = Nothing
findMatch target  (entry : rest)
  | sameFnStable target (stableFnName entry) = Just (mockName entry, entryPayload entry)
  | otherwise = findMatch target rest

data UnitTag

type UnitStableName = StableName UnitTag

data UnitMeta = UnitMeta
  { unitGuardRef :: TVar Bool
  , unitUsedRef :: TVar Bool
  }

data UnitEntry = UnitEntry !UnitStableName !UnitMeta

unitEntryStable :: UnitEntry -> UnitStableName
unitEntryStable (UnitEntry stable _) = stable

unitEntryMeta :: UnitEntry -> UnitMeta
unitEntryMeta (UnitEntry _ meta) = meta

toUnitStable :: forall a. StableName a -> UnitStableName
toUnitStable = unsafeCoerce

fromUnitStable :: forall a. UnitStableName -> StableName a
fromUnitStable = unsafeCoerce

sameUnitStable :: UnitStableName -> UnitStableName -> Bool
sameUnitStable a b = eqStableName (fromUnitStable a) (fromUnitStable b)

type UnitRegistry = IntMap [UnitEntry]

{-# NOINLINE unitRegistry #-}
unitRegistry :: TVar UnitRegistry
unitRegistry = unsafePerformIO $ newTVarIO empty

registerUnitMeta :: TVar ref -> IO UnitMeta
registerUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  fresh <- createUnitMeta
  atomically $ do
    store <- readTVar unitRegistry
    case lookup key store of
      Just entries ->
        case findUnit unitStable entries of
          Just existing -> pure existing
          Nothing -> do
            let newEntries = UnitEntry unitStable fresh : entries
            writeTVar unitRegistry (insert key newEntries store)
            pure fresh
      Nothing -> do
        writeTVar unitRegistry (insert key [UnitEntry unitStable fresh] store)
        pure fresh

lookupUnitMeta :: TVar ref -> IO (Maybe UnitMeta)
lookupUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  store <- readTVarIO unitRegistry
  pure $ lookup key store >>= findUnit unitStable

withUnitGuard :: UnitMeta -> IO a -> IO a
withUnitGuard meta =
  bracket_
    (atomically $ writeTVar (unitGuardRef meta) True)
    (atomically $ writeTVar (unitGuardRef meta) False)

withAllUnitGuards :: IO a -> IO a
withAllUnitGuards = bracket_ (setAllUnitGuards True) (setAllUnitGuards False)

markUnitUsed :: UnitMeta -> IO ()
markUnitUsed meta = atomically $ writeTVar (unitUsedRef meta) True

isGuardActive :: UnitMeta -> IO Bool
isGuardActive meta = readTVarIO (unitGuardRef meta)

createUnitMeta :: IO UnitMeta
createUnitMeta = do
  guardRef <- newTVarIO False
  usedRef <- newTVarIO False
  pure UnitMeta {unitGuardRef = guardRef, unitUsedRef = usedRef}

findUnit :: UnitStableName -> [UnitEntry] -> Maybe UnitMeta
findUnit _ [] = Nothing
findUnit target (entry : rest)
  | sameUnitStable target (unitEntryStable entry) = Just (unitEntryMeta entry)
  | otherwise = findUnit target rest

setAllUnitGuards :: Bool -> IO ()
setAllUnitGuards flag =
  atomically $ do
    store <- readTVar unitRegistry
    forM_ (concat (elems store)) $ \entry ->
      writeTVar (unitGuardRef (unitEntryMeta entry)) flag


