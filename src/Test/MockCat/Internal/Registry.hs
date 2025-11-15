{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.MockCat.Internal.Registry
  ( attachVerifierToFn
  , lookupVerifierForFn
  , registerUnitMeta
  , lookupUnitMeta
  , UnitMeta
  , withUnitGuard
  , withAllUnitGuards
  , markUnitUsed
  , isGuardActive
  ) where

import Data.Dynamic
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Test.MockCat.Internal.Types (MockName, Verifier)
import Unsafe.Coerce (unsafeCoerce)
import Data.IntMap.Strict (IntMap, alter, empty, insert, lookup, elems)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lookup)
import Control.Exception (bracket_)
import Control.Monad (forM_)

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
registry :: IORef Registry
registry = unsafePerformIO $ newIORef empty

attachVerifierToFn ::
  forall fn params.
  (Typeable (Verifier params)) =>
  fn ->
  (Maybe MockName, Verifier params) ->
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
  store <- readIORef registry
  pure $ lookup key store >>= findMatch stableFn

attachDynamicVerifierToFn :: forall fn. fn -> (Maybe MockName, Dynamic) -> IO ()
attachDynamicVerifierToFn fn (name, payload) = do
  stable <- makeStableName fn
  let
    stableFn = toFnStable stable
    key = hashStableName stable
    entry = toEntry name stableFn payload
  atomicModifyIORef' registry $ \m -> (alter (updateEntries entry stableFn) key m, ())

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
  { unitGuardRef :: IORef Bool
  , unitUsedRef :: IORef Bool
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
unitRegistry :: IORef UnitRegistry
unitRegistry = unsafePerformIO $ newIORef empty

registerUnitMeta :: IORef ref -> IO UnitMeta
registerUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  meta <- createUnitMeta
  atomicModifyIORef' unitRegistry $ \store ->
    case lookup key store of
      Just entries ->
        case findUnit unitStable entries of
          Just existing -> (store, existing)
          Nothing ->
            let newEntries = UnitEntry unitStable meta : entries
             in (insert key newEntries store, meta)
      Nothing ->
        (insert key [UnitEntry unitStable meta] store, meta)

lookupUnitMeta :: IORef ref -> IO (Maybe UnitMeta)
lookupUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  store <- readIORef unitRegistry
  pure $ lookup key store >>= findUnit unitStable

withUnitGuard :: UnitMeta -> IO a -> IO a
withUnitGuard meta = bracket_ (writeIORef (unitGuardRef meta) True) (writeIORef (unitGuardRef meta) False)

withAllUnitGuards :: IO a -> IO a
withAllUnitGuards = bracket_ (setAllUnitGuards True) (setAllUnitGuards False)

markUnitUsed :: UnitMeta -> IO ()
markUnitUsed meta = writeIORef (unitUsedRef meta) True

isGuardActive :: UnitMeta -> IO Bool
isGuardActive meta = readIORef (unitGuardRef meta)

createUnitMeta :: IO UnitMeta
createUnitMeta = do
  guardRef <- newIORef False
  usedRef <- newIORef False
  pure UnitMeta {unitGuardRef = guardRef, unitUsedRef = usedRef}

findUnit :: UnitStableName -> [UnitEntry] -> Maybe UnitMeta
findUnit _ [] = Nothing
findUnit target (entry : rest)
  | sameUnitStable target (unitEntryStable entry) = Just (unitEntryMeta entry)
  | otherwise = findUnit target rest

setAllUnitGuards :: Bool -> IO ()
setAllUnitGuards flag = do
  store <- readIORef unitRegistry
  forM_ (concat (elems store)) $ \entry ->
    writeIORef (unitGuardRef (unitEntryMeta entry)) flag
