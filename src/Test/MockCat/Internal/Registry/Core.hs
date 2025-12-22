{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.MockCat.Internal.Registry.Core
  ( attachVerifierToFn
  , lookupVerifierForFn
  , attachDynamicVerifierToFn
  , createOverlay
  , installOverlay
  , clearOverlay
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
import Data.Dynamic (Dynamic, toDyn)
import Data.Typeable (Typeable)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)
import Test.MockCat.Internal.Types (MockName, InvocationRecorder(..))
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)

data SomeStableName = forall a. SomeStableName (StableName a)

instance Eq SomeStableName where
  (SomeStableName sn1) == (SomeStableName sn2) = sn1 `eqStableName` sn2

type FnStableName = SomeStableName

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
toFnStable = SomeStableName

sameFnStable :: FnStableName -> FnStableName -> Bool
sameFnStable a b = a == b

type Registry = IntMap [Entry]

registry :: TVar Registry
registry = (unsafePerformIO $ newTVarIO IntMap.empty) :: TVar Registry


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
  -- ONLY use direct StableName matching in the registry.
  -- Name-based resolution via lookupNameByHash here is unsafe because it can
  -- return a verifier from a previous session if the hash collided or was reused.
  case IntMap.lookup key store >>= findMatch stableFn of
    Just res -> pure (Just res)
    Nothing -> pure Nothing

attachDynamicVerifierToFn :: forall fn. fn -> (Maybe MockName, Dynamic) -> IO ()
attachDynamicVerifierToFn fn (name, payload) = do
  -- Record stable-name of the passed function
  stable <- makeStableName fn
  let stableFn = toFnStable stable
  let passedKey = hashStableName stable
  -- Always attach to the passed function stable-name directly.
  -- Avoiding lookupFnByName here prevents cross-session identity pollution.
  let key = passedKey
  let entry = toEntry name stableFn payload
  atomically $
    modifyTVar' registry $ \m -> IntMap.alter (updateEntries entry stableFn) key m

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







type UnitStableName = SomeStableName

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
toUnitStable = SomeStableName

sameUnitStable :: UnitStableName -> UnitStableName -> Bool
sameUnitStable a b = a == b

type UnitRegistry = IntMap [UnitEntry]

unitRegistry :: TVar UnitRegistry
unitRegistry = (unsafePerformIO $ newTVarIO IntMap.empty) :: TVar UnitRegistry

-- | Per-run overlay registry (optional).
data Overlay = Overlay

-- | Run the given IO action with a per-run overlay registry active.
-- The overlay is cleaned up after the action completes.
createOverlay :: IO Overlay
createOverlay = pure Overlay

installOverlay :: Overlay -> IO ()
installOverlay _ = pure ()

clearOverlay :: IO ()
clearOverlay = pure ()



registerUnitMeta :: TVar ref -> IO UnitMeta
registerUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  fresh <- createUnitMeta
  atomically $ do
    store <- readTVar unitRegistry
    case IntMap.lookup key store of
      Just entries ->
        case findUnit unitStable entries of
          Just existing -> pure existing
          Nothing -> do
            let newEntries = UnitEntry unitStable fresh : entries
            writeTVar unitRegistry (IntMap.insert key newEntries store)
            pure fresh
      Nothing -> do
        writeTVar unitRegistry (IntMap.insert key [UnitEntry unitStable fresh] store)
        pure fresh

lookupUnitMeta :: TVar ref -> IO (Maybe UnitMeta)
lookupUnitMeta ref = do
  stable <- makeStableName ref
  let key = hashStableName stable
      unitStable = toUnitStable stable
  store <- readTVarIO unitRegistry
  pure $ IntMap.lookup key store >>= findUnit unitStable

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
    forM_ (concat (IntMap.elems store)) $ \entry ->
      writeTVar (unitGuardRef (unitEntryMeta entry)) flag


