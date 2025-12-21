{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.MockCat.Internal.Registry.Core
  ( attachVerifierToFn
  , lookupVerifierForFn
  , attachDynamicVerifierToFn
  , registerNameAndHash
  , registerNameAndFn
  , registerNameForce
  , lookupFnByName
  , registerNameIfAbsent
  , registerNameWithCreator
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
import Control.Exception (bracket_, catch, SomeException)
import Control.Monad (forM_)
import Data.Dynamic
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)
import Test.MockCat.Internal.Types (MockName, InvocationRecorder(..))
import Unsafe.Coerce (unsafeCoerce)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Applicative ((<|>))
import qualified Type.Reflection
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.MVar (newMVar, takeMVar)
import Control.Exception (bracket)

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
registry = (unsafePerformIO $ newTVarIO IntMap.empty) :: TVar Registry

-- name-based index for named mocks (MockName -> Dynamic payload)
type NameRegistry = Map MockName Dynamic

{-# NOINLINE nameRegistry #-}
nameRegistry = (unsafePerformIO $ newTVarIO Map.empty) :: TVar NameRegistry

-- reverse map from stable hash to MockName for fast fallback lookup
type NameByHash = IntMap MockName

{-# NOINLINE nameByHash #-}
nameByHash = (unsafePerformIO $ newTVarIO IntMap.empty) :: TVar NameByHash

-- map MockName -> stored function dynamic (wrapper), used for resolution by name
-- map MockName -> (map TypeStr -> Dynamic) used for resolution by name
-- Use TypeRep as the inner key instead of a String to reduce fragile string-based type keys.
type NameFnRegistry = Map MockName (Map Type.Reflection.SomeTypeRep Dynamic)

{-# NOINLINE nameFnRegistry #-}
nameFnRegistry = (unsafePerformIO $ newTVarIO Map.empty) :: TVar NameFnRegistry

-- preferred type per name (chosen atomically on first registration)
-- preferred type per name (chosen atomically on first registration)
type NamePreferred = Map MockName Type.Reflection.SomeTypeRep

{-# NOINLINE namePreferred #-}
namePreferred = (unsafePerformIO $ newTVarIO Map.empty) :: TVar NamePreferred

-- central creator registry to reserve a creator or store a created Dynamic
type NameCreatorRegistry = Map MockName (Either Dynamic (MVar Dynamic))

{-# NOINLINE nameCreatorRegistry #-}
nameCreatorRegistry :: TVar NameCreatorRegistry
nameCreatorRegistry = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE creatorCounter #-}
creatorCounter :: IORef Int
creatorCounter = unsafePerformIO $ newIORef 0

-- per-name lock registry to serialize name-based writes
type NameLockRegistry = Map MockName (MVar ())

{-# NOINLINE nameLockRegistry #-}
nameLockRegistry :: IORef NameLockRegistry
nameLockRegistry = unsafePerformIO $ newIORef Map.empty

getOrCreateNameLock :: MockName -> IO (MVar ())
getOrCreateNameLock n = do
  -- Fast path: check existing without allocating
  m <- readIORef nameLockRegistry
  case Map.lookup n m of
    Just l -> pure l
    Nothing -> do
      -- Allocate a new MVar in IO (safe), then try to install it.
      l <- newMVar ()
      installed <- atomicModifyIORef' nameLockRegistry $ \m' ->
        case Map.lookup n m' of
          Just existing -> (m', existing)
          Nothing -> (Map.insert n l m', l)
      pure installed

withNameLock :: MockName -> IO a -> IO a
withNameLock n action = do
  l <- getOrCreateNameLock n
  bracket (takeMVar l) (\_ -> putMVar l ()) (const action)

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

-- | Lookup by mock name (fallback). Returns the stored Dynamic payload, if any.
lookupByName :: MockName -> IO (Maybe Dynamic)
lookupByName n = do
  tNameRegistry <- getNameRegistryIO
  store <- readTVarIO tNameRegistry
  pure $ Map.lookup n store

lookupNameByHash :: Int -> IO (Maybe MockName)
lookupNameByHash h = do
  tNameByHash <- getNameByHashIO
  store <- readTVarIO tNameByHash
  pure $ IntMap.lookup h store

-- | Register a mock name and its Dynamic payload for a given function value.
-- This inserts into both `nameRegistry` (MockName -> Dynamic) and
-- `nameByHash` (stable-hash -> MockName) so that lookups by name or by
-- stable hash can resolve to the same payload.
registerNameAndHash :: forall fn. fn -> MockName -> Dynamic -> IO ()
registerNameAndHash fn name payload = do
  -- serialize per-name writes to avoid races between callers creating different payloads
  tNameRegistry <- getNameRegistryIO
  tNameByHash <- getNameByHashIO
  withNameLock name $ do
    stable <- makeStableName fn
    let key = hashStableName stable
    atomically $ do
      -- Always update the name registry with the latest payload for this name
      -- to ensure session-local mocks are preferred.
      store <- readTVar tNameRegistry
      writeTVar tNameRegistry (Map.insert name payload store)
      hb <- readTVar tNameByHash
      writeTVar tNameByHash (IntMap.insert key name hb)
      -- Also purge any existing NamedEntry for this key in the main registry
      -- to ensure lookupVerifierForFn (which prefers StableName) uses the new name.
      modifyTVar' registry $ \m ->
        IntMap.adjust (filter (not . isNamedEntry)) key m
  where
    isNamedEntry (NamedEntry {}) = True
    isNamedEntry _ = False

-- | Register the actual function value (wrapper) by name for direct resolution.
registerNameAndFn :: forall fn. Typeable fn => fn -> MockName -> IO ()
registerNameAndFn fn name = do
  let dyn = toDyn fn
  let typRep = Type.Reflection.typeRep @fn
  let typKey = Type.Reflection.SomeTypeRep typRep
  -- choose per-run or global registries
  tCreator <- getNameCreatorRegistryIO
  tNameFn <- getNameFnRegistryIO
  tPref <- getNamePreferredIO
  -- If a creator reservation exists for this name, prefer the creator's
  -- produced Dynamic to avoid races where callers register their own wrappers
  -- before the canonical creator finishes.
  mcre <- readTVarIO tCreator >>= \m -> pure (Map.lookup name m)
  chosenDyn <- case mcre of
    Just (Left existingDyn) -> pure existingDyn
    Just (Right mv) -> readMVar mv
    Nothing -> pure dyn
  -- ensure mutations are serialized per-name to avoid interleaving of writes
  withNameLock name $ do
    atomically $ do
      store <- readTVar tNameFn
      let inner = Map.findWithDefault Map.empty name store
      -- Always update the naming registry for the given type key with the chosen dyn
      -- so session-local definition updates are reflected.
      let newInner = Map.insert typKey chosenDyn inner
      writeTVar tNameFn (Map.insert name newInner store)
      pref <- readTVar tPref
      case Map.lookup name pref of
        Just _ -> pure ()
        Nothing -> writeTVar tPref (Map.insert name typKey pref)

-- | Lookup the stored function (wrapper) by name, if any.
lookupFnByName :: MockName -> IO (Maybe Dynamic)
lookupFnByName n = do
  -- Serialize reads with the per-name lock to avoid observing partially updated
  -- name mappings during concurrent register/force flows.
  tNameFn <- getNameFnRegistryIO
  tPref <- getNamePreferredIO
  withNameLock n $ do
    store <- readTVarIO tNameFn
    prefStore <- readTVarIO tPref
    let inner = Map.lookup n store
    case inner of
      Nothing -> pure Nothing
      Just m -> do
        let chosen = case Map.lookup n prefStore of
              Just prefType -> Map.lookup prefType m <|> (snd <$> Map.lookupMin m)
              Nothing -> (snd <$> Map.lookupMin m)
        pure chosen

-- | Atomically register the given Dynamic for the name if absent.
-- Returns the stored Dynamic (either the existing one or the newly inserted one).
registerNameIfAbsent :: MockName -> Dynamic -> IO Dynamic
registerNameIfAbsent n dyn = do
  -- serialize name-based writes to avoid races with force/register flows
  tNameFn <- getNameFnRegistryIO
  tPref <- getNamePreferredIO
  withNameLock n $ do
    atomically $ do
      store <- readTVar tNameFn
      pref <- readTVar tPref
      let typRep = dynTypeRep dyn
      let typKey = typRep
      -- Always prefer the newly passed dyn to allow session-local overwrites
      let inner = Map.findWithDefault Map.empty n store
      let newInner = Map.insert typKey dyn inner
      writeTVar tNameFn (Map.insert n newInner store)
      writeTVar tPref (Map.insert n typKey pref)
      pure dyn

-- | Force-set the Dynamic payload for a name+type. This overwrites any
-- existing mapping for the given type and sets the preferred type to this one.
registerNameForce :: MockName -> Dynamic -> IO ()
registerNameForce n dyn = do
  let typRep = dynTypeRep dyn
  let typKey = typRep
  -- choose overlay/global
  tCreator <- getNameCreatorRegistryIO
  tNameFn <- getNameFnRegistryIO
  tPref <- getNamePreferredIO
  -- serialize force-sets with name lock to avoid racing with other registration flows
  withNameLock n $ do
    -- If a creator exists for this name, do not allow external force to overwrite.
    mcre <- readTVarIO tCreator
    case Map.lookup n mcre of
      -- If a creator exists (in-progress or produced), skip external force.
      Just _ -> pure ()
      _ -> do
        atomically $ do
          store <- readTVar tNameFn
          let inner = Map.findWithDefault Map.empty n store
          let newInner = Map.insert typKey dyn inner
          writeTVar tNameFn (Map.insert n newInner store)
          pref <- readTVar tPref
          writeTVar tPref (Map.insert n typKey pref)


-- | Atomically reserve a creator and run the provided creation action if this
-- caller becomes the creator. Returns the stored Dynamic (existing or newly created).
registerNameWithCreator :: MockName -> (Int -> IO Dynamic) -> IO Dynamic
registerNameWithCreator n createAction = do
  tCreator <- getNameCreatorRegistryIO
  tNameFn <- getNameFnRegistryIO
  tPref <- getNamePreferredIO
  -- serialize per-name writes to avoid races between callers creating different payloads
  withNameLock n $ do
    -- allocate an MVar safely and try to install it; if another thread
    -- installed in the meantime, reuse the existing one.
    mv <- newEmptyMVar
    installed <- atomically $ do
      m' <- readTVar tCreator
      case Map.lookup n m' of
        Just existing -> pure (Left existing)
        Nothing -> do
          let newMap = Map.insert n (Right mv) m'
          writeTVar tCreator newMap
          pure (Right ())
    case installed of
      Left existing -> case existing of
        Left dyn -> pure dyn
        Right mvOld -> readMVar mvOld
      Right () -> do
        -- allocate a creation id
        cid <- atomicModifyIORef' creatorCounter $ \c -> (c + 1, c + 1)
        -- this caller is responsible for creating the dynamic; pass creator id
        -- Creation is executed while holding the name lock to make wrapper
        -- allocation and registration fully serialized per-name.
        dyn <- createAction cid
        -- store as Left dyn
        atomically $ modifyTVar' tCreator (Map.insert n (Left dyn))
        -- if an MVar was used, try to put (some callers expect it)
        putMVar mv dyn `catch` ((\_ -> pure ()) :: SomeException -> IO ())
        -- Make creator-produced dynamic the preferred entry immediately to
        -- avoid races where later registrations override the creator's wrapper.
        let typRepCreated = dynTypeRep dyn
        let typKeyCreated = typRepCreated
        atomically $ do
          store2 <- readTVar tNameFn
          let inner2 = Map.findWithDefault Map.empty n store2
          let newInner2 = Map.insert typKeyCreated dyn inner2
          writeTVar tNameFn (Map.insert n newInner2 store2)
          pref2 <- readTVar tPref
          writeTVar tPref (Map.insert n typKeyCreated pref2)
        pure dyn

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
unitRegistry = (unsafePerformIO $ newTVarIO IntMap.empty) :: TVar UnitRegistry

-- | Per-run overlay registry (optional). When set, registry operations
-- will use the overlay TVars instead of the global ones. This enables
-- per-run isolation without changing user-facing APIs.
data Overlay = Overlay
  { overlayNameRegistry :: TVar NameRegistry
  , overlayNameFnRegistry :: TVar NameFnRegistry
  , overlayNamePreferred :: TVar NamePreferred
  , overlayNameCreatorRegistry :: TVar NameCreatorRegistry
  }

{-# NOINLINE activeOverlay #-}
activeOverlay :: IORef (Maybe Overlay)
activeOverlay = unsafePerformIO $ newIORef Nothing

getNameFnRegistryIO :: IO (TVar NameFnRegistry)
getNameFnRegistryIO = do
  mo <- readIORef activeOverlay
  case mo of
    Just o -> pure (overlayNameFnRegistry o)
    Nothing -> pure nameFnRegistry

getNamePreferredIO :: IO (TVar NamePreferred)
getNamePreferredIO = do
  mo <- readIORef activeOverlay
  case mo of
    Just o -> pure (overlayNamePreferred o)
    Nothing -> pure namePreferred

getNameCreatorRegistryIO :: IO (TVar NameCreatorRegistry)
getNameCreatorRegistryIO = do
  mo <- readIORef activeOverlay
  case mo of
    Just o -> pure (overlayNameCreatorRegistry o)
    Nothing -> pure nameCreatorRegistry

getNameRegistryIO :: IO (TVar NameRegistry)
getNameRegistryIO = do
  mo <- readIORef activeOverlay
  case mo of
    Just o -> pure (overlayNameRegistry o)
    Nothing -> pure nameRegistry

getNameByHashIO :: IO (TVar NameByHash)
getNameByHashIO = pure nameByHash

-- | Run the given IO action with a per-run overlay registry active.
-- The overlay is cleaned up after the action completes.
createOverlay :: IO Overlay
createOverlay = Overlay <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty

installOverlay :: Overlay -> IO ()
installOverlay o = writeIORef activeOverlay (Just o)

clearOverlay :: IO ()
clearOverlay = writeIORef activeOverlay Nothing

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


