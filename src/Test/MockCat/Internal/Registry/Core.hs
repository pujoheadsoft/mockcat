{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}


module Test.MockCat.Internal.Registry.Core
  ( attachVerifierToFn
  , lookupVerifierForFn
  , attachDynamicVerifierToFn
  , registerUnitMeta
  , lookupUnitMeta
  , UnitMeta
  , withUnitGuard
  , withAllUnitGuards
  , isGuardActive
  , getLastRecorder
  , getLastRecorderRaw
  , resetMockHistory
  , getThreadWithMockContext
  , setThreadWithMockContext
  , clearThreadWithMockContext
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
import Control.Concurrent (ThreadId, myThreadId)
import Data.Dynamic (Dynamic(..), toDyn, fromDynamic)
import Data.Typeable (Typeable)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import Test.MockCat.Internal.Types (MockName, InvocationRecorder(..), WithMockContext(..))
import Test.MockCat.Internal.GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)

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

-- | Thread-local storage for mock history in the current thread.
--   Used for:
--   1. 'expects' to retrieve the recorder without StableName lookup.
--   2. 'lookupVerifierForFn' fallback when StableName lookup fails (HPC workaround).
threadMockHistory :: TVar (Map.Map ThreadId [(Maybe MockName, Dynamic)])
threadMockHistory = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE threadMockHistory #-}

-- | Add a recorder to the current thread's history.
addToHistory :: Maybe MockName -> Dynamic -> IO ()
addToHistory name dyn = do
  tid <- myThreadId
  atomically $ modifyTVar' threadMockHistory $ \m ->
    Map.insertWith (++) tid [(name, dyn)] m

-- | Get the last registered recorder (peek only, does not remove).
-- | Get the last registered recorder (peek only, does not remove).
getLastRecorder :: Typeable a => IO (Maybe MockName, Maybe a)
getLastRecorder = do
  tid <- myThreadId
  atomically $ do
    store <- readTVar threadMockHistory
    case Map.lookup tid store of
      Nothing -> pure (Nothing, Nothing)
      Just [] -> pure (Nothing, Nothing)
      Just ((name, dyn) : _) -> pure (name, fromDynamic dyn)

-- | Get the last registered recorder as raw Any (unwrapped from Dynamic).
getLastRecorderRaw :: IO (Maybe MockName, Maybe Any)
getLastRecorderRaw = do
  tid <- myThreadId
  atomically $ do
    store <- readTVar threadMockHistory
    case Map.lookup tid store of
      Nothing -> pure (Nothing, Nothing)
      Just [] -> pure (Nothing, Nothing)
      Just ((name, Dynamic _ v) : _) -> pure (name, Just (unsafeCoerce v))

-- | Reset the mock history for the current thread.
resetMockHistory :: IO ()
resetMockHistory = do
  tid <- myThreadId
  atomically $ do
    modifyTVar' threadMockHistory (Map.delete tid)
    modifyTVar' threadWithMockStore (Map.delete tid)

-- | Thread-local storage for WithMockContext
threadWithMockStore :: TVar (Map.Map ThreadId WithMockContext)
threadWithMockStore = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE threadWithMockStore #-}

-- | Get the WithMockContext for the current thread.
getThreadWithMockContext :: IO (Maybe WithMockContext)
getThreadWithMockContext = do
  tid <- myThreadId
  atomically $ Map.lookup tid <$> readTVar threadWithMockStore

-- | Set the WithMockContext for the current thread.
setThreadWithMockContext :: WithMockContext -> IO ()
setThreadWithMockContext ctx = do
  tid <- myThreadId
  atomically $ modifyTVar' threadWithMockStore (Map.insert tid ctx)

-- | Clear the WithMockContext for the current thread.
clearThreadWithMockContext :: IO ()
clearThreadWithMockContext = do
  tid <- myThreadId
  atomically $ modifyTVar' threadWithMockStore (Map.delete tid)



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
  IO [(Maybe MockName, Dynamic)]
lookupVerifierForFn fn = do
  stable <- makeStableName fn
  let key = hashStableName stable
  let stableFn = toFnStable stable
  
  -- 1. Try StableName lookup
  mbMatch <- atomically $ do
    m <- readTVar registry
    case IntMap.lookup key m of
      Nothing -> pure Nothing
      Just entries -> pure $ findMatch stableFn entries
      
  case mbMatch of
    Just match -> pure [match]
    Nothing -> do
      -- 2. Fallback: return thread's mock history
      --    This handles case where StableName is unstable (e.g. HPC enabled)
      tid <- myThreadId
      atomically $ do
        hist <- readTVar threadMockHistory
        case Map.lookup tid hist of
           Nothing -> pure []
           Just list -> pure list

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
  -- Save for expects and fallback lookup
  addToHistory name payload

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

isGuardActive :: UnitMeta -> IO Bool
isGuardActive (UnitMeta guardRef) = readTVarIO guardRef

createUnitMeta :: IO UnitMeta
createUnitMeta = do
  guardRef <- newTVarIO False
  pure $ UnitMeta guardRef

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


