{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.MockCat.Internal.Registry (attachVerifierToFn, lookupVerifierForFn) where

import Data.Dynamic
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Test.MockCat.Internal.Types (MockName, Verifier)
import Unsafe.Coerce (unsafeCoerce)
import Data.IntMap.Strict (IntMap, empty, alter, lookup)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lookup)

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
