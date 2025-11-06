{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.Internal.Builder where

import Control.Monad (guard, when, ap)
import Data.Function ((&))

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (elemIndex, intercalate)
import Data.Maybe

import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.AssociationList (AssociationList, lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans
import Control.Monad.State
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Test.MockCat.Internal.Types
import Test.MockCat.Internal.Core
import Test.MockCat.Internal.Message


-- | Class for building a curried function.
-- The purpose of this class is to automatically generate and provide
-- an implementation for the corresponding curried function type (such as `a -> b -> ... -> IO r`)
-- when given the argument list type of the mock (`Param a :> Param b :> ...`).
-- @args@ is the argument list type of the mock.
-- @r@ is the return type of the function.
-- @fn@ is the curried function type.
class BuildCurried args r fn | args r -> fn where
  -- | Build a curried function.
  -- Accept a function that combines all arguments and convert it into a curried function.
  buildCurried :: (args -> IO r) -> fn
  buildCurriedPure :: (args -> r) -> fn

-- | Base case: The last parameter.
-- Converts a single-argument function (Param a -> IO r) into a final
-- curried function (a -> r) by performing the IO action.
instance BuildCurried (Param a) r (a -> r) where
  buildCurried :: (Param a -> IO r) -> a -> r
  buildCurried a2r a = perform $ a2r (param a)

  buildCurriedPure :: (Param a -> r) -> a -> r
  buildCurriedPure a2r a = a2r (param a)

-- | Recursive case: Consumes the head parameter and generates the next curried function.
-- Generates a function of type (a -> fn) that immediately calls the next
-- 'BuildCurried' instance with the remaining arguments.
instance BuildCurried rest r fn
      => BuildCurried (Param a :> rest) r (a -> fn) where
  buildCurried :: ((Param a :> rest) -> IO r) -> a -> fn
  buildCurried args2r a = buildCurried (\rest -> args2r (p a :> rest))

  buildCurriedPure :: ((Param a :> rest) -> r) -> a -> fn
  buildCurriedPure args2r a = buildCurriedPure (\rest -> args2r (p a :> rest))

-- | Like 'BuildCurried' but returns an IO result at the end of the curried function.
-- This is used by the MockIO builder to produce functions whose final result is in IO,
-- allowing them to be lifted into other monads via 'LiftFunTo'.
class BuildCurriedIO args r fn | args r -> fn where
  buildCurriedIO :: (args -> IO r) -> fn

instance BuildCurriedIO (Param a) r (a -> IO r) where
  buildCurriedIO a2r a = a2r (param a)

instance BuildCurriedIO rest r fn
      => BuildCurriedIO (Param a :> rest) r (a -> fn) where
  buildCurriedIO :: ((Param a :> rest) -> IO r) -> a -> fn
  buildCurriedIO args2r a = buildCurriedIO (\rest -> args2r (p a :> rest))



-- | Class for creating a mock corresponding to the parameter.
class MockBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  -- build a mock
  build :: MonadIO m => Maybe MockName -> params -> m (Mock fn verifyParams)

-- | Instance for building a mock for a constant function.
instance
  MockBuilder (IO r) (IO r) ()
  where
  build name a = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (do
      liftIO $ appendAppliedParams s ()
      a)

-- | Instance for building a mock for a function with a single parameter.
instance
  MockBuilder (Param r) r ()
  where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    let v = value params
    makeMock name s $ perform (do
      liftIO $ appendAppliedParams s ()
      pure v)

-- | Instance for building a mock for a function with multiple parameters.
instance MockBuilder (Cases (IO a) ()) (IO a) () where
  build name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (do
      count <- readAppliedCount s ()
      let index = min count (length params - 1)
          r = safeIndex params index
      appendAppliedParams s ()
      incrementAppliedParamCount s ()
      fromJust r)

-- | Overlapping instance for building a mock for a function with multiple parameters.
-- This instance is used when the parameter type is a 'Cases' type.
instance {-# OVERLAPPABLE #-}
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , BuildCurried args r fn
  , Eq args
  , Show args
  ) => MockBuilder (Cases params ()) fn args where
  build name cases = do
    let paramsList = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (buildCurried (\inputParams -> findReturnValueWithStore name paramsList inputParams s))

-- | Overlapping instance for building a mock for a function with multiple parameters.
-- This instance is used when the parameter type is a 'Param a :> rest' type.
instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ProjectionArgs p
  , ProjectionReturn p
  , ArgsOf p ~ args
  , ReturnOf p ~ Param r
  , BuildCurried args r fn
  , Eq args
  , Show args
  ) => MockBuilder (Param a :> rest) fn args where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (buildCurried (\inputParams -> extractReturnValueWithValidate name params inputParams s))




class MockIOBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  -- build a mock
  buildIO :: MonadIO m => Maybe MockName -> params -> m (MockIO m fn verifyParams)

instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ProjectionArgs p
  , ProjectionReturn p
  , ArgsOf p ~ args
  , ReturnOf p ~ Param r
  , BuildCurriedIO args r fn
  , Eq args
  , Show args
  ) => MockIOBuilder (Param a :> rest) fn args where
  buildIO name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMockIO name s (buildCurriedIO (\inputParams -> extractReturnValueWithValidate name params inputParams s))





makeMock :: MonadIO m => Maybe MockName -> IORef (AppliedRecord params) -> fn -> m (Mock fn params)
makeMock (Just name) l fn = pure $ NamedMock name fn (Verifier l)
makeMock Nothing l fn = pure $ Mock fn (Verifier l)


makeMockIO :: MonadIO m => Maybe MockName -> IORef (AppliedRecord params) -> fn -> m (MockIO m fn params)
makeMockIO Nothing l fn = pure $ MockIO fn (Verifier l)
makeMockIO (Just name) l fn = pure $ NamedMockIO name fn (Verifier l)




appliedRecord :: AppliedRecord params
appliedRecord = AppliedRecord {
  appliedParamsList = mempty,
  appliedParamsCounter = empty
}

appendAppliedParams :: IORef (AppliedRecord params) -> params -> IO ()
appendAppliedParams ref inputParams = do
  atomicModifyIORef' ref (\AppliedRecord {appliedParamsList, appliedParamsCounter} ->
    let newRecord = AppliedRecord {
          appliedParamsList = appliedParamsList ++ [inputParams],
          appliedParamsCounter = appliedParamsCounter
        }
    in (newRecord, ()))

readAppliedCount :: Eq params => IORef (AppliedRecord params) -> params -> IO Int
readAppliedCount ref params = do
  record <- readIORef ref
  let count = appliedParamsCounter record
  pure $ fromMaybe 0 (lookup params count)

incrementAppliedParamCount :: Eq params => IORef (AppliedRecord params) -> params -> IO ()
incrementAppliedParamCount ref inputParams = do
  atomicModifyIORef' ref (\AppliedRecord {appliedParamsList, appliedParamsCounter} ->
    let newRecord = AppliedRecord {
          appliedParamsList = appliedParamsList,
          appliedParamsCounter = incrementCount inputParams appliedParamsCounter
        }
    in (newRecord, ()))

incrementCount :: Eq k => k -> AppliedParamsCounter k -> AppliedParamsCounter k
incrementCount key list =
  if member key list then update (+ 1) key list
  else insert key 1 list



findReturnValueWithStore ::
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , Eq args
  , Show args
  ) =>
  Maybe MockName ->
  AppliedParamsList params ->
  args ->
  IORef (AppliedRecord args) ->
  IO r
findReturnValueWithStore name paramsList inputParams ref = do
  appendAppliedParams ref inputParams
  let expectedArgs = projArgs <$>paramsList
  r <- findReturnValue paramsList inputParams ref
  maybe
    (errorWithoutStackTrace $ messageForMultiMock name expectedArgs inputParams)
    pure
    r

findReturnValue ::
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , Eq args
  ) =>
  AppliedParamsList params ->
  args ->
  IORef (AppliedRecord args) ->
  IO (Maybe r)
findReturnValue paramsList inputParams ref = do
  let matchedParams = filter (\params -> projArgs params == inputParams) paramsList
  case matchedParams of
    [] -> pure Nothing
    _ -> do
      count <- readAppliedCount ref inputParams
      let index = min count (length matchedParams - 1)
      incrementAppliedParamCount ref inputParams
      pure $ returnValue <$> safeIndex matchedParams index




extractReturnValueWithValidate ::
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , Eq args
  , Show args
  ) =>
  Maybe MockName ->
  params ->
  args ->
  IORef (AppliedRecord args) ->
  IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (projArgs params) inputParams
  pure $ returnValue params

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (AppliedRecord a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  appendAppliedParams ref actual

validateParams :: (Eq a, Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual
    then pure ()
    else errorWithoutStackTrace $ message name expected actual





runCase :: Cases a b -> [a]
runCase (Cases s) = execState s []

p :: a -> Param a
p = param



class StubBuilder params fn | params -> fn where
  buildStub :: Maybe MockName -> params -> fn

-- | Instance for building a mock for a constant function.
instance
  StubBuilder (IO r) (IO r)
  where
  buildStub name a = a

-- | Instance for building a mock for a function with a single parameter.
instance
  StubBuilder (Param r) r
  where
  buildStub name params = do
    let v = value params
    v

-- | Instance for building a mock for a function with multiple parameters.
instance StubBuilder (Cases (IO a) ()) (IO a) where
  buildStub name cases = do
    let params = runCase cases
    s <- liftIO $ newIORef appliedRecord
    (do
      count <- readAppliedCount s ()
      let index = min count (length params - 1)
          r = safeIndex params index
      appendAppliedParams s ()
      incrementAppliedParamCount s ()
      fromJust r)

-- | Overlapping instance for building a mock for a function with multiple parameters.
-- This instance is used when the parameter type is a 'Cases' type.
-- instance {-# OVERLAPPABLE #-}
--   ( ProjectionArgs params
--   , ProjectionReturn params
--   , ArgsOf params ~ args
--   , ReturnOf params ~ Param r
--   , BuildCurried args r fn
--   , Eq args
--   , Show args
--   ) => StubBuilder (Cases params ()) fn where
--   buildStub name cases = do
--     let paramsList = runCase cases
--     s <- liftIO $ newIORef appliedRecord
--     (buildCurriedPure (\inputParams -> findReturnValueWithStore name paramsList inputParams s))


instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ProjectionArgs p
  , ProjectionReturn p
  , ArgsOf p ~ args
  , ReturnOf p ~ Param r
  , BuildCurried args r fn
  , Eq args
  , Show args
  ) => StubBuilder (Param a :> rest) fn where
  buildStub name params = buildCurriedPure (\inputParams -> extractReturnValue name params inputParams)

extractReturnValue ::
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , Eq args
  , Show args
  ) =>
  Maybe MockName ->
  params ->
  args ->
  r
extractReturnValue name params inputParams = do
  let _ = validateOnly name (projArgs params) inputParams
  returnValue params

validateOnly :: (Eq a, Show a) => Maybe MockName -> a -> a -> ()
validateOnly name expected actual = do
  validateParamsPure name expected actual

validateParamsPure :: (Eq a, Show a) => Maybe MockName -> a -> a -> ()
validateParamsPure name expected actual =
  if expected == actual
    then ()
    else errorWithoutStackTrace $ message name expected actual

