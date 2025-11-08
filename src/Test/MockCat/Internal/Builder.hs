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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
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

-- ------------
data Pure
data PureWithIO
data InIO

type family ApplyMode (mode :: k) (r :: Type) :: Type where
  ApplyMode Pure r = r
  ApplyMode PureWithIO r = r
  ApplyMode InIO r = IO r

type family ModeInput (mode :: k) (r :: Type) :: Type where
  ModeInput Pure r = r
  ModeInput PureWithIO r = IO r
  ModeInput InIO r = IO r

class ModeSpec mode where
  finalize :: forall r. ModeInput mode r -> ApplyMode mode r

instance ModeSpec Pure where
  finalize :: ModeInput Pure r -> ApplyMode Pure r
  finalize = id

instance ModeSpec PureWithIO where
  finalize :: ModeInput PureWithIO r -> ApplyMode PureWithIO r
  finalize = perform

instance ModeSpec InIO where
  finalize :: ModeInput InIO r -> ApplyMode InIO r
  finalize = id

type InputFn mode args r = args -> ModeInput mode r

type family CurriedFn (mode :: k) (args :: Type) (r :: Type) :: Type where
  CurriedFn mode (Param a) r = a -> ApplyMode mode r
  CurriedFn mode (Param a :> rest) r = a -> CurriedFn mode rest r

class ModeSpec mode => BuildCurriedGeneric mode args r fn | mode args r -> fn where
  buildGeneric :: InputFn mode args r -> fn

instance (ModeSpec mode, fn ~ (a -> ApplyMode mode r))
      => BuildCurriedGeneric mode (Param a) r fn where
  buildGeneric f a = finalize @mode @r (f (param a))

instance (ModeSpec mode, BuildCurriedGeneric mode rest r fn, fn' ~ (a -> fn))
      => BuildCurriedGeneric mode (Param a :> rest) r fn' where
  buildGeneric input a = buildGeneric @mode @rest @r (input . (\rest -> p a :> rest))
-- ------------

-- | Class for building a curried function.
-- The purpose of this class is to automatically generate and provide
-- an implementation for the corresponding curried function type (such as `a -> b -> ... -> IO r`)
-- when given the argument list type of the mock (`Param a :> Param b :> ...`).
-- @args@ is the argument list type of the mock.
-- @r@ is the return type of the function.
-- @fn@ is the curried function type.
type BuildCurried args r fn = BuildCurriedGeneric PureWithIO args r fn

-- | Build a curried function that returns an IO result.
buildCurried :: forall args r fn. BuildCurried args r fn => (args -> IO r) -> fn
buildCurried = buildGeneric @PureWithIO @args @r

-- | Build a curried function that returns a pure result.
buildCurriedPure :: forall args r fn. BuildCurried args r fn => (args -> r) -> fn
buildCurriedPure a2r = buildCurried (pure . a2r)

-- | Like 'BuildCurried' but returns an IO result at the end of the curried function.
-- This is used by the MockIO builder to produce functions whose final result is in IO,
-- allowing them to be lifted into other monads via 'LiftFunTo'.
type BuildCurriedIO args r fn = BuildCurriedGeneric InIO args r fn

-- | Build a curried function that returns an IO result.
buildCurriedIO :: forall args r fn. BuildCurriedIO args r fn => (args -> IO r) -> fn
buildCurriedIO = buildGeneric @InIO @args @r


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
  ( ParamConstraints params args r
  , BuildCurried args r fn
  ) => MockBuilder (Cases params ()) fn args where
  build name cases = do
    let paramsList = runCase cases
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (buildCurried (\inputParams -> findReturnValueWithStore name paramsList inputParams s))

-- | Overlapping instance for building a mock for a function with multiple parameters.
-- This instance is used when the parameter type is a 'Param a :> rest' type.
instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurried args r fn
  ) => MockBuilder (Param a :> rest) fn args where
  build name params = do
    s <- liftIO $ newIORef appliedRecord
    makeMock name s (buildCurried (\inputParams -> extractReturnValueWithValidate name params inputParams s))




class MockIOBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  -- build a mock
  buildIO :: MonadIO m => Maybe MockName -> params -> m (MockIO m fn verifyParams)

instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurriedIO args r fn
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
  ( ParamConstraints params args r
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
  ( ParamConstraints params args r
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
  ( ParamConstraints params args r
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
  buildStub _ = id

-- | Instance for building a mock for a function with a single parameter.
instance
  StubBuilder (Param r) r
  where
  buildStub _ = value

-- | Instance for building a mock for a function with multiple parameters.
instance StubBuilder (Cases (IO a) ()) (IO a) where
  buildStub _ cases = do
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
instance {-# OVERLAPPABLE #-}
  ( ParamConstraints params args r
  , BuildCurried args r fn
  ) => StubBuilder (Cases params ()) fn where
  buildStub name cases = do
    let paramsList = runCase cases
    buildCurriedPure (findReturnValueWithPure name paramsList)

instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurried args r fn
  ) => StubBuilder (Param a :> rest) fn where
  buildStub name params = buildCurriedPure (extractReturnValue name params)

type ParamConstraints params args r =
  ( ProjectionArgs params
  , ProjectionReturn params
  , ArgsOf params ~ args
  , ReturnOf params ~ Param r
  , Eq args
  , Show args
  )

extractReturnValue :: ParamConstraints params args r => Maybe MockName -> params -> args -> r
extractReturnValue name params inputParams = do
  validateOnly name (projArgs params) inputParams `seq` returnValue params

validateOnly :: (Eq a, Show a) => Maybe MockName -> a -> a -> ()
validateOnly name expected actual = do
  validateParamsPure name expected actual

validateParamsPure :: (Eq a, Show a) => Maybe MockName -> a -> a -> ()
validateParamsPure name expected actual =
  if expected == actual
    then ()
    else errorWithoutStackTrace $ message name expected actual

findReturnValueWithPure ::
  ( ParamConstraints params args r
  ) =>
  Maybe MockName ->
  AppliedParamsList params ->
  args ->
  r
findReturnValueWithPure name paramsList inputParams = do
  let
    expectedArgs = projArgs <$> paramsList
    r = findReturnValuePure paramsList inputParams
  fromMaybe (errorWithoutStackTrace $ messageForMultiMock name expectedArgs inputParams) r

findReturnValuePure ::
  ( ParamConstraints params args r
  ) =>
  AppliedParamsList params ->
  args ->
  Maybe r
findReturnValuePure paramsList inputParams = do
  let matchedParams = filter (\params -> projArgs params == inputParams) paramsList
  case matchedParams of
    [] -> Nothing
    _ -> do
      returnValue <$> safeIndex matchedParams 0