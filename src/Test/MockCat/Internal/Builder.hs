{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.Internal.Builder where


import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Data.Maybe
import Test.MockCat.Cons (Head(..), (:>)(..))
import Test.MockCat.Param
import Test.MockCat.AssociationList (lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import Control.Monad.State
import Test.MockCat.Internal.Types
import Test.MockCat.Internal.Message

-- | Class for building a curried function.
-- The purpose of this class is to automatically generate and provide
-- an implementation for the corresponding curried function type (such as `a -> b -> ... -> IO r`)
-- when given the argument list type of the mock (`Param a :> Param b :> ...`).
-- @args@ is the argument list type of the mock.
-- @r@ is the return type of the function.
-- @fn@ is the curried function type.
class BuildCurried args r fn | args r -> fn where
  buildCurriedImpl :: (args -> IO r) -> fn

-- | Build a curried function that returns an IO result.
buildCurried :: forall args r fn. BuildCurried args r fn => (args -> IO r) -> fn
buildCurried = buildCurriedImpl

instance (fn ~ (a -> r)) => BuildCurried (Param a) r fn where
  buildCurriedImpl f a = perform (f (param a))

instance
  ( BuildCurried rest r fn
  , fn' ~ (a -> fn)
  ) =>
  BuildCurried (Param a :> rest) r fn'
  where
  buildCurriedImpl input a =
    buildCurriedImpl @rest @r @fn (input . (\rest -> p a :> rest))

-- | Class for building a curried pure function without relying on IO.
class BuildCurriedPure args r fn | args r -> fn where
  buildCurriedPureImpl :: (args -> r) -> fn

-- | Build a curried function that returns a pure result.
buildCurriedPure :: forall args r fn. BuildCurriedPure args r fn => (args -> r) -> fn
buildCurriedPure = buildCurriedPureImpl

instance (fn ~ (a -> r)) => BuildCurriedPure (Param a) r fn where
  buildCurriedPureImpl f a = f (param a)

instance
  ( BuildCurriedPure rest r fn
  , fn' ~ (a -> fn)
  ) =>
  BuildCurriedPure (Param a :> rest) r fn'
  where
  buildCurriedPureImpl input a =
    buildCurriedPureImpl @rest @r @fn (input . (\rest -> p a :> rest))

-- | Class for building a curried function whose result stays in IO.
class BuildCurriedIO args r fn | args r -> fn where
  buildCurriedIOImpl :: (args -> IO r) -> fn

-- | Build a curried IO function without hiding the IO layer.
buildCurriedIO :: forall args r fn. BuildCurriedIO args r fn => (args -> IO r) -> fn
buildCurriedIO = buildCurriedIOImpl

instance (fn ~ (a -> IO r)) => BuildCurriedIO (Param a) r fn where
  buildCurriedIOImpl f a = f (param a)

instance
  ( BuildCurriedIO rest r fn
  , fn' ~ (a -> fn)
  ) =>
  BuildCurriedIO (Param a :> rest) r fn'
  where
  buildCurriedIOImpl input a =
    buildCurriedIOImpl @rest @r @fn (input . (\rest -> p a :> rest))

-- | Class for creating a stub corresponding to the parameter description.
class MockBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  build ::
    MonadIO m =>
    Maybe MockName ->
    params ->
    m (BuiltMock fn verifyParams)

-- | New name for `build` to make intent explicit.
--   `buildMock` constructs a mock function and its verifier.
buildMock ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  ) =>
  Maybe MockName ->
  params ->
  m (BuiltMock fn verifyParams)
buildMock = build

-- | Instance for building a stub for a constant IO action.
instance
  MockBuilder (IO r) (IO r) ()
  where
  build _ action = do
    ref <- liftIO $ newTVarIO invocationRecord
    let
      fn = do
        result <- action
        liftIO $ appendAppliedParams ref ()
        pure result
      recorder = InvocationRecorder ref IOConstant
    pure (BuiltMock fn recorder)

-- | Instance for building a stub for a constant value (with Head marker).
instance
  MockBuilder (Head :> Param r) r ()
  where
  build _ (Head :> params) = do
    ref <- liftIO $ newTVarIO invocationRecord
    let v = value params
        fn = perform $ do
          liftIO $ appendAppliedParams ref ()
          pure v
        recorder = InvocationRecorder ref PureConstant
    pure (BuiltMock fn recorder)

-- | Instance for building a stub for a value (backward compatibility).
instance
  MockBuilder (Param r) r ()
  where
  build _ params = do
    ref <- liftIO $ newTVarIO invocationRecord
    let v = value params
        fn = perform $ do
          liftIO $ appendAppliedParams ref ()
          pure v
        recorder = InvocationRecorder ref PureConstant
    pure (BuiltMock fn recorder)

-- | Instance for building a stub for `Cases (IO a) ()`.
instance MockBuilder (Cases (IO a) ()) (IO a) () where
  build _ cases = do
    let params = runCase cases
    ref <- liftIO $ newTVarIO invocationRecord
    let fn = do
          count <- readInvocationCount ref ()
          let index = min count (length params - 1)
              r = safeIndex params index
          appendAppliedParams ref ()
          incrementInvocationCount ref ()
          fromJust r
        recorder = InvocationRecorder ref IOConstant
    pure (BuiltMock fn recorder)

-- | Overlapping instance for building a stub when parameters are provided as 'Cases'.
instance {-# OVERLAPPABLE #-}
  ( ParamConstraints params args r
  , BuildCurried args r fn
  ) => MockBuilder (Cases params ()) fn args where
  build name cases = do
    let paramsList = runCase cases
    buildWithRecorder (\ref inputParams -> executeInvocation ref (casesInvocationStep name paramsList inputParams))

-- | Overlapping instance for building a stub defined via chained 'Param'.
instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurried args r fn
  ) => MockBuilder (Param a :> rest) fn args where
  build name params =
    buildWithRecorder (\ref inputParams -> executeInvocation ref (singleInvocationStep name params inputParams))

-- | Class for building mocks whose resulting functions stay in IO.
class MockIOBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  buildIO ::
    MonadIO m =>
    Maybe MockName ->
    params ->
    m (BuiltMock fn verifyParams)

instance {-# OVERLAPPABLE #-}
  ( ParamConstraints params args r
  , BuildCurriedIO args r fn
  ) => MockIOBuilder (Cases params ()) fn args where
  buildIO name cases = do
    let paramsList = runCase cases
    buildWithRecorderIO (\ref inputParams -> executeInvocation ref (casesInvocationStep name paramsList inputParams))

instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurriedIO args r fn
  ) => MockIOBuilder (Param a :> rest) fn args where
  buildIO name params =
    buildWithRecorderIO (\ref inputParams -> executeInvocation ref (singleInvocationStep name params inputParams))


buildWithRecorder ::
  ( MonadIO m
  , BuildCurried args r fn
  ) =>
  (TVar (InvocationRecord args) -> args -> IO r) ->
  m (BuiltMock fn args)
buildWithRecorder handler = do
  ref <- liftIO $ newTVarIO invocationRecord
  let fn = buildCurried (handler ref)
      recorder = InvocationRecorder ref ParametricFunction
  pure (BuiltMock fn recorder)

buildWithRecorderIO ::
  ( MonadIO m
  , BuildCurriedIO args r fn
  ) =>
  (TVar (InvocationRecord args) -> args -> IO r) ->
  m (BuiltMock fn args)
buildWithRecorderIO handler = do
  ref <- liftIO $ newTVarIO invocationRecord
  let fn = buildCurriedIO (handler ref)
      recorder = InvocationRecorder ref ParametricFunction
  pure (BuiltMock fn recorder)

invocationRecord :: InvocationRecord params
invocationRecord =
  InvocationRecord
    { invocations = mempty
    , invocationCounts = empty
    }

appendAppliedParams :: TVar (InvocationRecord params) -> params -> IO ()
appendAppliedParams ref inputParams =
  atomically $
    modifyTVar' ref $ \record ->
      record
        { invocations = invocations record ++ [inputParams]
        }

readInvocationCount :: Eq params => TVar (InvocationRecord params) -> params -> IO Int
readInvocationCount ref params = do
  record <- readTVarIO ref
  pure $ fromMaybe 0 (lookup params (invocationCounts record))

incrementInvocationCount :: Eq params => TVar (InvocationRecord params) -> params -> IO ()
incrementInvocationCount ref inputParams =
  atomically $
    modifyTVar' ref $ \record ->
      record
        { invocationCounts = incrementCount inputParams (invocationCounts record)
        }

incrementCount :: Eq k => k -> InvocationCounts k -> InvocationCounts k
incrementCount key list =
  if member key list then update (+ 1) key list
  else insert key 1 list

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
    s <- liftIO $ newTVarIO invocationRecord
    (do
      count <- readInvocationCount s ()
      let index = min count (length params - 1)
          r = safeIndex params index
      appendAppliedParams s ()
      incrementInvocationCount s ()
      fromJust r)

-- | Overlapping instance for building a mock for a function with multiple parameters.
-- This instance is used when the parameter type is a 'Cases' type.
instance {-# OVERLAPPABLE #-}
  ( ParamConstraints params args r
  , BuildCurried args r fn
  , BuildCurriedPure args r fn
  ) => StubBuilder (Cases params ()) fn where
  buildStub name cases = do
    let paramsList = runCase cases
    buildCurriedPure (findReturnValueWithPure name paramsList)

instance {-# OVERLAPPABLE #-}
  ( p ~ (Param a :> rest)
  , ParamConstraints p args r
  , BuildCurried args r fn
  , BuildCurriedPure args r fn
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
  InvocationList params ->
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
  InvocationList params ->
  args ->
  Maybe r
findReturnValuePure paramsList inputParams = do
  let matchedParams = filter (\params -> projArgs params == inputParams) paramsList
  case matchedParams of
    [] -> Nothing
    _ -> do
      returnValue <$> safeIndex matchedParams 0

type InvocationStep args r = InvocationRecord args -> (InvocationRecord args, Either Message r)

executeInvocation ::
  TVar (InvocationRecord args) ->
  InvocationStep args r ->
  IO r
executeInvocation ref step = do
  result <-
    atomically $ do
      current <- readTVar ref
      let (next, outcome) = step(current)
      writeTVar ref next
      pure outcome
  either errorWithoutStackTrace pure result

singleInvocationStep ::
  ParamConstraints params args r =>
  Maybe MockName ->
  params ->
  args ->
  InvocationStep args r
singleInvocationStep name params inputParams record@InvocationRecord {invocations, invocationCounts} = do
  let expected = projArgs params
  if expected == inputParams
    then
      (InvocationRecord {
        invocations = invocations ++ [inputParams]
      , invocationCounts = invocationCounts
      }, Right (returnValue params))
    else (record, Left $ message name expected inputParams)

casesInvocationStep ::
  ParamConstraints params args r =>
  Maybe MockName ->
  InvocationList params ->
  args ->
  InvocationStep args r
casesInvocationStep name paramsList inputParams InvocationRecord {invocations, invocationCounts} = do
  let newInvocations = invocations ++ [inputParams]
      matchedParams = filter (\params -> projArgs params == inputParams) paramsList
      expectedArgs = projArgs <$> paramsList
   in case matchedParams of
        [] ->
          ( InvocationRecord {invocations = newInvocations, invocationCounts},
            Left (messageForMultiMock name expectedArgs inputParams)
          )
        _ ->
          let appliedCount = fromMaybe 0 (lookup inputParams invocationCounts)
              index = min appliedCount (length matchedParams - 1)
              nextCounter = incrementCount inputParams invocationCounts
              nextRecord =
                InvocationRecord
                  { invocations = newInvocations,
                    invocationCounts = nextCounter
                  }
           in case safeIndex matchedParams index of
                Nothing ->
                  ( nextRecord,
                    Left (messageForMultiMock name expectedArgs inputParams)
                  )
                Just selected ->
                  (nextRecord, Right (returnValue selected))