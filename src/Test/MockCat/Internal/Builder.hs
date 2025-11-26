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


import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
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

-- | Build a curried function that returns a pure result.
buildCurriedPure :: forall args r fn. BuildCurried args r fn => (args -> r) -> fn
buildCurriedPure a2r = buildCurried (pure . a2r)

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

-- | Class for creating a stub corresponding to the parameter description.
class MockBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  build ::
    MonadIO m =>
    Maybe MockName ->
    params ->
    m (fn, Verifier verifyParams)

-- | Instance for building a stub for a constant IO action.
instance
  MockBuilder (IO r) (IO r) ()
  where
  build _ action = do
    ref <- liftIO $ newIORef appliedRecord
    let fn = do
          result <- action
          liftIO $ appendAppliedParams ref ()
          pure result
        verifier = Verifier ref
    pure (fn, verifier)

-- | Instance for building a stub for a constant value (with Head marker).
instance
  MockBuilder (Head :> Param r) r ()
  where
  build _ (Head :> params) = do
    ref <- liftIO $ newIORef appliedRecord
    let v = value params
        fn = perform $ do
          liftIO $ appendAppliedParams ref ()
          pure v
        verifier = Verifier ref
    pure (fn, verifier)

-- | Instance for building a stub for a value (backward compatibility).
instance
  MockBuilder (Param r) r ()
  where
  build _ params = do
    ref <- liftIO $ newIORef appliedRecord
    let v = value params
        fn = perform $ do
          liftIO $ appendAppliedParams ref ()
          pure v
        verifier = Verifier ref
    pure (fn, verifier)

-- | Instance for building a stub for `Cases (IO a) ()`.
instance MockBuilder (Cases (IO a) ()) (IO a) () where
  build _ cases = do
    let params = runCase cases
    ref <- liftIO $ newIORef appliedRecord
    let fn = do
          count <- readAppliedCount ref ()
          let index = min count (length params - 1)
              r = safeIndex params index
          appendAppliedParams ref ()
          incrementAppliedParamCount ref ()
          fromJust r
        verifier = Verifier ref
    pure (fn, verifier)

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


buildWithRecorder ::
  ( MonadIO m
  , BuildCurried args r fn
  ) =>
  (IORef (AppliedRecord args) -> args -> IO r) ->
  m (fn, Verifier args)
buildWithRecorder handler = do
  ref <- liftIO $ newIORef appliedRecord
  let fn = buildCurried (handler ref)
      verifier = Verifier ref
  pure (fn, verifier)

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

type InvocationStep args r = AppliedRecord args -> (AppliedRecord args, Either Message r)

executeInvocation ::
  IORef (AppliedRecord args) ->
  InvocationStep args r ->
  IO r
executeInvocation ref step = do
  outcome <- atomicModifyIORef' ref $ \record -> step record
  either errorWithoutStackTrace pure outcome

singleInvocationStep ::
  ParamConstraints params args r =>
  Maybe MockName ->
  params ->
  args ->
  InvocationStep args r
singleInvocationStep name params inputParams record@AppliedRecord {appliedParamsList, appliedParamsCounter} = do
  let expected = projArgs params
  if expected == inputParams
    then
      (AppliedRecord {
        appliedParamsList = appliedParamsList ++ [inputParams]
      , appliedParamsCounter = appliedParamsCounter
      }, Right (returnValue params))
    else (record, Left $ message name expected inputParams)

casesInvocationStep ::
  ParamConstraints params args r =>
  Maybe MockName ->
  AppliedParamsList params ->
  args ->
  InvocationStep args r
casesInvocationStep name paramsList inputParams AppliedRecord {appliedParamsList, appliedParamsCounter} = do
  let newAppliedList = appliedParamsList ++ [inputParams]
      matchedParams = filter (\params -> projArgs params == inputParams) paramsList
      expectedArgs = projArgs <$> paramsList
   in case matchedParams of
        [] ->
          ( AppliedRecord {appliedParamsList = newAppliedList, appliedParamsCounter},
            Left (messageForMultiMock name expectedArgs inputParams)
          )
        _ ->
          let appliedCount = fromMaybe 0 (lookup inputParams appliedParamsCounter)
              index = min appliedCount (length matchedParams - 1)
              nextCounter = incrementCount inputParams appliedParamsCounter
              nextRecord =
                AppliedRecord
                  { appliedParamsList = newAppliedList,
                    appliedParamsCounter = nextCounter
                  }
           in case safeIndex matchedParams index of
                Nothing ->
                  ( nextRecord,
                    Left (messageForMultiMock name expectedArgs inputParams)
                  )
                Just selected ->
                  (nextRecord, Right (returnValue selected))