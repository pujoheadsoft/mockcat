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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Test.MockCat.Verify where

import Test.MockCat.Internal.Types
import Control.Monad (guard, when)
import Data.Function ((&))
import Data.IORef (IORef, readIORef)
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Test.MockCat.Param
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Test.MockCat.Internal.Core
import Test.MockCat.Internal.Message
import Data.Kind (Type)
import Test.MockCat.Cons ((:>))
import Data.Typeable (Typeable)
import Test.MockCat.Internal.Registry (lookupVerifierForFn, withAllUnitGuards)
import Data.Dynamic (fromDynamic)

-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  -- Generic over mock representation `m` which must satisfy `IsMock` and
  -- whose `MockParams m` match this class's `params`.
  shouldApplyTo ::
    ( MockResolvable m
    , ResolvableParams m ~ params
    , HasCallStack) =>
    m ->
    input ->
    IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

verify ::
  ( MockResolvable m
  , Eq (ResolvableParams m)
  , Show (ResolvableParams m)
  ) =>
  m ->
  VerifyMatchType (ResolvableParams m) ->
  IO ()
verify m matchType = do
  ResolvedMock mockName (Verifier ref) <- requireResolved m
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerify mockName appliedParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a

readAppliedParamsList :: IORef (AppliedRecord params) -> IO (AppliedParamsList params)
readAppliedParamsList ref = do
  record <- readIORef ref
  pure $ appliedParamsList record

class VerifyCount countType params a where
  -- | Verify the number of times a function has been applied to an argument.
  --
  -- @
  -- import Test.Hspec
  -- import Test.MockCat
  -- ...
  -- it "verify to applied times." do
  --   m \<- createMock $ "value" |\> True
  --   print $ stubFn m "value"
  --   print $ stubFn m "value"
  --   m \`shouldApplyTimes\` (2 :: Int) \`to\` "value" 
  -- @
  --
  shouldApplyTimes ::
    ( MockResolvable m
    , ResolvableParams m ~ params
    , HasCallStack
    , Eq params
    ) =>
    m ->
    countType ->
    a ->
    IO ()

instance VerifyCount CountVerifyMethod (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) count

instance VerifyCount Int (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) (Equal count)

instance {-# OVERLAPPABLE #-} VerifyCount CountVerifyMethod a a where
  shouldApplyTimes v count a = verifyCount v a count

instance {-# OVERLAPPABLE #-} VerifyCount Int a a where
  shouldApplyTimes v count a = verifyCount v a (Equal count)



compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a = a == e
compareCount (LessThanEqual e) a = a <= e
compareCount (LessThan e) a = a < e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a = a > e

verifyCount ::
  ( MockResolvable m
  , Eq (ResolvableParams m)) =>
  m ->
  ResolvableParams m ->
  CountVerifyMethod ->
  IO ()
verifyCount m v method = do
  ResolvedMock mockName (Verifier ref) <- requireResolved m
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length (filter (v ==) appliedParamsList)
  if compareCount method appliedCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel mockName <> " was not applied the expected number of times to the expected arguments.",
            "  expected: " <> show method,
            "   but got: " <> show appliedCount
          ]



class VerifyOrder params input where
  -- | Verify functions are applied in the expected order.
  --
  -- @
  -- import Test.Hspec
  -- import Test.MockCat
  -- import Prelude hiding (any)
  -- ...
  -- it "verify order of apply" do
  --   m \<- createMock $ any |\> True |\> ()
  --   print $ stubFn m "a" True
  --   print $ stubFn m "b" True
  --   m \`shouldApplyInOrder\` ["a" |\> True, "b" |\> True]
  -- @
  shouldApplyInOrder :: (MockResolvable m, ResolvableParams m ~ params, HasCallStack) => m -> [input] -> IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder :: (MockResolvable m, ResolvableParams m ~ params, HasCallStack) => m -> [input] -> IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  shouldApplyInOrder v a = verifyOrder ExactlySequence v $ param <$> a
  shouldApplyInPartialOrder v a = verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  shouldApplyInOrder = verifyOrder ExactlySequence
  shouldApplyInPartialOrder = verifyOrder PartiallySequence

verifyOrder ::
  (MockResolvable m, Eq (ResolvableParams m), Show (ResolvableParams m)) =>
  VerifyOrderMethod ->
  m ->
  [ResolvableParams m] ->
  IO ()
verifyOrder method m matchers = do
  ResolvedMock mockName (Verifier ref) <- requireResolved m
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerifyOrder method mockName appliedParamsList matchers
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerifyOrder ::
  (Eq a, Show a) =>
  VerifyOrderMethod ->
  Maybe MockName ->
  AppliedParamsList a ->
  [a] ->
  Maybe VerifyFailed
doVerifyOrder ExactlySequence name appliedValues expectedValues
  | length appliedValues /= length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name appliedValues expectedValues
  | otherwise = do
      let unexpectedOrders = collectUnExpectedOrder appliedValues expectedValues
      guard $ length unexpectedOrders > 0
      pure $ verifyFailedSequence name unexpectedOrders
doVerifyOrder PartiallySequence name appliedValues expectedValues
  | length appliedValues < length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name appliedValues expectedValues
  | otherwise = do
      guard $ isOrderNotMatched appliedValues expectedValues
      pure $ verifyFailedPartiallySequence name appliedValues expectedValues

verifyFailedPartiallySequence :: Show a => Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name appliedValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  but got:",
        intercalate "\n" $ ("    " <>) . show <$> appliedValues
      ]

isOrderNotMatched :: Eq a => AppliedParamsList a -> [a] -> Bool
isOrderNotMatched appliedValues expectedValues =
  isNothing $
    foldl
      ( \candidates e -> do
          candidates >>= \c -> do
            index <- elemIndex e c
            Just $ drop (index + 1) c
      )
      (Just appliedValues)
      expectedValues

verifyFailedOrderParamCountMismatch :: Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch name appliedValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order (count mismatch).",
        "  expected: " <> show (length expectedValues),
        "   but got: " <> show (length appliedValues)
      ]

verifyFailedSequence :: Show a => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("function" <> mockNameLabel name <> " was not applied to the expected arguments in the expected order.") : (verifyOrderFailedMesssage <$> fails)
      )



collectUnExpectedOrder :: Eq a => AppliedParamsList a -> [a] -> [VerifyOrderResult a]
collectUnExpectedOrder appliedValues expectedValues =
  catMaybes $
    mapWithIndex
      ( \i expectedValue -> do
          let appliedValue = appliedValues !! i
          guard $ expectedValue /= appliedValue
          pure VerifyOrderResult {index = i, appliedValue, expectedValue}
      )
      expectedValues

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = [f i x | (i, x) <- zip [0 ..] xs]

-- | Verify that the function has been applied to the expected arguments at least the expected number of times.
shouldApplyTimesGreaterThanEqual ::
  ( VerifyCount CountVerifyMethod params a
  , Eq params
  , MockResolvable m
  , ResolvableParams m ~ params
  , HasCallStack
  ) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThanEqual m i = shouldApplyTimes m (GreaterThanEqual i)

-- | Verify that the function is applied to the expected arguments less than or equal to the expected number of times.
shouldApplyTimesLessThanEqual ::
  ( VerifyCount CountVerifyMethod params a
  , Eq params
  , MockResolvable m
  , ResolvableParams m ~ params
  , HasCallStack
  ) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThanEqual m i = shouldApplyTimes m (LessThanEqual i)

-- | Verify that the function has been applied to the expected arguments a greater number of times than expected.
shouldApplyTimesGreaterThan ::
  ( VerifyCount CountVerifyMethod params a
  , Eq params
  , MockResolvable m
  , ResolvableParams m ~ params
  , HasCallStack
  ) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThan m i = shouldApplyTimes m (GreaterThan i)

-- | Verify that the function has been applied to the expected arguments less than the expected number of times.
shouldApplyTimesLessThan ::
  ( VerifyCount CountVerifyMethod params a
  , Eq params
  , MockResolvable m
  , ResolvableParams m ~ params
  , HasCallStack
  ) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThan m i = shouldApplyTimes m (LessThan i)

-- | Verify that it was apply to anything.
-- shouldApplyToAnything :: (IsMock m, MockParams m ~ params, HasCallStack) => m -> IO ()
-- shouldApplyToAnything m = do
--   let Verifier ref = mockVerifier m
--   appliedParamsList <- readAppliedParamsList ref
--   when (null appliedParamsList) $ error $ "It has never been applied function" <> mockNameLabel (mockName m)

shouldApplyToAnything ::
 ( MockResolvable m
 , ResolvableParams m ~ params
 , HasCallStack
 ) => m -> IO ()
shouldApplyToAnything m = do
  ResolvedMock mockName (Verifier ref) <- requireResolved m
  appliedParamsList <- readAppliedParamsList ref
  when (null appliedParamsList) $ error $ "It has never been applied function" <> mockNameLabel mockName

-- | Verify that it was apply to anything (times).
shouldApplyTimesToAnything ::
  ( MockResolvable m
  , ResolvableParams m ~ params
  , HasCallStack
  ) =>
  m ->
  Int ->
  IO ()
shouldApplyTimesToAnything m count = do
  ResolvedMock mockName (Verifier ref) <- requireResolved m
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length appliedParamsList
  when (count /= appliedCount) $
        errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel mockName <> " was not applied the expected number of times.",
            "  expected: " <> show count,
            "   but got: " <> show appliedCount
          ]

type family PrependParam a rest where
  PrependParam a () = Param a
  PrependParam a rest = Param a :> rest

type family FunctionParams fn where
  FunctionParams (a -> fn) = PrependParam a (FunctionParams fn)
  FunctionParams fn = ()

type family IsValueType target :: Bool where
  IsValueType (Mock fn params) = 'False
  IsValueType (MockIO m fn params) = 'False
  IsValueType (IO r) = 'False
  IsValueType (a -> fn) = 'False
  IsValueType target = 'True

type family ResolvableParamsOf target :: Type where
  ResolvableParamsOf (Mock fn params) = params
  ResolvableParamsOf (MockIO m fn params) = params
  ResolvableParamsOf (IO r) = FunctionParams (IO r)
  ResolvableParamsOf (a -> fn) = FunctionParams (a -> fn)
  ResolvableParamsOf target = ()

class MockResolvable target where
  type ResolvableParams target :: Type
  resolveMock :: target -> IO (Maybe (Maybe MockName, Verifier (ResolvableParams target)))

instance
  {-# OVERLAPPING #-}
  MockResolvable (Mock fn params) where
  type ResolvableParams (Mock fn params) = ResolvableParamsOf (Mock fn params)
  resolveMock mock = pure $ Just (mockName mock, mockVerifier mock)

instance
  {-# OVERLAPPING #-}
  MockResolvable (MockIO m fn params) where
  type ResolvableParams (MockIO m fn params) = ResolvableParamsOf (MockIO m fn params)
  resolveMock mock = pure $ Just (mockName mock, mockVerifier mock)

instance
  {-# OVERLAPPING #-}
  Typeable (Verifier (FunctionParams (IO r))) =>
  MockResolvable (IO r) where
  type ResolvableParams (IO r) = ResolvableParamsOf (IO r)
  resolveMock fn = do
    m <- lookupVerifierForFn fn
    case m of
      Nothing -> pure Nothing
      Just (name, dynVerifier) ->
        case fromDynamic dynVerifier of
          Just verifier -> pure $ Just (name, verifier)
          Nothing -> pure Nothing

instance
  {-# OVERLAPPING #-}
  Typeable (Verifier (FunctionParams (a -> fn))) =>
  MockResolvable (a -> fn) where
  type ResolvableParams (a -> fn) = ResolvableParamsOf (a -> fn)
  resolveMock fn = do
    m <- lookupVerifierForFn fn
    case m of
      Nothing -> pure Nothing
      Just (name, dynVerifier) ->
        case fromDynamic dynVerifier of
          Just verifier -> pure $ Just (name, verifier)
          Nothing -> pure Nothing

instance
  {-# OVERLAPPABLE #-}
  ( Typeable (Verifier ())
  , FunctionParams value ~ ()
  , ResolvableParamsOf value ~ ()
  , IsValueType value ~ 'True
  , Typeable (ResolvableParamsOf value)
  ) =>
  MockResolvable value where
  type ResolvableParams value = ResolvableParamsOf value
  resolveMock value = do
    m <- withAllUnitGuards $ lookupVerifierForFn value
    case m of
      Nothing -> pure Nothing
      Just (name, dynVerifier) ->
        case fromDynamic dynVerifier of
          Just verifier -> pure $ Just (name, verifier)
          Nothing -> pure Nothing

data ResolvedMock params = ResolvedMock {
  resolvedMockName :: Maybe MockName,
  resolvedMockVerifier :: Verifier params
}

requireResolved ::
  forall target.
  MockResolvable target =>
  target ->
  IO (ResolvedMock (ResolvableParams target))
requireResolved target = do
  resolveMock target >>= \case
    Just (name, verifier) -> pure $ ResolvedMock name verifier
    Nothing ->
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "The provided stub cannot be verified.",
            "Please create it via createStubFn/createNamedStubFn or createStubFnIO when verification is required."
          ]