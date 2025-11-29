{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Test.MockCat.Verify where

import Control.Concurrent.STM (TVar, readTVarIO)
import Test.MockCat.Internal.Types
import Control.Monad (guard, when)
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Test.MockCat.Param
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Test.MockCat.Internal.Message
import Data.Kind (Type, Constraint)
import Test.MockCat.Cons ((:>))
import Data.Typeable (Typeable, eqT)
import Test.MockCat.Internal.Registry (lookupVerifierForFn, withAllUnitGuards)
import Data.Type.Equality ((:~:) (Refl))
import Data.Dynamic (fromDynamic)
import GHC.TypeLits (TypeError, ErrorMessage(..), Symbol)
import Unsafe.Coerce (unsafeCoerce)

-- | Class for verifying mock function.
verify ::
  ( ResolvableMock m
  , Eq (ResolvableParamsOf m)
  , Show (ResolvableParamsOf m)
  ) =>
  m ->
  VerifyMatchType (ResolvableParamsOf m) ->
  IO ()
verify m matchType = do
  ResolvedMock mockName verifier <- requireResolved m
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  case doVerify mockName appliedParamsList matchType of
    Nothing -> pure ()
    Just (VerifyFailed msg) ->
      errorWithoutStackTrace msg `seq` pure ()

doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a

readAppliedParamsList :: TVar (AppliedRecord params) -> IO (AppliedParamsList params)
readAppliedParamsList ref = do
  record <- readTVarIO ref
  pure $ appliedParamsList record

-- | Verify that a resolved mock function was called at least once.
--   This is used internally by typeclass mock verification.
verifyResolvedAny :: ResolvedMock params -> IO ()
verifyResolvedAny (ResolvedMock mockName verifier) = do
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  when (null appliedParamsList) $
    errorWithoutStackTrace $
      intercalate
        "\n"
        [ "It has never been applied function" <> mockNameLabel mockName
        ]



compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a = a == e
compareCount (LessThanEqual e) a = a <= e
compareCount (LessThan e) a = a < e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a = a > e

verifyCount ::
  ( ResolvableMock m
  , Eq (ResolvableParamsOf m)
  ) =>
  m ->
  ResolvableParamsOf m ->
  CountVerifyMethod ->
  IO ()
verifyCount m v method = do
  ResolvedMock mockName verifier <- requireResolved m
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  let appliedCount = length (filter (v ==) appliedParamsList)
  if compareCount method appliedCount
    then pure ()
    else
      errorWithoutStackTrace $
        countWithArgsMismatchMessage mockName method appliedCount

-- | Generate error message for count mismatch with arguments
countWithArgsMismatchMessage :: Maybe MockName -> CountVerifyMethod -> Int -> String
countWithArgsMismatchMessage mockName method appliedCount =
  intercalate
    "\n"
    [ "function" <> mockNameLabel mockName <> " was not applied the expected number of times to the expected arguments.",
      "  expected: " <> show method,
      "   but got: " <> show appliedCount
    ]



verifyOrder ::
  (ResolvableMock m
  , Eq (ResolvableParamsOf m)
  , Show (ResolvableParamsOf m)) =>
  VerifyOrderMethod ->
  m ->
  [ResolvableParamsOf m] ->
  IO ()
verifyOrder method m matchers = do
  ResolvedMock mockName verifier <- requireResolved m
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  case doVerifyOrder method mockName appliedParamsList matchers of
    Nothing -> pure ()
    Just (VerifyFailed msg) ->
      errorWithoutStackTrace msg `seq` pure ()

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

-- Legacy shouldApply* helpers removed. Use shouldBeCalled API instead.

type family PrependParam a rest where
  PrependParam a () = Param a
  PrependParam a rest = Param a :> rest

type family FunctionParams fn where
  FunctionParams (a -> fn) = PrependParam a (FunctionParams fn)
  FunctionParams fn = ()

type family ResolvableParamsOf target :: Type where
  ResolvableParamsOf (a -> fn) = FunctionParams (a -> fn)
  ResolvableParamsOf target = ()

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True _ = 'True
  Or _ 'True = 'True
  Or 'False 'False = 'False

type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family IsFunctionType target :: Bool where
  IsFunctionType (_a -> _b) = 'True
  IsFunctionType _ = 'False

type family IsIOType target :: Bool where
  IsIOType (IO _) = 'True
  IsIOType _ = 'False

type family IsPureConstant target :: Bool where
  IsPureConstant target = Not (Or (IsFunctionType target) (IsIOType target))

type family RequireCallable (fn :: Symbol) target :: Constraint where
  RequireCallable fn target =
    RequireCallableImpl fn (IsPureConstant target) target

type family RequireCallableImpl (fn :: Symbol) (isPure :: Bool) target :: Constraint where
  RequireCallableImpl fn 'True target =
    TypeError
      ( 'Text fn ':<>: 'Text " is not available for pure constant mocks."
          ':$$: 'Text "  target type: " ':<>: 'ShowType target
          ':$$: 'Text "  hint: convert it into a callable mock or use shouldBeCalled with 'anything'."
      )
  RequireCallableImpl _ 'False _ = ()

-- | Constraint alias for resolvable mock types.
type ResolvableMock m = (Typeable (ResolvableParamsOf m), Typeable (Verifier (ResolvableParamsOf m)))

-- | Constraint alias for resolvable mock types with specific params.
type ResolvableMockWithParams m params = (ResolvableParamsOf m ~ params, ResolvableMock m)

resolveForVerification ::
  forall target params.
  ( params ~ ResolvableParamsOf target
  , Typeable params
  , Typeable (Verifier params)
  ) =>
  target ->
  IO (Maybe (Maybe MockName, Verifier params))
resolveForVerification target = do
  let fetch = lookupVerifierForFn target
  result <-
    case eqT :: Maybe (params :~: ()) of
      Just Refl -> withAllUnitGuards fetch
      Nothing -> fetch
  case result of
    Nothing -> pure Nothing
    Just (name, dynVerifier) ->
      case fromDynamic dynVerifier of
        Just verifier -> pure $ Just (name, verifier)
        Nothing -> pure Nothing

-- | Verify that a function was applied the expected number of times
verifyAppliedCount ::
  Maybe MockName ->
  Verifier params ->
  Int ->
  IO ()
verifyAppliedCount maybeName verifier expected = do
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  let appliedCount = length appliedParamsList
  when (expected /= appliedCount) $
    errorWithoutStackTrace $
      countMismatchMessage maybeName expected appliedCount

-- | Generate error message for count mismatch
countMismatchMessage :: Maybe MockName -> Int -> Int -> String
countMismatchMessage maybeName expected appliedCount =
  intercalate
    "\n"
    [ "function" <> mockNameLabel maybeName <> " was not applied the expected number of times.",
      "  expected: " <> show expected,
      "   but got: " <> show appliedCount
    ]

verificationFailure :: IO a
verificationFailure =
  errorWithoutStackTrace verificationFailureMessage

data ResolvedMock params = ResolvedMock {
  resolvedMockName :: Maybe MockName,
  resolvedMockVerifier :: Verifier params
}

requireResolved ::
  forall target params.
  ( params ~ ResolvableParamsOf target
  , Typeable params
  , Typeable (Verifier params)
  ) =>
  target ->
  IO (ResolvedMock params)
requireResolved target = do
  resolveForVerification target >>= \case
    Just (name, verifier) -> pure $ ResolvedMock name verifier
    Nothing -> verificationFailure

-- | Error message for when a stub cannot be verified
verificationFailureMessage :: String
verificationFailureMessage =
  intercalate
    "\n"
    [ "The provided stub cannot be verified.",
      "Please create it via mock when verification is required."
    ]

-- ============================================
-- shouldBeCalled API
-- ============================================

-- | Verification specification for shouldBeCalled
data VerificationSpec params where
  -- | Count verification with specific arguments
  CountVerification :: CountVerifyMethod -> params -> VerificationSpec params
  -- | Count verification without arguments (any arguments)
  CountAnyVerification :: Int -> VerificationSpec params
  -- | Order verification
  OrderVerification :: VerifyOrderMethod -> [params] -> VerificationSpec params
  -- | Simple verification with arguments (at least once)
  SimpleVerification :: params -> VerificationSpec params
  -- | Simple verification without arguments (at least once, any arguments)
  AnyVerification :: VerificationSpec params

-- | Times condition for count verification
newtype TimesSpec = TimesSpec CountVerifyMethod

-- | Create a times condition for exact count
times :: Int -> TimesSpec
times n = TimesSpec (Equal n)

-- | Create a times condition for at least count
atLeast :: Int -> TimesSpec
atLeast n = TimesSpec (GreaterThanEqual n)

-- | Create a times condition for at most count
atMost :: Int -> TimesSpec
atMost n = TimesSpec (LessThanEqual n)

-- | Create a times condition for greater than count
greaterThan :: Int -> TimesSpec
greaterThan n = TimesSpec (GreaterThan n)

-- | Create a times condition for less than count
lessThan :: Int -> TimesSpec
lessThan n = TimesSpec (LessThan n)

-- | Create a times condition for exactly once
once :: TimesSpec
once = TimesSpec (Equal 1)

-- | Create a times condition for never (zero times)
never :: TimesSpec
never = TimesSpec (Equal 0)

-- | Order condition for order verification
newtype OrderSpec = OrderSpec VerifyOrderMethod

-- | Create an order condition for exact sequence
inOrder :: OrderSpec
inOrder = OrderSpec ExactlySequence

-- | Create an order condition for partial sequence
inPartialOrder :: OrderSpec
inPartialOrder = OrderSpec PartiallySequence

-- | Create a simple verification with arguments
--   This accepts both raw values and Param chains
calledWith :: params -> VerificationSpec params
calledWith = SimpleVerification

-- | Create a simple verification without arguments
--   Note: This is polymorphic and will be resolved based on the mock type
anything :: forall params. VerificationSpec params
anything = AnyVerification

-- | Type class for combining times condition with arguments
class WithArgs spec params where
  type WithResult spec params :: Type
  with :: spec -> params -> WithResult spec params

-- | Instance for times condition with arguments
instance (Eq params, Show params) => WithArgs TimesSpec params where
  type WithResult TimesSpec params = VerificationSpec params
  with (TimesSpec method) args = CountVerification method args

-- | Type family to normalize argument types for 'withArgs'
--   This helps avoid Type Family conflicts by normalizing types
type family NormalizeWithArg a :: Type where
  NormalizeWithArg (Param a :> rest) = Param a :> rest
  NormalizeWithArg (Param a) = Param a
  NormalizeWithArg a = Param a

-- | New function for combining times condition with arguments (supports raw values)
--   This will replace 'with' once the old 'with' is removed
--   We use a regular function with Type Family normalization instead of a type class
withArgs ::
  forall params.
  ( Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  , Typeable params
  , Typeable (NormalizeWithArg params)
  ) => TimesSpec -> params -> VerificationSpec (NormalizeWithArg params)
withArgs (TimesSpec method) args = 
  case eqT :: Maybe (params :~: NormalizeWithArg params) of
    Just Refl -> CountVerification method args
    Nothing -> 
      -- For raw values, convert to Param
      -- For Param a, this branch should not be taken, but we handle it safely
      CountVerification method (unsafeCoerce args :: NormalizeWithArg params)

infixl 8 `withArgs`

-- | Helper function for order verification (direct list, no 'with' needed)
--   Supports both Param types and raw values
--   Uses Type Family normalization similar to withArgs
inOrderWith ::
  forall params.
  ( Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  , Typeable params
  , Typeable (NormalizeWithArg params)
  ) => [params] -> VerificationSpec (NormalizeWithArg params)
inOrderWith args = 
  case eqT :: Maybe (params :~: NormalizeWithArg params) of
    Just Refl -> OrderVerification ExactlySequence args
    Nothing -> 
      -- For raw values, convert to Param
      OrderVerification ExactlySequence (unsafeCoerce (param <$> args) :: [NormalizeWithArg params])

-- | Helper function for partial order verification (direct list, no 'with' needed)
--   Supports both Param types and raw values
--   Uses Type Family normalization similar to withArgs
inPartialOrderWith ::
  forall params.
  ( Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  , Typeable params
  , Typeable (NormalizeWithArg params)
  ) => [params] -> VerificationSpec (NormalizeWithArg params)
inPartialOrderWith args = 
  case eqT :: Maybe (params :~: NormalizeWithArg params) of
    Just Refl -> OrderVerification PartiallySequence args
    Nothing -> 
      -- For raw values, convert to Param
      OrderVerification PartiallySequence (unsafeCoerce (param <$> args) :: [NormalizeWithArg params])

-- | Main verification function class
class ShouldBeCalled m spec where
  shouldBeCalled :: HasCallStack => m -> spec -> IO ()

-- | Instance for times spec alone (without arguments)
instance 
  ( ResolvableMockWithParams m params
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m TimesSpec where
  shouldBeCalled m (TimesSpec (Equal n)) = do
    ResolvedMock mockName verifier <- requireResolved m
    verifyAppliedCount mockName verifier n
  shouldBeCalled _ (TimesSpec _) = 
    errorWithoutStackTrace "times without args only supports Equal (use 'with' for other methods)"

-- | Instance for VerificationSpec (handles all verification types)
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m params
  , Eq params
  , Show params
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m (VerificationSpec params) where
  shouldBeCalled m spec = case spec of
    CountVerification method args -> 
      verifyCount m args method
    CountAnyVerification count -> 
      do
        ResolvedMock mockName verifier <- requireResolved m
        verifyAppliedCount mockName verifier count
    OrderVerification method argsList -> 
      verifyOrder method m argsList
    SimpleVerification args -> 
      verify m (MatchAny args)
    AnyVerification -> 
      do
        ResolvedMock mockName verifier <- requireResolved m
        appliedParamsList <- readAppliedParamsList (verifierRef verifier)
        when (null appliedParamsList) $
          errorWithoutStackTrace $
            intercalate
              "\n"
              [ "It has never been applied function" <> mockNameLabel mockName
              ]

-- | Instance for Param chains (e.g., "a" |> "b")
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m (Param a :> rest)
  , Eq (Param a :> rest)
  , Show (Param a :> rest)
  ) => ShouldBeCalled m (Param a :> rest) where
  shouldBeCalled m args = 
    verify m (MatchAny args)

-- | Instance for single Param (e.g., param "a")
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m (Param a)
  , Eq (Param a)
  , Show (Param a)
  ) => ShouldBeCalled m (Param a) where
  shouldBeCalled m args = 
    verify m (MatchAny args)

-- | Instance for raw values (e.g., "a")
--   This converts raw values to Param at runtime
instance {-# OVERLAPPABLE #-}
  ( ResolvableMockWithParams m (Param a)
  , Eq (Param a)
  , Show (Param a)
  ) => ShouldBeCalled m a where
  shouldBeCalled m arg = 
    verify m (MatchAny (param arg))