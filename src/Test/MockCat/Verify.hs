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
{-# LANGUAGE FunctionalDependencies #-}
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

-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  -- Generic over mock representation `m` which must satisfy `IsMock` and
  -- whose `MockParams m` match this class's `params`.
  shouldApplyTo ::
    ( ResolvableMockWithParams m params
    , HasCallStack) =>
    m ->
    input ->
    IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

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
    ( ResolvableMockWithParams m params
    , HasCallStack
    , Eq params
    , RequireCallable "shouldApplyTimes" m
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
  ( ResolvableMock m
  , Eq (ResolvableParamsOf m)
  , RequireCallable "shouldApplyTimes" m) =>
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
  shouldApplyInOrder ::
    ( ResolvableMockWithParams m params
    , HasCallStack
    , RequireCallable "shouldApplyInOrder" m
    ) =>
    m ->
    [input] ->
    IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder ::
    ( ResolvableMockWithParams m params
    , HasCallStack
    , RequireCallable "shouldApplyInPartialOrder" m
    ) =>
    m ->
    [input] ->
    IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  shouldApplyInOrder v a = verifyOrder ExactlySequence v $ param <$> a
  shouldApplyInPartialOrder v a = verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  shouldApplyInOrder = verifyOrder ExactlySequence
  shouldApplyInPartialOrder = verifyOrder PartiallySequence

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

-- | Verify that the function has been applied to the expected arguments at least the expected number of times.
shouldApplyTimesGreaterThanEqual ::
  ( VerifyCount CountVerifyMethod params a
  , Eq params
  , ResolvableMockWithParams m params
  , HasCallStack
  , RequireCallable "shouldApplyTimes" m
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
  , ResolvableMockWithParams m params
  , HasCallStack
  , RequireCallable "shouldApplyTimes" m
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
  , ResolvableMockWithParams m params
  , HasCallStack
  , RequireCallable "shouldApplyTimes" m
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
  , ResolvableMockWithParams m params
  , HasCallStack
  , RequireCallable "shouldApplyTimes" m
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
  ( ResolvableMockWithParams m params
  , HasCallStack
  ) =>
  m ->
  IO ()
shouldApplyToAnything m = requireResolved m >>= shouldApplyToAnythingResolved

shouldApplyToAnythingResolved ::
  HasCallStack =>
  ResolvedMock params ->
  IO ()
shouldApplyToAnythingResolved ResolvedMock {resolvedMockName = mockName, resolvedMockVerifier = verifier} = do
  appliedParamsList <- readAppliedParamsList (verifierRef verifier)
  when (null appliedParamsList) $
    error $
      "It has never been applied function" <> mockNameLabel mockName

-- | Verify that it was apply to anything (times).
shouldApplyTimesToAnything ::
  ( ResolvableMockWithParams m params
  , RequireCallable "shouldApplyTimesToAnything" m
  ) =>
  m ->
  Int ->
  IO ()
shouldApplyTimesToAnything m count = do
  ResolvedMock mockName verifier <- requireResolved m
  case verifierKind verifier of
    VerifierPureConstant ->
      errorWithoutStackTrace $
        unsupportedTimesMessage mockName
    _ -> do
      appliedParamsList <- readAppliedParamsList (verifierRef verifier)
      let appliedCount = length appliedParamsList
      when (count /= appliedCount) $
        errorWithoutStackTrace $
          intercalate
            "\n"
            [ "function" <> mockNameLabel mockName <> " was not applied the expected number of times.",
              "  expected: " <> show count,
              "   but got: " <> show appliedCount
            ]

unsupportedTimesMessage :: Maybe MockName -> String
unsupportedTimesMessage mockName =
  intercalate
    "\n"
    [ "function" <> mockNameLabel mockName <> " does not support shouldApplyTimesToAnything.",
      "  reason: pure constant mocks are values, not callable functions.",
      "  hint: use shouldApplyToAnything instead."
    ]

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
          ':$$: 'Text "  hint: convert it into a callable mock or use shouldApplyToAnything."
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
      intercalate
        "\n"
        [ "function" <> mockNameLabel maybeName <> " was not applied the expected number of times.",
          "  expected: " <> show expected,
          "   but got: " <> show appliedCount
        ]

verificationFailure :: IO a
verificationFailure =
  errorWithoutStackTrace $
    intercalate
      "\n"
      [ "The provided stub cannot be verified.",
        "Please create it via mock when verification is required."
      ]

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
    Nothing ->
      errorWithoutStackTrace $
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
data TimesSpec = TimesSpec CountVerifyMethod

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
data OrderSpec = OrderSpec VerifyOrderMethod

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

-- | Helper function for order verification (direct list, no 'with' needed)
inOrderWith :: [params] -> VerificationSpec params
inOrderWith = OrderVerification ExactlySequence

-- | Helper function for partial order verification (direct list, no 'with' needed)
inPartialOrderWith :: [params] -> VerificationSpec params
inPartialOrderWith = OrderVerification PartiallySequence

-- | Main verification function class
class ShouldBeCalled m spec where
  shouldBeCalled :: HasCallStack => m -> spec -> IO ()

-- | Instance for times spec alone (without arguments)
instance 
  ( ResolvableMockWithParams m params
  , RequireCallable "shouldBeCalled" m
  , RequireCallable "shouldApplyTimesToAnything" m
  ) => ShouldBeCalled m TimesSpec where
  shouldBeCalled m (TimesSpec (Equal n)) = 
    shouldApplyTimesToAnything m n
  shouldBeCalled _ (TimesSpec _) = 
    errorWithoutStackTrace "times without args only supports Equal (use 'with' for other methods)"

-- | Instance for VerificationSpec (handles all verification types)
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m params
  , Eq params
  , Show params
  , RequireCallable "shouldBeCalled" m
  , RequireCallable "shouldApplyTimes" m
  , RequireCallable "shouldApplyTimesToAnything" m
  ) => ShouldBeCalled m (VerificationSpec params) where
  shouldBeCalled m spec = case spec of
    CountVerification method args -> 
      verifyCount m args method
    CountAnyVerification count -> 
      shouldApplyTimesToAnything m count
    OrderVerification method argsList -> 
      verifyOrder method m argsList
    SimpleVerification args -> 
      verify m (MatchAny args)
    AnyVerification -> 
      shouldApplyToAnything m