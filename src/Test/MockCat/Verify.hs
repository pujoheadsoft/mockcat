{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Test.MockCat.Verify where


import Test.MockCat.Internal.Verify
  ( tryVerifyCallCount
  , compareCount
  , countWithArgsMismatchMessage
  , doVerify
  , doVerifyOrder
  , readInvocationList
  )
import Test.MockCat.Internal.Types


import Control.Monad ()
import Data.List (intercalate)
import Data.Maybe
import Test.MockCat.Param
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Test.MockCat.Internal.Message
import Data.Kind (Type, Constraint)
import Test.MockCat.Cons ((:>))
import Data.Typeable (Typeable, eqT)
import Test.MockCat.Internal.MockRegistry (lookupVerifierForFn, withAllUnitGuards)
import Data.Type.Equality ((:~:) (Refl))
import Data.Dynamic (fromDynamic, Dynamic)
import GHC.TypeLits (TypeError, ErrorMessage(..), Symbol)

-- | Class for verifying mock function.
verify ::
  ( ResolvableMock m
  , EqParams (ResolvableParamsOf m)
  , Show (ResolvableParamsOf m)
  ) =>
  m ->
  VerifyMatchType (ResolvableParamsOf m) ->
  IO ()
verify m matchType = do
  candidates <- requireResolved m
  checkCandidates candidates $ \resolvedMock ->
    doVerifyResolved resolvedMock matchType

doVerifyResolved :: (EqParams params, Show params) => ResolvedMock params -> VerifyMatchType params -> IO (Maybe String)
doVerifyResolved (ResolvedMock mockName recorder) matchType = do
  invocationList <- readInvocationList (invocationRef recorder)
  case doVerify mockName invocationList matchType of
    Nothing -> pure Nothing
    Just (VerifyFailed msg) -> pure (Just msg)



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
type ResolvableMock m = (Typeable (ResolvableParamsOf m), Typeable (InvocationRecorder (ResolvableParamsOf m)))

-- | Constraint alias for resolvable mock types with specific params.
type ResolvableMockWithParams m params = (ResolvableParamsOf m ~ params, ResolvableMock m)






verificationFailure :: IO a
verificationFailure =
  errorWithoutStackTrace verificationFailureMessage



requireResolved ::
  forall target params.
  ( params ~ ResolvableParamsOf target
  , Typeable params
  , Typeable (InvocationRecorder params)
  ) =>
  target ->
  IO [ResolvedMock params]
requireResolved target = do
  candidates <- resolveForVerification target
  case candidates of
    [] -> verificationFailure
    _ -> pure $ map (uncurry ResolvedMock) candidates

resolveForVerification ::
  forall target params.
  ( params ~ ResolvableParamsOf target
  , Typeable params
  , Typeable (InvocationRecorder params)
  ) =>
  target ->
  IO [(Maybe MockName, InvocationRecorder params)]
resolveForVerification target = do
  let fetch = lookupVerifierForFn target
  result <-
    case eqT :: Maybe (params :~: ()) of
      Just Refl -> withAllUnitGuards fetch
      Nothing -> fetch
  findCompatible result

  where
    findCompatible :: [(Maybe MockName, Dynamic)] -> IO [(Maybe MockName, InvocationRecorder params)]
    findCompatible [] = pure []
    findCompatible ((name, dynVerifier) : rest) =
      case fromDynamic @(InvocationRecorder params) dynVerifier of
        Just verifier -> do
          matchRest <- findCompatible rest
          pure $ (name, verifier) : matchRest
        Nothing -> findCompatible rest

verificationFailureMessage :: String
verificationFailureMessage =
  intercalate
    "\n"
    [ "Error: 'shouldBeCalled' can only verify functions created by 'mock'.",
      "",
      "The value you passed could not be recognized as a mock function.",
      "",
      "This usually happens in one of the following cases:",
      "  - You passed a normal (non-mock) function.",
      "  - You passed a stub or value not created via 'mock' / 'mockIO'.",
      "  - You are trying to verify a value that was never registered as a mock.",
      "",
      "How to fix it:",
      "  1. Make sure you created the function with 'mock' (or 'mockIO' for IO)",
      "     before calling 'shouldBeCalled'.",
      "  2. Pass that mock value directly to 'shouldBeCalled'",
      "     (not the original function or a plain value).",
      "",
      "If this message still appears, check that:",
      "  - You are not passing a pure constant.",
      "  - The mock value is still in scope where 'shouldBeCalled' is used.",
      "",
      "Tip: If you prefer automatic verification,",
      "consider using 'withMock', which runs all expectations at the end",
      "of the block."
    ]

-- ============================================
-- shouldBeCalled API
-- ============================================

-- | Verification specification for shouldBeCalled
data VerificationSpec params where
  -- | Count verification with specific arguments
  CountVerification :: CountVerifyMethod -> params -> VerificationSpec params
  -- | Count verification without arguments (any arguments)
  CountAnyVerification :: CountVerifyMethod -> VerificationSpec params
  -- | Order verification
  OrderVerification :: VerifyOrderMethod -> [params] -> VerificationSpec params
  -- | Simple verification with arguments (at least once)
  SimpleVerification :: params -> VerificationSpec params
  -- | Simple verification without arguments (at least once, any arguments)
  AnyVerification :: VerificationSpec params

-- | Times condition for count verification
newtype TimesSpec = TimesSpec CountVerifyMethod

-- | Create a times condition for exact count.
--
--   > f `shouldBeCalled` times 3
--   > f `shouldBeCalled` (times 3 `with` "arg")
times :: Int -> TimesSpec
times n = TimesSpec (Equal n)

-- | Create a times condition for at least count (>=).
--
--   > f `shouldBeCalled` atLeast 1
atLeast :: Int -> TimesSpec
atLeast n = TimesSpec (GreaterThanEqual n)

-- | Create a times condition for at most count (<=).
--
--   > f `shouldBeCalled` atMost 2
atMost :: Int -> TimesSpec
atMost n = TimesSpec (LessThanEqual n)

-- | Create a times condition for greater than count (>).
greaterThan :: Int -> TimesSpec
greaterThan n = TimesSpec (GreaterThan n)

-- | Create a times condition for less than count (<).
lessThan :: Int -> TimesSpec
lessThan n = TimesSpec (LessThan n)

-- | Create a times condition for exactly once.
--   Equivalent to 'times 1'.
once :: TimesSpec
once = TimesSpec (Equal 1)

-- | Create a times condition for never (zero times).
--   Equivalent to 'times 0'.
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

-- | Create a simple verification with arguments.
--   This accepts both raw values and Param chains.
--
--   > f `shouldBeCalled` calledWith "a"
calledWith ::
  forall params.
  (ToNormalizedArg params) =>
  params ->
  VerificationSpec (NormalizeWithArg params)
calledWith = SimpleVerification . toNormalizedArg

-- | Create a simple verification without arguments.
--   It verifies that the function was called at least once, with ANY arguments.
--
--   > f `shouldBeCalled` anything
anything :: forall params. VerificationSpec params
anything = AnyVerification

-- | Type class for combining times condition with arguments
class WithArgs spec params where
  type WithResult spec params :: Type
  with :: spec -> params -> WithResult spec params

-- | Instance for times condition with arguments
instance (Eq params, Show params) => WithArgs TimesSpec params where
  type WithResult TimesSpec params = VerificationSpec params
  with (TimesSpec method) = CountVerification method

-- | Type family to normalize argument types for 'withArgs'
type family NormalizeWithArg a :: Type where
  NormalizeWithArg (Param a :> rest) = Param a :> rest
  NormalizeWithArg (Param a) = Param a
  NormalizeWithArg a = Param a

-- | Type class to normalize argument types (to Param or Param chain)
class ToNormalizedArg a where
  toNormalizedArg :: a -> NormalizeWithArg a

instance ToNormalizedArg (Param a :> rest) where
  toNormalizedArg = id

instance ToNormalizedArg (Param a) where
  toNormalizedArg = id

instance {-# OVERLAPPABLE #-} (NormalizeWithArg a ~ Param a, ToParamParam a, Normalize a ~ Param a) => ToNormalizedArg a where
  toNormalizedArg = toParamParam



-- | New function for combining times condition with arguments (supports raw values)
--   This will replace 'with' once the old 'with' is removed
withArgs ::
  forall params.
  ( ToNormalizedArg params
  , Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  ) => TimesSpec -> params -> VerificationSpec (NormalizeWithArg params)
withArgs (TimesSpec method) args = CountVerification method (toNormalizedArg args)

infixl 8 `withArgs`

-- | Verify that the mock was called with the specified sequence of arguments in exact order.
--
--   > f `shouldBeCalled` inOrderWith ["a", "b"]
inOrderWith ::
  forall params.
  ( ToNormalizedArg params
  , Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  ) => [params] -> VerificationSpec (NormalizeWithArg params)
inOrderWith args = OrderVerification ExactlySequence (map toNormalizedArg args)

-- | Verify that the mock was called with the specified sequence of arguments, allowing other calls in between.
--
--   > f `shouldBeCalled` inPartialOrderWith ["a", "c"]
--   > -- This passes if calls were: "a", "b", "c"
inPartialOrderWith ::
  forall params.
  ( ToNormalizedArg params
  , Eq (NormalizeWithArg params)
  , Show (NormalizeWithArg params)
  ) => [params] -> VerificationSpec (NormalizeWithArg params)
inPartialOrderWith args = OrderVerification PartiallySequence (map toNormalizedArg args)

-- | Main verification function class
class ShouldBeCalled m spec where
  shouldBeCalled :: HasCallStack => m -> spec -> IO ()

-- | Helper to verify multiple candidates. Passes if at least one candidate succeeds.
-- If all fail, throws the first error (or combined error).
checkCandidates :: [ResolvedMock params] -> (ResolvedMock params -> IO (Maybe String)) -> IO ()
checkCandidates [] _ = verificationFailure
checkCandidates candidates verifyFn = do
  results <- mapM verifyFn candidates
  let failures = catMaybes results
  -- If number of failures is less than number of candidates, it means at least one succeeded.
  if length failures < length candidates
    then pure ()
    else errorWithoutStackTrace $ intercalate "\n\n" failures

-- | Instance for times spec alone (without arguments)
instance
  ( ResolvableMockWithParams m params
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m TimesSpec where
  shouldBeCalled m (TimesSpec method) = do
    candidates <- requireResolved m
    checkCandidates candidates $ \(ResolvedMock mockName verifier) ->
      tryVerifyCallCount mockName verifier method

-- | Instance for VerificationSpec (handles all verification types)
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m params
  , EqParams params
  , Show params
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m (VerificationSpec params) where
  shouldBeCalled m spec = do
    candidates <- requireResolved m
    checkCandidates candidates $ \resolvedMock ->
      verifySpec resolvedMock spec

verifySpec :: (Typeable params, EqParams params, Show params) => ResolvedMock params -> VerificationSpec params -> IO (Maybe String)
verifySpec (ResolvedMock mockName recorder) spec = do
  invocationList <- readInvocationList (invocationRef recorder)
  case spec of
    CountVerification method args -> do
      let callCount = length (filter (`eqParams` args) invocationList)
      if compareCount method callCount
        then pure Nothing
        else pure $ Just $ countWithArgsMismatchMessage mockName method callCount

    CountAnyVerification method ->
      tryVerifyCallCount mockName recorder method

    OrderVerification method argsList ->
      case doVerifyOrder method mockName invocationList argsList of
        Nothing -> pure Nothing
        Just (VerifyFailed msg) -> pure $ Just msg

    SimpleVerification args ->
      case doVerify mockName invocationList (MatchAny args) of
        Nothing -> pure Nothing
        Just (VerifyFailed msg) -> pure $ Just msg

    AnyVerification -> do
      if null invocationList
        then pure $ Just $ intercalate "\n" ["Function" <> mockNameLabel mockName <> " was never called"]
        else pure Nothing

-- | Instance for Param chains (e.g., "a" ~> "b")
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m (Param a :> rest)
  , EqParams (Param a :> rest)
  , Show (Param a :> rest)
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m (Param a :> rest) where
  shouldBeCalled m args =
    shouldBeCalled m (SimpleVerification args)

-- | Instance for single Param (e.g., param "a")
instance {-# OVERLAPPING #-}
  ( ResolvableMockWithParams m (Param a)
  , EqParams (Param a)
  , Show (Param a)
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m (Param a) where
  shouldBeCalled m args =
    shouldBeCalled m (SimpleVerification args)

-- | Instance for raw values (e.g., "a")
--   This converts raw values to Param at runtime
instance {-# OVERLAPPABLE #-}
  ( ResolvableMockWithParams m (Param a)
  , EqParams (Param a)
  , Show (Param a)
  , Show a
  , Eq a
  , RequireCallable "shouldBeCalled" m
  ) => ShouldBeCalled m a where
  shouldBeCalled m arg =
    shouldBeCalled m (SimpleVerification (param arg))
