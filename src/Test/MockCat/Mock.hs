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

{- | This module provides the following functions.

  - Create mocks that can be stubbed and verified.

  - Create stub function.

  - Verify applied mock function.
-}
module Test.MockCat.Mock
  ( Mock
  , MockBuilder
  , build
  , createMock
  , createNamedMock
  , createConstantMock
  , createNamedConstantMock
  , createStubFn
  , createNamedStubFn
  , stubFn
  , stubFnMock
  , shouldApplyTo
  , shouldApplyTimes
  , shouldApplyInOrder
  , shouldApplyInPartialOrder
  , shouldApplyTimesGreaterThanEqual
  , shouldApplyTimesLessThanEqual
  , shouldApplyTimesGreaterThan
  , shouldApplyTimesLessThan
  , shouldApplyToAnything
  , shouldApplyTimesToAnything
  , to
  , onCase
  , cases
  , casesIO
  , createMockIO
  , stubFnMockIO
  , createStubFnIO
  )
where

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
import Test.MockCat.Internal.Builder

{- | Create a mock.
From this mock, you can generate stub functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    -- create a mock
    m \<- createMock $ "value" |\> True
    -- stub function
    let f = stubFn m
    -- assert
    f "value" \`shouldBe\` True
    -- verify
    m \`shouldApplyTo\` "value"
  @

  If you do not need verification and only need stub functions, you can use @'mockFun'@.

-}
createMock ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  params ->
  m (Mock fn verifyParams)
createMock params = liftIO $ build Nothing params

{- | Create a constant mock.
From this mock, you can generate constant functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    m \<- createConstantMock "foo"
    stubFn m \`shouldBe\` "foo"
    shouldApplyToAnything m
  @
-}
createConstantMock :: MonadIO m => a -> m (Mock a ())
createConstantMock a = liftIO $ build Nothing $ param a

{- | Create a named mock. If the test fails, this name is used. This may be useful if you have multiple mocks.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "named mock" do
    m \<- createNamedMock "mock" $ "value" |\> True
    stubFn m "value" \`shouldBe\` True
  @
-}
createNamedMock ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  MockName ->
  params ->
  m (Mock fn verifyParams)
createNamedMock name params = liftIO $ build (Just name) params

-- | Create a named constant mock.
createNamedConstantMock :: MonadIO m => MockName -> fn -> m (Mock fn ())
createNamedConstantMock name a = liftIO $ build (Just name) (param a)

-- | Extract the stub function from the mock.
stubFn :: forall m fn. (IsMock m, MockFn m ~ fn) => m -> fn
stubFn = mockStubFn

stubFnMock :: Mock fn v -> fn
stubFnMock (Mock f _) = f
stubFnMock (NamedMock _ f _) = f

{- | Create a stub function.
  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub function" do
    f \<- createStubFn $ "value" |\> True
    f "value" \`shouldBe\` True
  @
-}
createStubFn ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  params ->
  m fn
createStubFn params = stubFn <$> createMock params

-- | Create a named stub function.
createNamedStubFn ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  String ->
  params ->
  m fn
createNamedStubFn name params = stubFn <$> createNamedMock name params







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

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (AppliedRecord a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  appendAppliedParams ref actual

validateParams :: (Eq a, Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual
    then pure ()
    else errorWithoutStackTrace $ message name expected actual



messageForMultiMock :: Show a => Maybe MockName -> [a] -> a -> String
messageForMultiMock name expecteds actual =
  let fmtExpected e =
        let s = show e
            -- if it's parenthesised compound, strip outer parens then quote inner alpha tokens
            inner = if not (null s) && head s == '(' && last s == ')' then init (tail s) else s
            tokens = map (trim . quoteToken . trim) (splitByComma inner)
         in intercalate "," tokens
   in intercalate
        "\n"
        [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
          "  expected one of the following:",
          intercalate "\n" $ ("    " <>) . fmtExpected <$> expecteds,
          "  but got:",
          ("    " <>) . fmtExpected $ actual
        ]


-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  -- Generic over mock representation `m` which must satisfy `IsMock` and
  -- whose `MockParams m` match this class's `params`.
  shouldApplyTo :: (IsMock m, MockParams m ~ params, HasCallStack) => m -> input -> IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

verify :: (IsMock m, Eq (MockParams m), Show (MockParams m)) => m -> VerifyMatchType (MockParams m) -> IO ()
verify m matchType = do
  let Verifier ref = mockVerifier m
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerify (mockName m) appliedParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)


doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a


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
  shouldApplyTimes :: (IsMock m, MockParams m ~ params, HasCallStack, Eq params) => m -> countType -> a -> IO ()

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

verifyCount :: (IsMock m, Eq (MockParams m)) => m -> MockParams m -> CountVerifyMethod -> IO ()
verifyCount m v method = do
  let Verifier ref = mockVerifier m
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length (filter (v ==) appliedParamsList)
  if compareCount method appliedCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel (mockName m) <> " was not applied the expected number of times to the expected arguments.",
            "  expected: " <> show method,
            "   but got: " <> show appliedCount
          ]

to :: (a -> IO ()) -> a -> IO ()
to f = f

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
  shouldApplyInOrder :: (IsMock m, MockParams m ~ params, HasCallStack) => m -> [input] -> IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder :: (IsMock m, MockParams m ~ params, HasCallStack) => m -> [input] -> IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  shouldApplyInOrder v a = verifyOrder ExactlySequence v $ param <$> a
  shouldApplyInPartialOrder v a = verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  shouldApplyInOrder = verifyOrder ExactlySequence
  shouldApplyInPartialOrder = verifyOrder PartiallySequence

verifyOrder ::
  (IsMock m, Eq (MockParams m), Show (MockParams m)) =>
  VerifyOrderMethod ->
  m ->
  [MockParams m] ->
  IO ()
verifyOrder method m matchers = do
  let Verifier ref = mockVerifier m
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerifyOrder method (mockName m) appliedParamsList matchers
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

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, appliedValue, expectedValue} =
  let appliedCount = showHumanReadable (index + 1)
   in intercalate
        "\n"
        [ "  expected " <> appliedCount <> " applied: " <> show expectedValue,
          "   but got " <> appliedCount <> " applied: " <> show appliedValue
        ]
  where
    showHumanReadable :: Int -> String
    showHumanReadable 1 = "1st"
    showHumanReadable 2 = "2nd"
    showHumanReadable 3 = "3rd"
    showHumanReadable n = show n <> "th"

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
  (VerifyCount CountVerifyMethod params a, Eq params, IsMock m, MockParams m ~ params) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThanEqual m i = shouldApplyTimes m (GreaterThanEqual i)

-- | Verify that the function is applied to the expected arguments less than or equal to the expected number of times.
shouldApplyTimesLessThanEqual ::
  (VerifyCount CountVerifyMethod params a, Eq params, IsMock m, MockParams m ~ params) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThanEqual m i = shouldApplyTimes m (LessThanEqual i)

-- | Verify that the function has been applied to the expected arguments a greater number of times than expected.
shouldApplyTimesGreaterThan ::
  (VerifyCount CountVerifyMethod params a, Eq params, IsMock m, MockParams m ~ params) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThan m i = shouldApplyTimes m (GreaterThan i)

-- | Verify that the function has been applied to the expected arguments less than the expected number of times.
shouldApplyTimesLessThan ::
  (VerifyCount CountVerifyMethod params a, Eq params, IsMock m, MockParams m ~ params) =>
  m ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThan m i = shouldApplyTimes m (LessThan i)



readAppliedParamsList :: IORef (AppliedRecord params) -> IO (AppliedParamsList params)
readAppliedParamsList ref = do
  record <- readIORef ref
  pure $ appliedParamsList record

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

-- | Verify that it was apply to anything.
shouldApplyToAnything :: (IsMock m, MockParams m ~ params, HasCallStack) => m -> IO ()
shouldApplyToAnything m = do
  let Verifier ref = mockVerifier m
  appliedParamsList <- readAppliedParamsList ref
  when (null appliedParamsList) $ error $ "It has never been applied function" <> mockNameLabel (mockName m)

-- | Verify that it was apply to anything (times).
shouldApplyTimesToAnything :: (IsMock m, MockParams m ~ params) => m -> Int -> IO ()
shouldApplyTimesToAnything m count = do
  let Verifier ref = mockVerifier m
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length appliedParamsList
  when (count /= appliedCount) $
        errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel (mockName m) <> " was not applied the expected number of times.",
            "  expected: " <> show count,
            "   but got: " <> show appliedCount
          ]



runCase :: Cases a b -> [a]
runCase (Cases s) = execState s []

{- | Make a case for stub functions.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ do
        onCase $ "a" |> "return x"
        onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
onCase :: a -> Cases a ()
onCase a = Cases $ do
  st <- get
  put (st ++ [a])

{- | Make a list of patterns of arguments and returned values.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ cases [
        "a" |> "return x",
        "b" |> "return y"
      ]

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
cases :: [a] -> Cases a ()
cases a = Cases $ put a

{- | IO version of @'cases'@.  
@casesIO ["a", ""]@ has the same meaning as @cases [ pure \@IO "a", pure \@IO ""]@.
-}
casesIO :: [a] -> Cases (IO a) ()
casesIO = Cases . (put . map pure)



-- ------------------


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

makeMockIO :: MonadIO m => Maybe MockName -> IORef (AppliedRecord params) -> fn -> m (MockIO m fn params)
makeMockIO Nothing l fn = pure $ MockIO fn (Verifier l)
makeMockIO (Just name) l fn = pure $ NamedMockIO name fn (Verifier l)

createMockIO ::
  forall params fn fnM verifyParams m.
  ( MonadIO m
  , MockIOBuilder params fn verifyParams
  , LiftFunTo fn fnM m
  ) =>
  params ->
  m (MockIO m fnM verifyParams)
createMockIO params = do
  mockIO <- liftIO (buildIO Nothing params)
  pure $ widenMock mockIO

stubFnMockIO :: MockIO m fn params -> fn
stubFnMockIO (MockIO f _) = f
stubFnMockIO (NamedMockIO _ f _) = f

createStubFnIO ::
  forall params fn verifyParams m fnM.
  ( MockIOBuilder params fn verifyParams
  , MonadIO m
  , LiftFunTo fn fnM m
  ) =>
  params ->
  m fnM
createStubFnIO params = stubFnMockIO <$> createMockIO params

class LiftFunTo funIO funM (m :: Type -> Type) | funIO m -> funM where
  liftFunTo :: Proxy m -> funIO -> funM

instance MonadIO m => LiftFunTo (IO r) (m r) m where
  liftFunTo _ = liftIO

instance LiftFunTo restIO restM m => LiftFunTo (a -> restIO) (a -> restM) m where
  liftFunTo p f a = liftFunTo p (f a)

widenMock ::
  forall m funIO funM params.
  LiftFunTo funIO funM m => 
  MockIO IO funIO params ->
  MockIO m funM params
widenMock (MockIO f verifier) = MockIO (liftFunTo (Proxy :: Proxy m) f) verifier
widenMock (NamedMockIO name f verifier) = NamedMockIO name (liftFunTo (Proxy :: Proxy m) f) verifier


