{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

{- | This module provides the following functions.

  - Create mocks that can be stubbed and verified.

  - Create stub function.

  - Verify applied mock function.
-}
module Test.MockCat.Mock
  ( Mock,
    MockBuilder,
    build,
    createMock,
    createNamedMock,
    createConstantMock,
    createNamedConstantMock,
    createStubFn,
    createNamedStubFn,
    stubFn,
    shouldApplyTo,
    shouldApplyTimes,
    shouldApplyInOrder,
    shouldApplyInPartialOrder,
    shouldApplyTimesGreaterThanEqual,
    shouldApplyTimesLessThanEqual,
    shouldApplyTimesGreaterThan,
    shouldApplyTimesLessThan,
    shouldApplyToAnything,
    shouldApplyTimesToAnything,
    to,
    onCase,
    cases,
    casesIO
  )
where

import Control.Monad (guard, when, ap)
import Data.Function ((&))
import Data.Char (isLower)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.AssociationList (AssociationList, lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans
import Control.Monad.State

data Mock fn params = Mock (Maybe MockName) fn (Verifier params)

type MockName = String

newtype Verifier params = Verifier (IORef (AppliedRecord params))

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
stubFn :: Mock fn v -> fn
stubFn (Mock _ f _) = f

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

-- | Base case: The last parameter.
-- Converts a single-argument function (Param a -> IO r) into a final
-- curried function (a -> r) by performing the IO action.
instance BuildCurried (Param a) r (a -> r) where
  buildCurried :: (Param a -> IO r) -> a -> r
  buildCurried a2r a = perform $ a2r (param a)

-- | Recursive case: Consumes the head parameter and generates the next curried function.
-- Generates a function of type (a -> fn) that immediately calls the next
-- 'BuildCurried' instance with the remaining arguments.
instance BuildCurried rest r fn
      => BuildCurried (Param a :> rest) r (a -> fn) where
  buildCurried :: ((Param a :> rest) -> IO r) -> a -> fn
  buildCurried args2r a = buildCurried (\rest -> args2r (p a :> rest))

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

p :: a -> Param a
p = param

makeMock :: MonadIO m => Maybe MockName -> IORef (AppliedRecord params) -> fn -> m (Mock fn params)
makeMock name l fn = pure $ Mock name fn (Verifier l)

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

-- Helper: quote a token if it looks like an unquoted alpha token
quoteToken :: String -> String
quoteToken s
  | null s = s
  | head s == '"' = s
  | head s == '(' = s
  | head s == '[' = s
  | not (null s) && isLower (head s) = '"' : s ++ "\""
  | otherwise = s

-- Quote a show-produced string when appropriate for error messages.
showForMessage :: String -> String
showForMessage s =
  -- if it's a parenthesised compound, keep as-is; otherwise quote alpha-only tokens
  let trimmed = s
   in if not (null trimmed) && head trimmed == '(' && last trimmed == ')'
        then trimmed
        else quoteToken trimmed

message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
      "  expected: " <> showForMessage (show expected),
      "   but got: " <> showForMessage (show actual)
    ]

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

mockNameLabel :: Maybe MockName -> String
mockNameLabel = maybe mempty (" " <>) . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- verify
data VerifyMatchType a = MatchAny a | MatchAll a

-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  shouldApplyTo :: HasCallStack => Mock fn params -> input -> IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

verify :: (Eq params, Show params) => Mock fn params -> VerifyMatchType params -> IO ()
verify (Mock name _ (Verifier ref)) matchType = do
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerify name appliedParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

newtype VerifyFailed = VerifyFailed Message

type Message = String

doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a

verifyFailedMessage :: Show a => Maybe MockName -> AppliedParamsList a -> a -> VerifyFailed
verifyFailedMessage name appliedParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
        "  expected: " <> showForMessage (show expected),
        "   but got: " <> formatAppliedParamsList appliedParams
      ]

-- utilities for message formatting
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitByComma rest
  (a, _) -> [a]

formatAppliedParamsList :: Show a => AppliedParamsList a -> String
formatAppliedParamsList appliedParams
  | null appliedParams = "It has never been applied"
  | length appliedParams == 1 =
    -- show single element without surrounding list brackets, but quote tokens appropriately
    let s = show (head appliedParams)
        inner = if not (null s) && head s == '(' && last s == ')' then init (tail s) else s
        tokens = map (trim . quoteToken . trim) (splitByComma inner)
     in intercalate "," tokens
  | otherwise =
    -- for multiple applied params, show as a list but ensure tokens are quoted where appropriate
    let ss = map show appliedParams
        processed = map (\t -> let inner = if not (null t) && head t == '(' && last t == ')' then init (tail t) else t
                                in intercalate "," $ map (trim . quoteToken . trim) (splitByComma inner)) ss
     in show processed

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

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
  shouldApplyTimes :: HasCallStack => Eq params => Mock fn params -> countType -> a -> IO ()

instance VerifyCount CountVerifyMethod (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) count

instance VerifyCount Int (Param a) a where
  shouldApplyTimes v count a = verifyCount v (param a) (Equal count)

instance {-# OVERLAPPABLE #-} VerifyCount CountVerifyMethod a a where
  shouldApplyTimes v count a = verifyCount v a count

instance {-# OVERLAPPABLE #-} VerifyCount Int a a where
  shouldApplyTimes v count a = verifyCount v a (Equal count)

data CountVerifyMethod
  = Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int

instance Show CountVerifyMethod where
  show (Equal e) = show e
  show (LessThanEqual e) = "<= " <> show e
  show (LessThan e) = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e) = "> " <> show e

compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a = a == e
compareCount (LessThanEqual e) a = a <= e
compareCount (LessThan e) a = a < e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a = a > e

verifyCount :: Eq params => Mock fn params -> params -> CountVerifyMethod -> IO ()
verifyCount (Mock name _ (Verifier ref)) v method = do
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length (filter (v ==) appliedParamsList)
  if compareCount method appliedCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> " was not applied the expected number of times to the expected arguments.",
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
  shouldApplyInOrder :: HasCallStack => Mock fn params -> [input] -> IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder :: HasCallStack => Mock fn params -> [input] -> IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  shouldApplyInOrder v a = verifyOrder ExactlySequence v $ param <$> a
  shouldApplyInPartialOrder v a = verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  shouldApplyInOrder = verifyOrder ExactlySequence
  shouldApplyInPartialOrder = verifyOrder PartiallySequence

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence

verifyOrder ::
  (Eq params, Show params) =>
  VerifyOrderMethod ->
  Mock fn params ->
  [params] ->
  IO ()
verifyOrder method (Mock name _ (Verifier ref)) matchers = do
  appliedParamsList <- readAppliedParamsList ref
  let result = doVerifyOrder method name appliedParamsList matchers
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

data VerifyOrderResult a = VerifyOrderResult
  { index :: Int,
    appliedValue :: a,
    expectedValue :: a
  }

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
  (VerifyCount CountVerifyMethod params a, Eq params) =>
  Mock fn params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThanEqual m i = shouldApplyTimes m (GreaterThanEqual i)

-- | Verify that the function is applied to the expected arguments less than or equal to the expected number of times.
shouldApplyTimesLessThanEqual ::
  (VerifyCount CountVerifyMethod params a, Eq params) =>
  Mock fn params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThanEqual m i = shouldApplyTimes m (LessThanEqual i)

-- | Verify that the function has been applied to the expected arguments a greater number of times than expected.
shouldApplyTimesGreaterThan ::
  (VerifyCount CountVerifyMethod params a, Eq params) =>
  Mock fn params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThan m i = shouldApplyTimes m (GreaterThan i)

-- | Verify that the function has been applied to the expected arguments less than the expected number of times.
shouldApplyTimesLessThan ::
  (VerifyCount CountVerifyMethod params a, Eq params) =>
  Mock fn params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThan m i = shouldApplyTimes m (LessThan i)

type AppliedParamsList params = [params]
type AppliedParamsCounter params = AssociationList params Int

data AppliedRecord params = AppliedRecord {
  appliedParamsList :: AppliedParamsList params,
  appliedParamsCounter :: AppliedParamsCounter params
}

appliedRecord :: AppliedRecord params
appliedRecord = AppliedRecord {
  appliedParamsList = mempty,
  appliedParamsCounter = empty
}

readAppliedParamsList :: IORef (AppliedRecord params) -> IO (AppliedParamsList params)
readAppliedParamsList ref = do
  record <- readIORef ref
  pure $ appliedParamsList record

readAppliedCount :: Eq params => IORef (AppliedRecord params) -> params -> IO Int
readAppliedCount ref params = do
  record <- readIORef ref
  let count = appliedParamsCounter record
  pure $ fromMaybe 0 (lookup params count)

appendAppliedParams :: IORef (AppliedRecord params) -> params -> IO ()
appendAppliedParams ref inputParams = do
  atomicModifyIORef' ref (\AppliedRecord {appliedParamsList, appliedParamsCounter} ->
    let newRecord = AppliedRecord {
          appliedParamsList = appliedParamsList ++ [inputParams],
          appliedParamsCounter = appliedParamsCounter
        }
    in (newRecord, ()))

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

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)

-- | Verify that it was apply to anything.
shouldApplyToAnything :: HasCallStack => Mock fn params -> IO ()
shouldApplyToAnything (Mock name _ (Verifier ref)) = do
  appliedParamsList <- readAppliedParamsList ref
  when (null appliedParamsList) $ error $ "It has never been applied function" <> mockNameLabel name

-- | Verify that it was apply to anything (times).
shouldApplyTimesToAnything :: Mock fn params -> Int -> IO ()
shouldApplyTimesToAnything (Mock name _ (Verifier ref)) count = do
  appliedParamsList <- readAppliedParamsList ref
  let appliedCount = length appliedParamsList
  when (count /= appliedCount) $
        errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> " was not applied the expected number of times.",
            "  expected: " <> show count,
            "   but got: " <> show appliedCount
          ]

newtype Cases a b = Cases (State [a] b)

instance Functor (Cases a) where
  fmap f (Cases s) = Cases (fmap f s)

instance Applicative (Cases a) where
  pure x = Cases $ pure x
  (<*>) = ap

instance Monad (Cases a) where
  (Cases m) >>= f = Cases $ do
    result <- m
    let (Cases newState) = f result
    newState

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

{-# NOINLINE perform #-}
perform :: IO a -> a
perform = unsafePerformIO