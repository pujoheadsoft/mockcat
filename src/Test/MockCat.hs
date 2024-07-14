{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}
module Test.MockCat
  ( mock,
    fun,
    verify,
    hasBeenCalledWith,
    verifyCount,
    hasBeenCalledTimes,
    with,
    hasBeenCalledInOrder,
    verifySequence,
    hasBeenCalledInPartialOrder,
    verifyPartiallySequence
  )
where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, intercalate)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param hiding (any)
import Test.MockCat.ParamDivider
import Data.Function ((&))
import Data.List (find)

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)

type MockName = String

newtype Verifier params = Verifier (IORef (CalledParamsList params))

mock ::
  (MockBuilder params fun verifyParams) =>
  (MonadIO m) =>
  params ->
  m (Mock fun verifyParams)
mock params = liftIO $ build Nothing params

fun :: Mock fun v -> fun
fun (Mock _ f _) = f

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> IO (Mock fun verifyParams)

-- instances
instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 i2 ->
      unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 ->
      unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 ->
      unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param r)
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a :> Param b :> Param c :> Param r) (a -> b -> c -> r) (Param a :> Param b :> Param c) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2) s)

instance (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a :> Param b :> Param r) (a -> b -> r) (Param a :> Param b) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder (Param a :> Param r) (a -> r) (Param a)
  where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r]
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param r]
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param r]
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder
    [Param a :> Param b :> Param c :> Param r]
    (a -> b -> c -> r)
    (Param a :> Param b :> Param c) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2) s)

instance (Show a, Eq a, Show b, Eq b)
  => MockBuilder [Param a :> Param b :> Param r] (a -> b -> r) (Param a :> Param b) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2) s)

instance (Show a, Eq a)
  => MockBuilder [Param a :> Param r] (a -> r) (Param a) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2) s)

-- ------

p :: a -> Param a
p = param

createMock :: Maybe MockName -> IORef (CalledParamsList params) -> fun -> IO (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

type CalledParamsList params = [params]

extractReturnValueWithValidate ::
  (ParamDivider params args (Param r)) =>
  (Eq args) =>
  (Show args) =>
  Maybe MockName ->
  params ->
  args ->
  IORef (CalledParamsList args) ->
  IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (args params) inputParams
  pure $ returnValue params

findReturnValueWithStore ::
     Eq args
  => Show args
  => ParamDivider params args (Param r)
  => Maybe MockName
  -> CalledParamsList params
  -> args
  -> IORef (CalledParamsList args)
  -> IO r
findReturnValueWithStore name paramsList inputParams ref = do
  modifyIORef' ref (++ [inputParams])
  let 
    expectedArgs = args <$> paramsList
    r =  findReturnValue paramsList inputParams
  maybe 
    (errorWithoutStackTrace $ messageForMultiMock name expectedArgs inputParams)
    pure
    r

findReturnValue ::
     Eq args
  => ParamDivider params args (Param r)
  => CalledParamsList params
  -> args
  -> Maybe r
findReturnValue paramsList inputParams = do
  find (\params -> args params == inputParams) paramsList
    >>= \params -> pure $ returnValue params

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (CalledParamsList a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  modifyIORef' ref (++ [actual])

validateParams :: (Eq a) => (Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual then pure ()
  else errorWithoutStackTrace $ message name expected actual

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: (Show a) => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "function" <> mockNameLabel name <> "was not called with expected arguments.",
      "  expected: " <> show expected,
      "  but was : " <> show actual
    ]

{-
  Function was not called with expected arguments.
  expected one of the following:
    "a", 100
    "b", 200
  but was actual: "a", 200
-}
messageForMultiMock :: Show a => Maybe MockName -> [a] -> a -> String
messageForMultiMock name expecteds actual =
  intercalate "\n" [
    "function" <> mockNameLabel name <> "was not called with expected arguments.",
    "  expected one of the following:",
    intercalate "\n" $ ("    " <>) . show <$> expecteds,
    "  but was actual:",
    ("    " <>) . show $ actual
  ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = fromMaybe " " . enclose " " . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- verify
data VerifyMatchType a = MatchAny a | MatchAll a

class Verify params input where
  verify :: Mock fun params -> input -> IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  verify v a = _verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  verify v a = _verify v (MatchAny a)

_verify :: (Eq params) => (Show params) => Mock fun params -> VerifyMatchType params -> IO ()
_verify (Mock name _ (Verifier ref)) matchType = do
  calledParamsList <- liftIO $ readIORef ref
  let result = doVerify name calledParamsList matchType
  result & maybe mempty (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

newtype VerifyFailed = VerifyFailed Message

type Message = String

doVerify :: (Eq a, Show a) => Maybe MockName -> CalledParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ any (a /=) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: (Show a) => Maybe MockName -> CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> "wasn't called with expected arguments.",
        "  expected: " <> show expected,
        "  but was : " <> formatCalledParamsList calledParams
      ]

formatCalledParamsList :: (Show a) => CalledParamsList a -> String
formatCalledParamsList calledParams
  | length calledParams == 0 = "Never been called."
  | length calledParams == 1 = show $ (_replace "[" . _replace "]") calledParams
  | otherwise = show calledParams

_replace :: (Show a) => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

hasBeenCalledWith ::
  (Verify params input) =>
  Mock fun params ->
  input ->
  IO ()
hasBeenCalledWith = verify

class VerifyCount countType params a where
  verifyCount :: (Eq params) => Mock fun params -> countType -> a -> IO ()

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

instance VerifyCount CountVerifyMethod (Param a) a where
  verifyCount v count a = _verifyCount v (param a) count

instance VerifyCount Int (Param a) a where
  verifyCount v count a = _verifyCount v (param a) (Equal count)

instance {-# OVERLAPPABLE #-} VerifyCount CountVerifyMethod a a where
  verifyCount v count a = _verifyCount v a count

instance {-# OVERLAPPABLE #-} VerifyCount Int a a where
  verifyCount v count a = _verifyCount v a (Equal count)

_verifyCount :: (Eq params) => Mock fun params -> params -> CountVerifyMethod -> IO ()
_verifyCount (Mock name _ (Verifier ref)) v method = do
  calledParamsList <- readIORef ref
  let callCount = length (filter (v ==) calledParamsList)
  if compareCount method callCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> "was not called the expected number of times.",
            "  expected: " <> show method,
            "  but was : " <> show callCount
          ]

hasBeenCalledTimes ::
  (VerifyCount countType params a) =>
  (Eq params) =>
  Mock fun params ->
  countType ->
  a ->
  IO ()
hasBeenCalledTimes = verifyCount

with :: (a -> IO ()) -> a -> IO ()
with f = f

class VerifyOrder params input where
  verifySequence :: Mock fun params -> [input] -> IO ()
  verifyPartiallySequence :: Mock fun params -> [input] -> IO ()

instance (Eq a, Show a) => VerifyOrder (Param a) a where
  verifySequence v a = _verifyOrder ExactlySequence v $ param <$> a
  verifyPartiallySequence v a = _verifyOrder PartiallySequence v $ param <$> a

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => VerifyOrder a a where
  verifySequence = _verifyOrder ExactlySequence
  verifyPartiallySequence = _verifyOrder PartiallySequence

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence

_verifyOrder ::
  (Eq params) =>
  (Show params) =>
  VerifyOrderMethod ->
  Mock fun params ->
  [params] ->
  IO ()
_verifyOrder method (Mock name _ (Verifier ref)) matchers = do
  calledParamsList <- readIORef ref
  let result = doVerifyOrder method name calledParamsList matchers
  result & maybe mempty (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerifyOrder :: (Eq a, Show a) => VerifyOrderMethod -> Maybe MockName -> CalledParamsList a -> [a] -> Maybe VerifyFailed
doVerifyOrder ExactlySequence name calledValues expectedValues
  | length calledValues /= length expectedValues = do
      let header = "The number of function" <> mockNameLabel name <> "calls doesn't match the number of params."
      pure $ verifyFailedOrderParamCountMismatch header calledValues expectedValues
  | otherwise = do
      let unexpectedOrders = collectUnExpectedOrder calledValues expectedValues
      guard $ length unexpectedOrders > 0
      pure $ verifyFailedSequence name unexpectedOrders
doVerifyOrder PartiallySequence name calledValues expectedValues
  | length calledValues < length expectedValues = do
      let header = "The number of parameters exceeds the number of function" <> mockNameLabel name <> "calls."
      pure $ verifyFailedOrderParamCountMismatch header calledValues expectedValues
  | otherwise = do
      guard $ isOrderNotMatched calledValues expectedValues
      pure $ verifyFailedPartiallySequence name calledValues expectedValues

verifyFailedPartiallySequence :: (Show a) => Maybe MockName -> CalledParamsList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> "was not called with expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  actual order:",
        intercalate "\n" $ ("    " <>) . show <$> calledValues
      ]

isOrderNotMatched :: (Eq a) => CalledParamsList a -> [a] -> Bool
isOrderNotMatched calledValues expectedValues =
  isNothing $
    foldl
      ( \candidates e -> do
          candidates >>= \c -> do
            index <- elemIndex e c
            Just $ drop (index + 1) c
      )
      (Just calledValues)
      expectedValues

verifyFailedOrderParamCountMismatch :: String -> CalledParamsList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch header calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ header,
        "  number of function calls: " <> show (length calledValues),
        "  number of params:         " <> show (length expectedValues)
      ]

verifyFailedSequence :: (Show a) => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("function" <> mockNameLabel name <> "was not called with expected order.") : (verifyOrderFailedMesssage <$> fails)
      )

verifyOrderFailedMesssage :: (Show a) => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, calledValue, expectedValue} =
  let callCount = showHumanReadable (index + 1)
   in intercalate
        "\n"
        [ "  expected " <> callCount <> " call: " <> show expectedValue,
          "  but was  " <> callCount <> " call: " <> show calledValue
        ]
  where
    showHumanReadable :: Int -> String
    showHumanReadable 1 = "1st"
    showHumanReadable 2 = "2nd"
    showHumanReadable 3 = "3rd"
    showHumanReadable n = show n <> "th"

data VerifyOrderResult a = VerifyOrderResult
  { index :: Int,
    calledValue :: a,
    expectedValue :: a
  }

collectUnExpectedOrder :: (Eq a) => CalledParamsList a -> [a] -> [VerifyOrderResult a]
collectUnExpectedOrder calledValues expectedValues =
  catMaybes $
    mapWithIndex
      ( \i expectedValue -> do
          let calledValue = calledValues !! i
          guard $ expectedValue /= calledValue
          pure VerifyOrderResult {index = i, calledValue, expectedValue}
      )
      expectedValues

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = [f i x | (i, x) <- zip [0 ..] xs]

hasBeenCalledInOrder ::
  (VerifyOrder params input) =>
  Mock fun params ->
  [input] ->
  IO ()
hasBeenCalledInOrder = verifySequence

hasBeenCalledInPartialOrder ::
  (VerifyOrder params input) =>
  Mock fun params ->
  [input] ->
  IO ()
hasBeenCalledInPartialOrder = verifyPartiallySequence