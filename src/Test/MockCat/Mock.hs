{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}
module Test.MockCat.Mock
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
    verifyPartiallySequence,
    hasBeenCalledTimesGreaterThanEqual,
    hasBeenCalledTimesLessThanEqual,
    hasBeenCalledTimesGreaterThan,
    hasBeenCalledTimesLessThan,
    namedMock,
    module Test.MockCat.Cons,
    module Test.MockCat.Param,
    mockFun,
    namedMockFun,
  )
where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (elemIndex, find, intercalate)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.ParamDivider

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)

type MockName = String

newtype Verifier params = Verifier (IORef (CalledParamsList params))

mock ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m (Mock fun verifyParams)
mock params = liftIO $ build Nothing params

namedMock ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  MockName ->
  params ->
  m (Mock fun verifyParams)
namedMock name params = liftIO $ build (Just name) params

fun :: Mock fun v -> fun
fun (Mock _ f _) = f

mockFun ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m fun
mockFun params = fun <$> mock params

namedMockFun ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  String ->
  params ->
  m fun
namedMockFun name params = fun <$> namedMock name params

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: MonadIO m => Maybe MockName -> params -> m (Mock fun verifyParams)

-- instances
instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 h2 i2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 h2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock
      name
      s
      ( \a2 b2 c2 d2 e2 f2 g2 ->
          unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s
      )

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param r)
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder (Param a :> Param b :> Param c :> Param r) (a -> b -> c -> r) (Param a :> Param b :> Param c)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder (Param a :> Param b :> Param r) (a -> b -> r) (Param a :> Param b)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder (Param a :> Param r) (a -> r) (Param a)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 g2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r]
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param r]
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param r]
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param r]
    (a -> b -> c -> r)
    (Param a :> Param b :> Param c)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 c2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder [Param a :> Param b :> Param r] (a -> b -> r) (Param a :> Param b)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 b2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder [Param a :> Param r] (a -> r) (Param a)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2) s)

-- ------

p :: a -> Param a
p = param

createMock :: MonadIO m => Maybe MockName -> IORef (CalledParamsList params) -> fun -> m (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

type CalledParamsList params = [params]

extractReturnValueWithValidate ::
  ParamDivider params args (Param r) =>
  Eq args =>
  Show args =>
  Maybe MockName ->
  params ->
  args ->
  IORef (CalledParamsList args) ->
  IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (args params) inputParams
  pure $ returnValue params

findReturnValueWithStore ::
  Eq args =>
  Show args =>
  ParamDivider params args (Param r) =>
  Maybe MockName ->
  CalledParamsList params ->
  args ->
  IORef (CalledParamsList args) ->
  IO r
findReturnValueWithStore name paramsList inputParams ref = do
  modifyIORef' ref (++ [inputParams])
  let expectedArgs = args <$> paramsList
      r = findReturnValue paramsList inputParams
  maybe
    (errorWithoutStackTrace $ messageForMultiMock name expectedArgs inputParams)
    pure
    r

findReturnValue ::
  Eq args =>
  ParamDivider params args (Param r) =>
  CalledParamsList params ->
  args ->
  Maybe r
findReturnValue paramsList inputParams = do
  find (\params -> args params == inputParams) paramsList
    >>= \params -> pure $ returnValue params

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (CalledParamsList a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  modifyIORef' ref (++ [actual])

validateParams :: (Eq a, Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual
    then pure ()
    else errorWithoutStackTrace $ message name expected actual

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "expected arguments were not applied to the function" <> mockNameLabel name <> ".",
      "  expected: " <> show expected,
      "   but got: " <> show actual
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
  intercalate
    "\n"
    [ "expected arguments were not applied to the function" <> mockNameLabel name <> ".",
      "  expected one of the following:",
      intercalate "\n" $ ("    " <>) . show <$> expecteds,
      "  but got:",
      ("    " <>) . show $ actual
    ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = maybe mempty (" " <>) . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- verify
data VerifyMatchType a = MatchAny a | MatchAll a

class Verify params input where
  verify :: MonadIO m => Mock fun params -> input -> m ()

instance (Eq a, Show a) => Verify (Param a) a where
  verify v a = _verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  verify v a = _verify v (MatchAny a)

_verify :: (Eq params, Show params, MonadIO m) => Mock fun params -> VerifyMatchType params -> m ()
_verify (Mock name _ (Verifier ref)) matchType = do
  calledParamsList <- liftIO $ readIORef ref
  let result = doVerify name calledParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

newtype VerifyFailed = VerifyFailed Message

type Message = String

doVerify :: (Eq a, Show a) => Maybe MockName -> CalledParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: Show a => Maybe MockName -> CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "expected arguments were not applied to the function" <> mockNameLabel name <> ".",
        "  expected: " <> show expected,
        "   but got: " <> formatCalledParamsList calledParams
      ]

formatCalledParamsList :: Show a => CalledParamsList a -> String
formatCalledParamsList calledParams
  | length calledParams == 0 = "Never been called."
  | length calledParams == 1 = init . drop 1 . show $ calledParams
  | otherwise = show calledParams

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

hasBeenCalledWith ::
  (Verify params input) =>
  Mock fun params ->
  input ->
  IO ()
hasBeenCalledWith = verify

class VerifyCount countType params a where
  verifyCount :: (MonadIO m, Eq params) => Mock fun params -> countType -> a -> m ()

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

_verifyCount :: (MonadIO m, Eq params) => Mock fun params -> params -> CountVerifyMethod -> m ()
_verifyCount (Mock name _ (Verifier ref)) v method = do
  calledParamsList <- liftIO $ readIORef ref
  let callCount = length (filter (v ==) calledParamsList)
  if compareCount method callCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "function" <> mockNameLabel name <> " was not applied the expected number of times.",
            "  expected: " <> show method,
            "   but got: " <> show callCount
          ]

hasBeenCalledTimes ::
  VerifyCount countType params a =>
  Eq params =>
  MonadIO m =>
  Mock fun params ->
  countType ->
  a ->
  m ()
hasBeenCalledTimes = verifyCount

with :: (a -> IO ()) -> a -> IO ()
with f = f

class VerifyOrder params input where
  verifySequence :: MonadIO m => Mock fun params -> [input] -> m ()
  verifyPartiallySequence :: MonadIO m => Mock fun params -> [input] -> m ()

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
  Eq params =>
  Show params =>
  MonadIO m =>
  VerifyOrderMethod ->
  Mock fun params ->
  [params] ->
  m ()
_verifyOrder method (Mock name _ (Verifier ref)) matchers = do
  calledParamsList <- liftIO $ readIORef ref
  let result = doVerifyOrder method name calledParamsList matchers
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerifyOrder :: 
  Eq a =>
  Show a => 
  VerifyOrderMethod -> 
  Maybe MockName -> 
  CalledParamsList a -> 
  [a] -> 
  Maybe VerifyFailed
doVerifyOrder ExactlySequence name calledValues expectedValues
  | length calledValues /= length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name calledValues expectedValues
  | otherwise = do
      let unexpectedOrders = collectUnExpectedOrder calledValues expectedValues
      guard $ length unexpectedOrders > 0
      pure $ verifyFailedSequence name unexpectedOrders
doVerifyOrder PartiallySequence name calledValues expectedValues
  | length calledValues < length expectedValues = do
      pure $ verifyFailedOrderParamCountMismatch name calledValues expectedValues
  | otherwise = do
      guard $ isOrderNotMatched calledValues expectedValues
      pure $ verifyFailedPartiallySequence name calledValues expectedValues

verifyFailedPartiallySequence :: Show a => Maybe MockName -> CalledParamsList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied with expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  but got:",
        intercalate "\n" $ ("    " <>) . show <$> calledValues
      ]

isOrderNotMatched :: Eq a => CalledParamsList a -> [a] -> Bool
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

verifyFailedOrderParamCountMismatch :: Maybe MockName -> CalledParamsList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied the expected number of times.",
        "  expected: " <> show (length expectedValues),
        "   but got: " <> show (length calledValues)
      ]

verifyFailedSequence :: Show a => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("function" <> mockNameLabel name <> " was not applied with expected order.") : (verifyOrderFailedMesssage <$> fails)
      )

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, calledValue, expectedValue} =
  let callCount = showHumanReadable (index + 1)
   in intercalate
        "\n"
        [ "  expected " <> callCount <> " call: " <> show expectedValue,
          "   but got " <> callCount <> " call: " <> show calledValue
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

collectUnExpectedOrder :: Eq a => CalledParamsList a -> [a] -> [VerifyOrderResult a]
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
  VerifyOrder params input =>
  MonadIO m =>
  Mock fun params ->
  [input] ->
  m ()
hasBeenCalledInOrder = verifySequence

hasBeenCalledInPartialOrder ::
  VerifyOrder params input =>
  MonadIO m =>
  Mock fun params ->
  [input] ->
  m ()
hasBeenCalledInPartialOrder = verifyPartiallySequence

hasBeenCalledTimesGreaterThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  MonadIO m =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  m ()
hasBeenCalledTimesGreaterThanEqual m i = hasBeenCalledTimes m (GreaterThanEqual i)

hasBeenCalledTimesLessThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  MonadIO m =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  m ()
hasBeenCalledTimesLessThanEqual m i = hasBeenCalledTimes m (LessThanEqual i)

hasBeenCalledTimesGreaterThan ::
  VerifyCount CountVerifyMethod params a =>
  MonadIO m =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  m ()
hasBeenCalledTimesGreaterThan m i = hasBeenCalledTimes m (GreaterThan i)

hasBeenCalledTimesLessThan ::
  VerifyCount CountVerifyMethod params a =>
  MonadIO m =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  m ()
hasBeenCalledTimesLessThan m i = hasBeenCalledTimes m (LessThan i)