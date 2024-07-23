{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}

{- | This module provides bellow functions.

  - Create mocks that can be stubbed and verified.

  - Create stub function.

  - Verify applied mock function.
-}
module Test.MockCat.Mock
  ( createMock,
    createNamedMock,
    createStubFn,
    createNamedStubFun,
    stubFn,       
    shouldApplyTo,
    shouldApplyTimes,
    shouldApplyInOrder,
    shouldApplyInPartialOrder,
    shouldApplyTimesGreaterThanEqual,
    shouldApplyTimesLessThanEqual,
    shouldApplyTimesGreaterThan,
    shouldApplyTimesLessThan,
    to,
    module Test.MockCat.Cons,
    module Test.MockCat.Param
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

newtype Verifier params = Verifier (IORef (AppliedParamsList params))

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
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m (Mock fun verifyParams)
createMock params = liftIO $ build Nothing params

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
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  MockName ->
  params ->
  m (Mock fun verifyParams)
createNamedMock name params = liftIO $ build (Just name) params

-- | Extract the stub function from the mock.
stubFn :: Mock fun v -> fun
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
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  params ->
  m fun
createStubFn params = stubFn <$> createMock params

-- | Create a named stub function.
createNamedStubFun ::
  MockBuilder params fun verifyParams =>
  MonadIO m =>
  String ->
  params ->
  m fun
createNamedStubFun name params = stubFn <$> createNamedMock name params

-- | Class for creating a mock corresponding to the parameter.
class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  -- build a mock
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
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock
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
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock
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
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock
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
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    (Param a :> Param b :> Param c :> Param d :> Param r)
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder (Param a :> Param b :> Param c :> Param r) (a -> b -> c -> r) (Param a :> Param b :> Param c)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder (Param a :> Param b :> Param r) (a -> b -> r) (Param a :> Param b)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder (Param a :> Param r) (a -> r) (Param a)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 h2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r]
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 f2 g2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r]
    (a -> b -> c -> d -> e -> f -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e :> Param f)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 f2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param e :> Param r]
    (a -> b -> c -> d -> e -> r)
    (Param a :> Param b :> Param c :> Param d :> Param e)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 e2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param d :> Param r]
    (a -> b -> c -> d -> r)
    (Param a :> Param b :> Param c :> Param d)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 d2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2 :> p d2) s)

instance
  (Show a, Eq a, Show b, Eq b, Show c, Eq c) =>
  MockBuilder
    [Param a :> Param b :> Param c :> Param r]
    (a -> b -> c -> r)
    (Param a :> Param b :> Param c)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 c2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2 :> p c2) s)

instance
  (Show a, Eq a, Show b, Eq b) =>
  MockBuilder [Param a :> Param b :> Param r] (a -> b -> r) (Param a :> Param b)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 b2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2 :> p b2) s)

instance
  (Show a, Eq a) =>
  MockBuilder [Param a :> Param r] (a -> r) (Param a)
  where
  build name params = do
    s <- liftIO $ newIORef ([] :: AppliedParamsList params)
    makeMock name s (\a2 -> unsafePerformIO $ findReturnValueWithStore name params (p a2) s)

-- ------

p :: a -> Param a
p = param

makeMock :: MonadIO m => Maybe MockName -> IORef (AppliedParamsList params) -> fun -> m (Mock fun params)
makeMock name l fn = pure $ Mock name fn (Verifier l)

type AppliedParamsList params = [params]

extractReturnValueWithValidate ::
  ParamDivider params args (Param r) =>
  Eq args =>
  Show args =>
  Maybe MockName ->
  params ->
  args ->
  IORef (AppliedParamsList args) ->
  IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (args params) inputParams
  pure $ returnValue params

findReturnValueWithStore ::
  Eq args =>
  Show args =>
  ParamDivider params args (Param r) =>
  Maybe MockName ->
  AppliedParamsList params ->
  args ->
  IORef (AppliedParamsList args) ->
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
  AppliedParamsList params ->
  args ->
  Maybe r
findReturnValue paramsList inputParams = do
  find (\params -> args params == inputParams) paramsList
    >>= \params -> pure $ returnValue params

validateWithStoreParams :: (Eq a, Show a) => Maybe MockName -> IORef (AppliedParamsList a) -> a -> a -> IO ()
validateWithStoreParams name ref expected actual = do
  validateParams name expected actual
  modifyIORef' ref (++ [actual])

validateParams :: (Eq a, Show a) => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual
    then pure ()
    else errorWithoutStackTrace $ message name expected actual

message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "Expected arguments were not applied to the function" <> mockNameLabel name <> ".",
      "  expected: " <> show expected,
      "   but got: " <> show actual
    ]

messageForMultiMock :: Show a => Maybe MockName -> [a] -> a -> String
messageForMultiMock name expecteds actual =
  intercalate
    "\n"
    [ "Expected arguments were not applied to the function" <> mockNameLabel name <> ".",
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

-- | Class for verifying mock function.
class Verify params input where
  -- | Verifies that the function has been applied to the expected arguments.
  shouldApplyTo :: Mock fun params -> input -> IO ()

instance (Eq a, Show a) => Verify (Param a) a where
  shouldApplyTo v a = verify v (MatchAny (param a))

instance (Eq a, Show a) => Verify a a where
  shouldApplyTo v a = verify v (MatchAny a)

verify :: (Eq params, Show params) => Mock fun params -> VerifyMatchType params -> IO ()
verify (Mock name _ (Verifier ref)) matchType = do
  calledParamsList <- readIORef ref
  let result = doVerify name calledParamsList matchType
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

newtype VerifyFailed = VerifyFailed Message

type Message = String

doVerify :: (Eq a, Show a) => Maybe MockName -> AppliedParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: Show a => Maybe MockName -> AppliedParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "Expected arguments were not applied to the function" <> mockNameLabel name <> ".",
        "  expected: " <> show expected,
        "   but got: " <> formatCalledParamsList calledParams
      ]

formatCalledParamsList :: Show a => AppliedParamsList a -> String
formatCalledParamsList calledParams
  | length calledParams == 0 = "Never been called."
  | length calledParams == 1 = init . drop 1 . show $ calledParams
  | otherwise = show calledParams

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
  shouldApplyTimes :: Eq params => Mock fun params -> countType -> a -> IO ()

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

verifyCount :: Eq params => Mock fun params -> params -> CountVerifyMethod -> IO ()
verifyCount (Mock name _ (Verifier ref)) v method = do
  calledParamsList <- readIORef ref
  let callCount = length (filter (v ==) calledParamsList)
  if compareCount method callCount
    then pure ()
    else
      errorWithoutStackTrace $
        intercalate
          "\n"
          [ "The expected argument was not applied the expected number of times to the function" <> mockNameLabel name <> ".",
            "  expected: " <> show method,
            "   but got: " <> show callCount
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
  shouldApplyInOrder :: Mock fun params -> [input] -> IO ()

  -- | Verify that functions are applied in the expected order.
  --
  -- Unlike @'shouldApplyInOrder'@, not all applications need to match exactly.
  --
  -- As long as the order matches, the verification succeeds.
  shouldApplyInPartialOrder :: Mock fun params -> [input] -> IO ()

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
  Eq params =>
  Show params =>
  VerifyOrderMethod ->
  Mock fun params ->
  [params] ->
  IO ()
verifyOrder method (Mock name _ (Verifier ref)) matchers = do
  calledParamsList <- readIORef ref
  let result = doVerifyOrder method name calledParamsList matchers
  result & maybe (pure ()) (\(VerifyFailed msg) -> errorWithoutStackTrace msg)

doVerifyOrder :: 
  Eq a =>
  Show a => 
  VerifyOrderMethod -> 
  Maybe MockName -> 
  AppliedParamsList a -> 
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

verifyFailedPartiallySequence :: Show a => Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "Expected arguments were not applied to the function" <> mockNameLabel name <> " in the expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  but got:",
        intercalate "\n" $ ("    " <>) . show <$> calledValues
      ]

isOrderNotMatched :: Eq a => AppliedParamsList a -> [a] -> Bool
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

verifyFailedOrderParamCountMismatch :: Maybe MockName -> AppliedParamsList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "Expected arguments were not applied to the function" <> mockNameLabel name <> " in the expected order (count mismatch).",
        "  expected: " <> show (length expectedValues),
        "   but got: " <> show (length calledValues)
      ]

verifyFailedSequence :: Show a => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("Expected arguments were not applied to the function" <> mockNameLabel name <> " in the expected order.") : (verifyOrderFailedMesssage <$> fails)
      )

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, calledValue, expectedValue} =
  let callCount = showHumanReadable (index + 1)
   in intercalate
        "\n"
        [ "  expected " <> callCount <> " applied: " <> show expectedValue,
          "   but got " <> callCount <> " applied: " <> show calledValue
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

collectUnExpectedOrder :: Eq a => AppliedParamsList a -> [a] -> [VerifyOrderResult a]
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

shouldApplyTimesGreaterThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThanEqual m i = shouldApplyTimes m (GreaterThanEqual i)

shouldApplyTimesLessThanEqual ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThanEqual m i = shouldApplyTimes m (LessThanEqual i)

shouldApplyTimesGreaterThan ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesGreaterThan m i = shouldApplyTimes m (GreaterThan i)

shouldApplyTimesLessThan ::
  VerifyCount CountVerifyMethod params a =>
  Eq params =>
  Mock fun params ->
  Int ->
  a ->
  IO ()
shouldApplyTimesLessThan m i = shouldApplyTimes m (LessThan i)