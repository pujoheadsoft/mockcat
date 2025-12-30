{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.MockCat.Internal.Verify where

import Control.Concurrent.STM (readTVarIO, TVar)
import Control.Monad (guard, when)
import Data.List (intercalate, elemIndex)
import Data.Maybe (listToMaybe, catMaybes, isNothing)
import Test.MockCat.Internal.Types
import Test.MockCat.Internal.Message
import Test.MockCat.AssociationList (AssociationList)
import Prelude hiding (lookup)

-- | Verify an expectation directly against a resolved mock.
--   This is used by 'mock' when expectations are provided.
verifyExpectationDirect :: (Eq params, Show params) => ResolvedMock params -> Expectation params -> IO ()
verifyExpectationDirect resolved (CountExpectation method args) =
  verifyResolvedCount resolved args method
verifyExpectationDirect resolved (CountAnyExpectation method) =
  verifyResolvedCallCount resolved method
verifyExpectationDirect resolved (OrderExpectation method matchers) =
  verifyResolvedOrder method resolved matchers
verifyExpectationDirect resolved (SimpleExpectation args) =
  verifyResolvedMatch resolved (MatchAny args)
verifyExpectationDirect resolved AnyExpectation =
  verifyResolvedAny resolved

-- | Verify that a resolved mock function was called at least once.
verifyResolvedAny :: ResolvedMock params -> IO ()
verifyResolvedAny (ResolvedMock mockName recorder) = do
  invocationList <- readInvocationList (invocationRef recorder)
  when (null invocationList) $
    errorWithoutStackTrace $
      intercalate
        "\n"
        [ "Function" <> mockNameLabel mockName <> " was never called"
        ]

-- | Verify that mock was called with specific arguments using resolved mock directly.
verifyResolvedMatch :: (Eq params, Show params) => ResolvedMock params -> VerifyMatchType params -> IO ()
verifyResolvedMatch (ResolvedMock mockName recorder) matchType = do
  invocationList <- readInvocationList (invocationRef recorder)
  case doVerify mockName invocationList matchType of
    Nothing -> pure ()
    Just (VerifyFailed msg) ->
      errorWithoutStackTrace msg `seq` pure ()

-- | Verify call count with specific arguments using resolved mock directly.
verifyResolvedCount :: (Eq params, Show params) => ResolvedMock params -> params -> CountVerifyMethod -> IO ()
verifyResolvedCount (ResolvedMock mockName recorder) v method = do
  invocationList <- readInvocationList (invocationRef recorder)
  let callCount = length (filter (v ==) invocationList)
  if compareCount method callCount
    then pure ()
    else
      errorWithoutStackTrace $
        -- If we expected some calls (e.g. atLeast 1) but got 0, and there were OTHER calls,
        -- show the closest match diff to help debugging.
        if callCount == 0 && not (null invocationList)
           && expectsPositive method
          then countWithArgsMismatchMessageWithDiff mockName v invocationList
          else countWithArgsMismatchMessage mockName method callCount

expectsPositive :: CountVerifyMethod -> Bool
expectsPositive (Equal n) = n > 0
expectsPositive (GreaterThanEqual n) = n > 0
expectsPositive (GreaterThan _) = True
expectsPositive (LessThan _) = False -- usually "less than 2" allows 0, so 0 is valid.
expectsPositive (LessThanEqual _) = False -- "at most N" allows 0.

-- | Verify overall call count (ignoring arguments)
verifyResolvedCallCount :: ResolvedMock params -> CountVerifyMethod -> IO ()
verifyResolvedCallCount (ResolvedMock mockName recorder) method =
  verifyCallCount mockName recorder method

-- | Verify call order using resolved mock directly.
verifyResolvedOrder :: (Eq params, Show params) => VerifyOrderMethod -> ResolvedMock params -> [params] -> IO ()
verifyResolvedOrder method (ResolvedMock mockName recorder) matchers = do
  invocationList <- readInvocationList (invocationRef recorder)
  case doVerifyOrder method mockName invocationList matchers of
    Nothing -> pure ()
    Just (VerifyFailed msg) ->
      errorWithoutStackTrace msg `seq` pure ()

-- | Internal helper to read invocation list
readInvocationList :: TVar (InvocationRecord params) -> IO (InvocationList params)
readInvocationList ref = do
  record <- readTVarIO ref
  pure $ invocations record

-- | Helper to verify call count (low level)
verifyCallCount ::
  Maybe MockName ->
  InvocationRecorder params ->
  CountVerifyMethod ->
  IO ()
verifyCallCount maybeName recorder method = do
  result <- tryVerifyCallCount maybeName recorder method
  case result of
    Nothing -> pure ()
    Just msg -> errorWithoutStackTrace msg

tryVerifyCallCount ::
  Maybe MockName ->
  InvocationRecorder params ->
  CountVerifyMethod ->
  IO (Maybe String)
tryVerifyCallCount maybeName recorder method = do
  invocationList <- readInvocationList (invocationRef recorder)
  let callCount = length invocationList
  if compareCount method callCount
    then pure Nothing
    else pure $ Just $ countMismatchMessage maybeName method callCount

compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a = a == e
compareCount (LessThanEqual e) a = a <= e
compareCount (LessThan e) a = a < e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a = a > e

countWithArgsMismatchMessage :: Maybe MockName -> CountVerifyMethod -> Int -> String
countWithArgsMismatchMessage mockName method callCount =
  intercalate
    "\n"
    [ "function" <> mockNameLabel mockName <> " was not called the expected number of times with the expected arguments.",
      "  expected: " <> show method,
      "   but got: " <> show callCount
    ]

countMismatchMessage :: Maybe MockName -> CountVerifyMethod -> Int -> String
countMismatchMessage maybeName method callCount =
  intercalate
    "\n"
    [ "function" <> mockNameLabel maybeName <> " was not called the expected number of times.",
      "  expected: " <> showCountMethod method,
      "   but got: " <> show callCount
    ]
  where
    showCountMethod (Equal n) = show n
    showCountMethod (LessThanEqual n) = "<= " <> show n
    showCountMethod (GreaterThanEqual n) = ">= " <> show n
    showCountMethod (LessThan n) = "< " <> show n
    showCountMethod (GreaterThan n) = "> " <> show n

doVerify :: (Eq a, Show a) => Maybe MockName -> InvocationList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMessage name list a
doVerify name list (MatchAll a) = do
  guard $ Prelude.any (a /=) list
  pure $ verifyFailedMessage name list a

doVerifyOrder ::
  (Eq a, Show a) =>
  VerifyOrderMethod ->
  Maybe MockName ->
  InvocationList a ->
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

verifyFailedPartiallySequence :: Show a => Maybe MockName -> InvocationList a -> [a] -> VerifyFailed
verifyFailedPartiallySequence name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not called with the expected arguments in the expected order.",
        "  expected order:",
        intercalate "\n" $ ("    " <>) . show <$> expectedValues,
        "  but got:",
        intercalate "\n" $ ("    " <>) . show <$> calledValues
      ]

isOrderNotMatched :: Eq a => InvocationList a -> [a] -> Bool
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

verifyFailedOrderParamCountMismatch :: Maybe MockName -> InvocationList a -> [a] -> VerifyFailed
verifyFailedOrderParamCountMismatch name calledValues expectedValues =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not called with the expected arguments in the expected order (count mismatch).",
        "  expected: " <> show (length expectedValues),
        "   but got: " <> show (length calledValues)
      ]

verifyFailedSequence :: Show a => Maybe MockName -> [VerifyOrderResult a] -> VerifyFailed
verifyFailedSequence name fails =
  VerifyFailed $
    intercalate
      "\n"
      ( ("function" <> mockNameLabel name <> " was not called with the expected arguments in the expected order.") : (verifyOrderFailedMesssage <$> fails)
      )

collectUnExpectedOrder :: Eq a => InvocationList a -> [a] -> [VerifyOrderResult a]
collectUnExpectedOrder calledValues expectedValues =
  catMaybes $
    mapWithIndex
      ( \i expectedValue -> do
          let calledValue = calledValues !! i
          guard $ expectedValue /= calledValue
          pure VerifyOrderResult {index = i, calledValue = calledValue, expectedValue}
      )
      expectedValues

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = [f i x | (i, x) <- zip [0 ..] xs]
