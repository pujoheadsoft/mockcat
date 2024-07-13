{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.MockCat (mock, fun, verify, hasBeenCalledWith, verifyCount, hasBeenCalledTimes, with) where

import Test.MockCat.Param hiding (any)
import Test.MockCat.Cons
import Data.Maybe
import Data.List (intercalate)
import Test.MockCat.ParamDivider
import Data.IORef (newIORef, IORef, modifyIORef', readIORef)
import GHC.IO (unsafePerformIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (guard)
import Data.Text (unpack, replace, pack)
import Control.Exception (evaluate)

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)
type MockName = String
newtype Verifier params = Verifier (IORef (CalledParamsList params))


mock :: MockBuilder params fun verifyParams
  => MonadIO m
  => params
  -> m (Mock fun verifyParams)
mock params = liftIO $ build Nothing params

fun :: Mock fun v -> fun
fun (Mock _ f _) = f

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> IO (Mock fun verifyParams)


instance (Show a, Eq a)
  => MockBuilder (Param a :> Param r) (a -> r) (Param a) where
  build name params = do
    s <- newIORef ([] :: CalledParamsList params)
    createMock name s (\a2 -> unsafePerformIO $ extractReturnValueWithValidate name params (p a2) s)

p :: a -> Param a
p = param

createMock :: Maybe MockName -> IORef (CalledParamsList params) -> fun -> IO (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

type CalledParamsList params = [params]

extractReturnValueWithValidate ::
     ParamDivider params args (Param r)
  => Eq args
  => Show args
  => Maybe MockName
  -> params
  -> args
  -> IORef (CalledParamsList args)
  -> IO r
extractReturnValueWithValidate name params inputParams s = do
  validateWithStoreParams name s (args params) inputParams
  pure $ returnValue params

validateWithStoreParams :: Eq a => Show a => Maybe MockName -> IORef (CalledParamsList a) -> a -> a -> IO ()
validateWithStoreParams name s expected actual = do
  a <- storeCalledParams s actual
  validateParams name expected a

storeCalledParams :: IORef (CalledParamsList a) -> a -> IO a
storeCalledParams ref a = do
  modifyIORef' ref (++ [a])
  pure a

validateParams :: Eq a => Show a => Maybe MockName -> a -> a -> IO ()
validateParams name expected actual =
  if expected == actual then pure ()
  else evaluate . errorWithoutStackTrace $ message name expected actual

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate "\n" [
    "function" <> mockNameLabel name <> "was not called with expected arguments.",
    "  expected: " <> show expected,
    "  but was : " <> show actual
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

-- instance (Eq a, Show a) => Verify a a where
--   verify v a = _verify v (MatchAny a)

_verify :: Eq params => Show params => Mock fun params -> VerifyMatchType params -> IO ()
_verify (Mock name _ (Verifier ref)) matchType = do
  calledParamsList <- liftIO $ readIORef ref
  case doVerify name calledParamsList matchType of
    Just (VerifyFailed msg) -> errorWithoutStackTrace msg
    Nothing -> pure ()

newtype VerifyFailed = VerifyFailed Message
type Message = String

doVerify :: Eq a => Show a => Maybe MockName -> CalledParamsList a -> VerifyMatchType a -> Maybe VerifyFailed
doVerify name list (MatchAny a) = do
  guard $ notElem a list
  pure $ verifyFailedMesssage name list a
doVerify name list (MatchAll a) = do
  guard $ any (a /=) list
  pure $ verifyFailedMesssage name list a

verifyFailedMesssage :: Show a => Maybe MockName -> CalledParamsList a -> a -> VerifyFailed
verifyFailedMesssage name calledParams expected =
  VerifyFailed $ intercalate "\n"
    ["function" <> mockNameLabel name <> "wasn't called with expected arguments.",
     "  expected: " <> show expected,
     "  but was : " <> formatCalledParamsList calledParams]

formatCalledParamsList :: Show a => CalledParamsList a -> String
formatCalledParamsList calledParams
  | length calledParams == 0 = "Never been called."
  | length calledParams == 1 =
    show $ (_replace "[" . _replace "]") calledParams
  | otherwise = show calledParams

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

hasBeenCalledWith
  :: Verify params input
  => Mock fun params
  -> input
  -> IO ()
hasBeenCalledWith = verify

class VerifyCount countType params a where
  verifyCount :: Eq params => Mock fun params -> countType -> a -> IO ()

data CountVerifyMethod =
    Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int

instance Show CountVerifyMethod where
  show (Equal e)            = show e
  show (LessThanEqual e)    = "<= " <> show e
  show (LessThan e)         = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e)      = "> " <> show e

compareCount :: CountVerifyMethod -> Int -> Bool
compareCount (Equal e) a            = a == e
compareCount (LessThanEqual e) a    = a <= e
compareCount (LessThan e) a         = a <  e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a      = a >  e

instance VerifyCount CountVerifyMethod (Param a) a where
  verifyCount v count a = _verifyCount v (param a) count

instance VerifyCount Int (Param a) a where
  verifyCount v count a =  _verifyCount v (param a) (Equal count)

instance VerifyCount CountVerifyMethod a a where
  verifyCount v count a = _verifyCount v a count

instance VerifyCount Int a a where
  verifyCount v count a = _verifyCount v a (Equal count)

_verifyCount :: Eq params => Mock fun params -> params -> CountVerifyMethod -> IO ()
_verifyCount (Mock name _ (Verifier ref)) v method = do
  calledParamsList <- readIORef ref
  let
    callCount = length (filter (v ==) calledParamsList)
  if compareCount method callCount then pure ()
  else errorWithoutStackTrace $ intercalate "\n" [
    "function" <> mockNameLabel name <> "was not called the expected number of times.",
    "  expected: " <> show method,
    "  but was : " <> show callCount]

hasBeenCalledTimes
  :: VerifyCount countType params a
  => Eq params
  => Mock fun params
  -> countType
  -> a
  -> IO ()
hasBeenCalledTimes = verifyCount

with :: (a -> IO ()) -> a -> IO ()
with f = f