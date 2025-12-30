{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.Internal.Types where

import Control.Monad (ap)
import Control.Concurrent.STM (TVar)
import Data.Maybe (listToMaybe)
import GHC.IO (unsafePerformIO)
import Test.MockCat.AssociationList (AssociationList)
import Prelude hiding (lookup)
import Control.Monad.State ( State, MonadState, execState, modify )
import Control.Monad.Reader (MonadReader, ask)

type MockName = String

data InvocationRecorder params = InvocationRecorder
  { invocationRef :: TVar (InvocationRecord params)
  , functionNature :: FunctionNature
  }

-- | Result of building a mock: function plus its recorder.
data BuiltMock fn params = BuiltMock
  { builtMockFn :: fn
  , builtMockRecorder :: InvocationRecorder params
  }

data ResolvedMock params = ResolvedMock {
  resolvedMockName :: Maybe MockName,
  resolvedMockRecorder :: InvocationRecorder params
}

data FunctionNature
  = PureConstant
  | IOConstant
  | ParametricFunction
  deriving (Eq, Show)

type InvocationList params = [params]
type InvocationCounts params = AssociationList params Int

data InvocationRecord params = InvocationRecord {
  invocations :: InvocationList params,
  invocationCounts :: InvocationCounts params
}
  deriving (Eq, Show)

data CountVerifyMethod
  = Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int
  deriving (Eq)

instance Show CountVerifyMethod where
  show (Equal e) = show e
  show (LessThanEqual e) = "<= " <> show e
  show (LessThan e) = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e) = "> " <> show e

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

newtype VerifyFailed = VerifyFailed Message

data VerifyOrderMethod
  = ExactlySequence
  | PartiallySequence
  deriving (Show, Eq)

data VerifyOrderResult a = VerifyOrderResult
  { index :: Int,
    calledValue :: a,
    expectedValue :: a
  }

type Message = String

data VerifyMatchType a = MatchAny a | MatchAll a

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)

{-# NOINLINE perform #-}
perform :: IO a -> a
perform = unsafePerformIO

-- | Mock expectation context holds verification actions to run at the end
--   of the `withMock` block. Storing `IO ()` avoids forcing concrete param
--   types at registration time.
newtype WithMockContext = WithMockContext (TVar [IO ()])

class MonadWithMockContext m where
  askWithMockContext :: m WithMockContext

instance {-# OVERLAPPABLE #-} (MonadReader WithMockContext m) => MonadWithMockContext m where
  askWithMockContext = ask

-- | Expectation specification
data Expectation params where
  -- | Count expectation with specific arguments
  CountExpectation :: CountVerifyMethod -> params -> Expectation params
  -- | Count expectation without arguments (any arguments)
  CountAnyExpectation :: CountVerifyMethod -> Expectation params
  -- | Order expectation
  OrderExpectation :: VerifyOrderMethod -> [params] -> Expectation params
  -- | Simple expectation (at least once) with arguments
  SimpleExpectation :: params -> Expectation params
  -- | Simple expectation (at least once) without arguments
  AnyExpectation :: Expectation params
  deriving (Show, Eq)

-- | Expectations builder (Monad instance for do syntax)
newtype Expectations params a = Expectations (State [Expectation params] a)
  deriving (Functor, Applicative, Monad, MonadState [Expectation params])

-- | Run the Expectations monad to get a list of Expectation
runExpectations :: Expectations params () -> [Expectation params]
runExpectations (Expectations s) = execState s []

-- | Add an expectation to the list
addExpectation :: Expectation params -> Expectations params ()
addExpectation e = modify (++ [e])
