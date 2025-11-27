{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.Internal.Types where

import Control.Monad (ap)
import Control.Concurrent.STM (TVar)
import Data.Maybe
import GHC.IO (unsafePerformIO)
import Test.MockCat.AssociationList (AssociationList)
import Prelude hiding (lookup)
import Control.Monad.State ( State )

data Verifier params = Verifier
  { verifierRef :: TVar (AppliedRecord params)
  , verifierKind :: VerifierKind
  }

data VerifierKind
  = VerifierPureConstant
  | VerifierIOConstant
  | VerifierFunction
  deriving (Eq, Show)

type AppliedParamsList params = [params]
type AppliedParamsCounter params = AssociationList params Int

data AppliedRecord params = AppliedRecord {
  appliedParamsList :: AppliedParamsList params,
  appliedParamsCounter :: AppliedParamsCounter params
}
  deriving (Eq, Show)

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

data VerifyOrderResult a = VerifyOrderResult
  { index :: Int,
    appliedValue :: a,
    expectedValue :: a
  }

type Message = String

-- verify
data VerifyMatchType a = MatchAny a | MatchAll a

type MockName = String

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)

{-# NOINLINE perform #-}
perform :: IO a -> a
perform = unsafePerformIO
