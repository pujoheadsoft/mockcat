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
module Test.MockCat.Internal.Types where

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
    ( execState, MonadState(put, get), State )
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

newtype Verifier params = Verifier (IORef (AppliedRecord params))

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

data Mock fn params =
    Mock fn (Verifier params)
  | NamedMock MockName fn (Verifier params)

-- MockIO
data MockIO (m :: Type -> Type) fn params =
   MockIO fn (Verifier params)
 | NamedMockIO MockName fn (Verifier params)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 = Nothing
  | otherwise = listToMaybe (drop n xs)

{-# NOINLINE perform #-}
perform :: IO a -> a
perform = unsafePerformIO
