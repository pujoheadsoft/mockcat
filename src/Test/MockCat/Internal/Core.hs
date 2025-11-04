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
module Test.MockCat.Internal.Core where


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
import Test.MockCat.Internal.Types

-- ------------------
-- Abstract mock interface
--
-- A small type class to abstract over `Mock` and `MockIO` so verification
-- code can be generic over different mock representations.
class IsMock m where
  type MockFn m :: Type
  type MockParams m :: Type
  mockName :: m -> Maybe MockName
  mockStubFn :: m -> MockFn m
  mockVerifier :: m -> Verifier (MockParams m)

instance IsMock (Mock fn params) where
  type MockFn (Mock fn params) = fn
  type MockParams (Mock fn params) = params
  mockName (Mock _ _) = Nothing
  mockName (NamedMock name _ _) = Just name
  mockStubFn (Mock f _) = f
  mockStubFn (NamedMock _ f _) = f
  mockVerifier (Mock _ v) = v
  mockVerifier (NamedMock _ _ v) = v

instance IsMock (MockIO m fn params) where
  type MockFn (MockIO m fn params) = fn
  type MockParams (MockIO m fn params) = params
  mockName (MockIO _ _) = Nothing
  mockName (NamedMockIO name _ _) = Just name
  mockStubFn (MockIO f _) = f
  mockStubFn (NamedMockIO _ f _) = f
  mockVerifier (MockIO _ v) = v
  mockVerifier (NamedMockIO _ _ v) = v