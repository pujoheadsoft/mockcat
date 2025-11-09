{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.Internal.Core where


import Data.Maybe
import Prelude hiding (lookup)
import Data.Kind (Type)
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
  mockName (Mock _ _ _) = Nothing
  mockName (NamedMock name _ _ _) = Just name
  mockStubFn (Mock f _ _) = f
  mockStubFn (NamedMock _ f _ _) = f
  mockVerifier (Mock _ v _) = v
  mockVerifier (NamedMock _ _ v _) = v

instance IsMock (MockIO m fn params) where
  type MockFn (MockIO m fn params) = fn
  type MockParams (MockIO m fn params) = params
  mockName (MockIO _ _ _) = Nothing
  mockName (NamedMockIO name _ _ _) = Just name
  mockStubFn (MockIO f _ _) = f
  mockStubFn (NamedMockIO _ f _ _) = f
  mockVerifier (MockIO _ v _) = v
  mockVerifier (NamedMockIO _ _ v _) = v