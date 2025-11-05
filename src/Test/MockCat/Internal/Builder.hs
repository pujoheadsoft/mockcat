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
module Test.MockCat.Internal.Builder where

import Control.Monad (guard, when, ap)
import Data.Function ((&))

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List (elemIndex, intercalate)
import Data.Maybe

import GHC.IO (unsafePerformIO)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.AssociationList (AssociationList, lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans
import Control.Monad.State
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Test.MockCat.Internal.Types
import Test.MockCat.Internal.Core
import Test.MockCat.Internal.Message


-- | Class for creating a mock corresponding to the parameter.
class MockBuilder params fn verifyParams | params -> fn, params -> verifyParams where
  -- build a mock
  build :: MonadIO m => Maybe MockName -> params -> m (Mock fn verifyParams)

-- | Class for building a curried function.
-- The purpose of this class is to automatically generate and provide
-- an implementation for the corresponding curried function type (such as `a -> b -> ... -> IO r`)
-- when given the argument list type of the mock (`Param a :> Param b :> ...`).
-- @args@ is the argument list type of the mock.
-- @r@ is the return type of the function.
-- @fn@ is the curried function type.
class BuildCurried args r fn | args r -> fn where
  -- | Build a curried function.
  -- Accept a function that combines all arguments and convert it into a curried function.
  buildCurried :: (args -> IO r) -> fn
