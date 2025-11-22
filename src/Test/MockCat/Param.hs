{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module is a parameter of the mock function.
--
-- This parameter can be used when creating and verifying the mock.
module Test.MockCat.Param
  ( Param(..),
    value,
    param,
    (|>),
    expect,
    expect_,
    any,
    ArgsOf,
    ProjectionArgs,
    projArgs,
    ReturnOf,
    ProjectionReturn,
    projReturn,
    returnValue
  )
where

import Test.MockCat.Cons ((:>) (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (any)
import Data.Typeable (Typeable, typeOf)
import Foreign.Ptr (Ptr, ptrToIntPtr, castPtr, IntPtr)

data Param v
  = ExpectValue v
  | ExpectCondition (v -> Bool) String

instance {-# OVERLAPPABLE #-} (Eq a) => Eq (Param a) where
  (ExpectValue a) == (ExpectValue b) = a == b
  (ExpectValue a) == (ExpectCondition m2 _) = m2 a
  (ExpectCondition m1 _) == (ExpectValue b) = m1 b
  (ExpectCondition _ l1) == (ExpectCondition _ l2) = l1 == l2

-- | Overlapping instance for function types
instance {-# OVERLAPPING #-} (Typeable (a -> b)) => Eq (Param (a -> b)) where
  (ExpectValue a) == (ExpectValue b) = compareFunction a b
  (ExpectValue a) == (ExpectCondition m2 _) = m2 a
  (ExpectCondition m1 _) == (ExpectValue b) = m1 b
  (ExpectCondition _ l1) == (ExpectCondition _ l2) = l1 == l2

type family ShowResult a where
  ShowResult String = String
  ShowResult a = String

class ShowParam a where
  showParam :: a -> ShowResult a

instance {-# OVERLAPPING #-} ShowParam (Param String) where
  showParam (ExpectCondition _ l) = l
  showParam (ExpectValue a) = a

instance {-# INCOHERENT #-} (Show a) => ShowParam (Param a) where
  showParam (ExpectCondition _ l) = l
  showParam (ExpectValue a) = show a

-- | Overlapping instance for function types
instance {-# OVERLAPPING #-} (Typeable (a -> b)) => ShowParam (Param (a -> b)) where
  showParam (ExpectCondition _ l) = l
  showParam (ExpectValue a) = showFunction a

instance (ShowParam (Param a)) => Show (Param a) where
  show = showParam

value :: Param v -> v
value (ExpectValue a) = a
value _ = error "not implement"

param :: v -> Param v
param = ExpectValue

class ConsGen a b r | a b -> r where
  (|>) :: a -> b -> r

instance {-# OVERLAPPING #-} (Param a ~ a', (Param b :> c) ~ bc) => ConsGen a (Param b :> c) (a' :> bc) where
  (|>) a = (:>) (param a)
instance {-# OVERLAPPING #-} ((Param b :> c) ~ bc) => ConsGen (Param a) (Param b :> c) (Param a :> bc) where
  (|>) = (:>)
instance ConsGen (Param a) (Param b) (Param a :> Param b) where
  (|>) = (:>)
instance {-# OVERLAPPABLE #-} ((Param b) ~ b') => ConsGen (Param a) b (Param a :> b') where
  (|>) a b = (:>) a (param b)
instance {-# OVERLAPPABLE #-} ((Param a) ~ a') => ConsGen a (Param b) (a' :> Param b) where
  (|>) a = (:>) (param a)
instance {-# OVERLAPPABLE #-} (Param a ~ a', Param b ~ b') => ConsGen a b (a' :> b') where
  (|>) a b = (:>) (param a) (param b)

infixr 8 |>

-- | Make a parameter to which any value is expected to apply.
any :: Param a
any = unsafeCoerce (ExpectCondition (const True) "any")

{- | Create a conditional parameter.

   When applying a mock function, if the argument does not satisfy this condition, an error occurs.

   In this case, the specified label is included in the error message.
-}
expect :: (a -> Bool) -> String -> Param a
expect = ExpectCondition

{- | Create a conditional parameter.

  In applied a mock function, if the argument does not satisfy this condition, an error occurs.

  Unlike @'expect'@, it does not require a label, but the error message is displayed as [some condition].
-}
expect_ :: (a -> Bool) -> Param a
expect_ f = ExpectCondition f "[some condition]"

-- | The type of the argument parameters of the parameters.
type family ArgsOf params where
  ArgsOf (Param a :> Param r) = Param a
  ArgsOf (Param a :> rest) = Param a :> ArgsOf rest

-- | Class for projecting the arguments of the parameter.
class ProjectionArgs params where
  projArgs :: params -> ArgsOf params

instance {-# OVERLAPPING #-} ProjectionArgs (Param a :> Param r) where
  projArgs (a :> _) = a

instance
  {-# OVERLAPPABLE #-}
  (ProjectionArgs rest, ArgsOf (Param a :> rest) ~ (Param a :> ArgsOf rest)) =>
  ProjectionArgs (Param a :> rest) where
  projArgs (a :> rest) = a :> projArgs rest

-- | The type of the return parameter of the parameters.
type family ReturnOf params where
  ReturnOf (Param a :> Param r) = Param r
  ReturnOf (Param a :> rest) = ReturnOf rest

class ProjectionReturn param where
  projReturn :: param -> ReturnOf param

instance {-# OVERLAPPING #-} ProjectionReturn (Param a :> Param r) where
  projReturn (_ :> r) = r

instance
  {-# OVERLAPPABLE #-}
  (ProjectionReturn rest, ReturnOf (Param a :> rest) ~ ReturnOf rest) =>
  ProjectionReturn (Param a :> rest) where
  projReturn (_ :> rest) = projReturn rest

returnValue :: (ProjectionReturn params, ReturnOf params ~ Param r) => params -> r
returnValue = value . projReturn

-- | Get the pointer address of a value (used for both comparison and display)
getPtrAddr :: forall a. a -> IntPtr
getPtrAddr x = ptrToIntPtr (castPtr (unsafeCoerce x :: Ptr ()))

-- | Helper function to compare function values using pointer equality
-- Uses the same pointer calculation as showFunction for consistency
compareFunction :: forall a. a -> a -> Bool
compareFunction x y = getPtrAddr x == getPtrAddr y

-- | Show function using type information and a pointer hash
showFunction :: forall a. Typeable a => a -> String
showFunction x =
  let typeStr = show (typeOf x)
      -- Use the same pointer address calculation as compareFunction
      ptrAddr = show (getPtrAddr x)
   in typeStr ++ "@" ++ ptrAddr