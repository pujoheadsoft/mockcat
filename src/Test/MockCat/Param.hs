{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides types and functions for representing mock parameters.
-- Parameters are used both for setting up expectations and for verification.
module Test.MockCat.Param
  ( Param(..),
    WrapParam(..),
    value,
    param,
    ConsGen(..),
    expect,
    expect_,
    any,
    ArgsOf,
    ProjectionArgs,
    projArgs,
    ReturnOf,
    ProjectionReturn,
    projReturn,
    returnValue,
    Normalize,
    ToParamArg(..)
  )
where

import Test.MockCat.Cons ((:>) (..), Head(..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (any)
import Data.Typeable (Typeable, typeOf)
import Foreign.Ptr (Ptr, ptrToIntPtr, castPtr, IntPtr)
import qualified Data.Text as T (Text)

infixr 0 ~>

data Param v where
  -- | A parameter that expects a specific value.
  ExpectValue :: (Show v, Eq v) => v -> String -> Param v
  -- | A parameter that expects a value satisfying a condition.
  ExpectCondition :: (v -> Bool) -> String -> Param v
  -- | A parameter that wraps a value without Eq or Show constraints.
  ValueWrapper :: v -> String -> Param v

-- | Class for wrapping raw values into Param.
-- For types with Show and Eq, it uses ExpectValue to enable comparison and display.
-- For other types, it uses ValueWrapper.
class WrapParam a where
  wrap :: a -> Param a

instance {-# OVERLAPPING #-} WrapParam String where
  wrap s = ExpectValue s (show s)

instance {-# OVERLAPPING #-} WrapParam Int where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam Integer where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam Bool where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam Double where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam Float where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam Char where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} WrapParam T.Text where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} (Show a, Eq a) => WrapParam [a] where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPING #-} (Show a, Eq a) => WrapParam (Maybe a) where
  wrap v = ExpectValue v (show v)

instance {-# OVERLAPPABLE #-} WrapParam a where
  wrap v = ValueWrapper v "ValueWrapper"

instance Eq (Param a) where
  (ExpectValue a _) == (ExpectValue b _) = a == b
  (ExpectValue a _) == (ExpectCondition m2 _) = m2 a
  (ExpectCondition m1 _) == (ExpectValue b _) = m1 b
  (ExpectCondition _ l1) == (ExpectCondition _ l2) = l1 == l2
  ValueWrapper a _ == ExpectCondition m _ = m a
  ExpectCondition m _ == ValueWrapper a _ = m a
  ExpectValue a _ == ValueWrapper b _ = a == b
  ValueWrapper a _ == ExpectValue b _ = a == b
  ValueWrapper _ _ == ValueWrapper _ _ = False

instance Show (Param v) where
  show (ExpectValue _ l) = l
  show (ExpectCondition _ l) = l
  show (ValueWrapper _ l) = l

value :: Param v -> v
value (ExpectValue a _) = a
value (ValueWrapper a _) = a
value _ = error "not implemented"

-- | Create a Param from a value. Requires Eq and Show.
param :: (Show v, Eq v) => v -> Param v
param v = ExpectValue v (show v)


-- | Type family to untie the knot for ConsGen instances
type family Normalize a where
  Normalize (a :> b) = a :> b
  Normalize (Param a) = Param a
  Normalize a = Param a

class ToParamArg a where
  toParamArg :: a -> Normalize a

instance {-# OVERLAPPING #-} (Typeable (a -> b)) => ToParamArg (a -> b) where
  toParamArg f = ExpectCondition (compareFunction f) (showFunction f)

instance {-# OVERLAPPING #-} ToParamArg (Param a) where
  toParamArg = id

instance {-# OVERLAPPABLE #-} (Normalize a ~ Param a, WrapParam a) => ToParamArg a where
  toParamArg = wrap

class ToParamResult b where
  toParamResult :: b -> Normalize b

instance {-# OVERLAPPING #-} ToParamResult (Param a) where
  toParamResult = id

instance {-# OVERLAPPING #-} ToParamResult (a :> b) where
  toParamResult = id

instance {-# OVERLAPPABLE #-} (Normalize b ~ Param b, WrapParam b) => ToParamResult b where
  toParamResult = wrap

class ConsGen a b where
  (~>) :: a -> b -> Normalize a :> Normalize b

instance (ToParamArg a, ToParamResult b) => ConsGen a b where
  (~>) a b = (:>) (toParamArg a) (toParamResult b)

-- | Make a parameter to which any value is expected to apply.
--   Use with type application to specify the type: @any \@String@
--
--   > f <- mock $ any ~> True
any :: forall a. Param a
any = ExpectCondition (const True) "any"

{- | Create a conditional parameter with a label.
    When calling a mock function, if the argument does not satisfy this condition, an error occurs.
    In this case, the specified label is included in the error message.

    > expect (>5) ">5"
-}
expect :: (a -> Bool) -> String -> Param a
expect = ExpectCondition

{- | Create a conditional parameter without a label.
  The error message is displayed as "[some condition]".

  > expect_ (>5)
-}
expect_ :: (a -> Bool) -> Param a
expect_ f = ExpectCondition f "[some condition]"

-- | The type of the argument parameters of the parameters.
type family ArgsOf params where
  ArgsOf (Head :> Param r) = ()                        -- Constant value has no arguments
  ArgsOf (Param a :> Param r) = Param a
  ArgsOf (Param a :> rest) = Param a :> ArgsOf rest

-- | Class for projecting the arguments of the parameter.
class ProjectionArgs params where
  projArgs :: params -> ArgsOf params

instance {-# OVERLAPPING #-} ProjectionArgs (Head :> Param r) where
  projArgs (_ :> _) = ()

instance {-# OVERLAPPING #-} ProjectionArgs (Param a :> Param r) where
  projArgs (a :> _) = a

instance
  {-# OVERLAPPABLE #-}
  (ProjectionArgs rest, ArgsOf (Param a :> rest) ~ (Param a :> ArgsOf rest)) =>
  ProjectionArgs (Param a :> rest) where
  projArgs (a :> rest) = a :> projArgs rest

-- | The type of the return parameter of the parameters.
type family ReturnOf params where
  ReturnOf (Head :> Param r) = Param r                 -- Constant value returns Param r
  ReturnOf (Param a :> Param r) = Param r
  ReturnOf (Param a :> rest) = ReturnOf rest

class ProjectionReturn param where
  projReturn :: param -> ReturnOf param

instance {-# OVERLAPPING #-} ProjectionReturn (Head :> Param r) where
  projReturn (_ :> r) = r

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