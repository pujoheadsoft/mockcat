{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.MockCat.Param
  ( Param,
    value,
    param,
    (|>),
    expect,
    expect_,
    expectByExpr,
    any,
    or,
    and,
    notEqual,
  )
where

import Data.Text hiding (any, head)
import Language.Haskell.TH
import Test.MockCat.Cons ((:>) (..))
import Test.MockCat.TH
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (and, any, or)

data Param v
  = ExpectValue v
  | ExpectCondition (v -> Bool) String

instance (Eq a) => Eq (Param a) where
  (ExpectValue a) == (ExpectValue b) = a == b
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
instance {-# OVERLAPPABLE #-} ((Param b) ~ b') => ConsGen (Param a) b (Param a :> b') where
  (|>) a b = (:>) a (param b)
instance {-# OVERLAPPABLE #-} (Param a ~ a', Param b ~ b') => ConsGen a b (a' :> b') where
  (|>) a b = (:>) (param a) (param b)

infixr 9 |>

any :: Param a
any = unsafeCoerce (ExpectCondition (const True) "any")

expect :: (a -> Bool) -> String -> Param a
expect = ExpectCondition

expect_ :: (a -> Bool) -> Param a
expect_ f = ExpectCondition f "[some condition]"

expectByExpr :: Q Exp -> Q Exp
expectByExpr qf = do
  str <- showExp qf
  [|ExpectCondition $qf str|]

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (ExpectValue a) = ExpectCondition (/= a) ("Not " <> showWithRemoveEscape a)
  notEqual (ExpectCondition f l) = ExpectCondition (not . f) ("Not " <> showWithRemoveEscape l)

instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = ExpectCondition (/= v) ("Not " <> showWithRemoveEscape v)

class LogicalMatcher a b r | a b -> r where
  or :: a -> b -> r
  and :: a -> b -> r

instance {-# OVERLAPPING #-} (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or = composeOr
  and = composeAnd

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1 a = composeOr p1 $ ExpectCondition (== a) (showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a)
  and p1 a = composeAnd p1 $ ExpectCondition (== a) (showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a)

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a (Param a) (Param a) where
  or a p2 = composeOr p2 $ ExpectCondition (== a) (showWithRemoveEscape a <> " || " <> showWithRemoveEscape p2)
  and a p2 = composeAnd p2 $ ExpectCondition (== a) (showWithRemoveEscape a <> " && " <> showWithRemoveEscape p2)

instance {-# OVERLAPPABLE #-} (Eq a, Show a, Param a ~ a') => LogicalMatcher a a a' where
  or a1 a2 = ExpectCondition (\a -> a == a1 || a == a2) (showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2)
  and a1 a2 = ExpectCondition (\a -> a == a1 && a == a2) (showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2)

composeOr :: (Eq a, Show a) => Param a -> Param a -> Param a
composeOr (ExpectValue a) (ExpectValue b) = ExpectCondition (\x -> a == x || b == x) ""
composeOr (ExpectValue a) (ExpectCondition m2 l2) = ExpectCondition (\x -> x == a || m2 x) l2
composeOr (ExpectCondition m1 l1) (ExpectValue a) = ExpectCondition (\x -> m1 x || x == a) l1
composeOr (ExpectCondition m1 l1) (ExpectCondition m2 l2) = ExpectCondition (\a -> m1 a || m2 a) (l1 <> " || " <> l2)

composeAnd :: (Eq a, Show a) => Param a -> Param a -> Param a
composeAnd (ExpectValue a) (ExpectValue b) = ExpectCondition (\x -> a == x && b == x) ""
composeAnd (ExpectValue a) (ExpectCondition m2 l2) = ExpectCondition (\x -> x == a && m2 a) l2
composeAnd (ExpectCondition m1 l1) (ExpectValue b) = ExpectCondition (\x -> m1 x && x == b) l1
composeAnd (ExpectCondition m1 l1) (ExpectCondition m2 l2) = ExpectCondition (\x -> m1 x && m2 x) (l1 <> " && " <> l2)

showWithRemoveEscape :: (Show a) => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
