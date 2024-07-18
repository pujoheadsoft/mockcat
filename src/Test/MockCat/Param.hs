{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

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
    notEqual
  )
where

import Data.Text hiding (any, head)
import Test.MockCat.Cons ((:>) (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (and, any, or)
import Language.Haskell.TH
import Test.MockCat.TH

data Param v = Value v
  | LabelledCustom (v -> Bool) String

instance (Eq a) => Eq (Param a) where
  (Value a) == (Value b) = a == b
  (Value a) == (LabelledCustom m2 _) = m2 a
  (LabelledCustom m1 _) == (Value b) = m1 b
  (LabelledCustom _ l1) == (LabelledCustom _ l2) = l1 == l2

type family ShowResult a where
  ShowResult String = String
  ShowResult a = String

class ShowParam a where
  showParam :: a -> ShowResult a

instance {-# OVERLAPPING #-} ShowParam (Param String) where
  showParam (LabelledCustom _ l) = l
  showParam (Value a) = a

instance {-# INCOHERENT #-} (Show a) => ShowParam (Param a) where
  showParam (LabelledCustom _ l) = l
  showParam (Value a) = show a

instance (ShowParam (Param a)) => Show (Param a) where
  show = showParam

value :: Param v -> v
value (Value a) = a
value _ = error "not implement"

param :: v -> Param v
param = Value

class ConsGen a b r | a b -> r where
  (|>) :: a -> b -> r

instance {-# OVERLAPPING #-} ((Param a :> Param b) ~ x, (Param c :> Param d) ~ y) => ConsGen (Param a :> Param b) (Param c :> Param d) (x :> y) where
  (|>) = (:>)

instance {-# OVERLAPPING #-} ConsGen (Param a) (b :> c) (Param a :> b :> c) where
  (|>) = (:>)

instance ((Param a) ~ a') => ConsGen a (b :> c) (a' :> b :> c) where
  (|>) a = (:>) (param a)

instance {-# OVERLAPPING #-} ConsGen (Param a) (Param b) (Param a :> Param b) where
  (|>) = (:>)

instance {-# OVERLAPPABLE #-} ((Param b) ~ b') => ConsGen (Param a) b (Param a :> b') where
  (|>) a b = (:>) a (param b)

instance {-# INCOHERENT #-} ((Param a) ~ a') => ConsGen a (Param b) (a' :> Param b) where
  (|>) a = (:>) (param a)

instance {-# OVERLAPPABLE #-} (Param a ~ a', Param b ~ b') => ConsGen a b (a' :> b') where
  (|>) a b = (:>) (param a) (param b)

infixr 8 |>

anyMatcher :: a -> Bool
anyMatcher _ = True

any :: Param a
any = unsafeCoerce (LabelledCustom anyMatcher "any")

expect :: (a -> Bool) -> String -> Param a
expect = LabelledCustom

expect_ :: (a -> Bool) -> Param a
expect_ f = LabelledCustom f "[some condition]"

expectByExpr :: Q Exp -> Q Exp
expectByExpr qf = do
  str <- showExp qf
  [| LabelledCustom $qf str |]

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Value a) = LabelledCustom (/= a) (unsafeCoerce $ "Not " <> showWithRemoveEscape a)
  notEqual (LabelledCustom f l) = LabelledCustom (not . f) (unsafeCoerce $ "Not " <> showWithRemoveEscape l)

instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = LabelledCustom (/= v) (unsafeCoerce $ "Not " <> showWithRemoveEscape v)

class LogicalMatcher a b r | a b -> r where
  or :: a -> b -> r
  and :: a -> b -> r

instance {-# OVERLAPPING #-} (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or = composeOr
  and = composeAnd

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1 a = composeOr p1 $ LabelledCustom (== a) (showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a)
  and p1 a = composeAnd p1 $ LabelledCustom (== a) (showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a)

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a (Param a) (Param a) where
  or a p2 = composeOr p2 $ LabelledCustom (== a) (showWithRemoveEscape a <> " || " <> showWithRemoveEscape p2)
  and a p2 = composeAnd p2 $ LabelledCustom (== a) (showWithRemoveEscape a <> " && " <> showWithRemoveEscape p2)

instance {-# OVERLAPPABLE #-} (Eq a, Show a, Param a ~ a') => LogicalMatcher a a a' where
  or a1 a2 = LabelledCustom (\a -> a == a1 || a == a2) (showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2)
  and a1 a2 = LabelledCustom (\a -> a == a1 && a == a2) (showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2)

composeOr :: (Eq a, Show a) => Param a -> Param a -> Param a
composeOr (Value a) (Value b) = LabelledCustom (\x -> a == x || b == x) ""
composeOr (Value a) (LabelledCustom m2 l2) = LabelledCustom (\x -> x == a || m2 x) l2
composeOr (LabelledCustom m1 l1) (Value a) = LabelledCustom (\x -> m1 x || x == a) l1
composeOr (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\a -> m1 a || m2 a) (l1 <> " || " <> l2)

composeAnd :: (Eq a, Show a) => Param a -> Param a -> Param a
composeAnd (Value a) (Value b) = LabelledCustom (\x -> a == x && b == x) ""
composeAnd (Value a) (LabelledCustom m2 l2) = LabelledCustom (\x -> x == a && m2 a) l2
composeAnd (LabelledCustom m1 l1) (Value b) = LabelledCustom (\x -> m1 x && x == b) l1
composeAnd (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\x -> m1 x && m2 x) (l1 <> " && " <> l2)

showWithRemoveEscape :: (Show a) => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
