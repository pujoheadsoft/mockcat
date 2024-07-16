{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.MockCat.Param
  ( Param,
    value,
    param,
    (|>),
    matcher,
    any,
    or,
    and,
    notEqual,
    matcher_,
  )
where

import Data.Text hiding (any, head)
import Test.MockCat.Cons ((:>) (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (and, any, or)
import GHC.Stack (HasCallStack)

data Param v = Param v (ParamType v)

instance (Eq a) => Eq (Param a) where
  (Param _ (Value a)) == (Param _ (Value b)) = a == b
  (Param _ (Value a)) == (Param b (LabelledCustom m2 _)) = m2 a b
  (Param a (LabelledCustom m1 _)) == (Param _ (Value b)) = m1 a b
  (Param a (LabelledCustom m1 _)) == (Param b (LabelledCustom m2 _)) = m1 a b && m2 a b

type family ShowResult a where
  ShowResult String = String
  ShowResult a = String

class ShowParam a where
  showParam :: a -> ShowResult a

instance {-# OVERLAPPING #-} ShowParam (Param String) where
  showParam (Param v m) = case m of
    LabelledCustom _ l -> l
    (Value a) -> a

instance {-# INCOHERENT #-} (Show a) => ShowParam (Param a) where
  showParam (Param v m) = case m of
    LabelledCustom _ l -> l
    (Value a) -> show a

instance (ShowParam (Param a)) => Show (Param a) where
  show = showParam

value :: Param v -> v
value (Param v _) = v

param :: v -> Param v
param a = Param a (Value a)

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

anyMatcher :: a -> a -> Bool
anyMatcher _ _ = True

any :: Param a
any = unsafeCoerce (Param "any" $ LabelledCustom anyMatcher "any")

matcher :: (a -> Bool) -> String -> Param a
matcher f msg = Param (unsafeCoerce ()) (LabelledCustom (\_ a -> f a) msg)

matcher_ :: HasCallStack => (a -> Bool) -> Param a
matcher_ f = Param (unsafeCoerce ()) (LabelledCustom (\_ a -> f a) "[some matcher]")

data ParamType v
  = Value v
  | LabelledCustom (v -> v -> Bool) String

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param v m) =
    let newMatcher = case m of
          LabelledCustom f l -> LabelledCustom (\a b -> not (f a b)) l
          Value v -> Value v
     in Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) newMatcher

instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) (LabelledCustom (\_ a -> a /= v) (unsafeCoerce $ "Not " <> showWithRemoveEscape v))

class LogicalMatcher a b r where
  or :: a -> b -> r
  and :: a -> b -> r

instance {-# OVERLAPPING #-} (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape p2) (composeOr m1 m2)
  and p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape p2) (composeAnd m1 m2)

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a) (composeOr m1 $ LabelledCustom (\_ v -> v == a) (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a))
  and p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a) (composeAnd m1 $ LabelledCustom (\_ v -> v == a) (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a (Param a) (Param a) where
  or a p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p2 <> " || " <> showWithRemoveEscape a) (composeOr m2 $ LabelledCustom (\_ v -> v == a) (unsafeCoerce $ showWithRemoveEscape p2 <> " || " <> showWithRemoveEscape a))
  and a p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p2 <> " && " <> showWithRemoveEscape a) (composeAnd m2 $ LabelledCustom (\_ v -> v == a) (unsafeCoerce $ showWithRemoveEscape p2 <> " && " <> showWithRemoveEscape a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2) (LabelledCustom (\_ a -> a == a1 || a == a2) (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2))
  and a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2) (LabelledCustom (\_ a -> a == a1 && a == a2) (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2))

composeOr :: (Eq a, Show a) => ParamType a -> ParamType a -> ParamType a
composeOr (Value a) (Value b) = LabelledCustom (\a' b' -> a == a' || b == b') ""
composeOr (Value a) (LabelledCustom m2 l) = LabelledCustom m2 l
composeOr (LabelledCustom m1 l1) (Value a) = LabelledCustom m1 l1
composeOr (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\a b -> m1 a b || m2 a b) (l1 <> " and " <> l2)

composeAnd :: (Eq a, Show a) => ParamType a -> ParamType a -> ParamType a
composeAnd (Value a) (Value b) = LabelledCustom (\a' b' -> a == a' && b == b') ""
composeAnd (Value a) (LabelledCustom m2 l) = LabelledCustom m2 l
composeAnd (LabelledCustom m1 l1) (Value b) = LabelledCustom m1 l1
composeAnd (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\a b -> m1 a b && m2 a b) (l1 <> " and " <> l2)

showWithRemoveEscape :: (Show a) => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
