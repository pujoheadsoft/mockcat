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

module Test.MockCat.Param
  ( Param,
    value,
    param,
    (|>),
    matcher,
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
import Language.Haskell.TH.Syntax

data Param v = Param (ParamType v)

instance (Eq a) => Eq (Param a) where
  (Param (Value a)) == (Param (Value b)) = a == b
  (Param (Value a)) == (Param (LabelledCustom m2 _)) = m2 a
  (Param (LabelledCustom m1 _)) == (Param (Value b)) = m1 b
  (Param (LabelledCustom _ l1)) == (Param (LabelledCustom _ l2)) = l1 == l2

type family ShowResult a where
  ShowResult String = String
  ShowResult a = String

class ShowParam a where
  showParam :: a -> ShowResult a

instance {-# OVERLAPPING #-} ShowParam (Param String) where
  showParam (Param m) = case m of
    LabelledCustom _ l -> l
    (Value a) -> a

instance {-# INCOHERENT #-} (Show a) => ShowParam (Param a) where
  showParam (Param m) = case m of
    LabelledCustom _ l -> l
    (Value a) -> show a

instance (ShowParam (Param a)) => Show (Param a) where
  show = showParam

value :: Param v -> v
value (Param m) = case m of
  Value a -> a
  _ -> error "not implement"

param :: v -> Param v
param a = Param (Value a)

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
any = unsafeCoerce (Param $ LabelledCustom anyMatcher "any")

matcher :: Q Exp -> Q Exp
matcher qf = do
  str <- showExpr qf
  [| Param (LabelledCustom $qf str) |]

-- matcher :: (a -> Bool) -> Param a
-- matcher f = $(do
--   let fName = 'f
--   str <- showExpr (varE fName)
--   [| Param (LabelledCustom f str) |])

data ParamType v
  = Value v
  | LabelledCustom (v -> Bool) String

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param m) =
    let newMatcher = case m of
          LabelledCustom f l -> LabelledCustom (not . f) (unsafeCoerce $ "Not " <> showWithRemoveEscape l)
          Value v -> Value v
     in Param newMatcher

instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (LabelledCustom (/= v) (unsafeCoerce $ "Not " <> showWithRemoveEscape v))

class LogicalMatcher a b r where
  or :: a -> b -> r
  and :: a -> b -> r

instance {-# OVERLAPPING #-} (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or p1@(Param m1) p2@(Param m2) = Param (composeOr m1 m2)
  and p1@(Param m1) p2@(Param m2) = Param (composeAnd m1 m2)

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1@(Param m1) a = Param (composeOr m1 $ LabelledCustom (\v -> v == a) (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a))
  and p1@(Param m1) a = Param (composeAnd m1 $ LabelledCustom (\v -> v == a) (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a (Param a) (Param a) where
  or a p2@(Param m2) = Param (composeOr m2 $ LabelledCustom (\v -> v == a) (unsafeCoerce $ showWithRemoveEscape p2 <> " || " <> showWithRemoveEscape a))
  and a p2@(Param m2) = Param (composeAnd m2 $ LabelledCustom (\v -> v == a) (unsafeCoerce $ showWithRemoveEscape p2 <> " && " <> showWithRemoveEscape a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or a1 a2 = Param (LabelledCustom (\a -> a == a1 || a == a2) (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2))
  and a1 a2 = Param (LabelledCustom (\a -> a == a1 && a == a2) (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2))

composeOr :: (Eq a, Show a) => ParamType a -> ParamType a -> ParamType a
composeOr (Value a) (Value b) = LabelledCustom (\x -> a == x || b == x) ""
composeOr (Value a) (LabelledCustom m2 l2) = LabelledCustom (\x -> x == a || m2 x) l2
composeOr (LabelledCustom m1 l1) (Value a) = LabelledCustom (\x -> m1 x || x == a) l1
composeOr (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\a -> m1 a || m2 a) (l1 <> " and " <> l2)

composeAnd :: (Eq a, Show a) => ParamType a -> ParamType a -> ParamType a
composeAnd (Value a) (Value b) = LabelledCustom (\x -> a == x && b == x) ""
composeAnd (Value a) (LabelledCustom m2 l2) = LabelledCustom (\x -> x == a && m2 a) l2
composeAnd (LabelledCustom m1 l1) (Value b) = LabelledCustom (\x -> m1 x && x == b) l1
composeAnd (LabelledCustom m1 l1) (LabelledCustom m2 l2) = LabelledCustom (\x -> m1 x && m2 x) (l1 <> " and " <> l2)

showWithRemoveEscape :: (Show a) => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
