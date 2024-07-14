{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.MockCat.Param (Param, value, param, (|>), matcher, any, or, and, notEqual) where

import Data.Text hiding (any, head)
import Test.MockCat.Cons ((:>) (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (and, any, or)

data Param v = Param v (Maybe (Matcher v))

instance (Eq a) => Eq (Param a) where
  (Param a (Just (Matcher m1))) == (Param b (Just (Matcher m2))) = m1 a b && m2 a b
  (Param a (Just (Matcher m1))) == (Param b Nothing) = m1 a b
  (Param a Nothing) == (Param b (Just (Matcher m2))) = m2 a b
  (Param a Nothing) == (Param b Nothing) = a == b

type family ShowResult a where
  ShowResult String = String
  ShowResult a = String

class ShowParam a where
  showParam :: a -> ShowResult a

instance {-# OVERLAPPING #-} ShowParam (Param String) where
  showParam (Param v _) = v
instance {-# INCOHERENT #-} Show a => ShowParam (Param a) where
  showParam (Param v _) = show v

instance ShowParam (Param a) => Show (Param a) where
  show = showParam

value :: Param v -> v
value (Param v _) = v

param :: v -> Param v
param a = Param a Nothing

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
any = unsafeCoerce (Param "any" $ Just $ Matcher anyMatcher)

matcher :: (a -> Bool) -> String -> Param a
matcher f msg = Param (unsafeCoerce msg) (Just $ Matcher (\_ a -> f a))

newtype Matcher v = Matcher (v -> v -> Bool)

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param v m) = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) ((\(Matcher f) -> Matcher (\a b -> not (f a b))) <$> m)

instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) (Just $ Matcher (\_ a -> a /= v))

class LogicalMatcher a b r where
  or :: a -> b -> r
  and :: a -> b -> r

instance {-# OVERLAPPING #-} (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape p2) (composeOr m1 m2)
  and p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape p2) (composeAnd m1 m2)

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a) (composeOr m1 $ Just $ Matcher (\_ v -> v == a))
  and p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a) (composeAnd m1 $ Just $ Matcher (\_ v -> v == a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a (Param a) (Param a) where
  or a p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p2 <> " || " <> showWithRemoveEscape a) (composeOr m2 $ Just $ Matcher (\_ v -> v == a))
  and a p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p2 <> " && " <> showWithRemoveEscape a) (composeAnd m2 $ Just $ Matcher (\_ v -> v == a))

instance {-# OVERLAPPABLE #-} (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 || a == a2))
  and a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 && a == a2))

composeOr :: Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeOr (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b || m2 a b)
composeOr (Just (Matcher m1)) Nothing = Just $ Matcher m1
composeOr Nothing (Just (Matcher m2)) = Just $ Matcher m2
composeOr Nothing Nothing = Nothing

composeAnd :: Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeAnd (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b && m2 a b)
composeAnd (Just (Matcher m1)) Nothing = Just $ Matcher m1
composeAnd Nothing (Just (Matcher m2)) = Just $ Matcher m2
composeAnd Nothing Nothing = Nothing

showWithRemoveEscape :: (Show a) => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
