{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.MockCat.Param where

import Unsafe.Coerce
import Data.Text hiding (head)
import Test.MockCat.Cons

newtype Matcher v = Matcher (v -> v -> Bool)

data Param v = Param v (Maybe (Matcher v))

instance Eq a => Eq (Param a) where
  (Param a (Just (Matcher m1))) == (Param b (Just (Matcher m2))) = (m1 a b) && (m2 a b)
  (Param a (Just (Matcher m1))) == (Param b Nothing)             = m1 a b
  (Param a Nothing)             == (Param b (Just (Matcher m2))) = m2 a b
  (Param a Nothing)             == (Param b Nothing)             = a == b

instance Show a => Show (Param a) where
  show (Param v _) = show v

-- param :: a -> Param a
-- param a = Param a Nothing

value :: Param v -> v
value (Param v _) = v

param :: v -> Param v
param a = Param a Nothing

-- class ParamGen a b | a -> b where
--   param :: a -> b

-- -- ラップされていなかったらする
-- instance {-# OVERLAPPABLE #-} (b ~ (Param a)) => ParamGen a b where
--   param a = Param a Nothing

-- -- ラップされていたら何もしない
-- instance  ParamGen (Param a) (Param a) where
--   param a = a

-- (|>) :: a -> b -> a #> b
-- (|>) = Cons

class ConsGen a b r | a b -> r where
  (|>) :: a -> b -> r



-- 左はラップされていない、右は結合済み
instance {-# OVERLAPPING #-} ConsGen (Param a) (b #> c) (Param a #> b #> c) where
  (|>) = Cons
-- 左はラップ済み、右は結合済み
instance ((Param a) ~ a') => ConsGen a (b #> c) (a' #> b #> c) where
  (|>) a = Cons (param a)
-- 左右どちらもラップ済み
instance {-# OVERLAPPING #-} ConsGen (Param a) (Param b) (Param a #> Param b) where
  (|>) = Cons
-- 左はラップ済み
instance {-# OVERLAPPABLE #-} ((Param b) ~ b') => ConsGen (Param a) b (Param a #> b') where
  (|>) a b = Cons a (param b)

-- -- 左結合済み
-- instance {-# OVERLAPPING #-} ((Param a #> Param b) ~ x, Param c ~ c') => ConsGen (a #> b) c (x #> b #> c') where
--   (|>) (Cons a b) c = Cons a (Cons b (param c))

-- 右はラップ済み
instance {-# INCOHERENT #-} ((Param a) ~ a') => ConsGen a (Param b) (a' #> Param b) where
  (|>) a = Cons (param a)
-- 左右どちらもラップされてない
instance {-# OVERLAPPABLE #-} (Param a ~ a', Param b ~ b') => ConsGen a b (a' #> b') where
  (|>) a b = Cons (param a) (param b)





infixr 8 |>

anyMatcher :: a -> a -> Bool
anyMatcher _ _ = True

any :: Param a
any = unsafeCoerce (Param "any" $ Just $ Matcher anyMatcher)

matcher :: (a -> Bool) -> String -> Param a
matcher f msg = Param (unsafeCoerce msg) (Just $ Matcher (\_ a -> f a))

class NotMatcher a r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param v m) = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) ((\(Matcher f) -> Matcher (\a b -> True /= f a b)) <$> m)
instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) (Just $ Matcher (\_ a -> a /= v))

class LogicalMatcher a b r where
  or :: a -> b -> r
  and :: a -> b -> r

instance (Eq a, Show a) => LogicalMatcher (Param a) (Param a) (Param a) where
  or  p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape p2) (composeOr m1 m2)
  and p1@(Param _ m1) p2@(Param _ m2) = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape p2) (composeAnd m1 m2)
instance (Eq a, Show a) => LogicalMatcher (Param a) a (Param a) where
  or  p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " || " <> showWithRemoveEscape a) (composeOr m1 $ Just $ Matcher (\_ v -> v == a))
  and p1@(Param _ m1) a = Param (unsafeCoerce $ showWithRemoveEscape p1 <> " && " <> showWithRemoveEscape a) (composeAnd m1 $ Just $ Matcher (\_ v -> v == a))
instance (Eq a, Show a) => LogicalMatcher a a (Param a) where
  or  a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " || " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 || a == a2))
  and a1 a2 = Param (unsafeCoerce $ showWithRemoveEscape a1 <> " && " <> showWithRemoveEscape a2) (Just $ Matcher (\_ a -> a == a1 && a == a2))

composeOr :: Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeOr (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b || m2 a b)
composeOr (Just (Matcher m1)) Nothing             = Just $ Matcher (\a b -> m1 a b)
composeOr Nothing             (Just (Matcher m2)) = Just $ Matcher (\a b -> m2 a b)
composeOr Nothing             Nothing             = Nothing

composeAnd :: Maybe (Matcher a) -> Maybe (Matcher a) -> Maybe (Matcher a)
composeAnd (Just (Matcher m1)) (Just (Matcher m2)) = Just $ Matcher (\a b -> m1 a b && m2 a b)
composeAnd (Just (Matcher m1)) Nothing             = Just $ Matcher (\a b -> m1 a b)
composeAnd Nothing             (Just (Matcher m2)) = Just $ Matcher (\a b -> m2 a b)
composeAnd Nothing             Nothing             = Nothing

showWithRemoveEscape :: Show a => a -> String
showWithRemoveEscape s = unpack $ replace (pack "\\") (pack "") (pack (show s))
