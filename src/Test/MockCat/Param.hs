{-# LANGUAGE TypeOperators #-}
module Test.MockCat.Param where

import Test.MockCat.Cons

data Param v = Param v (Maybe (Matcher v))

instance Eq a => Eq (Param a) where
  (Param a (Just (Matcher m1))) == (Param b (Just (Matcher m2))) = (m1 a b) && (m2 a b)
  (Param a (Just (Matcher m1))) == (Param b Nothing)             = m1 a b
  (Param a Nothing)             == (Param b (Just (Matcher m2))) = m2 a b
  (Param a Nothing)             == (Param b Nothing)             = a == b

instance Show a => Show (Param a) where
  show (Param v _) = show v

param :: a -> Param a
param a = Param a Nothing

value :: Param v -> v
value (Param v _) = v


class ConsGen a b r | a -> r, b -> r where
  cons :: a -> b -> r

instance ConsGen (a #> b) (b #> c) ((a #> b) #> (b #> c)) where
  cons = Cons
instance ConsGen (a #> b) (Param b) ((a #> b) (Param b)) where
  cons = Cons
instance ConsGen (Param a) (b #> c) ((Param a) #> (b #> c)) where
  cons = Cons
instance ConsGen a (b #> c) ((Param a) #> (b #> c)) where
  cons a b = Cons (Param a Nothing) b
instance ConsGen (a #> b) c ((a #> b) #> (Param c)) where
  cons a b = Cons a (Param b Nothing)
instance ConsGen (Param a) (Param b) ((Param a) #> (Param b)) where
  cons = Cons
instance ConsGen a (Param b) ((Param a) #> (Param b)) where
  cons a b = Cons (Param a Nothing) b
instance ConsGen (Param a) b ((Param a) #> (Param b)) where
  cons a b = Cons a (Param b Nothing)
instance ConsGen a b ((Param a) #> (Param b)) where
  cons a b = Cons (param a) (param b)

infixr 8 :>

newtype Matcher v = Matcher (v -> v -> Bool)

anyMatcher :: a -> a -> Bool
anyMatcher _ _ = true

any :: Param a
any = unsafeCoerce (Param "any" $ Just $ Matcher anyMatcher)

matcher :: (a -> Bool) -> String -> Param a
matcher f msg = Param (unsafeCoerce msg) (Just $ Matcher (\_ a -> f a))

class NotMatcher a r | a -> r where
  notEqual :: a -> r

instance (Eq a, Show a) => NotMatcher (Param a) (Param a) where
  notEqual (Param v m) = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) ((\(Matcher f) -> Matcher (\a b -> true /= f a b)) <$> m)
instance (Eq a, Show a) => NotMatcher a (Param a) where
  notEqual v = Param (unsafeCoerce $ "Not " <> showWithRemoveEscape v) (Just $ Matcher (\_ a -> a /= v))

class LogicalMatcher a b r | a -> r, b -> r where
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
showWithRemoveEscape s = do
  show s # (replace (unsafeRegex "\\\"" global) "")