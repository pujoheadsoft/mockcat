{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Test.MockCat.Cons where

data a #> b = Cons a b

instance (Show a, Show b) => Show (a #> b) where
  show (Cons a b) = show a <> "," <> show b

instance (Eq a, Eq b) => Eq (a #> b) where
  (Cons a b) == (Cons a2 b2) = (a == a2) && (b == b2)

infixr 8 #>