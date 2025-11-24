{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Test.MockCat.Cons ((:>)(..), Head(..)) where

data a :> b = a :> b

instance (Show a, Show b) => Show (a :> b) where
  show (a :> b) = show a <> "," <> show b

instance (Eq a, Eq b) => Eq (a :> b) where
  (a :> b) == (a2 :> b2) = (a == a2) && (b == b2)

infixr 8 :>

-- | Marker type for constant value mock functions.
--   Used to distinguish constant values (Head :> Param r) from
--   regular mock functions (Param a :> rest).
data Head = Head
  deriving (Eq, Show)
