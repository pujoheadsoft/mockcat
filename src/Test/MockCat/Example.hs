{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.MockCat.Example where
import Test.MockCat.TH
import Test.MockCat.Param


type Coproduct :: [*] -> *
data Coproduct tys where
  Inject :: x -> Coproduct (x:xs)
  Reject :: Coproduct xs -> Coproduct (x:xs)
deriving instance (Show x, Show (Coproduct xs)) => Show (Coproduct (x:xs))
deriving instance (Show (Coproduct '[]))


class CoproductElemDep x xs where
  coproductInject :: x -> Coproduct xs

instance {-# OVERLAPPING #-} CoproductElemDep x (x ': xs) where
  coproductInject = Inject
instance {-# OVERLAPPABLE #-} CoproductElemDep x xs => CoproductElemDep x (y ': xs) where
  coproductInject = Reject . coproductInject

class ElemDep xs where
  type family Injector xs
  inject :: Injector xs -> xs

instance (CoproductElemDep (CoproductInjector xs) xs) => ElemDep (Coproduct xs) where
  type Injector (Coproduct xs) = CoproductInjector xs
  inject = coproductInject

-- other instances for injection
instance ElemDep Double where
  type Injector Double = Int
  inject = fromIntegral

-- type program to find x to inject into coproducts
type CoproductInjector :: [*] -> *
type family CoproductInjector xs where
  CoproductInjector ((x, y) ': xs) = (x, y)
  CoproductInjector (Bool ': x ': xs) = x
  CoproductInjector (x ': xs) = CoproductInjector xs

-- -------------------------------------------------
type family MyType a where
  MyType Int = Bool   -- Intの場合はBool型を返す
  MyType Float = String -- Floatの場合はString型を返す
  MyType a = a         -- それ以外の型はそのまま返す

-- 型クラスの定義
class MyClass a where
  myFunction :: a -> MyType a

-- Intの場合のインスタンス
instance MyClass Int where
  myFunction x = x > 0  -- Intの場合、Boolを返す

-- Floatの場合のインスタンス
instance MyClass Float where
  myFunction x = if x > 0 then "Positive" else "Negative"  -- Floatの場合、Stringを返す

-- -- それ以外の型の場合のインスタンス
-- instance  MyClass a where
--   myFunction x = x  -- それ以外の場合、同じ型を返す

-- class ParamGen a where
--   param :: a -> ParamType a

-- instance ParamGen (Param a) where
--   param :: Param a -> ParamType (Param a)
--   param (Param v m) = Param v m

-- instance ParamGen a where
--   param :: a -> ParamType a
--   param a = Param a Nothing
