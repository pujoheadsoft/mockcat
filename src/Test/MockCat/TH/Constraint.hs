{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Test.MockCat.TH.Constraint
  ( liftConstraint
  )
where

import Language.Haskell.TH
  ( Name,
    Type (..),
  )
import Test.MockCat.MockT (MockT)

liftConstraint :: Name -> Type -> Type
liftConstraint monadVarName = go
  where
    go predTy@(AppT (ConT ty) (VarT varName))
      | monadVarName == varName && ty == ''Monad = predTy
      | monadVarName == varName =
          AppT (ConT ty) (AppT (ConT ''MockT) (VarT varName))
    go (AppT ty (VarT varName))
      | monadVarName == varName =
          AppT ty (AppT (ConT ''MockT) (VarT varName))
    go (AppT t1 t2) = AppT (go t1) (go t2)
    go ty = ty

