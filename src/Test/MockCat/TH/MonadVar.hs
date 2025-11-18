{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.MonadVar
  ( mockTType,
    tyVarBndrToType,
    applyFamilyArg
  )
where

import Language.Haskell.TH
  ( Name,
    TyVarBndr (..),
    Type (..),
  )
import Test.MockCat.MockT (MockT)

mockTType :: Name -> Type
mockTType monadVarName = AppT (ConT ''MockT) (VarT monadVarName)

liftTyVar :: Name -> Name -> Type
liftTyVar monadVarName varName
  | monadVarName == varName = mockTType monadVarName
  | otherwise = VarT varName

tyVarBndrToType :: Name -> TyVarBndr a -> Type
tyVarBndrToType monadVarName (PlainTV name _) = liftTyVar monadVarName name
tyVarBndrToType monadVarName (KindedTV name _ _) = liftTyVar monadVarName name

applyFamilyArg :: Name -> TyVarBndr a -> Type
applyFamilyArg = tyVarBndrToType

