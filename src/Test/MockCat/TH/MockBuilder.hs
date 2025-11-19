{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Test.MockCat.TH.MockBuilder
  ( createMockBuilderVerifyParams
  , createMockBuilderFnType
  )
where

import Language.Haskell.TH
  ( Name,
    Type (..),
  )
import Test.MockCat.Cons ((:>))
import Test.MockCat.Param (Param)

createMockBuilderVerifyParams :: Type -> Type
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) (AppT (VarT _) _)) =
  AppT (ConT ''Param) ty
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) ty2) =
  AppT
    (AppT (ConT ''(:>)) (AppT (ConT ''Param) ty))
    (createMockBuilderVerifyParams ty2)
createMockBuilderVerifyParams (AppT (VarT _) _) = TupleT 0
createMockBuilderVerifyParams (AppT (ConT _) _) = TupleT 0
createMockBuilderVerifyParams (ForallT _ _ ty) = createMockBuilderVerifyParams ty
createMockBuilderVerifyParams (VarT _) = TupleT 0
createMockBuilderVerifyParams (ConT _) = TupleT 0
createMockBuilderVerifyParams _ = TupleT 0

createMockBuilderFnType :: Name -> Type -> Type
createMockBuilderFnType monadVarName a@(AppT (VarT var) ty)
  | monadVarName == var = ty
  | otherwise = a
createMockBuilderFnType monadVarName (AppT ty ty2) =
  AppT ty (createMockBuilderFnType monadVarName ty2)
createMockBuilderFnType monadVarName (ForallT _ _ ty) =
  createMockBuilderFnType monadVarName ty
createMockBuilderFnType _ ty = ty

