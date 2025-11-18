{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.FunctionType
  ( isNotConstantFunctionType
  )
where

import Language.Haskell.TH
  ( Type (..),
  )

isNotConstantFunctionType :: Type -> Bool
isNotConstantFunctionType (AppT (AppT ArrowT _) _) = True
isNotConstantFunctionType (AppT t1 t2) = isNotConstantFunctionType t1 || isNotConstantFunctionType t2
isNotConstantFunctionType (TupleT _) = False
isNotConstantFunctionType (ForallT _ _ t) = isNotConstantFunctionType t
isNotConstantFunctionType _ = False


