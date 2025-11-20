{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Test.MockCat.TH.TypeUtils
  ( splitApps,
    substituteType,
    isNotConstantFunctionType,
    collectTypeVars,
    needsTypeable
  )
where

import qualified Data.Map.Strict as Map
import Language.Haskell.TH
  ( Name,
    Type (..),
  )

splitApps :: Type -> (Type, [Type])
splitApps ty = go ty []
  where
    go (AppT t1 t2) acc = go t1 (t2 : acc)
    go t acc = (t, acc)

substituteType :: Map.Map Name Type -> Type -> Type
substituteType subMap = go
  where
    go (VarT name) = Map.findWithDefault (VarT name) name subMap
    go (AppT t1 t2) = AppT (go t1) (go t2)
    go (SigT t k) = SigT (go t) k
    go (ParensT t) = ParensT (go t)
    go (InfixT t1 n t2) = InfixT (go t1) n (go t2)
    go (UInfixT t1 n t2) = UInfixT (go t1) n (go t2)
    go (ForallT tvs ctx t) = ForallT tvs (map go ctx) (go t)
    go t = t

isNotConstantFunctionType :: Type -> Bool
isNotConstantFunctionType (AppT (AppT ArrowT _) _) = True
isNotConstantFunctionType (AppT t1 t2) = isNotConstantFunctionType t1 || isNotConstantFunctionType t2
isNotConstantFunctionType (TupleT _) = False
isNotConstantFunctionType (ForallT _ _ t) = isNotConstantFunctionType t
isNotConstantFunctionType _ = False

needsTypeable :: Type -> Bool
needsTypeable = go
  where
    go (ForallT _ _ t) = go t
    go (AppT t1 t2) = go t1 || go t2
    go (SigT t _) = go t
    go (VarT _) = True
    go (ParensT t) = go t
    go (InfixT t1 _ t2) = go t1 || go t2
    go (UInfixT t1 _ t2) = go t1 || go t2
    go (ImplicitParamT _ t) = go t
    go _ = False

collectTypeVars :: Type -> [Name]
collectTypeVars (VarT name) = [name]
collectTypeVars (AppT t1 t2) = collectTypeVars t1 ++ collectTypeVars t2
collectTypeVars (SigT t _) = collectTypeVars t
collectTypeVars (ParensT t) = collectTypeVars t
collectTypeVars (InfixT t1 _ t2) = collectTypeVars t1 ++ collectTypeVars t2
collectTypeVars (UInfixT t1 _ t2) = collectTypeVars t1 ++ collectTypeVars t2
collectTypeVars (ForallT _ _ t) = collectTypeVars t
collectTypeVars (ImplicitParamT _ t) = collectTypeVars t
collectTypeVars _ = []

