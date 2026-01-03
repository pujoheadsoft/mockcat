{-# LANGUAGE TemplateHaskellQuotes #-}
module Test.MockCat.TH.TypeUtils
  ( splitApps,
    substituteType,
    isNotConstantFunctionType,
    collectTypeVars,
    needsTypeable,
    collectTypeableTargets,
    isStandardTypeCon,
    isTypeFamily,
    getReturnType

  )
where

import qualified Data.Map.Strict as Map
import Language.Haskell.TH (Name, Type (..), Q, reify, Info(FamilyI), recover)
import Test.MockCat.Param (Param)
import Test.MockCat.Cons ((:>))

splitApps :: Type -> (Type, [Type])
splitApps ty = go ty []
  where
    go (Language.Haskell.TH.AppT t1 t2) acc = go t1 (t2 : acc)
    go t acc = (t, acc)

substituteType :: Map.Map Language.Haskell.TH.Name Language.Haskell.TH.Type -> Language.Haskell.TH.Type -> Language.Haskell.TH.Type
substituteType subMap = go
  where
    go (Language.Haskell.TH.VarT name) = Map.findWithDefault (Language.Haskell.TH.VarT name) name subMap
    go (Language.Haskell.TH.AppT t1 t2) = Language.Haskell.TH.AppT (go t1) (go t2)
    go (Language.Haskell.TH.SigT t k) = Language.Haskell.TH.SigT (go t) k
    go (Language.Haskell.TH.ParensT t) = Language.Haskell.TH.ParensT (go t)
    go (Language.Haskell.TH.InfixT t1 n t2) = Language.Haskell.TH.InfixT (go t1) n (go t2)
    go (Language.Haskell.TH.UInfixT t1 n t2) = Language.Haskell.TH.UInfixT (go t1) n (go t2)
    go (Language.Haskell.TH.ForallT tvs ctx t) = Language.Haskell.TH.ForallT tvs (map go ctx) (go t)
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
    
peel :: Type -> Type
peel (SigT t _) = peel t
peel (ParensT t) = peel t
peel t = t

collectTypeableTargets :: Type -> Q [Type]
collectTypeableTargets ty =
  case ty of
    VarT _ -> pure [ty]
    AppT _ _ ->
      let (headTy, args) = splitApps ty
       in do
         isTF <- isTypeFamily headTy
         if isTF
           then pure [ty]
           else case peel headTy of
             ConT _ -> concat <$> mapM collectTypeableTargets args
             ListT -> pure [ty]
             TupleT _ -> concat <$> mapM collectTypeableTargets args
             VarT _ -> do
               headResult <- pure (peel headTy)
               argsResult <- concat <$> mapM collectTypeableTargets args
               pure (headResult : argsResult)
             ArrowT -> concat <$> mapM collectTypeableTargets args
             _ -> concat <$> mapM collectTypeableTargets args
    SigT t _ -> collectTypeableTargets t
    ParensT t -> collectTypeableTargets t
    InfixT t1 _ t2 -> (++) <$> collectTypeableTargets t1 <*> collectTypeableTargets t2
    UInfixT t1 _ t2 -> (++) <$> collectTypeableTargets t1 <*> collectTypeableTargets t2
    ForallT _ _ t -> collectTypeableTargets t
    _ -> pure []

isTypeFamily :: Type -> Q Bool
isTypeFamily (ConT name) = recover (pure False) $ do
  info <- reify name
  case info of
    FamilyI _ _ -> pure True
    _ -> pure False
isTypeFamily _ = pure False

isStandardTypeCon :: Type -> Bool
isStandardTypeCon ArrowT = True
isStandardTypeCon ListT = True
isStandardTypeCon (TupleT _) = True
isStandardTypeCon (ConT n) =
  n `elem`
    [ ''Maybe
    , ''IO
    , ''Either
    , ''[]
    , ''(,)
    , ''(,,)
    , ''(,,,)
    , ''(,,,,)
    , ''Param
    , ''(:>)
    ]
isStandardTypeCon _ = False


getReturnType :: Type -> Type
getReturnType (AppT (AppT ArrowT _) t) = getReturnType t
getReturnType (ForallT _ _ t) = getReturnType t
getReturnType (SigT t _) = getReturnType t
getReturnType (ParensT t) = getReturnType t
getReturnType t = t
