{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.TypeUtils
  ( splitApps,
    substituteType
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

