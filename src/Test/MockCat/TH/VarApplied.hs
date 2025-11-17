{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.VarApplied
  ( VarAppliedType (..),
    applyVarAppliedTypes,
    updateType,
    findClass,
    hasClass
  )
where

import Control.Monad (guard)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Language.Haskell.TH
  ( Name,
    Type (..),
  )

data VarAppliedType = VarAppliedType {name :: Name, appliedClassName :: Maybe Name}
  deriving (Show)

applyVarAppliedTypes :: [VarAppliedType] -> Type -> Type
applyVarAppliedTypes varAppliedTypes = transform
  where
    mapping =
      Map.fromList
        [ (varName, className)
        | VarAppliedType varName (Just className) <- varAppliedTypes
        ]
    transform (VarT n) =
      case Map.lookup n mapping of
        Just className -> ConT className
        Nothing -> VarT n
    transform (AppT t1 t2) = AppT (transform t1) (transform t2)
    transform (SigT t k) = SigT (transform t) k
    transform (ParensT t) = ParensT (transform t)
    transform (InfixT t1 n t2) = InfixT (transform t1) n (transform t2)
    transform (UInfixT t1 n t2) = UInfixT (transform t1) n (transform t2)
    transform (ForallT tvs ctx t) = ForallT tvs (map transform ctx) (transform t)
    transform t = t

updateType :: Type -> [VarAppliedType] -> Type
updateType (AppT (VarT v1) (VarT v2)) varAppliedTypes =
  let x = maybe (VarT v1) ConT (findClass v1 varAppliedTypes)
      y = maybe (VarT v2) ConT (findClass v2 varAppliedTypes)
   in AppT x y
updateType ty _ = ty

hasClass :: Name -> [VarAppliedType] -> Bool
hasClass varName = any (\(VarAppliedType v c) -> v == varName && isJust c)

findClass :: Name -> [VarAppliedType] -> Maybe Name
findClass varName types = do
  guard $ hasClass varName types
  (VarAppliedType _ c) <- find (\(VarAppliedType v _) -> v == varName) types
  c

