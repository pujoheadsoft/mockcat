{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Test.MockCat.TH.Context
  ( MockType (..),
    buildContext,
    toVarTs,
    constructClassAppT,
    getTypeVarNames,
    getTypeVarName
  )
where

import Control.Monad.IO.Class (MonadIO)

import Language.Haskell.TH
  ( Name,
    Pred,
    TyVarBndr (..),
    Type (..),
  )
import Test.MockCat.TH.ClassInfo
  ( ClassName2VarNames (..),
    toClassInfos
  )
import Test.MockCat.TH.Constraint (liftConstraint)
import Test.MockCat.TH.VarApplied
  ( VarAppliedType (..),
    updateType
  )

data MockType = Total | Partial
  deriving (Eq)

buildContext ::
  [Pred] ->
  MockType ->
  Name ->
  Name ->
  [TyVarBndr a] ->
  [VarAppliedType] ->
  [Pred]
buildContext cxt mockType className monadVarName tyVars varAppliedTypes =
  let newCxtRaw = fmap (liftConstraint monadVarName) cxt

      isRedundantMonad (AppT (ConT m) (VarT v)) = m == ''Monad && v == monadVarName
      isRedundantMonad _ = False
      newCxt = filter (not . isRedundantMonad) newCxtRaw

      monadIOAppT = AppT (ConT ''MonadIO) (VarT monadVarName)

      classInfos = toClassInfos newCxt
      hasMonadIO = any (\(ClassName2VarNames c _) -> c == ''MonadIO) classInfos
      addedMonads = [monadIOAppT | not hasMonadIO]
   in case mockType of
        Total -> newCxt ++ addedMonads
        Partial ->
          let classAppT = constructClassAppT className $ toVarTs tyVars
              varAppliedClassAppT = updateType classAppT varAppliedTypes
           in newCxt ++ addedMonads ++ [varAppliedClassAppT]

toVarTs :: [TyVarBndr a] -> [Type]
toVarTs tyVars = VarT <$> getTypeVarNames tyVars

constructClassAppT :: Name -> [Type] -> Type
constructClassAppT className = foldl AppT (ConT className)

getTypeVarNames :: [TyVarBndr a] -> [Name]
getTypeVarNames = map getTypeVarName

getTypeVarName :: TyVarBndr a -> Name
getTypeVarName (PlainTV varName _) = varName
getTypeVarName (KindedTV varName _ _) = varName

