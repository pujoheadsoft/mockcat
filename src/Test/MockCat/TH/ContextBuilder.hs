{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Test.MockCat.TH.ContextBuilder
  ( -- Constraint rewrite
    liftConstraint,
    -- MonadVar helpers
    mockTType,
    tyVarBndrToType,
    applyFamilyArg,
    -- Context builder
    MockType (..),
    buildContext,
    toVarTs,
    constructClassAppT,
    getTypeVarNames,
    getTypeVarName
  )
where
import Language.Haskell.TH
  ( Name,
    TyVarBndr (..),
    Type (..),
    Pred
  )
import Control.Monad.IO.Class (MonadIO)
import Test.MockCat.MockT (MockT)
import Test.MockCat.TH.ClassAnalysis (ClassName2VarNames (..), toClassInfos, VarAppliedType (..), updateType)

-- | Rewrite constraint types to use 'MockT' for the monad variable where needed.
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

-- MonadVar helpers
mockTType :: Name -> Type
mockTType monadVarName = AppT (ConT ''MockT) (VarT monadVarName)

liftTyVar :: Name -> Name -> Type
liftTyVar monadVarName varName
  | monadVarName == varName = mockTType monadVarName
  | otherwise = VarT varName

tyVarBndrToType :: Name -> TyVarBndr a -> Type
tyVarBndrToType monadVarName (PlainTV binderName _) = liftTyVar monadVarName binderName
tyVarBndrToType monadVarName (KindedTV binderName _ _) = liftTyVar monadVarName binderName

applyFamilyArg :: Name -> TyVarBndr a -> Type
applyFamilyArg = tyVarBndrToType

-- Context builder
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


