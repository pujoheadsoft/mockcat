{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.MockFnContext
  ( partialAdditionalPredicates
  , collectTypeVars
  , needsTypeable
  )
where

import Data.List (nub)
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Name,
    Pred,
    Type (..),
  )
import Test.MockCat.Verify (ResolvableParamsOf)

partialAdditionalPredicates :: Type -> Type -> [Pred]
partialAdditionalPredicates funType verifyParams =
  typeablePreds ++ eqConstraint
  where
    typeablePreds =
      [ AppT (ConT ''Typeable) (VarT varName)
      | varName <- nub (collectTypeVars funType ++ collectTypeVars verifyParams)
      ]
    eqConstraint =
      [ AppT
          (AppT EqualityT (AppT (ConT ''ResolvableParamsOf) funType))
          verifyParams
      | not (null (collectTypeVars funType))
      ]

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

