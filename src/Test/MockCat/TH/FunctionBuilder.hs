{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Test.MockCat.TH.FunctionBuilder
  ( createMockBuilderVerifyParams
  , createMockBuilderFnType
  , MockFnContext(..)
  , MockFnBuilder(..)
  , buildMockFnContext
  , buildMockFnDeclarations
  , determineMockFnBuilder
  , createNoInlinePragma
  , doCreateMockFnDecs
  , doCreateConstantMockFnDecs
  , doCreateEmptyVerifyParamMockFnDecs
  , createMockBody
  , createFnName
  , findParam
  , typeToNames
  , safeIndex
  , generateInstanceMockFnBody
  , generateInstanceRealFnBody
  , generateStubFn
  , partialAdditionalPredicates
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Language.Haskell.TH
  ( Dec (..),
    Exp (..),
    Name,
    Pred,
    Q,
    Quote,
    Type (..),
    TyVarBndr(..),
    Inline (NoInline),
    RuleMatch (FunLike),
    Phases (AllPhases),
    mkName,
    newName
  )
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (nameBase, Specificity (SpecifiedSpec))
import Test.MockCat.Mock ( createNamedMockFnWithParams, MockBuilder )
import Test.MockCat.Cons (Head(..), (:>)(..))
import Test.MockCat.MockT
  ( MockT (..),
    Definition (..),
    getDefinitions,
    addDefinition
  )
import Test.MockCat.TH.TypeUtils
  ( isNotConstantFunctionType,
    needsTypeable,
    collectTypeVars
  )
import Test.MockCat.TH.ContextBuilder
  ( MockType (..)
  )
import Test.MockCat.TH.ClassAnalysis
  ( VarAppliedType (..),
    updateType
  )
import Test.MockCat.Verify (ResolvableParamsOf, resolveForVerification, verificationFailure)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Data.List (find, nub, nubBy)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Ppr (pprint)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- copy of createMockBuilder functions
import Test.MockCat.Param (Param, param)
import Test.MockCat.TH.Types (MockOptions(..))

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

-- remove Typeable () (unit) and duplicate preds (comparing pretty-printed)
filterTypeablePreds :: [Pred] -> [Pred]
filterTypeablePreds preds =
  nubBy (\a b -> pprint a == pprint b) $ filter (not . isUnitTypePred) preds

isUnitTypePred :: Pred -> Bool
isUnitTypePred (AppT (ConT _) t) = isUnitType t
isUnitTypePred _ = False

isUnitType :: Type -> Bool
isUnitType (TupleT 0) = True
isUnitType (ParensT t) = isUnitType t
isUnitType _ = False

-- The following are copies of the function generation utilities from TH.hs

data MockFnContext = MockFnContext
  { mockType :: MockType,
    monadVarName :: Name,
    mockOptions :: MockOptions,
    originalType :: Type,
    fnNameStr :: String,
    mockFnName :: Name,
    paramsName :: Name,
    updatedType :: Type,
    fnType :: Type
  }

data MockFnBuilder = VariadicBuilder | ConstantImplicitBuilder | ConstantExplicitBuilder

buildMockFnContext ::
  MockType ->
  Name ->
  [VarAppliedType] ->
  MockOptions ->
  Name ->
  Type ->
  MockFnContext
buildMockFnContext mockType monadVarName varAppliedTypes mockOptions sigFnName ty =
  let fnNameStr = createFnName sigFnName mockOptions
      mockFnName = mkName fnNameStr
      params = mkName "p"
      updatedType = updateType ty varAppliedTypes
      fnType =
        if mockOptions.implicitMonadicReturn
          then createMockBuilderFnType monadVarName updatedType
          else updatedType
   in MockFnContext
        { mockType,
          monadVarName,
          mockOptions,
          originalType = ty,
          fnNameStr,
          mockFnName,
          paramsName = params,
          updatedType,
          fnType
        }

buildMockFnDeclarations :: MockFnContext -> Q [Dec]
buildMockFnDeclarations ctx@MockFnContext{mockType, fnNameStr, mockFnName, paramsName, fnType, monadVarName, updatedType} =
  case determineMockFnBuilder ctx of
    VariadicBuilder ->
      doCreateMockFnDecs mockType fnNameStr mockFnName paramsName fnType monadVarName updatedType
    ConstantImplicitBuilder ->
      doCreateConstantMockFnDecs mockType fnNameStr mockFnName fnType monadVarName
    ConstantExplicitBuilder ->
      doCreateEmptyVerifyParamMockFnDecs fnNameStr mockFnName paramsName fnType monadVarName updatedType

determineMockFnBuilder :: MockFnContext -> MockFnBuilder
determineMockFnBuilder ctx
  | isNotConstantFunctionType (originalType ctx) = VariadicBuilder
  | (mockOptions ctx).implicitMonadicReturn = ConstantImplicitBuilder
  | otherwise = ConstantExplicitBuilder

createNoInlinePragma :: Name -> Q Dec
createNoInlinePragma name = pragInlD name NoInline FunLike AllPhases

doCreateMockFnDecs :: (Quote m) => MockType -> String -> Name -> Name -> Type -> Name -> Type -> m [Dec]
doCreateMockFnDecs mockType funNameStr mockFunName params funType monadVarName updatedType = do
  newFunSig <- do
    let verifyParams = createMockBuilderVerifyParams updatedType
        mockBuilderPred =
          AppT (AppT (AppT (ConT ''MockBuilder) (VarT params)) funType) verifyParams
        eqConstraint =
          [ AppT
              (AppT EqualityT (AppT (ConT ''ResolvableParamsOf) funType))
              verifyParams
          | not (null (collectTypeVars funType))
          ]
        baseCtx =
          ([mockBuilderPred | verifyParams /= TupleT 0])
            ++ [AppT (ConT ''MonadIO) (VarT monadVarName)]
        ctx = case mockType of
          Partial ->
            baseCtx ++ partialAdditionalPredicates funType verifyParams
          Total ->
            let typeableTargetsBase =
                  [ funType
                  , verifyParams
                  ]
                typeableTargets =
                  typeableTargetsBase
                    ++ (if null eqConstraint then [AppT (ConT ''ResolvableParamsOf) funType] else [])
                typeablePreds =
                  [ AppT (ConT ''Typeable) target
                  | target <- typeableTargets
                  , needsTypeable target
                  ]
             in baseCtx ++ eqConstraint ++ filterTypeablePreds typeablePreds
        resultType =
          AppT
            (AppT ArrowT (VarT params))
            (AppT (AppT (ConT ''MockT) (VarT monadVarName)) funType)
    sigD mockFunName (pure (ForallT [] ctx resultType))

  createMockFn <- [|createNamedMockFnWithParams|]

  mockBody <- createMockBody funNameStr createMockFn [|p|]
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]

  pure $ newFunSig : [newFun]

doCreateConstantMockFnDecs :: (Quote m) => MockType -> String -> Name -> Type -> Name -> m [Dec]
doCreateConstantMockFnDecs Partial funNameStr mockFunName _ monadVarName = do
  stubVar <- newName "r"
  let ctx =
        [ AppT
            (AppT EqualityT (AppT (ConT ''ResolvableParamsOf) (VarT stubVar)))
            (TupleT 0)
        , AppT (ConT ''MonadIO) (VarT monadVarName)
        , AppT (ConT ''Typeable) (VarT stubVar)
        ]
      resultType =
        AppT
          (AppT ArrowT (VarT stubVar))
          (AppT (AppT (ConT ''MockT) (VarT monadVarName)) (VarT stubVar))
  newFunSig <-
    sigD
      mockFunName
      ( pure
          (ForallT
              [ PlainTV stubVar SpecifiedSpec
              , PlainTV monadVarName SpecifiedSpec
              ]
              ctx
              resultType
          )
      )
  createMockFn <- [|createNamedMockFnWithParams|]
  headParam <- [|Head :> param p|]
  mockBody <- createMockBody funNameStr createMockFn (pure headParam)
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]
  pure $ newFunSig : [newFun]
doCreateConstantMockFnDecs Total funNameStr mockFunName ty monadVarName = do
  newFunSig <- case ty of
    AppT (ConT _) (VarT mv) | mv == monadVarName -> do
      a <- newName "a"
      let ctx =
            [ AppT (ConT ''MonadIO) (VarT monadVarName)
            , AppT (AppT EqualityT (AppT (ConT ''ResolvableParamsOf) (VarT a))) (TupleT 0)
            , AppT (ConT ''Typeable) (VarT a)
            ]
          resultType =
            AppT
              (AppT ArrowT (VarT a))
              (AppT (AppT (ConT ''MockT) (VarT monadVarName)) (VarT a))
      sigD
        mockFunName
        ( pure
            (ForallT
                [PlainTV a SpecifiedSpec, PlainTV monadVarName SpecifiedSpec]
                ctx
                resultType
            )
        )
    _ -> do
      let headParamType = AppT (AppT (ConT ''(:>)) (ConT ''Head)) (AppT (ConT ''Param) ty)
          typeableTargets =
            [ ty
            , AppT (ConT ''ResolvableParamsOf) ty
            ]
          typeablePreds =
            [ AppT (ConT ''Typeable) target
            | target <- typeableTargets
            , needsTypeable target
            ]
          verifyParams' = createMockBuilderVerifyParams ty
          mockBuilderPred' = AppT (AppT (AppT (ConT ''MockBuilder) headParamType) ty) (TupleT 0)
          typeablePreds' =
            [ AppT (ConT ''Typeable) ty
            | needsTypeable ty
            ]
            ++ typeablePreds
          ctx =
            [ AppT (ConT ''MonadIO) (VarT monadVarName)
            ]
            ++ ([mockBuilderPred' | verifyParams' /= TupleT 0])
            ++ typeablePreds'
          resultType =
            AppT
              (AppT ArrowT ty)
              (AppT (AppT (ConT ''MockT) (VarT monadVarName)) ty)
      sigD mockFunName (pure (ForallT [PlainTV monadVarName SpecifiedSpec] ctx resultType))
  createMockFn <- [|createNamedMockFnWithParams|]
  headParam <- [|Head :> param p|]
  mockBody <- createMockBody funNameStr createMockFn (pure headParam)
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]
  pure $ newFunSig : [newFun]

doCreateEmptyVerifyParamMockFnDecs :: (Quote m) => String -> Name -> Name -> Type -> Name -> Type -> m [Dec]
doCreateEmptyVerifyParamMockFnDecs funNameStr mockFunName params funType monadVarName updatedType = do
  newFunSig <- do
    let verifyParams = createMockBuilderVerifyParams updatedType
        typeableTargets =
          [ funType
          , verifyParams
          , AppT (ConT ''ResolvableParamsOf) funType
          ]
        typeablePreds =
          [ AppT (ConT ''Typeable) target
          | target <- typeableTargets
          , needsTypeable target
          ]
        mockBuilderPred = AppT (AppT (AppT (ConT ''MockBuilder) (VarT params)) funType) verifyParams
        ctx =
          [mockBuilderPred]
            ++ [AppT (ConT ''MonadIO) (VarT monadVarName)]
            ++ filterTypeablePreds typeablePreds
        resultType =
          AppT
            (AppT ArrowT (VarT params))
            (AppT (AppT (ConT ''MockT) (VarT monadVarName)) funType)
    sigD mockFunName (pure (ForallT [] ctx resultType))

  createMockFn <- [|createNamedMockFnWithParams|]

  mockBody <- createMockBody funNameStr createMockFn [|p|]
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]

  pure $ newFunSig : [newFun]

createMockBody :: (Quote m) => String -> Exp -> m Exp -> m Exp
createMockBody funNameStr createMockFn paramsExp =
  paramsExp >>= \params ->
  [|
    MockT $ do
      mockInstance <- liftIO $ $(pure createMockFn) $(litE (stringL funNameStr)) $(pure params)
      liftIO do
        resolveForVerification mockInstance >>= \case
          Just _ -> pure ()
          Nothing -> verificationFailure
      let verifyStub _ = pure ()
      addDefinition
        ( Definition
            (Proxy :: Proxy $(litT (strTyLit funNameStr)))
            mockInstance
            verifyStub
        )
      pure mockInstance
    |]

createFnName :: Name -> MockOptions -> String
createFnName funName opts = do
  opts.prefix <> nameBase funName <> opts.suffix

findParam :: (KnownSymbol sym) => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFunction _) -> unsafeCoerce mockFunction) definition

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT _) t2) = newName "a" : typeToNames t2
typeToNames (ForallT _ _ ty) = typeToNames ty
typeToNames _ = []

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) n
  | n < 0 = Nothing
  | otherwise = safeIndex xs (n - 1)

generateInstanceMockFnBody :: String -> [Q Exp] -> Name -> MockOptions -> Q Exp
generateInstanceMockFnBody fnNameStr args r opts = do
  returnExp <- if opts.implicitMonadicReturn
    then [| pure $(varE r) |]
    else [| lift $(varE r) |]
  [|
    MockT $ do
      defs <- getDefinitions
      let _mock =
            defs
              & findParam (Proxy :: Proxy $(litT (strTyLit fnNameStr)))
              & fromMaybe (error $ "no answer found stub function `" ++ fnNameStr ++ "`.")
          $(bangP $ varP r) = $(generateStubFn args [|_mock|])
      $(pure returnExp)
    |]

generateInstanceRealFnBody :: Name -> String -> [Q Exp] -> Name -> MockOptions -> Q Exp
generateInstanceRealFnBody fnName fnNameStr args r opts = do
  returnExp <- if opts.implicitMonadicReturn
    then [| pure $(varE r) |]
    else [| lift $(varE r) |]
  [|
    MockT $ do
      defs <- getDefinitions
      case findParam (Proxy :: Proxy $(litT (strTyLit fnNameStr))) defs of
        Just mock -> do
          let $(bangP $ varP r) = $(generateStubFn args [|mock|])
          $(pure returnExp)
        Nothing -> lift $ $(foldl appE (varE fnName) args)
    |]

generateStubFn :: [Q Exp] -> Q Exp -> Q Exp
generateStubFn [] mock = mock
generateStubFn args mock = foldl appE mock args


