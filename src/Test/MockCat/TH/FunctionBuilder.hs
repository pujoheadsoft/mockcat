{-# LANGUAGE TemplateHaskell #-}
-- Force recompile 1
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
  , createTypeablePreds
  , partialAdditionalPredicates
  , createFnName
  , findParam
  , typeToNames
  , safeIndex
  , generateInstanceMockFnBody
  , generateInstanceRealFnBody
  , generateStubFn
  )
where

import Control.Monad.IO.Class (MonadIO)
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
import Test.MockCat.Mock (IsMockSpec, MockDispatch(..), label)
import Test.MockCat.Internal.Types (InvocationRecorder)
import Test.MockCat.Cons ((:>)(..))
import Test.MockCat.MockT
  ( MockT (..),
    Definition (..),
    getDefinitions,
    addDefinition
  )
import Test.MockCat.TH.TypeUtils
  ( isNotConstantFunctionType,
    needsTypeable,
    collectTypeVars,
    collectTypeableTargets,
    splitApps
  )
import Test.MockCat.TH.ContextBuilder
  ( MockType (..)
  )
import Test.MockCat.TH.ClassAnalysis
  ( VarAppliedType (..),
    updateType
  )
import Test.MockCat.Verify (ResolvableParamsOf)
import Data.Dynamic (Dynamic, toDyn)
import Data.Proxy (Proxy(..))
import Data.List (find, nubBy, nub)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Ppr (pprint)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (KnownSymbol, symbolVal)
 

import Test.MockCat.Param (Param)
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
  [ AppT
      (AppT EqualityT (AppT (ConT ''ResolvableParamsOf) funType))
      verifyParams
  | not (null (collectTypeVars funType))
  ]

-- Helper to create Typeable predicates using the smart collection logic
createTypeablePreds :: [Type] -> [Pred]
createTypeablePreds targets =
  [ AppT (ConT ''Typeable) t
  | t <- nubBy (\a b -> pprint a == pprint b) (concatMap collectTypeableTargets targets)
  , needsTypeable t
  ]


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
doCreateMockFnDecs mockType funNameStr mockFunName params funTypeInput monadVarName _ = do
  let funType = sanitizeType [monadVarName] funTypeInput
  newFunSig <- do
    let resultType =
          AppT
            (AppT ArrowT (VarT params))
            (AppT (AppT (ConT ''MockT) (VarT monadVarName)) funType)
        
        mockTType = AppT (ConT ''MockT) (VarT monadVarName)
        flag = AppT (ConT ''IsMockSpec) (VarT params)
        createMockFnPred =
          AppT (AppT (AppT (AppT (ConT ''MockDispatch) flag) (VarT params)) mockTType) funType
        
        recType = AppT (ConT ''InvocationRecorder) (AppT (ConT ''ResolvableParamsOf) funType)
        recConstraint = AppT (ConT ''Typeable) recType
        
        paramsType = AppT (ConT ''ResolvableParamsOf) funType
        paramsConstraint = AppT (ConT ''Typeable) paramsType

        baseCtx =
          [createMockFnPred, AppT (ConT ''MonadIO) (VarT monadVarName), recConstraint, paramsConstraint]
          ++ createTypeablePreds [funType]
        
        ctx = case mockType of
          Partial -> baseCtx
          Total -> baseCtx
    
    let vars = collectFreeVars funType ++ [params, monadVarName]
    let tvs = map (\n -> PlainTV n SpecifiedSpec) (nub vars)
    let finalCtx = filter (not . isRedundantTypeable monadVarName) ctx
    sigD mockFunName (pure (ForallT tvs finalCtx resultType))

  mockBody <- createMockBody funNameStr [|p|] (VarT params)
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]

  pure $ newFunSig : [newFun]

doCreateConstantMockFnDecs :: (Quote m) => MockType -> String -> Name -> Type -> Name -> m [Dec]
doCreateConstantMockFnDecs Partial funNameStr mockFunName ty monadVarName = do
  let stubVar = mkName "p" 
  let tySanitized = sanitizeType [monadVarName] ty
  let resultType =
        AppT
          (AppT ArrowT (VarT stubVar))
          (AppT (AppT (ConT ''MockT) (VarT monadVarName)) tySanitized)
      
  let mockTType = AppT (ConT ''MockT) (VarT monadVarName)
  let flag = AppT (ConT ''IsMockSpec) (VarT stubVar)
  let createMockFnPred =
          AppT (AppT (AppT (AppT (ConT ''MockDispatch) flag) (VarT stubVar)) mockTType) tySanitized

  let recType = AppT (ConT ''InvocationRecorder) (AppT (ConT ''ResolvableParamsOf) tySanitized)
  let recConstraint = AppT (ConT ''Typeable) recType
  
  let paramsType = AppT (ConT ''ResolvableParamsOf) tySanitized
  let paramsConstraint = AppT (ConT ''Typeable) paramsType

  let ctx =
        [ createMockFnPred
        , AppT (ConT ''MonadIO) (VarT monadVarName)
        , recConstraint
        , paramsConstraint
        ]
        ++ createTypeablePreds [tySanitized]
  let finalCtx = filter (not . isRedundantTypeable monadVarName) ctx
  let vars = collectFreeVars tySanitized ++ [stubVar, monadVarName]
  let tvs = map (\n -> PlainTV n SpecifiedSpec) (nub vars)
  newFunSig <-
    sigD
      mockFunName
      ( pure
          (ForallT
              tvs
              finalCtx
              resultType
          )
      )
  mockBody <- createMockBody funNameStr [|p|] (VarT stubVar)
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]
  pure $ newFunSig : [newFun]
doCreateConstantMockFnDecs Total funNameStr mockFunName ty monadVarName = do
  case ty of
    -- Case 3: Generic (Polymorphic p)
    _ -> do
      let params = mkName "p"
      let tySanitized = sanitizeType [monadVarName] ty
      let resultType =
            AppT
              (AppT ArrowT (VarT params))
              (AppT (AppT (ConT ''MockT) (VarT monadVarName)) tySanitized)
          
          mockTType = AppT (ConT ''MockT) (VarT monadVarName)
          flag = AppT (ConT ''IsMockSpec) (VarT params)
          createMockFnPred =
              AppT (AppT (AppT (AppT (ConT ''MockDispatch) flag) (VarT params)) mockTType) tySanitized

          recType = AppT (ConT ''InvocationRecorder) (AppT (ConT ''ResolvableParamsOf) tySanitized)
          recConstraint = AppT (ConT ''Typeable) recType
          
          paramsType = AppT (ConT ''ResolvableParamsOf) tySanitized
          paramsConstraint = AppT (ConT ''Typeable) paramsType

          ctx =
            [ createMockFnPred
            , AppT (ConT ''MonadIO) (VarT monadVarName)
            , recConstraint
            , paramsConstraint
            ]
            ++ createTypeablePreds [tySanitized]
            
      let tvs = map (\n -> PlainTV n SpecifiedSpec) (nub (collectFreeVars tySanitized ++ [params, monadVarName]))
      let finalCtx = filter (not . isRedundantTypeable monadVarName) ctx
      newFunSig <- sigD mockFunName (pure (ForallT tvs finalCtx resultType))

      mockBody <- createMockBody funNameStr [|p|] (VarT params)
      newFun <- funD mockFunName [clause [varP params] (normalB (pure mockBody)) []]
      pure [newFunSig, newFun]

doCreateEmptyVerifyParamMockFnDecs :: (Quote m) => String -> Name -> Name -> Type -> Name -> Type -> m [Dec]
doCreateEmptyVerifyParamMockFnDecs funNameStr mockFunName params funTypeInput monadVarName updatedType = do
  let funType = sanitizeType [monadVarName] funTypeInput
  newFunSig <- do
    let verifyParams = createMockBuilderVerifyParams updatedType
        resultType =
          AppT
            (AppT ArrowT (VarT params))
            (AppT (AppT (ConT ''MockT) (VarT monadVarName)) funType)
        
        mockTType = AppT (ConT ''MockT) (VarT monadVarName)
        flag = AppT (ConT ''IsMockSpec) (VarT params)
        createMockFnPred =
          AppT (AppT (AppT (AppT (ConT ''MockDispatch) flag) (VarT params)) mockTType) funType

        recType = AppT (ConT ''InvocationRecorder) (AppT (ConT ''ResolvableParamsOf) funType)
        recConstraint = AppT (ConT ''Typeable) recType
        
        paramsType = AppT (ConT ''ResolvableParamsOf) funType
        paramsConstraint = AppT (ConT ''Typeable) paramsType

        ctx =
          [createMockFnPred]
            ++ [AppT (ConT ''MonadIO) (VarT monadVarName)]
            ++ [recConstraint, paramsConstraint]
            ++ createTypeablePreds [funType, verifyParams]
        
        finalCtx = filter (not . isRedundantTypeable monadVarName) ctx
    
    let vars = collectFreeVars funType ++ [params, monadVarName]
    let tvs = map (\n -> PlainTV n SpecifiedSpec) (nub vars)
    sigD mockFunName (pure (ForallT tvs finalCtx resultType))

  mockBody <- createMockBody funNameStr [|p|] (VarT params)
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]

  pure $ newFunSig : [newFun]

createMockBody :: (Quote m) => String -> m Exp -> Type -> m Exp
createMockBody funNameStr paramsExp paramsType = do
  params <- paramsExp
  let flag = AppT (ConT ''IsMockSpec) paramsType
  [|
    MockT $ do
      mockInstance <- unMockT $ $(appTypeE (varE 'mockDispatchImpl) (pure flag)) (label $(litE (stringL funNameStr))) $(pure params)
      addDefinition
        ( Definition
            (Proxy :: Proxy $(litT (strTyLit funNameStr)))
            mockInstance
            NoVerification
        )
      pure mockInstance
    |]

createFnName :: Name -> MockOptions -> String
createFnName funName opts = do
  opts.prefix <> nameBase funName <> opts.suffix

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe Dynamic
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFunction _) -> toDyn mockFunction) definition

collectFreeVars :: Type -> [Name]
collectFreeVars (ForallT bndrs _ t) =
  let boundNames = map getTVName bndrs
   in filter (`notElem` boundNames) (collectFreeVars t)
collectFreeVars (AppT t1 t2) = collectFreeVars t1 ++ collectFreeVars t2
collectFreeVars (SigT t _) = collectFreeVars t
collectFreeVars (VarT n) = [n]
collectFreeVars _ = []

getTVName :: TyVarBndr flag -> Name
getTVName (PlainTV n _) = n
getTVName (KindedTV n _ _) = n

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT _) t2) = newName "a" : typeToNames t2
typeToNames (ForallT _ _ ty) = typeToNames ty
typeToNames _ = []

sanitizeType :: [Name] -> Type -> Type
sanitizeType kept (AppT t1 t2) = AppT (sanitizeType kept t1) (sanitizeType kept t2)
sanitizeType kept (SigT t k) = SigT (sanitizeType kept t) (sanitizeType kept k)
sanitizeType kept (VarT n)
  | n `elem` kept = VarT n
  | otherwise = VarT (mkName (nameBase n))
sanitizeType kept (ForallT bndrs ctx t) =
  let sanitizeBndr (PlainTV n flag)
        | n `elem` kept = PlainTV n flag
        | otherwise = PlainTV (mkName (nameBase n)) flag
      sanitizeBndr (KindedTV n flag k)
        | n `elem` kept = KindedTV n flag (sanitizeType kept k)
        | otherwise = KindedTV (mkName (nameBase n)) flag (sanitizeType kept k)
  in ForallT (map sanitizeBndr bndrs) (map (sanitizeType kept) ctx) (sanitizeType kept t)
sanitizeType _ t = t

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
      let findDef = find (\(Definition s _ _) -> symbolVal s == $(litE (stringL fnNameStr))) defs
      case findDef of
        Just (Definition _ mf _) -> do
          let mockFn = unsafeCoerce mf
          let $(bangP $ varP r) = $(generateStubFn args [|mockFn|])
          $(pure returnExp)
        Nothing -> error $ "no answer found stub function `" ++ fnNameStr ++ "`."
    |]

generateInstanceRealFnBody :: Name -> String -> [Q Exp] -> Name -> MockOptions -> Q Exp
generateInstanceRealFnBody fnName fnNameStr args r opts = do
  returnExp <- if opts.implicitMonadicReturn
    then [| pure $(varE r) |]
    else [| lift $(varE r) |]
  [|
    MockT $ do
      defs <- getDefinitions
      let findDef = find (\(Definition s _ _) -> symbolVal s == $(litE (stringL fnNameStr))) defs
      case findDef of
        Just (Definition _ mf _) -> do
          let mockFn = unsafeCoerce mf
          let $(bangP $ varP r) = $(generateStubFn args [|mockFn|])
          $(pure returnExp)
        Nothing -> lift $ $(foldl appE (varE fnName) args)
    |]

generateStubFn :: [Q Exp] -> Q Exp -> Q Exp
generateStubFn [] mockFn = mockFn
generateStubFn args mockFn = foldl appE mockFn args


isRedundantTypeable :: Name -> Pred -> Bool
isRedundantTypeable monadName (AppT (ConT n) t)
  | n == ''Typeable =
      if null (collectFreeVars t)
        then True
        else case t of
          VarT vn | nameBase vn == nameBase monadName -> False
          _ -> if any (\v -> nameBase v == nameBase monadName) (collectFreeVars t)
                 then not (isProtectedType t)
                 else False
  where
    isProtectedType ty =
      let (headTy, _) = splitApps ty
      in case headTy of
           ConT hn -> nameBase hn `elem` protectedTypes
           _ -> False

    protectedTypes =
      [ "ResolvableParamsOf"
      , "ResultType"
      , "InvocationRecorder"
      , "PrependParam"
      , "FunctionParams"
      ]
isRedundantTypeable _ _ = False




