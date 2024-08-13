{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock, makeMockWithOptions, MockOptions(..), options) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, mkName, Type (..), Quote (newName), Cxt, TyVarBndr (..), Pred, Specificity)
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param (Param(..))
import Test.MockCat.Cons ((:>))
import Test.MockCat.MockT
import Data.Data (Proxy(..))
import Data.List (find, nub, elemIndex, intercalate)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe, isJust)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Lib
import Data.Text (splitOn, unpack, pack)
import Test.MockCat.Mock (MockBuilder)
import Control.Monad (filterM, guard)

showExp :: Q Exp -> Q String
showExp qexp = show . pprintExp <$> qexp

pprintExp :: Exp -> Doc
pprintExp (VarE name) = text (nameBase name)
pprintExp (ConE name) = text (nameBase name)
pprintExp (LitE lit) = pprintLit lit
pprintExp (AppE e1 e2) = parens $ hcat [pprintExp e1, text " ", pprintExp e2]
pprintExp (InfixE e1 e2 e3) = pprintInfixE e1 e2 e3
pprintExp (LamE pats body) = parens $ hcat [text "\\", pprintPats pats, text " -> ", pprintExp body]
pprintExp (TupE exps) = parens $ hcat (map (maybe (text "") pprintExp) exps)
pprintExp (ListE exps) = parens $ hcat (map pprintExp exps)
pprintExp (SigE e _) = pprintExp e
pprintExp x = text (pprint x)

pprintInfixE :: Maybe Exp -> Exp -> Maybe Exp -> Doc
pprintInfixE e1 e2 e3 =
  parens $
    hcat
      [ maybe (text "") pprintExp e1,
        maybe (text "") (const (text " ")) e1,
        pprintExp e2,
        text " ",
        maybe (text "") pprintExp e3
      ]

pprintPats :: [Pat] -> Doc
pprintPats = hcat . map pprintPat

pprintPat :: Pat -> Doc
pprintPat (VarP name) = text (nameBase name)
pprintPat p = text (pprint p)

pprintLit :: Lit -> Doc
pprintLit (IntegerL n) = text (show n)
pprintLit (RationalL r) = text (show r)
pprintLit (StringL s) = text (show s)
pprintLit (CharL c) = text (show c)
pprintLit l = text (pprint l)

{- | Create a conditional parameter based on @Q Exp@. 

  In applying a mock function, if the argument does not satisfy this condition, an error is raised.

  The conditional expression is displayed in the error message.
-}
expectByExpr :: Q Exp -> Q Exp
expectByExpr qf = do
  str <- showExp qf
  [|ExpectCondition $qf str|]

data MockOptions = MockOptions { prefix :: String, suffix :: String }

options :: MockOptions
options = MockOptions { prefix = "_", suffix = "" }

makeMockWithOptions :: Q Type -> MockOptions -> Q [Dec]
makeMockWithOptions = doMakeMock

makeMock :: Q Type -> Q [Dec]
makeMock = flip doMakeMock options

doMakeMock :: Q Type -> MockOptions -> Q [Dec]
doMakeMock t options = do
  ty <- t
  className <- getClassName <$> t

  reify className >>= \case
    ClassI (ClassD _ _ [] _ _) _ -> fail $ "A type parameter is required for class " <> show className
    ClassI (ClassD cxt _ typeParameters _ decs) _ -> do
      monadVarNames <- getMonadVarNames cxt typeParameters
      if length (nub monadVarNames) > 1 then fail "Monad parameter must be unique."
      else if null monadVarNames then fail "Monad parameter not found."
      else do
        let monadVarName = head monadVarNames

        let classParamNames = filter (className /=) (getClassNames ty)
            typeVars = drop (length classParamNames) typeParameters
            xx = zipWith (\t i -> VarAppliedType t (safeIndex classParamNames i)) (getTypeVarNames typeParameters) [0..]

        newCxt <- createCxt monadVarName cxt
        m <- appT (conT ''Monad) (varT monadVarName)

        instanceDec <- instanceD
          (pure $ newCxt ++ [m])
          (createInstanceType ty monadVarName typeVars)
          (map createInstanceFnDec decs)

        mockFnDecs <- concat <$> mapM (createMockFnDec monadVarName xx options) decs

        pure $ instanceDec : mockFnDecs
    t -> error $ "unsupported type: " <> show t

getMonadVarNames :: Cxt -> [TyVarBndr ()] -> Q [Name]
getMonadVarNames cxt typeVars = do
  let typeVarNames = getTypeVarNames typeVars

  -- VarInfos (class names is empty)
  let emptyClassVarInfos = map (`VarInfo` []) typeVarNames

  let parentClassInfos = toClassInfos cxt

  varInfos <- collectVarInfos parentClassInfos emptyClassVarInfos

  pure $ (\(VarInfo n _) -> n) <$> filterMonadicVarInfos varInfos

getTypeVarNames :: [TyVarBndr a] -> [Name]
getTypeVarNames = map getTypeVarName

getTypeVarName :: TyVarBndr a -> Name
getTypeVarName (PlainTV name _) = name
getTypeVarName (KindedTV name _ _) = name

toClassInfos :: Cxt -> [ClassInfo]
toClassInfos = map $ \ct ->  toClassInfo ct

toClassInfo :: Pred -> ClassInfo
toClassInfo (AppT t1 t2) = do
  let (ClassInfo name vars) = toClassInfo t1
  ClassInfo name (vars ++ getTypeNames t2)
toClassInfo (ConT name) = ClassInfo name []
toClassInfo _ = error "Unsupported Type structure"

getTypeNames :: Pred -> [Name]
getTypeNames (VarT name) = [name]
getTypeNames (ConT name) = [name]
getTypeNames _ = []

collectVarInfos :: [ClassInfo] -> [VarInfo] -> Q [VarInfo]
collectVarInfos classInfos = mapM (collectVarInfo classInfos)

collectVarInfo :: [ClassInfo] -> VarInfo -> Q VarInfo
collectVarInfo classInfos (VarInfo vName classNames) =  do
  varClassNames <- collectVarClassNames vName classInfos
  pure $ VarInfo vName (classNames ++ varClassNames)

collectVarClassNames :: Name -> [ClassInfo] -> Q [Name]
collectVarClassNames varName classInfos = do
  let targetClassInfos = filterClassInfo varName classInfos
  concat <$> mapM (collectVarClassNames_ varName) targetClassInfos

collectVarClassNames_ :: Name -> ClassInfo -> Q [Name]
collectVarClassNames_ name (ClassInfo cName vNames) = do
  case elemIndex name vNames of
    Nothing -> pure []
    Just i -> do
      ClassI (ClassD cxt _ typeVars _ _) _ <- reify cName
      -- type variable names
      let typeVarNames = getTypeVarNames typeVars
      -- type variable name of same position
      let typeVarName = typeVarNames !! i
      -- parent class information
      let parentClassInfos = toClassInfos cxt

      if null parentClassInfos then pure [cName]
      else do
        result <- concat <$> mapM (collectVarClassNames_ typeVarName) parentClassInfos
        pure $ cName : result

filterClassInfo :: Name -> [ClassInfo] -> [ClassInfo]
filterClassInfo name = filter (hasVarName name)
  where
  hasVarName :: Name -> ClassInfo -> Bool
  hasVarName name (ClassInfo _ varNames) = name `elem` varNames

filterMonadicVarInfos :: [VarInfo] -> [VarInfo]
filterMonadicVarInfos = filter hasMonadInVarInfo

hasMonadInVarInfo :: VarInfo -> Bool
hasMonadInVarInfo (VarInfo _ classNames) = ''Monad `elem` classNames

createCxt :: Name -> Cxt -> Q Cxt
createCxt monadVarName = mapM (createPred monadVarName)

createPred :: Name -> Pred -> Q Pred
createPred monadVarName (AppT ty a@(VarT varName))
  | monadVarName == varName = appT (pure ty) (appT (conT ''MockT) (varT varName))
  | otherwise = appT (createPred monadVarName ty) (pure a)
createPred monadVarName (AppT ty1 ty2) = appT (createPred monadVarName ty1) (createPred monadVarName ty2)
createPred _ ty = pure ty

createInstanceType :: Type -> Name -> [TyVarBndr ()] -> Q Type
createInstanceType className monadName tvbs = do
  types <- mapM (tyVarBndrToType monadName) tvbs
  pure $ foldl AppT className types

tyVarBndrToType :: Name -> TyVarBndr () -> Q Type
tyVarBndrToType monadName (PlainTV name _)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name
tyVarBndrToType monadName (KindedTV name _ _)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name

createInstanceFnDec :: Dec -> Q Dec
createInstanceFnDec (SigD funName funType) = do
  names <- sequence $ typeToNames funType
  let r = mkName "result"
      params = varP <$> names
      args = varE <$> names
      funNameStr = "_" <> nameBase funName

      funBody =  [| MockT $ do
                      defs <- get
                      let mock = fromMaybe (error $ "no answer found stub function `" ++ funNameStr ++ "`.") $ findParam (Proxy :: Proxy $(litT (strTyLit funNameStr))) defs
                          $(bangP $ varP r) = $(generateStubFn [| mock |] args)
                      pure $(varE r) |]
      funClause = clause params (normalB funBody) []
  funD funName [funClause]
createInstanceFnDec dec = fail $ "unsuported dec: " <> pprint dec

generateStubFn :: Q Exp -> [Q Exp] -> Q Exp
generateStubFn mock args = do
  foldl appE [| stubFn $(mock) |] args

createMockFnDec :: Name -> [VarAppliedType] -> MockOptions -> Dec -> Q [Dec]
createMockFnDec monadVarName varAppliedTypes options (SigD funName ty) = do
  let funNameStr = createFnName funName options
      mockFunName = mkName funNameStr
      mockBody = [| MockT $ modify (++ [Definition
                      (Proxy :: Proxy $(litT (strTyLit funNameStr)))
                      (unsafePerformIO $ createNamedMock $(litE (stringL funNameStr)) p)
                      shouldApplyAnythingTo]) |]
      updatedType = updateType ty varAppliedTypes
      funType = createMockBuilderFnType monadVarName updatedType
      verifyParams = createMockBuilderVerifyParams updatedType
      params = mkName "p"

  newFunSig <- sigD mockFunName [t|
    (MockBuilder $(varT params) ($(pure funType)) ($(pure verifyParams)), Monad $(varT monadVarName))
     => $(varT params) -> MockT $(varT monadVarName) ()|]
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB mockBody) []]

  -- fail $ intercalate "\n" [
  --     "",
  --     "元の関数のType: " <> show ty,
  --     "クラスに適用されたType: " <> show varAppliedTypes,
  --     "↑を考慮した関数のType" <> show updatedType,
  --     "Mock fun: " <> pprint funType,
  --     "Mock verifyParams: " <> pprint verifyParams,
  --     "関数のシグニチャ: " <> pprint newFunSig
  --   ]

  pure $ newFunSig : [newFun]
createMockFnDec _ _ _ dec = fail $ "unsupport dec: " <> pprint dec

updateType :: Type -> [VarAppliedType] -> Type
updateType (AppT (VarT v1) (VarT v2)) varAppliedTypes = do
  let 
    x = maybe (VarT v1) ConT (findClass v1 varAppliedTypes)
    y = maybe (VarT v2) ConT (findClass v2 varAppliedTypes)
  AppT x y
updateType ty _ = ty

hasClass :: Name -> [VarAppliedType] -> Bool
hasClass varName = any (\(VarAppliedType v c) -> (v == varName) && isJust c)

findClass :: Name -> [VarAppliedType] -> Maybe Name
findClass varName types = do
  guard $ hasClass varName types
  (VarAppliedType _ c) <- find (\(VarAppliedType v _) -> v == varName) types
  c

createFnName :: Name -> MockOptions -> String
createFnName funName options = do
  options.prefix <> nameBase funName <> options.suffix

createMockBuilderFnType :: Name -> Type -> Type
createMockBuilderFnType monadVarName a@(AppT (VarT var) ty)
  | monadVarName == var = ty
  | otherwise = a
createMockBuilderFnType monadVarName (AppT ty ty2) = AppT ty (createMockBuilderFnType monadVarName ty2)
createMockBuilderFnType monadVarName (ForallT _ _ ty) = createMockBuilderFnType monadVarName ty
createMockBuilderFnType _ ty = ty

createMockBuilderVerifyParams :: Type -> Type
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) (AppT (VarT _) _)) = AppT (ConT ''Param) ty
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) ty2) =
  AppT (AppT (ConT ''(:>)) (AppT (ConT ''Param) ty) ) (createMockBuilderVerifyParams ty2)
createMockBuilderVerifyParams (AppT (VarT v) (ConT c)) = (AppT (ConT ''Param) (ConT c))
createMockBuilderVerifyParams (ForallT _ _ ty) = createMockBuilderVerifyParams ty
createMockBuilderVerifyParams a = a

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT _) t2) = newName "a" : typeToNames t2
typeToNames (ForallT _ _ ty) = typeToNames ty
typeToNames _ = []

getClassName :: Type -> Name
getClassName (ConT name) = name
getClassName (AppT ty _) = getClassName ty
getClassName d = error $ "unsupported class definition: " <> show d

getClassNames :: Type -> [Name]
getClassNames (AppT (ConT name1) (ConT name2)) = [name1, name2]
getClassNames (AppT ty (ConT name)) = getClassNames ty ++ [name]
getClassNames (AppT ty1 ty2) = getClassNames ty1 ++ getClassNames ty2
getClassNames _ = []

data ClassInfo = ClassInfo { cName :: Name, varNames :: [Name] }

instance Show ClassInfo where
  show (ClassInfo cName varNames) = showClassDef cName varNames

data VarInfo = VarInfo { varName :: Name, classNames :: [Name] }

instance Show VarInfo where
  show (VarInfo varName classNames) = show varName <> " class is " <> unwords (showClassName <$> classNames)

data VarAppliedType = VarAppliedType { name :: Name, appliedClassName :: Maybe Name }
  deriving (Show)

showClassName :: Name -> String
showClassName n = splitLast "." $ show n

showClassDef :: Name -> [Name] -> String
showClassDef className varNames = showClassName className <> " " <> unwords (show <$> varNames)

splitLast :: String -> String -> String
splitLast delimiter = last . split delimiter

split :: String -> String -> [String]
split delimiter str = unpack <$> splitOn (pack delimiter) (pack str)

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)