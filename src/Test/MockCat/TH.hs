{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, mkName, Type (..), Quote (newName), Cxt, TyVarBndr (..), Pred)
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param (Param(..))
import Test.MockCat.Cons ((:>))
import Test.MockCat.MockT
import Data.Data (Proxy(..))
import Data.List (find, nub, elemIndex)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Lib
import Data.Text (splitOn, unpack, pack)
import Test.MockCat.Mock (MockBuilder)

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

makeMock :: Q Type -> Q [Dec]
makeMock t = do
  className <- getClassName <$> t

  reify className >>= \case
    ClassI (ClassD _ _ [] _ _) _ -> fail $ "A type parameter is required for class " <> show className
    ClassI (ClassD cxt _ typeParameters _ decs) _ -> do
      monadVarNames <- getMonadVarNames cxt typeParameters
      if length (nub monadVarNames) > 1 then fail "Monad parameter must be unique."
      else if null monadVarNames then fail "Monad parameter not found."
      else do
        let monadVarName = head monadVarNames

        newCtx <- createCxt monadVarName cxt
        m <- appT (conT ''Monad) (varT monadVarName)

        instanceDec <- instanceD
          (pure $ newCtx ++ [m])
          (createInstanceType className monadVarName typeParameters)
          (map createInstanceFnDec decs)

        mockFnDecs <- concat <$> mapM (createMockFnDec monadVarName) decs

        pure $ instanceDec : mockFnDecs
    t -> error $ "unsupported type: " <> show t

getMonadVarNames :: Cxt -> [TyVarBndr ()] -> Q [Name]
getMonadVarNames cxt typeVars = do
  typeVarNames <- getTypeVarNames typeVars

  -- VarInfos (class names is empty)
  emptyClassVarInfos <- mapM (\n -> pure $ VarInfo n []) typeVarNames

  parentClassInfos <- toClassInfos cxt

  varInfos <- collectVarInfos parentClassInfos emptyClassVarInfos

  pure $ (\(VarInfo n _) -> n) <$> filterMonadicVarInfos varInfos

getTypeVarNames :: [TyVarBndr ()] -> Q [Name]
getTypeVarNames = mapM getTypeVarName

getTypeVarName :: TyVarBndr () -> Q Name
getTypeVarName (PlainTV name _) = pure name
getTypeVarName (KindedTV name _ _) = pure name

toClassInfos :: Cxt -> Q [ClassInfo]
toClassInfos = mapM $ \ct -> pure $ toClassInfo ct

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
      typeVarNames <- getTypeVarNames typeVars
      -- type variable name of same position
      let typeVarName = typeVarNames !! i
      -- parent class information
      parentClassInfos <- toClassInfos cxt

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

createInstanceType :: Name -> Name -> [TyVarBndr ()] -> Q Type
createInstanceType className monadName tvbs = do
  types <- mapM (tyVarBndrToType monadName) tvbs
  pure $ foldl AppT (ConT className) types

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

createMockFnDec :: Name -> Dec -> Q [Dec]
createMockFnDec monadVarName (SigD funName ty) = do
  let funNameStr = "_" <> nameBase funName
      mockFunName = mkName funNameStr
      mockBody = [| MockT $ modify (++ [Definition
                      (Proxy :: Proxy $(litT (strTyLit funNameStr)))
                      (unsafePerformIO $ createNamedMock $(litE (stringL funNameStr)) p)
                      shouldApplyAnythingTo]) |]
      funType = createMockBuilderFnType monadVarName ty
      verifyParams = createMockBuilderVerifyParams ty

  newFunSig <- sigD mockFunName [t| forall p m. (MockBuilder p ($(pure funType)) ($(pure verifyParams)), Monad m) => p -> MockT m ()|]
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB mockBody) []]
  pure $ newFunSig : [newFun]
createMockFnDec _ dec = fail $ "unsupport dec: " <> pprint dec

createMockBuilderFnType :: Name -> Type -> Type
createMockBuilderFnType monadVarName a@(AppT (VarT var) ty)
  | monadVarName == var = ty
  | otherwise = a
createMockBuilderFnType monadVarName (AppT ty ty2) = AppT ty (createMockBuilderFnType monadVarName ty2)
createMockBuilderFnType _ ty = ty

createMockBuilderVerifyParams :: Type -> Type
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) (AppT (VarT _) _)) = AppT (ConT ''Param) ty
createMockBuilderVerifyParams (AppT (AppT ArrowT ty) ty2) =
  AppT (AppT (ConT ''(:>)) (AppT (ConT ''Param) ty) ) (createMockBuilderVerifyParams ty2)
createMockBuilderVerifyParams a = a

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT _) t2) = newName "a" : typeToNames t2
typeToNames _ = []

getClassName :: Type -> Name
getClassName (ConT name) = name
getClassName d = error $ "unsupported class definition: " <> show d


data ClassInfo = ClassInfo { cName :: Name, varNames :: [Name] }

instance Show ClassInfo where
  show (ClassInfo cName varNames) = showClassDef cName varNames

data VarInfo = VarInfo { varName :: Name, classNames :: [Name] }

instance Show VarInfo where
  show (VarInfo varName classNames) = show varName <> " class is " <> unwords (showClassName <$> classNames)

showClassName :: Name -> String
showClassName n = splitLast "." $ show n

showClassDef :: Name -> [Name] -> String
showClassDef className varNames = showClassName className <> " " <> unwords (show <$> varNames)

splitLast :: String -> String -> String
splitLast delimiter = last . split delimiter

split :: String -> String -> [String]
split delimiter str = unpack <$> splitOn (pack delimiter) (pack str)