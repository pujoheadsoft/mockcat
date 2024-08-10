{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, conT, instanceD, cxt, appT, varT, mkName, DecQ, litE, stringL, normalB, clause, funD, varP, litT, strTyLit, Type (..), conP, Quote (newName), runQ, Cxt, Pred, TyVarBndr (..))
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text, empty)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param (Param(..))
import Test.MockCat.MockT
import Data.Data (Proxy(..))
import Data.List (find, nub, elemIndex, intercalate)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust, isNothing)
import GHC.IO (unsafePerformIO)
import Control.Monad (replicateM, mplus, join, guard)
import Language.Haskell.TH.Lib
import Text.Printf (IsChar(toChar))
import Language.Haskell.TH (TyVarBndr)
import Data.Text (splitOn, unpack, pack)

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

-- Template Haskell code
makeMock :: Q Type -> Q [Dec]
makeMock = generateMock


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
split delimiter str = unpack <$> splitOn (pack delimiter) (pack $ str)

getClassInfos :: Cxt -> Q [ClassInfo]
getClassInfos cxt = catMaybes <$> mapM getClassInfo cxt

getClassInfo :: Type -> Q (Maybe ClassInfo)
getClassInfo (AppT (ConT className) (VarT varName)) = pure $ Just ClassInfo { cName = className, varNames = [varName] }
getClassInfo (AppT ty (VarT varName)) = do
  x <- getClassInfo ty
  pure $ (\(ClassInfo name names) -> ClassInfo  { cName = name, varNames = names ++ [varName] }) <$> x
getClassInfo _ = pure Nothing

hasVarName :: Name -> ClassInfo -> Bool
hasVarName name (ClassInfo _ varNames) = name `elem` varNames

filterClassInfo :: Name -> [ClassInfo] -> [ClassInfo]
filterClassInfo name = filter (hasVarName name)

collectVarClassNames_ :: Name -> ClassInfo -> Q [Name]
collectVarClassNames_ name (ClassInfo cName vNames) = do
  case elemIndex name vNames of
    Nothing -> pure []
    Just i -> do
      ClassI (ClassD cxt _ typeVars _ _) _ <- reify cName
      -- type variable names
      typeVarNames <- extractTypeVarNames typeVars
      -- type variable name of same position
      let typeVarName = typeVarNames !! i
      -- parent class information
      parentClassInfos <- getClassInfos cxt

      if null parentClassInfos then pure [cName]
      else do
        result <- concat <$> mapM (collectVarClassNames_ typeVarName) parentClassInfos
        pure $ cName : result

-- 型変数名とクラス情報から、型変数名が属するクラスを集める
collectVarClassNames :: Name -> [ClassInfo] -> Q [Name]
collectVarClassNames varName classInfos = do
  let targetClassInfos = filterClassInfo varName classInfos
  concat <$> mapM (collectVarClassNames_ varName) targetClassInfos

collectVarInfo :: [ClassInfo] -> VarInfo -> Q VarInfo
collectVarInfo classInfos (VarInfo vName classNames) =  do
  y <- collectVarClassNames vName classInfos
  pure $ varInfo vName (classNames ++ y)

collectVarInfos :: [ClassInfo] -> [VarInfo] -> Q [VarInfo]
collectVarInfos classInfos = mapM (collectVarInfo classInfos)

varInfo :: Name -> [Name] -> VarInfo
varInfo n ns = VarInfo { varName = n, classNames = ns }

hasMonadInVarInfo :: VarInfo -> Bool
hasMonadInVarInfo (VarInfo _ classNames) = ''Monad `elem` classNames

filterMonadicVarInfos :: [VarInfo] -> [VarInfo]
filterMonadicVarInfos = filter hasMonadInVarInfo

getMonadicVarNames :: Cxt -> [TyVarBndr ()] -> Q [Name]
getMonadicVarNames cxt typeVars = do
  typeVarNames <- extractTypeVarNames typeVars

  -- VarInfos (class names is empty)
  emptyClassVarInfos <- mapM (\n -> pure $ varInfo n []) typeVarNames

  parentClassInfos <- getClassInfos cxt

  varInfos <- collectVarInfos parentClassInfos emptyClassVarInfos

  pure $ (\(VarInfo n _) -> n) <$> filterMonadicVarInfos varInfos

extractTypeVarNames :: [TyVarBndr ()] -> Q [Name]
extractTypeVarNames = mapM extractTypeVarName

extractTypeVarName :: TyVarBndr () -> Q Name
extractTypeVarName (PlainTV name _) = pure name
extractTypeVarName (KindedTV name _ _) = pure name

generateMock :: Q Type -> Q [Dec]
generateMock t = do
  className <- getClassName <$> t

  reify className >>= \case
    ClassI (ClassD _ _ [] _ _) _ -> fail $ "A type parameter is required for class " <> show className
    ClassI (ClassD cxt _ typeParameters _ methods) _ -> do
      monadVarNames <- getMonadicVarNames cxt typeParameters
      if length (nub monadVarNames) > 1 then fail "Monad parameter must be unique."
      else if null monadVarNames then fail "Monad parameter not found."
      else do
        --fail $ "monad params:" <> show x
        --fail $ show ctx <> " ===" <> show monadVarNames
        let classNameStr = nameBase className
            monadVarName = head monadVarNames

        -- i <- (createInstanceType name (head monadVarNames) typeParameters)
        -- fail $ show ctx <> " => " <> show i
        -- x <- (createCxt monadVarName cxt)
        -- fail $ "\n" <> pprint cxt <> "\n" <> pprint x
        newCtx <- createCxt monadVarName cxt
        m <- appT (conT ''Monad) (varT monadVarName)

        instDec <- instanceD
          --(cxt [appT (conT ''Monad) (varT $ mkName "m")])
          --(pure cxt)
          (pure $ newCtx ++ [m])
          --(appT (conT name) (appT (conT ''MockT) (varT $ mkName "m")))
          (createInstanceType className monadVarName typeParameters)
          (map (generateMockMethod classNameStr) methods)

        funcDecs <- mapM (generateMockFunction classNameStr) methods

        pure $ instDec : funcDecs
    t -> error $ "unsupported type: " <> show t

createCxt :: Name -> Cxt -> Q Cxt
createCxt monadVarName = mapM (xxx monadVarName)

xxx :: Name -> Type -> Q Type
xxx monadVarName a@(AppT (ConT className) (VarT varName))
  | monadVarName == varName = appT (conT className) (appT (conT ''MockT) (varT varName))
  | otherwise = pure a
xxx monadVarName (AppT ty (VarT varName)) = do
  let x = xxx monadVarName ty
  appT x (varT varName)
xxx _ ty = pure ty

createInstanceType :: Name -> Name -> [TyVarBndr ()] -> Q Type
createInstanceType className monadName tvbs = do
  types <- mapM (tyVarBndrToType monadName) tvbs
  pure $ foldl AppT (ConT className) types

tyVarBndrToType :: Name -> TyVarBndr () -> Q Type
tyVarBndrToType monadName (PlainTV name _)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name
tyVarBndrToType monadName (KindedTV name _ kind)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name

generateMockMethod :: String -> Dec -> Q Dec
generateMockMethod classNameStr (SigD funName funType) = do
  names <- sequence $ typeToNames funType
  let r = mkName "result"
      params = varP <$> names
      args = varE <$> names
      funNameStr = "_" <> nameBase funName

      funBody =  [| MockT $ do
                      defs <- get
                      let mock = fromMaybe (error $ "no answer found stub function `" ++ funNameStr ++ "`.") $ findParam (Proxy :: Proxy $(litT (strTyLit funNameStr))) defs
                          $(bangP $ varP r) = $(generateStubFnCall [| mock |] args)
                      pure $(varE r) |]
      funClause = clause params (normalB funBody) []

  -- x <- mapM runQ params
  -- y <- mapM runQ args
  -- error $ show names <> ":" <> show x <> " ==> " <> show y
  -- x <- mapM runQ params
  -- error $ show funType <> " ==> " <> pprint x

  funD funName [funClause]
generateMockMethod classNameStr _ = error "adfasd"

generateStubFnCall :: Q Exp -> [Q Exp] -> Q Exp
generateStubFnCall mock args = do
  foldl appE [| stubFn $(mock) |] args

generateMockFunction :: String -> Dec -> Q Dec
generateMockFunction classNameStr (SigD funName funType) = do
  let funNameStr = "_" <> nameBase funName
      mockFunName = mkName funNameStr
      mockBody = [| MockT $ modify (++ [Definition
                      (Proxy :: Proxy $(litT (strTyLit funNameStr)))
                      (unsafePerformIO $ createNamedMock $(litE (stringL funNameStr)) p)
                      shouldApplyAnythingTo]) |]

  funD mockFunName [clause [varP $ mkName "p"] (normalB mockBody) []]
generateMockFunction classNameStr _ = error ""

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT t1) t2) = [newName "a"] ++ typeToNames t2
typeToNames _ = []

getClassName :: Type -> Name
getClassName (ConT name) = name
getClassName d = error $ "unsupported class definition: " <> show d
