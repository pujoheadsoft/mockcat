{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock, makeMockWithOptions, MockOptions(..), options) where

import Language.Haskell.TH
    ( Exp(..),
      Lit(..),
      Pat(..),
      Q,
      pprint,
      Name,
      Dec(..),
      Info(..),
      reify,
      mkName,
      Type(..),
      Quote(newName),
      Cxt,
      TyVarBndr(..),
      Pred,
      isExtEnabled,
      Extension(..) )
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param
import Test.MockCat.Cons
import Test.MockCat.MockT
import Test.MockCat.Mock
import Data.Data (Proxy(..))
import Data.List (find, nub, elemIndex)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe, isJust)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Lib
import Data.Text (splitOn, unpack, pack)
import Control.Monad (guard, unless)
import Data.Function ((&))
import Prelude as P

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

{- | Options for generating mocks. 

  - prefix: Stub function prefix
  - suffix: stub function suffix
-}
data MockOptions = MockOptions { prefix :: String, suffix :: String }

{- | Default Options.

  Stub function names are prefixed with “_”.
-}
options :: MockOptions
options = MockOptions { prefix = "_", suffix = "" }

{- | Create a mock of the typeclasses that returns a monad according to the `MockOptions`. 

  Given a monad type class, generate the following.

  - MockT instance of the given typeclass
  - A stub function corresponding to a function of the original class type.
The name of stub function is the name of the original function with a “_” appended.

  @
  class (Monad m) => FileOperation m where
    writeFile :: FilePath -\> Text -\> m ()
    readFile :: FilePath -\> m Text

  makeMockWithOptions [t|FileOperation|] options { prefix = "stub_" }

  it "test runMockT" do
    result \<- runMockT do
      stub_readFile $ "input.txt" |\> pack "content"
      stub_writeFile $ "output.text" |\> pack "content" |\> ()
      somethingProgram

    result `shouldBe` ()
  @

-}
makeMockWithOptions :: Q Type -> MockOptions -> Q [Dec]
makeMockWithOptions = doMakeMock

{- | Create a mock of a typeclasses that returns a monad. 

  Given a monad type class, generate the following.

  - MockT instance of the given typeclass
  - A stub function corresponding to a function of the original class type.
The name of stub function is the name of the original function with a “_” appended.

  The prefix can be changed.
  In that case, use `makeMockWithOptions`.

  @
  class (Monad m) => FileOperation m where
    writeFile :: FilePath -\> Text -\> m ()
    readFile :: FilePath -\> m Text

  makeMock [t|FileOperation|]

  it "test runMockT" do
    result \<- runMockT do
      _readFile $ "input.txt" |\> pack "content"
      _writeFile $ "output.text" |\> pack "content" |\> ()
      somethingProgram

    result `shouldBe` ()
  @

-}
makeMock :: Q Type -> Q [Dec]
makeMock = flip doMakeMock options

doMakeMock :: Q Type -> MockOptions -> Q [Dec]
doMakeMock t options = do
  verifyExtension DataKinds
  verifyExtension FlexibleInstances
  verifyExtension FlexibleContexts

  ty <- t
  className <- getClassName <$> t

  reify className >>= \case
    ClassI (ClassD _ _ [] _ _) _ ->
      fail $ "A type parameter is required for class " <> show className

    ClassI (ClassD cxt _ typeVars _ decs) _ -> do
      monadVarNames <- getMonadVarNames cxt typeVars
      case nub monadVarNames of
        [] -> fail "Monad parameter not found."
        (monadVarName : rest)
          | length rest > 1 -> fail "Monad parameter must be unique."
          | otherwise -> makeMockDecs ty className monadVarName cxt typeVars decs options

    t -> error $ "unsupported type: " <> show t


makeMockDecs :: Type -> Name -> Name -> Cxt -> [TyVarBndr a] -> [Dec] -> MockOptions -> Q [Dec]
makeMockDecs ty className monadVarName cxt typeVars decs options = do
  let classParamNames = filter (className /=) (getClassNames ty)
      newTypeVars = drop (length classParamNames) typeVars
      varAppliedTypes = zipWith (\t i -> VarAppliedType t (safeIndex classParamNames i)) (getTypeVarNames typeVars) [0..]

  newCxt <- createCxt monadVarName cxt
  m <- appT (conT ''Monad) (varT monadVarName)

  let hasMonad = P.any (\(ClassName2VarNames c _) -> c == ''Monad) $ toClassInfos newCxt

  instanceDec <- instanceD
    (pure $ newCxt ++ ([m | not hasMonad]))
    (createInstanceType ty monadVarName newTypeVars)
    (map (createInstanceFnDec options) decs)

  mockFnDecs <- concat <$> mapM (createMockFnDec monadVarName varAppliedTypes options) decs

  pure $ instanceDec : mockFnDecs

getMonadVarNames :: Cxt -> [TyVarBndr a] -> Q [Name]
getMonadVarNames cxt typeVars = do
  let
    parentClassInfos = toClassInfos cxt

    typeVarNames = getTypeVarNames typeVars
    -- VarInfos (class names is empty)
    emptyClassVarInfos = map (`VarName2ClassNames` []) typeVarNames

  varInfos <- collectVarInfos parentClassInfos emptyClassVarInfos

  pure $ (\(VarName2ClassNames n _) -> n) <$> filterMonadicVarInfos varInfos

getTypeVarNames :: [TyVarBndr a] -> [Name]
getTypeVarNames = map getTypeVarName

getTypeVarName :: TyVarBndr a -> Name
getTypeVarName (PlainTV name _) = name
getTypeVarName (KindedTV name _ _) = name

toClassInfos :: Cxt -> [ClassName2VarNames]
toClassInfos = map toClassInfo

toClassInfo :: Pred -> ClassName2VarNames
toClassInfo (AppT t1 t2) = do
  let (ClassName2VarNames name vars) = toClassInfo t1
  ClassName2VarNames name (vars ++ getTypeNames t2)
toClassInfo (ConT name) = ClassName2VarNames name []
toClassInfo _ = error "Unsupported Type structure"

getTypeNames :: Pred -> [Name]
getTypeNames (VarT name) = [name]
getTypeNames (ConT name) = [name]
getTypeNames _ = []

collectVarInfos :: [ClassName2VarNames] -> [VarName2ClassNames] -> Q [VarName2ClassNames]
collectVarInfos classInfos = mapM (collectVarInfo classInfos)

collectVarInfo :: [ClassName2VarNames] -> VarName2ClassNames -> Q VarName2ClassNames
collectVarInfo classInfos (VarName2ClassNames vName classNames) =  do
  varClassNames <- collectVarClassNames vName classInfos
  pure $ VarName2ClassNames vName (classNames ++ varClassNames)

collectVarClassNames :: Name -> [ClassName2VarNames] -> Q [Name]
collectVarClassNames varName classInfos = do
  let targetClassInfos = filterClassInfo varName classInfos
  concat <$> mapM (collectVarClassNames_ varName) targetClassInfos

collectVarClassNames_ :: Name -> ClassName2VarNames -> Q [Name]
collectVarClassNames_ name (ClassName2VarNames cName vNames) = do
  case elemIndex name vNames of
    Nothing -> pure []
    Just i -> do
      ClassI (ClassD cxt _ typeVars _ _) _ <- reify cName
      let
        -- type variable names
        typeVarNames = getTypeVarNames typeVars
        -- type variable name of same position
        typeVarName = typeVarNames !! i
        -- parent class information
        parentClassInfos = toClassInfos cxt

      case parentClassInfos of
        [] -> pure [cName]
        _ -> do
          result <- concat <$> mapM (collectVarClassNames_ typeVarName) parentClassInfos
          pure $ cName : result

filterClassInfo :: Name -> [ClassName2VarNames] -> [ClassName2VarNames]
filterClassInfo name = filter (hasVarName name)
  where
  hasVarName :: Name -> ClassName2VarNames -> Bool
  hasVarName name (ClassName2VarNames _ varNames) = name `elem` varNames

filterMonadicVarInfos :: [VarName2ClassNames] -> [VarName2ClassNames]
filterMonadicVarInfos = filter hasMonadInVarInfo

hasMonadInVarInfo :: VarName2ClassNames -> Bool
hasMonadInVarInfo (VarName2ClassNames _ classNames) = ''Monad `elem` classNames

createCxt :: Name -> Cxt -> Q Cxt
createCxt monadVarName = mapM (createPred monadVarName)

createPred :: Name -> Pred -> Q Pred
createPred monadVarName a@(AppT t@(ConT ty) b@(VarT varName))
  | monadVarName == varName && ty == ''Monad = pure a
  | monadVarName == varName && ty /= ''Monad = appT (pure t) (appT (conT ''MockT) (varT varName))
  | otherwise = appT (createPred monadVarName t) (pure b)
createPred monadVarName (AppT ty a@(VarT varName))
  | monadVarName == varName = appT (pure ty) (appT (conT ''MockT) (varT varName))
  | otherwise = appT (createPred monadVarName ty) (pure a)
createPred monadVarName (AppT ty1 ty2) = appT (createPred monadVarName ty1) (createPred monadVarName ty2)
createPred _ ty = pure ty

createInstanceType :: Type -> Name -> [TyVarBndr a] -> Q Type
createInstanceType className monadName tvbs = do
  types <- mapM (tyVarBndrToType monadName) tvbs
  pure $ foldl AppT className types

tyVarBndrToType :: Name -> TyVarBndr a -> Q Type
tyVarBndrToType monadName (PlainTV name _)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name
tyVarBndrToType monadName (KindedTV name _ _)
  | monadName == name = appT (conT ''MockT) (varT monadName)
  | otherwise = varT name

createInstanceFnDec :: MockOptions -> Dec -> Q Dec
createInstanceFnDec options (SigD fnName funType) = do
  names <- sequence $ typeToNames funType
  let r = mkName "result"
      params = varP <$> names
      args = varE <$> names
      fnNameStr = createFnName fnName options
      genStubFn = case names of
        [] -> $([|generateConstantStubFn|])
        _  -> $([|generateStubFn args|])

      fnBody = [| MockT $ do
                    defs <- get
                    let mock = defs
                               & findParam (Proxy :: Proxy $(litT (strTyLit fnNameStr)))
                               & fromMaybe (error $ "no answer found stub function `" ++ fnNameStr ++ "`.")
                        $(bangP $ varP r) = $(genStubFn [| mock |])
                    pure $(varE r) |]
      fnClause = clause params (normalB fnBody) []
  funD fnName [fnClause]
createInstanceFnDec _ dec = fail $ "unsuported dec: " <> pprint dec

generateStubFn :: [Q Exp] -> Q Exp -> Q Exp
generateStubFn args mock = foldl appE [| stubFn $(mock) |] args

generateConstantStubFn :: Q Exp -> Q Exp
generateConstantStubFn mock = [| stubFn $(mock) |]

createMockFnDec :: Name -> [VarAppliedType] -> MockOptions -> Dec -> Q [Dec]
createMockFnDec monadVarName varAppliedTypes options (SigD funName ty) = do
  let funNameStr = createFnName funName options
      mockFunName = mkName funNameStr
      params = mkName "p"

  if isConstant ty then
    doCreateConstantMockFnDec funNameStr mockFunName params monadVarName
  else do
    let
      updatedType = updateType ty varAppliedTypes
      funType = createMockBuilderFnType monadVarName updatedType
    doCreateMockFnDec funNameStr mockFunName params funType monadVarName updatedType

createMockFnDec _ _ _ dec = fail $ "unsupport dec: " <> pprint dec

doCreateMockFnDec :: Quote m => String -> Name -> Name -> Type -> Name -> Type -> m [Dec]
doCreateMockFnDec funNameStr mockFunName params funType monadVarName updatedType = do
  newFunSig <- do
    let verifyParams = createMockBuilderVerifyParams updatedType
    sigD mockFunName [t|
      (MockBuilder $(varT params) ($(pure funType)) ($(pure verifyParams)), Monad $(varT monadVarName))
      => $(varT params) -> MockT $(varT monadVarName) ()|]

  createMockFn <- [|createNamedMock|]

  mockBody <- createMockBody funNameStr createMockFn
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]

  pure $ newFunSig : [newFun]

doCreateConstantMockFnDec :: Quote m => String -> Name -> Name -> Name -> m [Dec]
doCreateConstantMockFnDec funNameStr mockFunName params monadVarName = do
  newFunSig <- sigD mockFunName [t|Monad $(varT monadVarName) => $(varT params) -> MockT $(varT monadVarName) ()|]
  createMockFn <- [|createNamedConstantMock|]
  mockBody <- createMockBody funNameStr createMockFn
  newFun <- funD mockFunName [clause [varP $ mkName "p"] (normalB (pure mockBody)) []]
  pure $ newFunSig : [newFun]

createMockBody :: Quote m => String -> Exp -> m Exp
createMockBody funNameStr createMockFn =
  [| MockT $ modify (++ [Definition
                    (Proxy :: Proxy $(litT (strTyLit funNameStr)))
                    (unsafePerformIO $ $(pure createMockFn) $(litE (stringL funNameStr)) p)
                    shouldApplyToAnything]) |]

isConstant :: Type -> Bool
isConstant (AppT (VarT _) (VarT _)) = True
isConstant (VarT _) = True
isConstant (ConT _) = True
isConstant _ = False

updateType :: Type -> [VarAppliedType] -> Type
updateType (AppT (VarT v1) (VarT v2)) varAppliedTypes = do
  let
    x = maybe (VarT v1) ConT (findClass v1 varAppliedTypes)
    y = maybe (VarT v2) ConT (findClass v2 varAppliedTypes)
  AppT x y
updateType ty _ = ty

hasClass :: Name -> [VarAppliedType] -> Bool
hasClass varName = P.any (\(VarAppliedType v c) -> (v == varName) && isJust c)

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
createMockBuilderVerifyParams (AppT (VarT _) (ConT c)) = AppT (ConT ''Param) (ConT c)
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

data ClassName2VarNames = ClassName2VarNames Name [Name]

instance Show ClassName2VarNames where
  show (ClassName2VarNames cName varNames) = showClassDef cName varNames

data VarName2ClassNames = VarName2ClassNames Name [Name]

instance Show VarName2ClassNames where
  show (VarName2ClassNames varName classNames) = show varName <> " class is " <> unwords (showClassName <$> classNames)

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
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)

verifyExtension :: Extension -> Q ()
verifyExtension e = isExtEnabled e >>= flip unless (fail $ "Language extensions `" ++ show e ++ "` is required.")