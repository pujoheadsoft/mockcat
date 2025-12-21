{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.MockCat.TH
  ( showExp,
    expectByExpr,
    makeMock,
    makeMockWithOptions,
    MockOptions (..),
    options,
    makePartialMock,
    makePartialMockWithOptions,
  )
where

import Control.Monad (unless)
import Data.List (elemIndex, nub)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Language.Haskell.TH
  ( Cxt,
    Dec (..),
    Exp (..),
    Extension (..),
    Info (..),
    Lit (..),
    Name,
    Pat (..),
    Pred,
    Q,
    TyVarBndr (..),
    TySynEqn (..),
    TypeFamilyHead (..),
    Type (..),
    isExtEnabled,
    mkName,
    pprint,
    reify,
  )
import Language.Haskell.TH.Lib
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Mock ()
import Test.MockCat.MockT
import Test.MockCat.TH.ClassAnalysis
  ( ClassName2VarNames(..),
    VarName2ClassNames(..),
    filterClassInfo,
    filterMonadicVarInfos,
    getClassName,
    getClassNames,
    toClassInfos,
    VarAppliedType(..),
    applyVarAppliedTypes )
import Test.MockCat.TH.ContextBuilder
  ( MockType (..),
    buildContext,
    getTypeVarName,
    getTypeVarNames,
    tyVarBndrToType,
    applyFamilyArg
  )
import Test.MockCat.TH.TypeUtils
  ( splitApps,
    substituteType
  )
import Test.MockCat.TH.FunctionBuilder
  ( createFnName,
    typeToNames,
    safeIndex,
    MockFnContext(..)
    , buildMockFnContext
    , buildMockFnDeclarations
    , createNoInlinePragma
    , generateInstanceMockFnBody
    , generateInstanceRealFnBody
  )
import Test.MockCat.TH.Types (MockOptions(..), options)
import Test.MockCat.Verify ()
import Test.MockCat.Param
import Unsafe.Coerce (unsafeCoerce)
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

-- | Create a conditional parameter based on @Q Exp@.
--
--  In applying a mock function, if the argument does not satisfy this condition, an error is raised.
--
--  The conditional expression is displayed in the error message.
expectByExpr :: Q Exp -> Q Exp
expectByExpr qf = do
  str <- showExp qf
  [|ExpectCondition $qf str|]

-- | Options for generating mocks.
--
--  - prefix: Stub function prefix
--  - suffix: stub function suffix
--  - implicitMonadicReturn: If True, the return value of the stub function is wrapped in a monad automatically.
--                           If Else, the return value of stub function is not wrapped in a monad,  so required explicitly return monadic values.

-- | Create a mock of the typeclasses that returns a monad according to the `MockOptions`.
--
--  Given a monad type class, generate the following.
--
--  - MockT instance of the given typeclass
--  - A stub function corresponding to a function of the original class type.
-- The name of stub function is the name of the original function with a "_" appended.
--
--  @
--  class (Monad m) => FileOperation m where
--    writeFile :: FilePath -\> Text -\> m ()
--    readFile :: FilePath -\> m Text
--
--  makeMockWithOptions [t|FileOperation|] options { prefix = "stub_" }
--
--  it "test runMockT" do
--    result \<- runMockT do
--      stub_readFile $ "input.txt" |\> pack "content"
--      stub_writeFile $ "output.text" |\> pack "content" |\> ()
--      somethingProgram
--
--    result `shouldBe` ()
--  @
makeMockWithOptions :: Q Type -> MockOptions -> Q [Dec]
makeMockWithOptions = flip doMakeMock Total

-- | Create a mock of a typeclasses that returns a monad.
--
--  Given a monad type class, generate the following.
--
--  - MockT instance of the given typeclass
--  - A stub function corresponding to a function of the original class type.
-- The name of stub function is the name of the original function with a "_" appended.
--
--  The prefix can be changed.
--  In that case, use `makeMockWithOptions`.
--
--  @
--  class (Monad m) => FileOperation m where
--    writeFile :: FilePath -\> Text -\> m ()
--    readFile :: FilePath -\> m Text
--
--  makeMock [t|FileOperation|]
--
--  spec :: Spec
--  spec = do
--    it "test runMockT" do
--      result \<- runMockT do
--        _readFile $ "input.txt" |\> pack "content"
--        _writeFile $ "output.text" |\> pack "content" |\> ()
--        somethingProgram
--
--      result `shouldBe` ()
--  @
makeMock :: Q Type -> Q [Dec]
makeMock t = doMakeMock t Total options

-- | Create a partial mock of a typeclasses that returns a monad.
--
--  Given a monad type class, generate the following.
--
--  - MockT instance of the given typeclass
--  - A stub function corresponding to a function of the original class type.
-- The name of stub function is the name of the original function with a "_" appended.
--
--  For functions that are not stubbed in the test, the real function is used as appropriate for the context.
--
--  The prefix can be changed.
--  In that case, use `makePartialMockWithOptions`.
--
--  @
--  class Monad m => Finder a b m | a -> b, b -> a where
--    findIds :: m [a]
--    findById :: a -> m b
--
--  instance Finder Int String IO where
--    findIds = pure [1, 2, 3]
--    findById id = pure $ "{id: " <> show id <> "}"
--
--  findValue :: Finder a b m => m [b]
--  findValue = do
--    ids <- findIds
--    mapM findById ids
--
--  makePartialMock [t|Finder|]
--
--  spec :: Spec
--  spec = do
--    it "Use all real functions." do
--      values <- runMockT findValue
--      values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]
--
--    it "Only findIds should be stubbed." do
--      values <- runMockT do
--        _findIds [1 :: Int, 2]
--        findValue
--      values `shouldBe` ["{id: 1}", "{id: 2}"]
--  @
makePartialMock :: Q Type -> Q [Dec]
makePartialMock t = doMakeMock t Partial options

-- | `makePartialMock` with options
makePartialMockWithOptions :: Q Type -> MockOptions -> Q [Dec]
makePartialMockWithOptions = flip doMakeMock Partial

doMakeMock :: Q Type -> MockType -> MockOptions -> Q [Dec]
doMakeMock qType mockType options = do
  verifyRequiredExtensions
  ty <- qType
  let className = getClassName ty
  classMetadata <- loadClassMetadata className
  monadVarName <- selectMonadVarName classMetadata
  makeMockDecs
    ty
    mockType
    className
    monadVarName
    (cmContext classMetadata)
    (cmTypeVars classMetadata)
    (cmDecs classMetadata)
    options

verifyRequiredExtensions :: Q ()
verifyRequiredExtensions =
  mapM_
    verifyExtension
    [DataKinds, FlexibleInstances, FlexibleContexts, TypeFamilies]

loadClassMetadata :: Name -> Q ClassMetadata
loadClassMetadata className = do
  info <- reify className
  case info of
    ClassI (ClassD _ _ [] _ _) _ ->
      fail $ "A type parameter is required for class " <> show className
    ClassI (ClassD cxt _ typeVars _ decs) _ ->
      pure $
        ClassMetadata
          { cmName = className,
            cmContext = cxt,
            cmTypeVars = unsafeCoerce typeVars,
            cmDecs = decs
          }
    other -> error $ "unsupported type: " <> show other

selectMonadVarName :: ClassMetadata -> Q Name
selectMonadVarName metadata = do
  monadVarNames <- getMonadVarNames (cmContext metadata) (cmTypeVars metadata)
  case nub monadVarNames of
    [] -> fail "Monad parameter not found."
    (monadVarName : rest)
      | length rest > 1 -> fail "Monad parameter must be unique."
      | otherwise -> pure monadVarName

makeMockDecs :: Type -> MockType -> Name -> Name -> Cxt -> [TyVarBndr a] -> [Dec] -> MockOptions -> Q [Dec]
makeMockDecs ty mockType className monadVarName cxt typeVars decs options = do
  let classParamNames = filter (className /=) (getClassNames ty)
      newTypeVars = drop (length classParamNames) typeVars
      varAppliedTypes = zipWith (\t i -> VarAppliedType t (safeIndex classParamNames i)) (getTypeVarNames typeVars) [0 ..]
      sigDecs = [dec | dec@(SigD _ _) <- decs]
      typeFamilyHeads =
        [head | OpenTypeFamilyD head <- decs] ++
        [head | ClosedTypeFamilyD head _ <- decs]

  let typeInstDecs = map (createTypeInstanceDec monadVarName) typeFamilyHeads
      instanceBodyDecs = map (createInstanceFnDec mockType options) sigDecs ++ typeInstDecs
      fullCxt = buildContext cxt mockType className monadVarName newTypeVars varAppliedTypes
  (superClassDecs, predsToDrop) <-
    deriveSuperClassInstances
      mockType
      monadVarName
      newTypeVars
      varAppliedTypes
      options
      cxt
  let filteredCxt = filter (`notElem` predsToDrop) fullCxt
  instanceDec <-
    instanceD
      (pure filteredCxt)
      (createInstanceType ty monadVarName newTypeVars)
      instanceBodyDecs
  mockFnDecs <- concat <$> mapM (createMockFnDec mockType monadVarName varAppliedTypes options) sigDecs

  pure $ superClassDecs ++ (instanceDec : mockFnDecs)

deriveSuperClassInstances ::
  MockType ->
  Name ->
  [TyVarBndr a] ->
  [VarAppliedType] ->
  MockOptions ->
  Cxt ->
  Q ([Dec], [Pred])
deriveSuperClassInstances mockType _ _ _ _ _
  | mockType /= Total = pure ([], [])
deriveSuperClassInstances _ monadVarName typeVars varAppliedTypes _ cxt = do
  results <- mapM (deriveSuperClassInstance monadVarName typeVars varAppliedTypes) cxt
  let valid = catMaybes results
  pure (map fst valid, map snd valid)

deriveSuperClassInstance ::
  Name ->
  [TyVarBndr a] ->
  [VarAppliedType] ->
  Pred ->
  Q (Maybe (Dec, Pred))
deriveSuperClassInstance _ _ varAppliedTypes pred = do
  superInfo <- resolveSuperClassInfo pred
  maybe (pure Nothing) (buildSuperClassDerivation varAppliedTypes) superInfo
  where
    resolveSuperClassInfo :: Pred -> Q (Maybe SuperClassInfo)
    resolveSuperClassInfo target =
      case splitApps target of
        (ConT superName, args) -> do
          info <- reify superName
          pure $
            case info of
              ClassI (ClassD superCxt _ superTypeVars _ superDecs) _ ->
                Just $ SuperClassInfo superName args superCxt (unsafeCoerce superTypeVars) superDecs
              _ -> Nothing
        _ -> pure Nothing

    buildSuperClassDerivation ::
      [VarAppliedType] ->
      SuperClassInfo ->
      Q (Maybe (Dec, Pred))
    buildSuperClassDerivation appliedTypes info
      | superClassHasMethods info = pure Nothing
      | otherwise = do
          superMonadVars <- getMonadVarNames (scContext info) (scTypeVars info)
          case superMonadVars of
            [superMonadVar] -> buildMockInstance appliedTypes info superMonadVar
            _ -> pure Nothing

    buildMockInstance ::
      [VarAppliedType] ->
      SuperClassInfo ->
      Name ->
      Q (Maybe (Dec, Pred))
    buildMockInstance appliedTypes info superMonadVar = do
      let superVarNames = map getTypeVarName (scTypeVars info)
      if length superVarNames /= length (scArgs info)
        then pure Nothing
        else do
          let (contextPreds, instanceType) =
                buildInstancePieces appliedTypes info superMonadVar superVarNames
          instanceDec <- instanceD (pure contextPreds) (pure instanceType) []
          pure $ Just (instanceDec, instanceType)

    buildInstancePieces ::
      [VarAppliedType] ->
      SuperClassInfo ->
      Name ->
      [Name] ->
      ([Pred], Pred)
    buildInstancePieces appliedTypes info superMonadVar superVarNames =
      let substitutedArgs = map (applyVarAppliedTypes appliedTypes) (scArgs info)
          subMap = Map.fromList (zip superVarNames substitutedArgs)
          instanceArgs =
            map
              (buildInstanceArg appliedTypes superMonadVar subMap)
              superVarNames
          instanceType = foldl AppT (ConT (scName info)) instanceArgs
          contextPreds =
            map
              (applyVarAppliedTypes appliedTypes . substituteType subMap)
              (scContext info)
       in (contextPreds, instanceType)

    buildInstanceArg ::
      [VarAppliedType] ->
      Name ->
      Map.Map Name Type ->
      Name ->
      Type
    buildInstanceArg appliedTypes superMonadVar subMap var =
      let applied = applyVarAppliedTypes appliedTypes (lookupType subMap var)
       in if var == superMonadVar
            then AppT (ConT ''MockT) applied
            else applied

    lookupType :: Map.Map Name Type -> Name -> Type
    lookupType subMap key = Map.findWithDefault (VarT key) key subMap

    superClassHasMethods :: SuperClassInfo -> Bool
    superClassHasMethods = P.any isSignature . scDecs

    isSignature (SigD _ _) = True
    isSignature _ = False


data SuperClassInfo = SuperClassInfo
  { scName :: Name,
    scArgs :: [Type],
    scContext :: Cxt,
    scTypeVars :: [TyVarBndr ()],
    scDecs :: [Dec]
  }

data ClassMetadata = ClassMetadata
  { cmName :: Name,
    cmContext :: Cxt,
    cmTypeVars :: [TyVarBndr ()],
    cmDecs :: [Dec]
  }

getMonadVarNames :: Cxt -> [TyVarBndr a] -> Q [Name]
getMonadVarNames cxt typeVars = do
  let parentClassInfos = toClassInfos cxt

      typeVarNames = getTypeVarNames typeVars
      -- VarInfos (class names is empty)
      emptyClassVarInfos = map (`VarName2ClassNames` []) typeVarNames

  varInfos <- collectVarInfos parentClassInfos emptyClassVarInfos

  pure $ (\(VarName2ClassNames n _) -> n) <$> filterMonadicVarInfos varInfos

collectVarInfos :: [ClassName2VarNames] -> [VarName2ClassNames] -> Q [VarName2ClassNames]
collectVarInfos classInfos = mapM (collectVarInfo classInfos)

collectVarInfo :: [ClassName2VarNames] -> VarName2ClassNames -> Q VarName2ClassNames
collectVarInfo classInfos (VarName2ClassNames vName classNames) = do
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
      let -- type variable names
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

createInstanceType :: Type -> Name -> [TyVarBndr a] -> Q Type
createInstanceType className monadName tvbs = do
  let types = fmap (tyVarBndrToType monadName) tvbs
  pure $ foldl AppT className types

createTypeInstanceDec :: Name -> TypeFamilyHead -> Q Dec
createTypeInstanceDec monadVarName (TypeFamilyHead familyName tfVars _ _) = do
  let lhsArgs = map (applyFamilyArg monadVarName) tfVars
      rhsArgs = map (VarT . getTypeVarName) tfVars
      lhsType = foldl AppT (ConT familyName) lhsArgs
      rhsType = foldl AppT (ConT familyName) rhsArgs
  pure $ TySynInstD (TySynEqn Nothing lhsType rhsType)

createInstanceFnDec :: MockType -> MockOptions -> Dec -> Q Dec
createInstanceFnDec mockType options (SigD fnName funType) = do
  names <- sequence $ typeToNames funType
  let r = mkName "result"
      params = varP <$> names
      args = varE <$> names
      fnNameStr = createFnName fnName options

      fnBody = case mockType of
        Total -> generateInstanceMockFnBody fnNameStr args r options
        Partial -> generateInstanceRealFnBody fnName fnNameStr args r options

      fnClause = clause params (normalB fnBody) []
  funD fnName [fnClause]
createInstanceFnDec _ _ dec = fail $ "unsuported dec: " <> pprint dec



createMockFnDec :: MockType -> Name -> [VarAppliedType] -> MockOptions -> Dec -> Q [Dec]
createMockFnDec mockType monadVarName varAppliedTypes options (SigD sigFnName ty) = do
  let ctx = buildMockFnContext mockType monadVarName varAppliedTypes options sigFnName ty
  fnDecs <- buildMockFnDeclarations ctx
  pragmaDec <- createNoInlinePragma (mockFnName ctx)
  pure $ pragmaDec : fnDecs
createMockFnDec _ _ _ _ dec = fail $ "unsupport dec: " <> pprint dec



verifyExtension :: Extension -> Q ()
verifyExtension e = isExtEnabled e >>= flip unless (fail $ "Language extensions `" ++ show e ++ "` is required.")