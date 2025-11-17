{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.MockCat.THCompareSpec (spec) where

import Test.Hspec
import Language.Haskell.TH (pprint, Dec(..), Type(..), litE, stringL, listE, tupE)
import Language.Haskell.TH.Syntax (nameBase)
import Data.Char (isDigit, isLower)
import Data.List (foldl', sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.IO as SIO
import Test.MockCat.SharedSpecDefs
import Test.MockCat.TH (makeMock, makeMockWithOptions, makePartialMock, makePartialMockWithOptions, options, implicitMonadicReturn, MockOptions(..))
import Language.Haskell.TH (lookupTypeName, conT)
import Control.Monad (forM_)

normalize :: String -> String
normalize =
  T.unpack
    . squeezePunctuation
    . T.unwords
    . map stripVarSuffix
    . T.words
    . stripQualifiers
    . T.pack

stripQualifiers :: T.Text -> T.Text
stripQualifiers =
  stripPrefixes
    (map
      T.pack
      [ "Test.MockCat.SharedSpecDefs."
      , "Test.MockCat.MockT."
      , "Control.Monad.IO.Class."
      , "GHC.Types."
      , "Data.Typeable.Internal."
      , "Test.MockCat.Verify."
      , "Verify."
      ])
  where
    stripPrefixes [] t = t
    stripPrefixes (p : ps) t = stripPrefixes ps (T.replace p T.empty t)

stripVarSuffix :: T.Text -> T.Text
stripVarSuffix = T.pack . go Nothing . T.unpack
  where
    go _ [] = []
    go prev (c : cs)
      | c == '_' && maybe False isLower prev =
          go prev (dropWhile isDigit cs)
      | otherwise = c : go (Just c) cs

squeezePunctuation :: T.Text -> T.Text
squeezePunctuation =
  applyReplacements
    [ ("( ", "(")
    , (" )", ")")
    , (" ,", ",")
    , ("[ ", "[")
    , (" ]", "]")
    , (" <" , "<")
    , ("> ", ">")
    ]
  where
    applyReplacements reps txt =
      foldl'
        (\acc (from, to) -> T.replace (T.pack from) (T.pack to) acc)
        txt
        reps

-- generate at compile time to avoid running restricted TH actions in IO
-- store pretty-printed generated declarations as strings
generatedFinderStr :: String
generatedFinderStr = $(do decs <- makePartialMock [t|Finder|]; litE (stringL (concatMap pprint decs)))

generatedFinderSigPairs :: [(String, String)]
generatedFinderSigPairs =
  $(
    do
      decs <- makePartialMock [t|Finder|]
      let sigPairs =
            [ (nameBase name, pprint ty)
            | SigD name ty <- decs
            ]
      listE
        [ tupE [litE (stringL fn), litE (stringL sig)]
        | (fn, sig) <- sigPairs
        ]
  )

generatedFinderSigMap :: Map.Map String String
generatedFinderSigMap = Map.fromList generatedFinderSigPairs

generatedFileOpStr :: String
generatedFileOpStr = $(do decs <- makePartialMock [t|FileOperation|]; litE (stringL (concatMap pprint decs)))

generatedTestClassStr :: String
generatedTestClassStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.TestClass"
    case m of
      Nothing -> fail "TestClass not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedMultiApplyStr :: String
generatedMultiApplyStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MultiApplyTest"
    case m of
      Nothing -> fail "MultiApplyTest not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedExplicitStr :: String
generatedExplicitStr = $(
  do
    let opts = options { implicitMonadicReturn = False }
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.ExplicitlyReturnMonadicValuesTest"
    case m of
      Nothing -> fail "ExplicitlyReturnMonadicValuesTest not found"
      Just n -> do decs <- makeMockWithOptions (conT n) opts; litE (stringL (concatMap pprint decs))
  )

generatedDefaultMethodStr :: String
generatedDefaultMethodStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.DefaultMethodTest"
    case m of
      Nothing -> fail "DefaultMethodTest not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedAssocTypeStr :: String
generatedAssocTypeStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.AssocTypeTest"
    case m of
      Nothing -> fail "AssocTypeTest not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedParamThreeStr :: String
generatedParamThreeStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.ParamThreeMonad"
    case m of
      Nothing -> fail "ParamThreeMonad not found"
      Just n -> do
        let targetType =
              AppT
                (AppT (ConT n) (ConT ''Int))
                (ConT ''Bool)
        decs <- makeMock (pure targetType)
        litE (stringL (concatMap pprint decs))
  )

generatedUserInputStr :: String
generatedUserInputStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.UserInputGetter"
    case m of
      Nothing -> fail "UserInputGetter not found"
      Just n -> do decs <- makePartialMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedExplicitPartialStr :: String
generatedExplicitPartialStr = $(
  do
    let opts = options { implicitMonadicReturn = False }
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.ExplicitlyReturnMonadicValuesPartialTest"
    case m of
      Nothing -> fail "ExplicitlyReturnMonadicValuesPartialTest not found"
      Just n -> do decs <- makePartialMockWithOptions (conT n) opts; litE (stringL (concatMap pprint decs))
  )

-- additional generated declarations for classes produced in `TypeClassTHSpec.hs`
generatedApiOperationStr :: String
generatedApiOperationStr = $(
  do
    let opts = options { prefix = "stub_", suffix = "_fn" }
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.ApiOperation"
    case m of
      Nothing -> fail "ApiOperation not found"
      Just n -> do decs <- makeMockWithOptions (conT n) opts; litE (stringL (concatMap pprint decs))
  )

generatedVar2_1SubStr :: String
generatedVar2_1SubStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MonadVar2_1Sub"
    case m of
      Nothing -> fail "MonadVar2_1Sub not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedVar2_2SubStr :: String
generatedVar2_2SubStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MonadVar2_2Sub"
    case m of
      Nothing -> fail "MonadVar2_2Sub not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedVar3_1SubStr :: String
generatedVar3_1SubStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MonadVar3_1Sub"
    case m of
      Nothing -> fail "MonadVar3_1Sub not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedVar3_2SubStr :: String
generatedVar3_2SubStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MonadVar3_2Sub"
    case m of
      Nothing -> fail "MonadVar3_2Sub not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedVar3_3SubStr :: String
generatedVar3_3SubStr = $(
  do
    m <- lookupTypeName "Test.MockCat.SharedSpecDefs.MonadVar3_3Sub"
    case m of
      Nothing -> fail "MonadVar3_3Sub not found"
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

partialMockSpecPath :: FilePath
partialMockSpecPath = "test/Test/MockCat/PartialMockSpec.hs"

typeClassSpecPath :: FilePath
typeClassSpecPath = "test/Test/MockCat/TypeClassSpec.hs"

-- extract textual instance context for handwritten module
extractHandwrittenInstanceCxts :: FilePath -> String -> IO [String]
extractHandwrittenInstanceCxts fp className = do
  src <- SIO.readFile fp
  pure $ extractInstanceHeaders src className

spec :: Spec
spec = describe "TH generated vs handwritten instances" do
  let cases =
        [ ("Finder partial mock instance matches handwritten", partialMockSpecPath, "Finder", generatedFinderStr)
        , ("FileOperation mock instance matches handwritten", partialMockSpecPath, "FileOperation", generatedFileOpStr)
        , ("UserInputGetter partial mock instance matches handwritten", partialMockSpecPath, "UserInputGetter", generatedUserInputStr)
        , ("ExplicitlyReturnMonadicValuesPartialTest instance matches handwritten", partialMockSpecPath, "ExplicitlyReturnMonadicValuesPartialTest", generatedExplicitPartialStr)
        , ("TypeClass: TestClass instance matches handwritten", typeClassSpecPath, "TestClass", generatedTestClassStr)
        , ("TypeClass: MultiApplyTest instance matches handwritten", typeClassSpecPath, "MultiApplyTest", generatedMultiApplyStr)
        , ("TypeClass: ExplicitlyReturnMonadicValuesTest instance matches handwritten", typeClassSpecPath, "ExplicitlyReturnMonadicValuesTest", generatedExplicitStr)
        , ("TypeClass: DefaultMethodTest instance matches handwritten", typeClassSpecPath, "DefaultMethodTest", generatedDefaultMethodStr)
        , ("TypeClass: AssocTypeTest instance matches handwritten", typeClassSpecPath, "AssocTypeTest", generatedAssocTypeStr)
        , ("TypeClass: ParamThreeMonad instance matches handwritten", typeClassSpecPath, "ParamThreeMonad", generatedParamThreeStr)
        , ("ApiOperation instance matches handwritten", typeClassSpecPath, "ApiOperation", generatedApiOperationStr)
        , ("MonadVar2_1Sub instance matches handwritten", typeClassSpecPath, "MonadVar2_1Sub", generatedVar2_1SubStr)
        , ("MonadVar2_2Sub instance matches handwritten", typeClassSpecPath, "MonadVar2_2Sub", generatedVar2_2SubStr)
        , ("MonadVar3_1Sub instance matches handwritten", typeClassSpecPath, "MonadVar3_1Sub", generatedVar3_1SubStr)
        , ("MonadVar3_2Sub instance matches handwritten", typeClassSpecPath, "MonadVar3_2Sub", generatedVar3_2SubStr)
        , ("MonadVar3_3Sub instance matches handwritten", typeClassSpecPath, "MonadVar3_3Sub", generatedVar3_3SubStr)
        ]
  forM_ cases \(desc, manualPath, className, generatedStr) ->
    it desc $
      assertInstancesEquivalent manualPath className generatedStr

  describe "Partial mock helper signatures match handwritten" do
    let helperFns = ["_findIds"]
    forM_ helperFns \fn ->
      it (fn ++ " signature matches handwritten") $
        assertHelperSigMatches partialMockSpecPath generatedFinderSigMap fn

-- helper to extract generated instance headers from a pre-pprinted string
extractGeneratedInstanceCxtsFromStr :: String -> String -> [String]
extractGeneratedInstanceCxtsFromStr s =
  extractInstanceHeaders s

extractInstanceHeaders :: String -> String -> [String]
extractInstanceHeaders input className =
  let txt = T.pack input
      classTxt = T.pack className
      insts = drop 1 $ T.splitOn (T.pack "instance") txt
      mkHeader body =
        let (beforeWhere, _) = T.breakOn (T.pack "where") body
            (beforeBrace, _) = T.breakOn (T.pack "{") beforeWhere
            header = T.strip beforeBrace
            headerWithoutQualifiers = stripQualifiers header
            headPart =
              let (_ctxPart, rest) = T.breakOn (T.pack "=>") headerWithoutQualifiers
               in if T.null rest
                    then headerWithoutQualifiers
                    else T.strip $ T.drop 2 rest
            actualClassName =
              case T.words headPart of
                [] -> Nothing
                (tok : _) ->
                  let cleaned = T.dropAround (`elem` ("()")) tok
                   in if T.null cleaned then Nothing else Just cleaned
            containsMockT = T.pack "MockT" `T.isInfixOf` headerWithoutQualifiers
         in case actualClassName of
              Just cls
                | cls == classTxt && containsMockT ->
                    Just $ normalize $ "instance " ++ T.unpack headerWithoutQualifiers
              _ -> Nothing
   in mapMaybe mkHeader insts

assertInstancesEquivalent :: FilePath -> String -> String -> Expectation
assertInstancesEquivalent manualPath className generatedStr = do
  hand <- extractHandwrittenInstanceCxts manualPath className
  let gen = extractGeneratedInstanceCxtsFromStr generatedStr className
      sortedHand = sort hand
      sortedGen = sort gen
  if null hand
    then expectationFailure $
      "手書きのインスタンスが見つかりません: " ++ className ++ " in " ++ manualPath
    else pure ()
  if null gen
    then expectationFailure $
      "TH生成インスタンスが見つかりません: " ++ className
        ++ "\n生成結果(先頭のみ):\n"
        ++ take 1000 generatedStr
    else pure ()
  sortedGen `shouldBe` sortedHand

extractHandwrittenFunctionSig :: FilePath -> String -> IO (Maybe String)
extractHandwrittenFunctionSig fp funName = do
  src <- SIO.readFile fp
  let linesText = T.lines (T.pack src)
      isSignatureLine line =
        let trimmed = T.stripStart line
         in (T.pack funName <> T.pack " ::") `T.isPrefixOf` trimmed
      continuation line =
        case T.uncons line of
          Just (c, _) -> c == ' ' || c == '\t'
          Nothing -> False
      collect [] = Nothing
      collect (line:rest)
        | isSignatureLine line =
            let cont = takeWhile continuation rest
             in Just $ T.unpack $ T.unlines (line : cont)
        | otherwise = collect rest
  pure $ collect linesText

stripForallClause :: String -> String
stripForallClause sig =
  let txt = T.pack sig
      (preForall, rest) = T.breakOn (T.pack "forall") txt
   in
    if T.null rest
      then sig
      else
        let afterForall = T.drop 1 $ T.dropWhile (/= '.') rest
         in T.unpack (preForall <> afterForall)

normalizeSignature :: String -> String
normalizeSignature =
  T.unpack
    . T.strip
    . T.pack
    . dropFunctionName
    . stripForallClause
    . normalize

dropFunctionName :: String -> String
dropFunctionName sig =
  let txt = T.pack sig
      (_, rest) = T.breakOn (T.pack "::") txt
   in
    if T.null rest
      then sig
      else T.unpack $ T.drop 2 rest

assertHelperSigMatches ::
  FilePath ->
  Map.Map String String ->
  String ->
  Expectation
assertHelperSigMatches manualPath generatedSigMap funName = do
  manualSigM <- extractHandwrittenFunctionSig manualPath funName
  manualSig <-
    case manualSigM of
      Nothing -> do
        expectationFailure $
          "手書きのシグネチャが見つかりません: "
            ++ funName
            ++ " in "
            ++ manualPath
        pure ""
      Just sig -> pure sig
  generatedSig <-
    case Map.lookup funName generatedSigMap of
      Nothing -> do
        expectationFailure $
          "TH生成シグネチャが見つかりません: "
            ++ funName
        pure ""
      Just sig -> pure sig
  normalizeSignature generatedSig `shouldBe` normalizeSignature manualSig


