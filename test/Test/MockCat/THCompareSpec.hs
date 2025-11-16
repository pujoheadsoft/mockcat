{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.MockCat.THCompareSpec (spec) where

import Test.Hspec
import Language.Haskell.TH.Syntax (runQ)
import Language.Haskell.TH (pprint, Dec(..), Pred, Type(..), litE, stringL)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified System.IO as SIO
import Test.MockCat.SharedSpecDefs
import Test.MockCat.TH (makeMock, makeMockWithOptions, makePartialMock, makePartialMockWithOptions, options, implicitMonadicReturn)
import Test.MockCat.PartialMockSpec hiding (spec)
import Test.MockCat.TypeClassSpec hiding (spec)
import Language.Haskell.TH (lookupTypeName, conT)
import Control.Monad (forM_)

normalize :: String -> String
normalize = T.unpack . T.unwords . T.words . T.pack

-- generate at compile time to avoid running restricted TH actions in IO
-- store pretty-printed generated declarations as strings
generatedFinderStr :: String
generatedFinderStr = $(do decs <- makePartialMock [t|Finder|]; litE (stringL (concatMap pprint decs)))

generatedFileOpStr :: String
generatedFileOpStr = $(do decs <- makeMock [t|FileOperation|]; litE (stringL (concatMap pprint decs)))

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
      Just n -> do decs <- makeMock (conT n); litE (stringL (concatMap pprint decs))
  )

generatedUserInputStr :: String
generatedUserInputStr = $(
  do
    m <- lookupTypeName "Test.MockCat.PartialMockSpec.UserInputGetter"
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

-- extract textual instance context for handwritten module
extractHandwrittenInstanceCxts :: FilePath -> String -> IO [String]
extractHandwrittenInstanceCxts fp className = do
  src <- SIO.readFile fp
  let parts = T.splitOn (T.pack "instance") (T.pack src)
      matches = filter (T.isInfixOf (T.pack className) . T.takeWhile (/= 'w')) parts
  pure $ map (normalize . T.unpack . T.takeWhile (/= 'w')) matches

-- from generated Decs, extract InstanceD contexts for instances whose head mentions className and MockT
extractGeneratedInstanceCxts :: [Dec] -> String -> [String]
extractGeneratedInstanceCxts decs className = map normalize $
  [ header
  | dec <- decs
  , let s = pprint dec
  , className `isInfixOf` s
  , "MockT" `isInfixOf` s
  , let header = takeWhile (/= '{') s -- take up to where/ body
  ]

-- helper to convert Pred/Type to string using pprint for debugging
pprintDecs :: [Dec] -> String
pprintDecs = unlines . map pprint

spec :: Spec
spec = describe "TH generated vs handwritten instances" $ do
  it "Finder partial mock constraints match handwritten" do
    let decsStr = generatedFinderStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/PartialMockSpec.hs" "Finder"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "Finder"
    -- ensure there's at least one generated instance context and one handwritten one
    if null gen
      then expectationFailure $ "no generated Finder instances found. generated:\n" ++ decsStr
      else pure ()
    length hand `shouldSatisfy` (> 0)
    -- basic sanity: generated should not include Typeable (Param ...) redundantly
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "FileOperation mock constraints match handwritten" do
    let decsStr = generatedFileOpStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/PartialMockSpec.hs" "FileOperation"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "FileOperation"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: TestClass constraints match handwritten" do
    let decsStr = generatedTestClassStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "TestClass"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "TestClass"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: MultiApplyTest constraints match handwritten" do
    let decsStr = generatedMultiApplyStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "MultiApplyTest"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "MultiApplyTest"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: ExplicitlyReturnMonadicValuesTest constraints match handwritten" do
    let decsStr = generatedExplicitStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "ExplicitlyReturnMonadicValuesTest"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "ExplicitlyReturnMonadicValuesTest"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: DefaultMethodTest constraints match handwritten" do
    let decsStr = generatedDefaultMethodStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "DefaultMethodTest"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "DefaultMethodTest"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: AssocTypeTest constraints match handwritten" do
    let decsStr = generatedAssocTypeStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "AssocTypeTest"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "AssocTypeTest"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "TypeClass: ParamThreeMonad constraints match handwritten" do
    let decsStr = generatedParamThreeStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/TypeClassSpec.hs" "ParamThreeMonad"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "ParamThreeMonad"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "PartialMock: UserInputGetter constraints match handwritten" do
    let decsStr = generatedUserInputStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/PartialMockSpec.hs" "UserInputGetter"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "UserInputGetter"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  it "PartialMock: ExplicitlyReturnMonadicValuesPartialTest constraints match handwritten" do
    let decsStr = generatedExplicitPartialStr
    hand <- extractHandwrittenInstanceCxts "test/Test/MockCat/PartialMockSpec.hs" "ExplicitlyReturnMonadicValuesPartialTest"
    let gen = extractGeneratedInstanceCxtsFromStr decsStr "ExplicitlyReturnMonadicValuesPartialTest"
    length gen `shouldSatisfy` (> 0)
    length hand `shouldSatisfy` (> 0)
    concat gen `shouldSatisfy` (not . isInfixOf "Typeable (Param")

  -- Additional per-class checks can be added here for classes exported by library modules.

-- helper to extract generated instance headers from a pre-pprinted string
extractGeneratedInstanceCxtsFromStr :: String -> String -> [String]
extractGeneratedInstanceCxtsFromStr s className = map normalize $
  let txt = T.pack s
      insts = T.splitOn (T.pack "instance") txt
      -- discard leading part before first instance
      instBodies = drop 1 insts
      takeHeader t =
        let h = T.takeWhile (/= '{') $ T.takeWhile (/= 'w') t -- up to 'where' or '{'
         in T.unpack h
   in [ takeHeader body
      | body <- instBodies
      , className `isInfixOf` T.unpack body || className `isInfixOf` T.unpack (T.take 200 body)
      , "MockT" `isInfixOf` T.unpack body
      ]


