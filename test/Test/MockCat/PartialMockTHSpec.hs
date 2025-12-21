{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.MockCat.PartialMockTHSpec (spec) where

import Test.MockCat
import Test.MockCat.PartialMockCommonSpec (specUserInputGetterPoly, specExplicitReturnPoly, specFileOperationPoly, specMultiParamPartial1, specMultiParamPartialFindById, specMultiParamAllReal, specPartialHandwrittenIO, specPartialHandwrittenMaybeT, specVerificationFailureFindIds, specVerificationFailureFindById, specFinderParallel, specFinderEdgeCases, specFinderEmptyIds, specFinderNamedError, specFinderMixedFallback, specFinderNoImplicit)
import Test.MockCat.SharedSpecDefs
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Typeable (Typeable)
import qualified Test.MockCat.Verify as Verify
import Data.List (find)
import Test.Hspec (Spec)

instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a

instance ExplicitlyReturnMonadicValuesPartialTest IO where
  echoExplicitPartial _ = pure () 
  getByExplicitPartial s = pure $ length s


makePartialMock [t|UserInputGetter|]
makePartialMock [t|Finder|]
makePartialMock [t|FileOperation|]
makePartialMockWithOptions [t|ExplicitlyReturnMonadicValuesPartialTest|] options { implicitMonadicReturn = False }
makePartialMockWithOptions [t|FinderNoImplicit|] options { implicitMonadicReturn = False }

-- Specialize polymorphic TH-generated builder to IO for common spec
_findByIdNI_IO ::
  ( MockBuilder params (Int -> IO String) (Param Int)
  , Typeable (Int -> IO String)
  , Verify.ResolvableParamsOf (Int -> IO String) ~ Param Int
  ) =>
  params ->
  MockT IO (Int -> IO String)
_findByIdNI_IO = _findByIdNI

spec :: Spec
spec = do
  specUserInputGetterPoly _getInput
  specExplicitReturnPoly _getByExplicitPartial _echoExplicitPartial
  -- FileOperation common tests (call common poly in addition to existing originals)
  specFileOperationPoly _writeFile
  specMultiParamPartial1 _findIds
  specMultiParamPartialFindById _findById
  specMultiParamAllReal
  specPartialHandwrittenIO _writeFile (program "input.txt" "output.text")
  specPartialHandwrittenMaybeT _writeFile
  specVerificationFailureFindIds _findIds
  specVerificationFailureFindById _findById
  specFinderParallel _findById
  specFinderEdgeCases _findById
  specFinderEmptyIds _findIds
  specFinderNamedError _findById
  specFinderMixedFallback _findById
  specFinderNoImplicit _findByIdNI_IO
  