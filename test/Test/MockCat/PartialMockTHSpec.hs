{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.MockCat.PartialMockTHSpec (spec) where

import Test.MockCat
import qualified Test.MockCat.PartialMockCommonSpec as PartialMockCommonSpec
import Test.MockCat.SharedSpecDefs
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Test.Hspec (Spec, describe)
import Test.MockCat.Impl ()

instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = (pure . Just . UserInput) a

instance ExplicitlyReturnMonadicValuesPartialTest IO where
  echoExplicitPartial _ = pure () 
  getByExplicitPartial s = pure $ length s


makeAutoLiftPartialMock [t|UserInputGetter|]
makeAutoLiftPartialMock [t|Finder|]
makeAutoLiftPartialMock [t|FileOperation|]
makePartialMock [t|ExplicitlyReturnMonadicValuesPartialTest|]
makePartialMock [t|FinderNoImplicit|]

spec :: Spec
spec = describe "PartialMockTHSpec" $
  PartialMockCommonSpec.spec deps (program "input.txt" "output.text")
  where
    deps = PartialMockCommonSpec.PartialMockDeps
      { PartialMockCommonSpec._getInput = Test.MockCat.PartialMockTHSpec._getInput
      , PartialMockCommonSpec._getBy = _getByExplicitPartial
      , PartialMockCommonSpec._echo = _echoExplicitPartial
      , PartialMockCommonSpec._writeFile = Test.MockCat.PartialMockTHSpec._writeFile
      , PartialMockCommonSpec._findIds = Test.MockCat.PartialMockTHSpec._findIds
      , PartialMockCommonSpec._findById = Test.MockCat.PartialMockTHSpec._findById
      , PartialMockCommonSpec._findByIdNI = Test.MockCat.PartialMockTHSpec._findByIdNI
      }