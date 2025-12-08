{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Test.Hspec
import Test.MockCat
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.TypeClassCommonSpec as SpecCommon

--makeMock [t|MonadReader Bool|]
makeMock [t|MonadReader SpecCommon.Environment|]
makeMockWithOptions [t|MonadVar2_1Sub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadVar2_2Sub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadVar3_1Sub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadVar3_2Sub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadVar3_3Sub|] options { implicitMonadicReturn = False }
makeMock [t|FileOperation|]
makeMock [t|ApiOperation|]
makeMockWithOptions [t|MultiApplyTest|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|ParamThreeMonad Int Bool|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadStateSub|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|MonadStateSub2|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }
makeMockWithOptions [t|ExplicitlyReturnMonadicValuesTest|] options { implicitMonadicReturn = False }
makeMock [t|DefaultMethodTest|]
makeMock [t|AssocTypeTest|]
makeMockWithOptions [t|TestClass|] options { implicitMonadicReturn = False }

instance (MonadUnliftIO m) => MonadAsync (MockT m) where
  mapConcurrently = traverse

instance AssocTypeTest IO where
  type ResultType IO = Int
  produce = pure 0

spec :: Spec
spec = do
  -- build SpecDeps and call aggregated spec entrypoint
  let deps = SpecCommon.SpecDeps
        { SpecCommon.basicDeps          = SpecCommon.BasicDeps _readFile _writeFile
        , SpecCommon.mixedDeps          = SpecCommon.MixedDeps _readFile _writeFile _post
        , SpecCommon.multipleDeps       = SpecCommon.MultipleDeps _ask _readFile _writeFile _post
        , SpecCommon.customNamingDeps   = SpecCommon.CustomNamingDeps _post
        , SpecCommon.readerContextDeps  = SpecCommon.ReaderContextDeps _ask _readFile _writeFile
        , SpecCommon.sequentialIODeps            = SpecCommon.SequentialIODeps _readTTY _writeTTY
        , SpecCommon.ttyDeps                      = SpecCommon.TtyDeps _readTTY _writeTTY
        , SpecCommon.implicitMonadicReturnDeps    = SpecCommon.ImplicitMonadicReturnDeps _getBy _echo
        , SpecCommon.testClassDeps                = SpecCommon.TestClassDeps _getBy _echo
        , SpecCommon.argumentPatternMatchingDeps  = SpecCommon.ArgumentPatternMatchingDeps _getValueBy
        , SpecCommon.multiApplyDeps               = SpecCommon.MultiApplyDeps _getValueBy
        , SpecCommon.monadStateTransformerDeps    = SpecCommon.MonadStateTransformerDeps _fnState _fnState2
        , SpecCommon.stateDeps                    = SpecCommon.StateDeps _fnState _fnState2
        , SpecCommon.multiParamTypeClassArityDeps = SpecCommon.MultiParamTypeClassArityDeps _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub
        , SpecCommon.multiParamDeps               = SpecCommon.MultiParamDeps _fn2_1Sub _fn2_2Sub _fn3_1Sub _fn3_2Sub _fn3_3Sub
        , SpecCommon.functionalDependenciesDeps   = SpecCommon.FunctionalDependenciesDeps _fnParam3_1 _fnParam3_2 _fnParam3_3
        , SpecCommon.funDeps                      = SpecCommon.FunDeps _fnParam3_1 _fnParam3_2 _fnParam3_3
        , SpecCommon.explicitMonadicReturnDeps    = SpecCommon.ExplicitMonadicReturnDeps _getByExplicit _echoExplicit
        , SpecCommon.explicitReturnDeps           = SpecCommon.ExplicitReturnDeps _getByExplicit _echoExplicit
        , SpecCommon.defaultMethodDeps            = SpecCommon.DefaultMethodDeps _defaultAction
        , SpecCommon.associatedTypeFamiliesDeps   = SpecCommon.AssociatedTypeFamiliesDeps _produce
        , SpecCommon.assocTypeDeps                = SpecCommon.AssocTypeDeps _produce
        , SpecCommon.concurrencyAndUnliftIODeps   = SpecCommon.ConcurrencyAndUnliftIODeps _readFile
        , SpecCommon.concurrencyDeps              = SpecCommon.ConcurrencyDeps _readFile
        }
  SpecCommon.spec deps

  -- describe "verification failures (State - Pending)" do
  --   it "fails when _fnState is defined but fnState is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

  --   it "fails when _fnState2 is defined but fnState2 is never called" do
  --     pendingWith "RegisterStub-based mocks require custom expectation handling"

