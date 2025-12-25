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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MockCat.TypeClassTHSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Test.Hspec
import Test.MockCat
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.TypeClassCommonSpec as SpecCommon

--makeAutoLiftMock [t|MonadReader Bool|]
makeAutoLiftMock [t|MonadReader SpecCommon.Environment|]
makeMock [t|MonadVar2_1Sub|]
makeMock [t|MonadVar2_2Sub|]
makeMock [t|MonadVar3_1Sub|]
makeMock [t|MonadVar3_2Sub|]
makeMock [t|MonadVar3_3Sub|]
makeAutoLiftMock [t|FileOperation|]
makeAutoLiftMock [t|ApiOperation|]
makeMock [t|MultiApplyTest|]
makeMock [t|ParamThreeMonad Int Bool|]
makeMock [t|MonadStateSub|]
makeMock [t|MonadStateSub2|]
makeMock [t|Teletype|]
makeMock [t|ExplicitlyReturnMonadicValuesTest|]
makeAutoLiftMock [t|DefaultMethodTest|]
makeAutoLiftMock [t|AssocTypeTest|]
makeMock [t|TestClass|]

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
