{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.PartialMockSpec (spec) where

import Data.Text (Text)
import Test.Hspec (Spec)
import Data.List (find)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import Test.MockCat.PartialMockCommonSpec (specUserInputGetterPoly, specExplicitReturnPoly, specFileOperationPoly, specMultiParamPartial1, specMultiParamPartialFindById, specMultiParamAllReal, specPartialHandwrittenIO, specPartialHandwrittenMaybeT, specVerificationFailureFindIds, specVerificationFailureFindById, specFinderParallel, specFinderEdgeCases, specFinderEmptyIds, specFinderNamedError, specFinderMixedFallback, specFinderNoImplicit)
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Data
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import qualified Test.MockCat.Verify as Verify


ensureVerifiable ::
  ( MonadIO m
  , Verify.ResolvableMock target
  ) =>
  target ->
  m ()
ensureVerifiable target =
  liftIO $
    Verify.resolveForVerification target >>= \case
      Just _ -> pure ()
      Nothing -> Verify.verificationFailure


instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = pure . Just . UserInput $ a

instance ExplicitlyReturnMonadicValuesPartialTest IO where
  echoExplicitPartial _ = pure ()
  getByExplicitPartial s = pure (length s)


instance (MonadIO m, FileOperation m) => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "readFile") defs of
      Just mockFn -> do
        let !result = mockFn path
        pure result
      Nothing -> lift $ readFile path

  writeFile path content = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "writeFile") defs of
      Just mockFn -> do
        let !result = mockFn path content
        pure result
      Nothing -> lift $ writeFile path content

instance (MonadIO m, UserInputGetter m) => UserInputGetter (MockT m) where
  getInput = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "getInput") defs of
      Just mockFn -> do
        let !result = mockFn
        pure result
      Nothing -> lift getInput

  toUserInput str = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "toUserInput") defs of
      Just mockFn -> do
        let !result = mockFn str
        lift result
      Nothing -> lift $ toUserInput str

instance
  ( MonadIO m
  , ExplicitlyReturnMonadicValuesPartialTest m
  ) =>
  ExplicitlyReturnMonadicValuesPartialTest (MockT m) where
  getByExplicitPartial label = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "getBy") defs of
      Just mockFn -> do
        let !result = mockFn label
        lift result
      Nothing -> lift $ getByExplicitPartial label

  echoExplicitPartial label = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "echo") defs of
      Just mockFn -> do
        let !result = mockFn label
        lift result
      Nothing -> lift $ echoExplicitPartial label

_readFile ::
  ( MockBuilder params (FilePath -> Text) (Param FilePath)
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text)
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "readFile" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance NoVerification)
  pure mockInstance

_writeFile ::
  ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text -> ())
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "writeFile" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance NoVerification)
  pure mockInstance

_getInput ::
  ( Verify.ResolvableParamsOf r ~ ()
  , MonadIO m
  , Typeable r
  ) =>
  r ->
  MockT m r
_getInput value = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "getInput" (Head :> param value)
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "getInput") mockInstance NoVerification)
  pure mockInstance

_toUserInput ::
  ( MockBuilder params (String -> m (Maybe UserInput)) (Param String)
  , MonadIO m
  , Typeable (String -> m (Maybe UserInput))
  , Verify.ResolvableParamsOf (String -> m (Maybe UserInput)) ~ Param String
  ) =>
  params ->
  MockT m (String -> m (Maybe UserInput))
_toUserInput p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "toUserInput" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "toUserInput") mockInstance NoVerification)
  pure mockInstance

_getByPartial ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable (String -> m Int)
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m (String -> m Int)
_getByPartial p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "getBy" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "getBy") mockInstance NoVerification)
  pure mockInstance

_echoPartial ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable (String -> m ())
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m (String -> m ())
_echoPartial p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "echo" p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "echo") mockInstance NoVerification)
  pure mockInstance

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFunction _) -> unsafeCoerce mockFunction) definition

instance (MonadIO m, Finder a b m) => Finder a b (MockT m) where

  findIds = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "_findIds") defs of
      Just mockFn -> do
        let !result = mockFn
        pure result
      Nothing -> lift findIds

  findById id = MockT do
    defs <- getDefinitions
    case findParam (Proxy :: Proxy "_findById") defs of
      Just mockFn -> do
        let !result = mockFn id
        pure result
      Nothing -> lift $ findById id

 

_findIds ::
  ( Verify.ResolvableParamsOf r ~ ()
  , MonadIO m
  , Typeable r
  ) =>
  r ->
  MockT m r
_findIds p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_findIds" (Head :> param p)
  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findIds")
        mockInstance
        NoVerification
    )
  pure mockInstance

_findById ::
  ( MockBuilder params (Int -> String) (Param Int)
  , MonadIO m
  , Typeable (Int -> String)
  , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
  ) =>
  params ->
  MockT m (Int -> String)
_findById p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_findById" p
  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findById")
        mockInstance
        NoVerification
    )
  pure mockInstance

_findByIdNI ::
  ( MockBuilder params (Int -> IO String) (Param Int)
  , MonadIO m
  ) =>
  params ->
  MockT m (Int -> IO String)
_findByIdNI p = MockT $ do
  mockInstance <- liftIO $ createNamedMockFnWithParams "_findByIdNI" p
  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findByIdNI")
        mockInstance
        NoVerification
    )
  pure mockInstance

spec :: Spec
spec = do
  specUserInputGetterPoly _getInput
  specExplicitReturnPoly _getByPartial _echoPartial
  -- FileOperation: keep originals, but also call common poly with polymorphic builder
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
  specFinderNoImplicit _findByIdNI
