{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.MockCat.PartialMockSpec (spec) where

import Data.Text (Text)
import Test.Hspec (Spec)
import Data.List (find)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.PartialMockCommonSpec as PartialMockCommonSpec
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Data
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Internal.Types (InvocationRecorder)


ensureVerifiable ::
  ( MonadIO m
  , Verify.ResolvableMock target
  ) =>
  target ->
  m ()
ensureVerifiable target =
  liftIO $ do
    candidates <- Verify.resolveForVerification target
    case candidates of
      [] -> Verify.verificationFailure
      _ -> pure ()


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
  ( MockDispatch (IsMockSpec params) params (MockT m) (FilePath -> Text)
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text)
_readFile p = MockT $ do
  mockInstance <- unMockT $ mock (label "readFile") p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance NoVerification)
  pure mockInstance

_writeFile ::
  ( MockDispatch (IsMockSpec params) params (MockT m) (FilePath -> Text -> ())
  , MonadIO m
  ) =>
  params ->
  MockT m (FilePath -> Text -> ())
_writeFile p = MockT $ do
  mockInstance <- unMockT $ mock (label "writeFile") p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance NoVerification)
  pure mockInstance

_getInput ::
  ( MockDispatch (IsMockSpec params) params (MockT m) String
  , MonadIO m
  , Typeable (Verify.ResolvableParamsOf String)
  , Typeable params
  , Show params
  , Eq params
  ) =>
  params ->
  MockT m String
_getInput value = MockT $ do
  mockInstance <- unMockT $ mock (label "getInput") value
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "getInput") mockInstance NoVerification)
  pure mockInstance

_toUserInput ::
  ( MockDispatch (IsMockSpec params) params (MockT m) (String -> m (Maybe UserInput))
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m (Maybe UserInput)) ~ Param String
  ) =>
  params ->
  MockT m (String -> m (Maybe UserInput))
_toUserInput p = MockT $ do
  mockInstance <- unMockT $ mock (label "toUserInput") p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "toUserInput") mockInstance NoVerification)
  pure mockInstance

_getByPartial ::
  ( MockDispatch (IsMockSpec params) params (MockT IO) (String -> IO Int)
  ) =>
  params ->
  MockT IO (String -> IO Int)
_getByPartial p = MockT $ do
  mockInstance <- unMockT $ mock (label "getBy") p
  ensureVerifiable mockInstance
  addDefinition (Definition (Proxy :: Proxy "getBy") mockInstance NoVerification)
  pure mockInstance

_echoPartial ::
  ( MockDispatch (IsMockSpec params) params (MockT IO) (String -> IO ())
  ) =>
  params ->
  MockT IO (String -> IO ())
_echoPartial p = MockT $ do
  mockInstance <- unMockT $ mock (label "echo") p
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
  ( MockDispatch (IsMockSpec p) p (MockT m) [a]
  , MonadIO m
  , Typeable p
  , Show p
  , Eq p
  , Typeable (InvocationRecorder (Verify.ResolvableParamsOf [a]))
  , Typeable (Verify.ResolvableParamsOf [a])
  , Typeable [a]
  , Typeable a
  ) =>
  p ->
  MockT m [a]
_findIds p = MockT $ do
  mockInstance <- unMockT $ mock (label "_findIds") p

  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findIds")
        mockInstance
        NoVerification
    )
  pure mockInstance

_findById ::
  ( MockDispatch (IsMockSpec params) params (MockT m) (Int -> String)
  , MonadIO m
  ) =>
  params ->
  MockT m (Int -> String)
_findById p = MockT $ do
  mockInstance <- unMockT $ mock (label "_findById") p
  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findById")
        mockInstance
        NoVerification
    )
  pure mockInstance

_findByIdNI ::
  ( MockDispatch (IsMockSpec params) params (MockT IO) (Int -> IO String)
  ) =>
  params ->
  MockT IO (Int -> IO String)
_findByIdNI p = MockT $ do
  mockInstance <- unMockT $ mock (label "_findByIdNI") p
  ensureVerifiable mockInstance
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findByIdNI")
        mockInstance
        NoVerification
    )
  pure mockInstance

spec :: Spec
spec = PartialMockCommonSpec.spec deps (program "input.txt" "output.text")
  where
    deps = PartialMockCommonSpec.PartialMockDeps
      { PartialMockCommonSpec._getInput = _getInput
      , PartialMockCommonSpec._getBy = _getByPartial
      , PartialMockCommonSpec._echo = _echoPartial
      , PartialMockCommonSpec._writeFile = _writeFile
      , PartialMockCommonSpec._findIds = _findIds
      , PartialMockCommonSpec._findById = _findById
      , PartialMockCommonSpec._findByIdNI = _findByIdNI
      }