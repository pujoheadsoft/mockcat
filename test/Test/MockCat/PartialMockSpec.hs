{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.PartialMockSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader hiding (ask)
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Internal.Message (mockNameLabel)
import Test.MockCat.Internal.Types (Verifier (..))

verifyResolvedAny :: Verify.ResolvedMock params -> IO ()
verifyResolvedAny (Verify.ResolvedMock name (Verifier ref)) = do
  appliedParamsList <- Verify.readAppliedParamsList ref
  when (null appliedParamsList) $
    error $ "It has never been applied function" <> mockNameLabel name

getUserInput :: UserInputGetter m => m (Maybe UserInput)
getUserInput = do
  i <- getInput
  toUserInput i

instance UserInputGetter IO where
  getInput = getLine
  toUserInput "" = pure Nothing
  toUserInput a = pure . Just . UserInput $ a

instance ExplicitlyReturnMonadicValuesPartialTest IO where
  echoExplicitPartial _ = pure ()
  getByExplicitPartial s = pure (length s)

echoProgramPartial :: ExplicitlyReturnMonadicValuesPartialTest m => String -> m ()
echoProgramPartial s = do
  v <- getByExplicitPartial s
  echoExplicitPartial (show v)

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
  MockT m ()
_readFile p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "readFile" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "readFile") mockInstance verifyStub)

_writeFile ::
  ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
_writeFile p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "writeFile" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "writeFile") mockInstance verifyStub)

_getInput ::
  ( Typeable r
  , Verify.ResolvableParamsOf r ~ ()
  , MonadIO m
  ) =>
  r ->
  MockT m ()
_getInput value = MockT $ do
  mockInstance <- liftIO $ createNamedConstantStubFn "getInput" value
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "getInput") mockInstance verifyStub)

_toUserInput ::
  ( MockBuilder params (String -> m (Maybe UserInput)) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m (Maybe UserInput)) ~ Param String
  ) =>
  params ->
  MockT m ()
_toUserInput p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "toUserInput" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "toUserInput") mockInstance verifyStub)

_getByPartial ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m ()
_getByPartial p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "getBy" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "getBy") mockInstance verifyStub)

_echoPartial ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_echoPartial p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "echo" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "echo") mockInstance verifyStub)

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFn _) -> unsafeCoerce mockFn) definition

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
  ( Typeable r
  , Verify.ResolvableParamsOf r ~ ()
  , MonadIO m
  ) =>
  r ->
  MockT m ()
_findIds p = MockT $ do
  mockInstance <- liftIO $ createNamedConstantStubFn "_findIds" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition
    ( Definition
        (Proxy :: Proxy "_findIds")
        mockInstance
        verifyStub
    )

spec :: Spec
spec = do
  it "Get user input (has input)" do
    result <- runMockT do
      _getInput "value"
      getUserInput
    result `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    result <- runMockT do
      _getInput ""
      getUserInput
    result `shouldBe` Nothing

  it "Return monadic value test (partial)" do
    result <- runMockT do
      _echoPartial $ "3" |> pure @IO ()
      echoProgramPartial "abc"
    result `shouldBe` ()

  it "Override getBy via stub" do
    result <- runMockT do
      _getByPartial $ "abc" |> pure @IO 123
      getByExplicitPartial "abc"
    result `shouldBe` 123

  describe "Partial Mock Test" do
    it "MaybeT" do
      result <- runMaybeT do
        runMockT do
          _writeFile $ "output.text" |> pack "MaybeT content" |> ()
          program "input.txt" "output.text"

      result `shouldBe` Just ()

    it "IO" do
      result <- runMockT do
        _writeFile $ "output.text" |> pack "IO content" |> ()
        program "input.txt" "output.text"

      result `shouldBe` ()

    it "ReaderT" do
      result <- flip runReaderT "foo" do
        runMockT do
          _writeFile $ "output.text" |> pack "ReaderT content foo" |> ()
          program "input.txt" "output.text"

      result `shouldBe` ()
    
    describe "MultiParamType" do
      it "all real function" do
        values <- runMockT findValue
        values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

      it "partial 1" do
        values <- runMockT  do
          _findIds [1 :: Int, 2]
          findValue
        values `shouldBe` ["{id: 1}", "{id: 2}"]