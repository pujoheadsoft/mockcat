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

import Data.Text (Text, pack)
import Control.Exception (ErrorCall(..), displayException)
import Data.List (find, isInfixOf)
import Test.Hspec (Spec, it, shouldBe, describe, shouldThrow, Selector)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import Test.MockCat.PartialMockCommonSpec (specUserInputGetterPoly, specExplicitReturnPoly, specFileOperationPoly, specMultiParamPartial1, specMultiParamPartialFindById, specMultiParamAllReal, specPartialHandwrittenIO, specPartialHandwrittenMaybeT, specVerificationFailureFindIds, specVerificationFailureFindById)
import Test.MockCat.Impl ()
import Prelude hiding (readFile, writeFile)
import Data.Data
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader hiding (ask)
import qualified Test.MockCat.Verify as Verify

missingCall :: String -> Selector ErrorCall
missingCall name err =
  let needle = "function `" <> name <> "` was not applied the expected number of times."
   in needle `isInfixOf` displayException err

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

  describe "Partial Mock Test (TH)" do
    it "MaybeT" do
      result <- runMaybeT do
        runMockT $ do
          _writeFile $ "output.text" |> pack "MaybeT content" |> ()
          program "input.txt" "output.text"
      result `shouldBe` Just ()

    it "IO" do
      result <- runMockT $ do
        _writeFile $ "output.text" |> pack "IO content" |> ()
        program "input.txt" "output.text"
      result `shouldBe` ()

    it "ReaderT" do
      result <- flip runReaderT "foo" $ do
        runMockT $ do
          _writeFile $ "output.text" |> pack "ReaderT content foo" |> ()
          program "input.txt" "output.text"
      result `shouldBe` ()

    describe "MultiParamType" do
      it "all real function" do
        values <- runMockT findValue
        values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

      it "partial findIds" do
        values <- runMockT $ do
          _findIds [1 :: Int, 2]
          findValue
        values `shouldBe` ["{id: 1}", "{id: 2}"]

      it "partial findById" do
        values <- runMockT $ do
          _findById $ do
            onCase $ (1 :: Int) |> "id1"
            onCase $ (2 :: Int) |> "id2"
            onCase $ (3 :: Int) |> "id3"
          findValue
        values `shouldBe` ["id1", "id2", "id3"]

    it "Return monadic value test" do
      result <- runMockT $ do
        _echoPartial $ "3" |> pure @IO ()
        echoProgramPartial "abc"
      result `shouldBe` ()

  describe "verification failures" do
    it "fails when _readFile is defined but readFile is never called" do
      (runMockT @IO do
        _ <- _readFile ("input.txt" |> pack "content")
          `expects` do
            called once
        -- readFile is never called
        pure ()) `shouldThrow` (missingCall "readFile")

    it "fails when _writeFile is defined but writeFile is never called" do
      (runMockT @IO do
        _ <- _writeFile ("output.txt" |> pack "content" |> ())
          `expects` do
            called once
        -- writeFile is never called
        pure ()) `shouldThrow` (missingCall "writeFile")

    it "fails when _getInput is defined but getInput is never called" do
      (runMockT @IO do
        _ <- _getInput "value"
          `expects` do
            called once
        -- getInput is never called
        pure ()) `shouldThrow` (missingCall "getInput")

    it "fails when _toUserInput is defined but toUserInput is never called" do
      (runMockT @IO do
        _ <- _toUserInput ("value" |> pure @IO (Just (UserInput "value")))
          `expects` do
            called once
        -- toUserInput is never called
        pure ()) `shouldThrow` (missingCall "toUserInput")

    it "fails when _getByPartial is defined but getByExplicitPartial is never called" do
      (runMockT @IO do
        _ <- _getByPartial ("abc" |> pure @IO 123)
          `expects` do
            called once
        -- getByExplicitPartial is never called
        pure ()) `shouldThrow` (missingCall "getBy")

    it "fails when _echoPartial is defined but echoExplicitPartial is never called" do
      (runMockT @IO do
        _ <- _echoPartial ("3" |> pure @IO ())
          `expects` do
            called once
        -- echoExplicitPartial is never called
        pure ()) `shouldThrow` (missingCall "echo")

    it "fails when _findIds is defined but findIds is never called" do
      (runMockT @IO do
        _ <- _findIds [1 :: Int, 2]
          `expects` do
            called once
        -- findIds is never called
        pure ()) `shouldThrow` (missingCall "_findIds")