{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.MockCat.PartialMockCommonSpec
  ( spec
  , PartialDeps(..)
  , UserInputGetterDeps(..)
  , ExplicitReturnDeps(..)
  , FileOperationDeps(..)
  , FileOperationMonadTransformerDeps(..)
  , FinderDeps(..)
  , VerificationFailureDeps(..)
  , MockFor
  , MockForM
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe, describe, shouldThrow, Selector)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import Test.MockCat.Impl ()
import Control.Exception (ErrorCall(..), displayException)
import Data.List (isInfixOf)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Kind (Type)
import qualified Test.MockCat.Verify as Verify
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)

-- Type-level utilities to derive MockBuilder arg-lists from function types
type family PrependParam (p :: Type) (rest :: Type) :: Type where
  PrependParam p () = p
  PrependParam p rest = p :> rest

type family ArgsOfF (f :: Type) :: Type where
  ArgsOfF (a -> b) = PrependParam (Param a) (ArgsOfF b)
  ArgsOfF r = ()

-- Generic Mock alias for a function type f
type MockFor f = forall params. (MockBuilder params f (ArgsOfF f)) => params -> MockT IO f
-- Generic Mock alias for an arbitrary base monad m
type MockForM m f = forall params. (MockBuilder params f (ArgsOfF f)) => params -> MockT m f

getUserInput :: UserInputGetter m => m (Maybe UserInput)
getUserInput = do
  i <- getInput
  toUserInput i

echoProgram :: ExplicitlyReturnMonadicValuesPartialTest m => String -> m ()
echoProgram s = do
  v <- getByExplicitPartial s
  echoExplicitPartial $ show v

missingCall :: String -> Selector ErrorCall
missingCall name err =
  let needle = "function `" <> name <> "` was not applied the expected number of times."
   in needle `isInfixOf` displayException err

-- Per-spec dependency records to group required builders/mocks
data UserInputGetterDeps = UserInputGetterDeps
  { _getInput ::
      forall r m.
      ( Verify.ResolvableParamsOf r ~ ()
      , MonadIO m
      , Typeable r
      ) =>
      r ->
      MockT m r
  , _toUserInput ::
      forall params m.
      ( MockBuilder params (String -> m (Maybe UserInput)) (ArgsOfF (String -> m (Maybe UserInput)))
      , MonadIO m
      , Typeable (String -> m (Maybe UserInput))
      , Verify.ResolvableParamsOf (String -> m (Maybe UserInput)) ~ Param String
      ) =>
      params ->
      MockT m (String -> m (Maybe UserInput))
  }

data ExplicitReturnDeps = ExplicitReturnDeps
  { _getByPartial ::
      forall params m.
      ( MockBuilder params (String -> m Int) (ArgsOfF (String -> m Int))
      , MonadIO m
      , Typeable (String -> m Int)
      , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
      ) =>
      params ->
      MockT m (String -> m Int)
  , _echoPartial ::
      forall params m.
      ( MockBuilder params (String -> m ()) (ArgsOfF (String -> m ()))
      , MonadIO m
      , Typeable (String -> m ())
      , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
      ) =>
      params ->
      MockT m (String -> m ())
  }

data FileOperationDeps = FileOperationDeps
  { _writeFile ::
      forall params m.
      ( MockBuilder params (FilePath -> Text -> ()) (ArgsOfF (FilePath -> Text -> ()))
      , MonadIO m
      ) =>
      params ->
      MockT m (FilePath -> Text -> ())
  }

data FileOperationMonadTransformerDeps = FileOperationMonadTransformerDeps
  { _writeFileMaybeT ::
      forall params.
      ( MockBuilder params (FilePath -> Text -> ()) (ArgsOfF (FilePath -> Text -> ()))
      ) =>
      params ->
      MockT (MaybeT IO) (FilePath -> Text -> ())
  , _writeFileReaderT ::
      forall params.
      ( MockBuilder params (FilePath -> Text -> ()) (ArgsOfF (FilePath -> Text -> ()))
      ) =>
      params ->
      MockT (ReaderT String IO) (FilePath -> Text -> ())
  }

data FinderDeps = FinderDeps
  { _findIds ::
      forall r m.
      ( Verify.ResolvableParamsOf r ~ ()
      , MonadIO m
      , Typeable r
      ) =>
      r ->
      MockT m r
  , _findById ::
      forall params m.
      ( MockBuilder params (Int -> String) (ArgsOfF (Int -> String))
      , MonadIO m
      , Typeable (Int -> String)
      , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
      ) =>
      params ->
      MockT m (Int -> String)
  }

data VerificationFailureDeps = VerificationFailureDeps
  { _readFile ::
      forall params m.
      ( MockBuilder params (FilePath -> Text) (ArgsOfF (FilePath -> Text))
      , MonadIO m
      ) =>
      params ->
      MockT m (FilePath -> Text)
  , _writeFile ::
      forall params m.
      ( MockBuilder params (FilePath -> Text -> ()) (ArgsOfF (FilePath -> Text -> ()))
      , MonadIO m
      ) =>
      params ->
      MockT m (FilePath -> Text -> ())
  , _getInput ::
      forall r m.
      ( Verify.ResolvableParamsOf r ~ ()
      , MonadIO m
      , Typeable r
      ) =>
      r ->
      MockT m r
  , _toUserInput ::
      forall params m.
      ( MockBuilder params (String -> m (Maybe UserInput)) (ArgsOfF (String -> m (Maybe UserInput)))
      , MonadIO m
      , Typeable (String -> m (Maybe UserInput))
      , Verify.ResolvableParamsOf (String -> m (Maybe UserInput)) ~ Param String
      ) =>
      params ->
      MockT m (String -> m (Maybe UserInput))
  , _getByPartial ::
      forall params m.
      ( MockBuilder params (String -> m Int) (ArgsOfF (String -> m Int))
      , MonadIO m
      , Typeable (String -> m Int)
      , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
      ) =>
      params ->
      MockT m (String -> m Int)
  , _echoPartial ::
      forall params m.
      ( MockBuilder params (String -> m ()) (ArgsOfF (String -> m ()))
      , MonadIO m
      , Typeable (String -> m ())
      , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
      ) =>
      params ->
      MockT m (String -> m ())
  , _findIds ::
      forall r m.
      ( Verify.ResolvableParamsOf r ~ ()
      , MonadIO m
      , Typeable r
      ) =>
      r ->
      MockT m r
  }

-- Aggregate all per-spec dependency groups into one record
data PartialDeps = PartialDeps
  { userInputGetterDeps              :: UserInputGetterDeps
  , explicitReturnDeps                :: ExplicitReturnDeps
  , fileOperationDeps                 :: FileOperationDeps
  , fileOperationMonadTransformerDeps:: FileOperationMonadTransformerDeps
  , finderDeps                        :: FinderDeps
  , verificationFailureDeps           :: VerificationFailureDeps
  }

spec ::
  ( UserInputGetter (MockT IO)
  , ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  , FileOperation (MockT IO)
  , FileOperation (MockT (MaybeT IO))
  , FileOperation (MockT (ReaderT String IO))
  , Finder Int String (MockT IO)
  ) =>
  PartialDeps ->
  Spec
spec deps = do
  specUserInputGetter (userInputGetterDeps deps)
  specExplicitReturn (explicitReturnDeps deps)
  specFileOperation (fileOperationDeps deps)
  specFileOperationMonadTransformer (fileOperationMonadTransformerDeps deps)
  specFinder (finderDeps deps)
  specVerificationFailures (verificationFailureDeps deps)

specUserInputGetter ::
  ( UserInputGetter (MockT IO)
  ) =>
  UserInputGetterDeps ->
  Spec
specUserInputGetter (UserInputGetterDeps { _getInput, _toUserInput }) = describe "UserInputGetter" do
  it "Get user input (has input)" do
    result <- runMockT do
      _ <- _getInput ("value" :: String)
      getUserInput
    result `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    result <- runMockT do
      _ <- _getInput ("" :: String)
      getUserInput
    result `shouldBe` Nothing

specExplicitReturn ::
  ( ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  ) =>
  ExplicitReturnDeps ->
  Spec
specExplicitReturn (ExplicitReturnDeps { _getByPartial, _echoPartial }) = describe "ExplicitlyReturnMonadicValuesPartialTest" do
  it "Return monadic value test" do
    result <- runMockT $ do
      _ <- _echoPartial (("3" :: String) |> pure @IO ())
      echoProgram "abc"
    result `shouldBe` ()

  it "Override getBy via stub" do
    result <- runMockT do
      _ <- _getByPartial (("abc" :: String) |> pure @IO (123 :: Int))
      getByExplicitPartial "abc"
    result `shouldBe` 123

specFileOperation ::
  ( FileOperation (MockT IO)
  ) =>
  FileOperationDeps ->
  Spec
specFileOperation (FileOperationDeps { _writeFile }) = describe "FileOperation" do
  it "IO" do
    result <- runMockT do
      _ <- _writeFile ((("output.text" :: FilePath) |> pack ("IO content" :: String) |> ()))
      program "input.txt" "output.text"
    result `shouldBe` ()

specFileOperationMonadTransformer ::
  ( FileOperation (MockT (MaybeT IO))
  , FileOperation (MockT (ReaderT String IO))
  ) =>
  FileOperationMonadTransformerDeps ->
  Spec
specFileOperationMonadTransformer (FileOperationMonadTransformerDeps { _writeFileMaybeT, _writeFileReaderT }) = describe "FileOperation - Monad Transformer Compatibility" do
  it "MaybeT" do
    result <- runMaybeT do
      runMockT do
        _ <- _writeFileMaybeT ((("output.text" :: FilePath) |> pack ("MaybeT content" :: String) |> ()))
        program "input.txt" "output.text"
    result `shouldBe` Just ()

  it "ReaderT" do
    result <- flip runReaderT "foo" do
      runMockT do
        _ <- _writeFileReaderT ((("output.text" :: FilePath) |> pack ("ReaderT content foo" :: String) |> ()))
        program "input.txt" "output.text"
    result `shouldBe` ()

specFinder ::
  ( Finder Int String (MockT IO)
  ) =>
  FinderDeps ->
  Spec
specFinder (FinderDeps { _findIds, _findById }) = describe "Finder" do
  it "all real function" do
    values <- runMockT findValue
    values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

  it "partial findIds" do
    values <- runMockT $ do
      _ <- _findIds ([1 :: Int, 2])
      findValue @Int @String
    values `shouldBe` ["{id: 1}", "{id: 2}"]

  it "partial findById" do
    values <- runMockT $ do
      _ <- _findById $ do
        onCase $ (1 :: Int) |> "id1"
        onCase $ (2 :: Int) |> "id2"
        onCase $ (3 :: Int) |> "id3"
      findValue @Int @String
    values `shouldBe` ["id1", "id2", "id3"]

specVerificationFailures ::
  ( FileOperation (MockT IO)
  , ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  , Finder Int String (MockT IO)
  ) =>
  VerificationFailureDeps ->
  Spec
specVerificationFailures (VerificationFailureDeps { _readFile, _writeFile, _getInput, _toUserInput, _getByPartial, _echoPartial, _findIds }) = describe "verification failures" do
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
      _ <- _getInput ("value" :: String)
        `expects` do
          called once
      -- getInput is never called
      pure ()) `shouldThrow` (missingCall "getInput")

  it "fails when _toUserInput is defined but toUserInput is never called" do
    (runMockT @IO do
      _ <- _toUserInput (("value" :: String) |> pure @IO (Just (UserInput "value")))
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
      _ <- _findIds ([1 :: Int, 2])
        `expects` do
          called once
      -- findIds is never called
      pure ()) `shouldThrow` (missingCall "_findIds")
