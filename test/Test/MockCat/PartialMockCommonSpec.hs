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
  ( specUserInputGetterPoly
  , specExplicitReturnPoly
  , specFileOperationPoly
  , specMultiParamPartial1
  , specMultiParamPartialFindById
  , specMultiParamAllReal
  , specPartialHandwrittenIO
  , specPartialHandwrittenMaybeT
  , specVerificationFailureFindIds
  , specVerificationFailureFindById
  ) where

import Prelude hiding (readFile, writeFile)
import Test.Hspec (Spec, it, shouldBe, describe, shouldThrow, Selector)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Impl ()
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Data.Text (Text, pack)
import Control.Exception (ErrorCall(..), displayException)
import Data.List (isInfixOf)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

-- Polymorphic entry for migrating from modules that expose more general builders
specUserInputGetterPoly
  :: UserInputGetter (MockT IO)
  => (forall r m. (Verify.ResolvableParamsOf r ~ (), MonadIO m, Typeable r) => r -> MockT m r)
  -> Spec
specUserInputGetterPoly getInputF = describe "UserInputGetter" do
  it "Get user input (has input)" do
    result <- runMockT do
      _ <- getInputF ("value" :: String)
      i <- getInput
      toUserInput i
    result `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    result <- runMockT do
      _ <- getInputF ("" :: String)
      i <- getInput
      toUserInput i
    result `shouldBe` Nothing


specExplicitReturnPoly
  :: ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  => ( forall params. ( MockBuilder params (String -> IO Int) (Param String)
                     , MonadIO IO
                     , Typeable (String -> IO Int)
                     , Verify.ResolvableParamsOf (String -> IO Int) ~ Param String
                     ) => params -> MockT IO (String -> IO Int))
  -> ( forall params. ( MockBuilder params (String -> IO ()) (Param String)
                     , MonadIO IO
                     , Typeable (String -> IO ())
                     , Verify.ResolvableParamsOf (String -> IO ()) ~ Param String
                     ) => params -> MockT IO (String -> IO ()))
  -> Spec
specExplicitReturnPoly getByF echoF = describe "ExplicitlyReturnMonadicValuesPartialTest" do
  it "Return monadic value test" do
    result <- runMockT $ do
      _ <- echoF $ ("3" :: String) |> pure @IO ()
      v <- getByExplicitPartial "abc"
      echoExplicitPartial (show v)
    result `shouldBe` ()

  it "Override getBy via stub" do
    result <- runMockT do
      _ <- getByF $ ("abc" :: String) |> pure @IO (123 :: Int)
      getByExplicitPartial "abc"
    result `shouldBe` 123


specFileOperationPoly
  :: ( forall params m. ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
                        , MonadIO m
                        , Typeable (FilePath -> Text -> ())
                        , Verify.ResolvableParamsOf (FilePath -> Text -> ()) ~ (Param FilePath :> Param Text)
                        ) => params -> MockT m (FilePath -> Text -> ()))
  -> Spec
specFileOperationPoly writeFileBuilder = describe "FileOperation" do
  it "IO" do
    result <- runMockT do
      _ <- writeFileBuilder (("output.text" :: FilePath) |> pack ("IO content" :: String) |> ())
      pure ()
    result `shouldBe` ()

  it "MaybeT" do
    result <- runMaybeT do
      runMockT do
        _ <- writeFileBuilder (("output.text" :: FilePath) |> pack ("MaybeT content" :: String) |> ())
        pure ()
    result `shouldBe` Just ()

  it "ReaderT" do
    result <- flip runReaderT "foo" do
      runMockT do
        _ <- writeFileBuilder (("output.text" :: FilePath) |> pack ("ReaderT content foo" :: String) |> ())
        pure ()
    result `shouldBe` ()


specMultiParamPartial1
  :: Finder Int String (MockT IO)
  => (forall r m. (Verify.ResolvableParamsOf r ~ (), MonadIO m, Typeable r) => r -> MockT m r)
  -> Spec
specMultiParamPartial1 findIdsBuilder = describe "MultiParamType" do
  it "partial 1" do
    values <- runMockT $ do
      _ <- findIdsBuilder ([1 :: Int, 2] :: [Int])
      findValue @Int @String
    values `shouldBe` ["{id: 1}", "{id: 2}"]


specMultiParamPartialFindById
  :: Finder Int String (MockT IO)
  => ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                        , MonadIO m
                        , Typeable (Int -> String)
                        , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
                        ) => params -> MockT m (Int -> String) )
  -> Spec
specMultiParamPartialFindById findByBuilder = describe "MultiParamType" do
  it "partial findById" do
    values <- runMockT $ do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "id1"
        onCase $ (2 :: Int) |> "id2"
        onCase $ (3 :: Int) |> "id3"
      findValue @Int @String
    values `shouldBe` ["id1", "id2", "id3"]


specMultiParamAllReal
  :: Finder Int String (MockT IO)
  => Spec
specMultiParamAllReal = describe "MultiParamType" do
  it "all real function" do
    values <- runMockT findValue
    values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]


specPartialHandwrittenIO
  :: ( forall params m. ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
                        , MonadIO m
                        , Typeable (FilePath -> Text -> ())
                        , Verify.ResolvableParamsOf (FilePath -> Text -> ()) ~ (Param FilePath :> Param Text)
                        ) => params -> MockT m (FilePath -> Text -> ()))
  -> MockT IO ()
  -> Spec
specPartialHandwrittenIO writeFileBuilder runAction = describe "Partial Mock Test - handwritten" do
  it "IO" do
    result <- runMockT do
      _ <- writeFileBuilder (("output.text" :: FilePath) |> pack ("IO content" :: String) |> ())
      runAction
    result `shouldBe` ()


specPartialHandwrittenMaybeT
  :: ( forall params m. ( MockBuilder params (FilePath -> Text -> ()) (Param FilePath :> Param Text)
                        , MonadIO m
                        , Typeable (FilePath -> Text -> ())
                        , Verify.ResolvableParamsOf (FilePath -> Text -> ()) ~ (Param FilePath :> Param Text)
                        ) => params -> MockT m (FilePath -> Text -> ()))
  -> Spec
specPartialHandwrittenMaybeT writeFileBuilder = describe "Partial Mock Test - handwritten" do
  it "MaybeT" do
    result <- runMaybeT do
      runMockT do
        _ <- writeFileBuilder (("output.text" :: FilePath) |> pack ("MaybeT content" :: String) |> ())
        pure ()
    result `shouldBe` Just ()


specVerificationFailureFindIds
  :: ( forall r m. ( Verify.ResolvableParamsOf r ~ (), MonadIO m, Typeable r ) => r -> MockT m r )
  -> Spec
specVerificationFailureFindIds findIdsBuilder = describe "verification failures - findIds" do
  let missingCall name err =
        let needle = "function `" <> name <> "` was not applied the expected number of times."
         in needle `isInfixOf` displayException (err :: ErrorCall)

  it "fails when _findIds is defined but findIds is never called" do
    (runMockT @IO do
      _ <- findIdsBuilder ([1 :: Int, 2] :: [Int])
        `expects` do
          called once
      -- findIds is never called
      pure ()) `shouldThrow` (missingCall "_findIds")


specVerificationFailureFindById
  :: ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                        , MonadIO m
                        , Typeable (Int -> String)
                        , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
                        ) => params -> MockT m (Int -> String) )
  -> Spec
specVerificationFailureFindById findByBuilder = describe "verification failures - findById" do
  let missingCall name err =
        let needle = "function `" <> name <> "` was not applied the expected number of times."
         in needle `isInfixOf` displayException (err :: ErrorCall)

  it "fails when _findById is defined but findById is never called" do
    (runMockT @IO do
      let cases = do
            onCase $ (1 :: Int) |> "id1"
            onCase $ (2 :: Int) |> "id2"
            onCase $ (3 :: Int) |> "id3"
      _ <- findByBuilder cases `expects` do
        called once
      -- findById is never called
      pure ()) `shouldThrow` (missingCall "_findById")
