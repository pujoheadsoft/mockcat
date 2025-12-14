{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , specFinderParallel
  , specFinderEdgeCases
  , specFinderEmptyIds
  , specFinderNamedError
  , specFinderMixedFallback
  , specFinderNoImplicit
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
import Data.List (isInfixOf, find)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Concurrent.Async (async, wait)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal, KnownSymbol)
import Unsafe.Coerce (unsafeCoerce)

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


specFinderParallel
  :: ( Finder Int String (MockT IO)
     , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
     ) =>
  ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                     , MonadIO m
                     , Typeable (Int -> String)
                     ) => params -> MockT m (Int -> String) )
  -> Spec
specFinderParallel findByBuilder = describe "Finder concurrency" do
  it "Concurrent execution correctly calls and collects results from mocks (async)" do
    result <- runMockT do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "id1"
        onCase $ (2 :: Int) |> "id2"
      withRunInIO $ \runInIO -> do
        a1 <- async $ runInIO (findById 1)
        a2 <- async $ runInIO (findById 2)
        r1 <- wait a1
        r2 <- wait a2
        pure [r1, r2]
    result `shouldBe` ["id1", "id2"]


specFinderEdgeCases
  :: Finder Int String (MockT IO)
  => ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                        , MonadIO m
                        , Typeable (Int -> String)
                        , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
                        ) => params -> MockT m (Int -> String) )
  -> Spec
specFinderEdgeCases findByBuilder = describe "Finder edge cases" do
  it "partial mock that doesn't cover all ids causes argument error" do
    let argError :: Selector ErrorCall
        argError err = "was not applied to the expected arguments" `isInfixOf` displayException (err :: ErrorCall)
    (runMockT @IO do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "id1"
      -- calling findValue will hit findById for id 2 which is not covered by mock
      findValue @Int @String) `shouldThrow` argError

  it "duplicate cases prefer first" do
    result <- runMockT $ do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "first"
        onCase $ (1 :: Int) |> "second"
      findById 1
    result `shouldBe` "first"


specFinderEmptyIds
  :: Finder Int String (MockT IO)
  => ( forall r m. ( Verify.ResolvableParamsOf r ~ (), MonadIO m, Typeable r ) => r -> MockT m r )
  -> Spec
specFinderEmptyIds findIdsBuilder = describe "Finder empty ids" do
  it "findValue returns empty when findIds returns empty list" do
    result <- runMockT $ do
      _ <- findIdsBuilder ([] :: [Int])
      findValue @Int @String
    result `shouldBe` []


specFinderNoImplicit
  :: ( forall params. ( MockBuilder params (Int -> IO String) (Param Int)
                        , Typeable (Int -> IO String)
                        , Verify.ResolvableParamsOf (Int -> IO String) ~ Param Int
                        ) => params -> MockT IO (Int -> IO String) )
  -> Spec
specFinderNoImplicit findByBuilder = describe "Finder no-implicit-monadic-return (TH implicitMonadicReturn=False)" do
  it "partial findById with explicit monadic returns" do
    result <- runMockT $ do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> pure @IO "id1"
        onCase $ (2 :: Int) |> pure @IO "id2"
        onCase $ (3 :: Int) |> pure @IO "id3"
      -- manually call the NI variant (avoid needing FinderNoImplicit MockT instance)
      defs <- getDefinitions
      case findParam (Proxy :: Proxy "_findByIdNI") defs of
        Just mockFn -> do
          ids <- lift (findIds :: IO [Int])
          results <- forM ids (lift . mockFn)
          pure results
        Nothing -> do
          ids <- lift (findIds :: IO [Int])
          results <- forM ids (lift . findById)
          pure results
    result `shouldBe` ["id1", "id2", "id3"]

-- Helper to find a definition by symbol and coerce the function
findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFunction _) -> unsafeCoerce mockFunction) definition

 
specFinderNamedError
  :: Finder Int String (MockT IO)
  => ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                        , MonadIO m
                        , Typeable (Int -> String)
                        , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
                        ) => params -> MockT m (Int -> String) )
  -> Spec
specFinderNamedError findByBuilder = describe "Finder named error messages" do
  it "error message contains mock name when unexpected arg is used" do
    let nameMsg = "function `_findById` was not applied to the expected arguments"
    (runMockT @IO do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "id1"
      -- call with unexpected arg to trigger message
      findById (2 :: Int)) `shouldThrow` (\(err :: ErrorCall) -> nameMsg `isInfixOf` displayException err)


specFinderMixedFallback
  :: Finder Int String (MockT IO)
  => ( forall params m. ( MockBuilder params (Int -> String) (Param Int)
                        , MonadIO m
                        , Typeable (Int -> String)
                        , Verify.ResolvableParamsOf (Int -> String) ~ Param Int
                        ) => params -> MockT m (Int -> String) )
  -> Spec
specFinderMixedFallback findByBuilder = describe "Finder mixed fallback" do
  it "when a per-id mock exists, unexpected args produce an argument error (no fallback)" do
    let argError :: Selector ErrorCall
        argError err = "was not applied to the expected arguments" `isInfixOf` displayException (err :: ErrorCall)
    (runMockT @IO do
      _ <- findByBuilder $ do
        onCase $ (1 :: Int) |> "id1"
      -- calling findValue will hit findById for id 2 which is not covered by mock,
      -- and because a mock function exists for _findById, it will error rather than fallback.
      findValue @Int @String) `shouldThrow` argError
