{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
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
  , PartialMockDeps(..)
  ) where



import Test.MockCat.Internal.Types (InvocationRecorder)
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
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (forM)
import Control.Concurrent.Async (async, wait)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (symbolVal, KnownSymbol)
import Unsafe.Coerce (unsafeCoerce)


-- Dependency record to group builders
data PartialMockDeps = PartialMockDeps
  { _getInput    :: forall params m. (MockDispatch (IsMockSpec params) params (MockT m) String, MonadIO m, Typeable (Verify.ResolvableParamsOf String), Typeable params, Show params, Eq params) => params -> MockT m String
  , _getBy       :: forall params. (MockDispatch (IsMockSpec params) params (MockT IO) (String -> IO Int)) => params -> MockT IO (String -> IO Int)
  , _echo        :: forall params. (MockDispatch (IsMockSpec params) params (MockT IO) (String -> IO ())) => params -> MockT IO (String -> IO ())
  , _writeFile   :: forall params m. (MockDispatch (IsMockSpec params) params (MockT m) (FilePath -> Text -> ()), MonadIO m) => params -> MockT m (FilePath -> Text -> ())
  , _findIds     :: forall p a m. (MockDispatch (IsMockSpec p) p (MockT m) [a], MonadIO m, Typeable p, Show p, Eq p, Typeable (InvocationRecorder (Verify.ResolvableParamsOf [a])), Typeable (Verify.ResolvableParamsOf [a]), Typeable [a], Typeable a) => p -> MockT m [a]
  , _findById    :: forall params m. (MockDispatch (IsMockSpec params) params (MockT m) (Int -> String), MonadIO m) => params -> MockT m (Int -> String)
  , _findByIdNI  :: forall params. (MockDispatch (IsMockSpec params) params (MockT IO) (Int -> IO String)) => params -> MockT IO (Int -> IO String)
  }

-- Main Entry Point
spec ::
  ( UserInputGetter (MockT IO)
  , ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  , Finder Int String (MockT IO)
  ) =>
  PartialMockDeps ->
  MockT IO () ->
  Spec
spec deps programAction = do
  specBasicPartialMocking deps programAction
  specExplicitMonadicReturns deps
  specFinderBehavior deps
  specVerificationFailures deps

-- Group: Basic Partial Mocking (UserInput, FileOperation)
specBasicPartialMocking ::
  ( UserInputGetter (MockT IO)
  ) =>
  PartialMockDeps ->
  MockT IO () ->
  Spec
specBasicPartialMocking (PartialMockDeps { _getInput, _writeFile }) programAction = describe "Basic Partial Mocking" do
  describe "UserInputGetter" do
    it "Get user input (has input)" do
      result <- runMockT do
        _ <- _getInput ("value" :: String)
        i <- getInput
        toUserInput i
      result `shouldBe` Just (UserInput "value")

    it "Get user input (no input)" do
      result <- runMockT do
        _ <- _getInput ("" :: String)
        i <- getInput
        toUserInput i
      result `shouldBe` Nothing

  describe "FileOperation" do
    it "IO" do
      result <- runMockT do
        _ <- _writeFile (("output.text" :: FilePath) ~> pack ("IO content" :: String) ~> ())
        pure ()
      result `shouldBe` ()

    it "MaybeT" do
      result <- runMaybeT do
        runMockT do
          _ <- _writeFile (("output.text" :: FilePath) ~> pack ("MaybeT content" :: String) ~> ())
          pure ()
      result `shouldBe` Just ()

    it "ReaderT" do
      result <- flip runReaderT "foo" do
        runMockT do
          _ <- _writeFile (("output.text" :: FilePath) ~> pack ("ReaderT content foo" :: String) ~> ())
          pure ()
      result `shouldBe` ()

  describe "Handwritten Partial Mock Test" do
    it "IO" do
      result <- runMockT do
        _ <- _writeFile (("output.text" :: FilePath) ~> pack ("IO content" :: String) ~> ())
        programAction
      result `shouldBe` ()

    it "MaybeT" do
      result <- runMaybeT do
        runMockT do
          _ <- _writeFile (("output.text" :: FilePath) ~> pack ("MaybeT content" :: String) ~> ())
          pure ()
      result `shouldBe` Just ()


-- Group: Explicit Monadic Returns
specExplicitMonadicReturns ::
  ( ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  ) =>
  PartialMockDeps ->
  Spec
specExplicitMonadicReturns (PartialMockDeps { _echo, _getBy }) = describe "Explicit Monadic Returns" do
  it "Return monadic value test" do
    result <- runMockT $ do
      _ <- _echo $ ("3" :: String) ~> pure @IO ()
      v <- getByExplicitPartial "abc"
      echoExplicitPartial (show v)
    result `shouldBe` ()

  it "Override getBy via stub" do
    result <- runMockT do
      _ <- _getBy $ ("abc" :: String) ~> pure @IO (123 :: Int)
      getByExplicitPartial "abc"
    result `shouldBe` 123


-- Group: Finder Behavior (Multi-param type class)
specFinderBehavior ::
  ( Finder Int String (MockT IO)
  ) =>
  PartialMockDeps ->
  Spec
specFinderBehavior (PartialMockDeps { _findIds, _findById, _findByIdNI }) = describe "Finder Behavior (MultiParamTypeClass)" do
  it "all real function" do
    values <- runMockT findValue
    values `shouldBe` ["{id: 1}", "{id: 2}", "{id: 3}"]

  it "partial findIds" do
    values <- runMockT $ do
      _ <- _findIds ([1 :: Int, 2] :: [Int])
      findValue @Int @String
    values `shouldBe` ["{id: 1}", "{id: 2}"]

  it "partial findById" do
    values <- runMockT $ do
      _ <- _findById $ do
        onCase $ (1 :: Int) ~> "id1"
        onCase $ (2 :: Int) ~> "id2"
        onCase $ (3 :: Int) ~> "id3"
      findValue @Int @String
    values `shouldBe` ["id1", "id2", "id3"]

  it "Concurrent execution correctly calls and collects results from mocks (async)" do
    result <- runMockT do
      _ <- _findById $ do
        onCase $ (1 :: Int) ~> "id1"
        onCase $ (2 :: Int) ~> "id2"
      withRunInIO $ \runInIO -> do
        a1 <- async $ runInIO (findById 1)
        a2 <- async $ runInIO (findById 2)
        r1 <- wait a1
        r2 <- wait a2
        pure [r1, r2]
    result `shouldBe` ["id1", "id2"]

  describe "Edge cases" do
    it "partial mock that doesn't cover all ids causes argument error" do
      let argError :: Selector ErrorCall
          argError err = "was not called with the expected arguments" `isInfixOf` displayException (err :: ErrorCall)
      (runMockT @IO do
        _ <- _findById $ do
          onCase $ (1 :: Int) ~> "id1"
        -- calling findValue will hit findById for id 2 which is not covered by mock
        findValue @Int @String) `shouldThrow` argError

    it "duplicate cases prefer first" do
      result <- runMockT $ do
        _ <- _findById $ do
          onCase $ (1 :: Int) ~> "first"
          onCase $ (1 :: Int) ~> "second"
        findById 1
      result `shouldBe` "first"

    it "findValue returns empty when findIds returns empty list" do
      result <- runMockT $ do
        _ <- _findIds ([] :: [Int])
        findValue @Int @String
      result `shouldBe` []

  describe "Named error messages" do
    it "error message contains mock name when unexpected arg is used" do
      let nameMsg = "function `_findById` was not called with the expected arguments"
      (runMockT @IO do
        _ <- _findById $ do
          onCase $ (1 :: Int) ~> "id1"
        -- call with unexpected arg to trigger message
        findById (2 :: Int)) `shouldThrow` (\(err :: ErrorCall) -> nameMsg `isInfixOf` displayException err)

  describe "Mixed fallback" do
    it "when a per-id mock exists, unexpected args produce an argument error (no fallback)" do
      let argError :: Selector ErrorCall
          argError err = "was not called with the expected arguments" `isInfixOf` displayException (err :: ErrorCall)
      (runMockT @IO do
        _ <- _findById $ do
          onCase $ (1 :: Int) ~> "id1"
        -- calling findValue will hit findById for id 2 which is not covered by mock,
        -- and because a mock function exists for _findById, it will error rather than fallback.
        findValue @Int @String) `shouldThrow` argError

  describe "Implicit monadic return options" do
    it "partial findById with explicit monadic returns (implicitMonadicReturn=False)" do
      result <- runMockT $ do
        _ <- _findByIdNI $ do
          onCase $ (1 :: Int) ~> pure @IO "id1"
          onCase $ (2 :: Int) ~> pure @IO "id2"
          onCase $ (3 :: Int) ~> pure @IO "id3"
        -- manually call the NI variant (avoid needing FinderNoImplicit MockT instance)
        defs <- getDefinitions
        case findParam (Proxy :: Proxy "_findByIdNI") defs of
          Just mockFn -> do
            ids <- lift (findIds :: IO [Int])
            forM ids (lift . mockFn)
          Nothing -> do
            ids <- lift (findIds :: IO [Int])
            forM ids (lift . findById)
      result `shouldBe` ["id1", "id2", "id3"]


-- Group: Verification Failures
specVerificationFailures ::
  PartialMockDeps ->
  Spec
specVerificationFailures (PartialMockDeps { _findIds, _findById }) = describe "Verification Failures" do
  let missingCall name err =
        let needle = "function `" <> name <> "` was not called the expected number of times."
         in needle `isInfixOf` displayException (err :: ErrorCall)

  it "fails when _findIds is defined but findIds is never called" do
    (runMockT @IO do
      _ <- _findIds $ Head :> param ([1 :: Int, 2] :: [Int])
        `expects` do
          called once
      -- findIds is never called
      pure ()) `shouldThrow` missingCall "_findIds"

  it "fails when _findById is defined but findById is never called" do
    (runMockT @IO do
      let casesDef = do
            onCase $ (1 :: Int) ~> "id1"
            onCase $ (2 :: Int) ~> "id2"
            onCase $ (3 :: Int) ~> "id3"
      _ <- _findById $ casesDef `expects` do
        called once
      -- findById is never called
      pure ()) `shouldThrow` missingCall "_findById"


-- Helper to find a definition by symbol and coerce the function
findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mockFunction _) -> unsafeCoerce mockFunction) definition
