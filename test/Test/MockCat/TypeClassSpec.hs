{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (ask, MonadReader (local))
import Control.Monad.Trans.Class (lift)
import qualified Test.MockCat.Verify as Verify
import Test.MockCat.Internal.Types (Verifier (..))
import Test.MockCat.Internal.Message (mockNameLabel)

verifyResolvedAny :: Verify.ResolvedMock params -> IO ()
verifyResolvedAny (Verify.ResolvedMock name (Verifier ref)) = do
  appliedParamsList <- Verify.readAppliedParamsList ref
  when (null appliedParamsList) $
    error $ "It has never been applied function" <> mockNameLabel name

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

class Monad m => ApiOperation m where
  post :: Text -> m ()

program ::
  (MonadReader String m, FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  e <- ask
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  post $ modifiedContent <> pack ("+" <> e)

instance MonadIO m => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `readFile`.") $ findParam (Proxy :: Proxy "readFile") defs
      !result = mockFn path
    pure result

  writeFile path content = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `writeFile`.") $ findParam (Proxy :: Proxy "writeFile") defs
      !result = mockFn path content
    pure result

instance MonadIO m => ApiOperation (MockT m) where
  post content = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `post`.") $ findParam (Proxy :: Proxy "post") defs
      !result = mockFn content
    pure result

instance MonadIO m => MonadReader String (MockT m) where
  ask = MockT do
    defs <- getDefinitions
    let
      mock = fromMaybe (error "no answer found stub function `ask`.") $ findParam (Proxy :: Proxy "ask") defs
      !result = mock
    pure result
  local = undefined

_ask ::
  ( Typeable env
  , Verify.ResolvableParamsOf env ~ ()
  , MonadIO m
  ) =>
  env -> MockT m ()
_ask p = MockT $ do
  mockInstance <- liftIO $ createNamedConstantStubFn "ask" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "ask") mockInstance verifyStub)

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

_post ::
  ( MockBuilder params (Text -> ()) (Param Text)
  , MonadIO m
  ) =>
  params ->
  MockT m ()
_post p = MockT $ do
  mockFn <- liftIO $ createNamedStubFn "post" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockFn
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = verifyResolvedAny resolved
  addDefinition (Definition (Proxy :: Proxy "post") mockFn verifyStub)

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ f _) -> unsafeCoerce f) definition

class Monad m => TestClass m where
  echo :: String -> m ()
  getBy :: String -> m Int

echoProgram :: MonadIO m => TestClass m => String -> m ()
echoProgram s = do
  v <- getBy s
  liftIO $ print v
  echo $ show v

instance MonadIO m => TestClass (MockT m) where
  getBy a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_getBy`.") $ findParam (Proxy :: Proxy "_getBy") defs
      !result = mockFn a
    lift result

  echo a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_echo`.") $ findParam (Proxy :: Proxy "_echo") defs
      !result = mockFn a
    lift result

_getBy ::
  ( MockBuilder params (String -> m Int) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m Int) ~ Param String
  ) =>
  params ->
  MockT m ()
_getBy p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "_getBy" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_getBy") mockInstance verifyStub)

_echo ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_echo p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "_echo" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_echo") mockInstance verifyStub)


class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

echo2 :: Teletype m => m ()
echo2 = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo2

instance MonadIO m => Teletype (MockT m) where
  readTTY = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_readTTY`.") $ findParam (Proxy :: Proxy "_readTTY") defs
      !result = mockFn
    lift result

  writeTTY a = MockT do
    defs <- getDefinitions
    let
      mockFn = fromMaybe (error "no answer found stub function `_writeTTY`.") $ findParam (Proxy :: Proxy "_writeTTY") defs
      !result = mockFn a
    lift result

_readTTY ::
  ( MockBuilder params (m String) ()
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (m String) ~ ()
  ) =>
  params ->
  MockT m ()
_readTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "_readTTY" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_readTTY") mockInstance verifyStub)

_writeTTY ::
  ( MockBuilder params (String -> m ()) (Param String)
  , MonadIO m
  , Typeable m
  , Verify.ResolvableParamsOf (String -> m ()) ~ Param String
  ) =>
  params ->
  MockT m ()
_writeTTY p = MockT $ do
  mockInstance <- liftIO $ createNamedStubFn "_writeTTY" p
  resolved <- liftIO $ do
    result <- Verify.resolveForVerification mockInstance
    case result of
      Just (maybeName, verifier) -> pure $ Verify.ResolvedMock maybeName verifier
      Nothing -> Verify.verificationFailure
  let verifyStub _ = Verify.shouldApplyToAnythingResolved resolved
  addDefinition (Definition (Proxy :: Proxy "_writeTTY") mockInstance verifyStub)

spec :: Spec
spec = do
  it "echo" do
    result <- runMockT do
      _readTTY $ casesIO [
        "a",
        ""
        ]
      _writeTTY $ "a" |> pure @IO ()
      echo2
    result `shouldBe` ()

  it "Read, edit, and output files" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _ask "environment"
      _readFile ("input.txt" |> pack "content")
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()
      _post $ pack "modifiedContent+environment" |> ()
      program "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()

  it "return monadic value test" do
    result <- runMockT do
      _getBy $ "s" |> pure @IO (10 :: Int)
      _echo $ "10" |> pure @IO ()
      echoProgram "s"

    result `shouldBe` ()