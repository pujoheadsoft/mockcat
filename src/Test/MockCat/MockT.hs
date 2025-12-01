{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Test.MockCat.MockT (
  MockT(..), Definition(..),
  runMockT,
  expectApplyTimes,
  applyTimesIs,
  neverApply,
  MonadMockDefs(..)
  ) where
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (ReaderT(..), runReaderT)
import GHC.TypeLits (KnownSymbol)
import Data.Data (Proxy, Typeable)
import Data.Foldable (for_)
import UnliftIO (MonadUnliftIO(..))
import Test.MockCat.Internal.Types (Verifier, CountVerifyMethod(..))
import Test.MockCat.Verify (ResolvableParamsOf, resolveForVerification, verificationFailure, verifyAppliedCount)

{- | MockT is a thin wrapper over @ReaderT (TVar [Definition])@ providing
     mock/stub registration and post-run verification.

Concurrency safety (summary):
  * Within a single 'runMockT' invocation, concurrent applications of stub
    functions are recorded without lost or double counts. This is achieved via
    STM updates ('modifyTVar'').
  * The /moment/ a call is recorded is when the stub's return value is evaluated;
    if you only create an application but never force the result, it will not
    appear in the verification log.
  * Order-sensitive checks reflect evaluation order, not necessarily wall-clock
    start order between threads.
  * Perform verification (e.g. 'shouldApplyTimes', 'expectApplyTimes') after all
    parallel work has completed; running it mid-flight may observe fewer calls
    simply because some results are still lazy.
  * Each 'runMockT' call uses a fresh TVar store; mocks are not shared across
    separate 'runMockT' boundaries.
-}
newtype MockT m a = MockT { unMockT :: ReaderT (TVar [Definition]) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class Monad m => MonadMockDefs m where
  addDefinition :: Definition -> m ()
  getDefinitions :: m [Definition]

instance MonadUnliftIO m => MonadUnliftIO (MockT m) where
  withRunInIO inner = MockT $ ReaderT $ \ref ->
    withRunInIO $ \run -> inner (\(MockT r) -> run (runReaderT r ref))


data Definition =
  forall f params sym.
  ( KnownSymbol sym
  , Typeable f
  , Typeable params
  , params ~ ResolvableParamsOf f
  , Typeable (Verifier params)
  ) =>
  Definition {
  symbol :: Proxy sym,
  mockFunction :: f,
  verify :: f -> IO ()
}

{- | Run MockT monad.
  After run, verification is performed to see if the stub function has been applied.

  @
  import Test.Hspec
  import Test.MockCat
  ...

  class (Monad m) => FileOperation m where
    writeFile :: FilePath -\> Text -\> m ()
    readFile :: FilePath -\> m Text

  operationProgram ::
    FileOperation m =\>
    FilePath -\>
    FilePath -\>
    m ()
  operationProgram inputPath outputPath = do
    content \<- readFile inputPath
    writeFile outputPath content

  makeMock [t|FileOperation|]

  spec :: Spec
  spec = do
    it "test runMockT" do
      result \<- runMockT do
        _readFile $ "input.txt" |\> pack "content"
        _writeFile $ "output.text" |\> pack "content" |\> ()
        operationProgram "input.txt" "output.text"

      result `shouldBe` ()
  @

-}
runMockT :: MonadIO m => MockT m a -> m a
runMockT (MockT r) = do
  ref <- liftIO $ newTVarIO []
  a <- runReaderT r ref
  defs <- liftIO $ readTVarIO ref
  for_ defs (\(Definition _ mockFunction verify) -> liftIO $ verify mockFunction)
  pure a

{- | (Preferred: 'expectApplyTimes'; legacy: 'applyTimesIs')
  Specify how many times a stub function (or group of stub definitions) must
  be applied (to /any/ arguments). The function patches the verification
  predicate for the provided stub definitions so that, after 'runMockT'
  completes, the total number of evaluated applications is checked.

  Concurrency & laziness notes:
    * Counting is thread-safe: each evaluated application contributes exactly 1.
    * An application is only counted once its return value is evaluated; ensure
      your test forces (e.g. via @shouldBe@ or sequencing) all stub results
      before relying on the count.
    * Invoke 'expectApplyTimes' (or legacy 'applyTimesIs') inside the 'runMockT' block during setup; do not
      call it after the block ends.

  @
  import Test.Hspec
  import Test.MockCat
  ...

  class (Monad m) => FileOperation m where
    writeFile :: FilePath -\> Text -\> m ()
    readFile :: FilePath -\> m Text

  operationProgram ::
    FileOperation m =>
    FilePath ->
    FilePath ->
    m ()
  operationProgram inputPath outputPath = do
    content <- readFile inputPath
    when (content == pack "ng") $ writeFile outputPath content

  makeMock [t|FileOperation|]

  spec :: Spec
  spec = do
    it "test runMockT" do
      result <- runMockT do
        _readFile ("input.txt" |> pack "content")
        _writeFile ("output.text" |> pack "content" |> ()) `expectApplyTimes` 0
        operationProgram "input.txt" "output.text"

      result `shouldBe` ()

  @

-}
expectApplyTimes :: MonadIO m => MockT m () -> Int -> MockT m ()
expectApplyTimes = applyTimesIs

applyTimesIs :: MonadIO m => MockT m () -> Int -> MockT m ()
applyTimesIs (MockT inner) a = MockT $ ReaderT $ \ref -> do
  tmp <- liftIO $ newTVarIO []
  _ <- runReaderT inner tmp
  defs <- liftIO $ readTVarIO tmp
  let patched = map (\(Definition s fn _) -> Definition s fn (`verifyApplyCount` (Equal a))) defs
  liftIO $ atomically $ modifyTVar' ref (++ patched)
  pure ()

neverApply :: MonadIO m => MockT m () -> MockT m ()
neverApply (MockT inner) = MockT $ ReaderT $ \ref -> do
  tmp <- liftIO $ newTVarIO []
  _ <- runReaderT inner tmp
  defs <- liftIO $ readTVarIO tmp
  let patched = map (\(Definition s m _) -> Definition s m (`verifyApplyCount` (Equal 0))) defs
  liftIO $ atomically $ modifyTVar' ref (++ patched)
  pure ()

-- | Alias for 'neverApply' providing naming symmetry with 'expectApplyTimes'.
-- note: `expectNever` removed; use `neverApply`

instance MonadIO m => MonadMockDefs (MockT m) where
  addDefinition d = MockT $ ReaderT $ \ref ->
    liftIO $ atomically $ modifyTVar' ref (++ [d])
  getDefinitions = MockT $ ReaderT $ \ref -> liftIO $ readTVarIO ref

instance MonadIO m => MonadMockDefs (ReaderT (TVar [Definition]) m) where
  addDefinition d = ReaderT $ \ref ->
    liftIO $ atomically $ modifyTVar' ref (++ [d])
  getDefinitions = ReaderT $ \ref -> liftIO $ readTVarIO ref

verifyApplyCount ::
  forall f params.
  ( Typeable params
  , params ~ ResolvableParamsOf f
  , Typeable (Verifier params)
  ) =>
  f ->
  CountVerifyMethod ->
  IO ()
verifyApplyCount stub method = do
  result <- resolveForVerification stub
  case result of
    Just (maybeName, verifier) ->
      verifyAppliedCount maybeName verifier method
    Nothing ->
      verificationFailure