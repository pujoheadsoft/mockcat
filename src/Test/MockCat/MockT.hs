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
{-# LANGUAGE UndecidableInstances #-}
module Test.MockCat.MockT (
  MockT(..), Definition(..), Verification(..),
  runMockT,
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
import Control.Monad.Reader (ReaderT(..), runReaderT, asks, MonadReader(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Data (Proxy, Typeable)
import Data.IORef (newIORef, IORef)
import Data.Dynamic (Dynamic)
import UnliftIO (MonadUnliftIO(..))
import Test.MockCat.Internal.Types (InvocationRecorder, WithMockContext(..), MonadWithMockContext(..))
import Test.MockCat.Verify (ResolvableParamsOf)
import Control.Concurrent.MVar (MVar)
import qualified Data.Map.Strict as Map
import qualified Test.MockCat.Internal.Registry.Core as Registry

{- | MockT is a thin wrapper over @ReaderT MockTEnv@ providing
     mock/stub registration and post-run verification.

Concurrency safety (summary):
  * Within a single 'runMockT' invocation, concurrent calls of stub
    functions are recorded without lost or double counts. This is achieved via
    STM updates ('modifyTVar'').
  * The /moment/ a call is recorded is when the stub's return value is evaluated;
    if you only create a call but never force the result, it will not
    appear in the verification log.
  * Order-sensitive checks reflect evaluation order, not necessarily wall-clock
    start order between threads.
  * Perform verification (e.g. 'shouldBeCalled', `expects`) after all
    parallel work has completed; running it mid-flight may observe fewer calls
    simply because some results are still lazy.
  * Each 'runMockT' call uses a fresh TVar store; mocks are not shared across
    separate 'runMockT' boundaries.
-}
data MockTEnv = MockTEnv
  { envDefinitions :: TVar [Definition]
  , envWithMockContext :: WithMockContext
  , envNameForwarders :: IORef (Map.Map String (Either Dynamic (MVar Dynamic)))
  }

newtype MockT m a = MockT { unMockT :: ReaderT MockTEnv m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class Monad m => MonadMockDefs m where
  addDefinition :: Definition -> m ()
  getDefinitions :: m [Definition]

instance MonadUnliftIO m => MonadUnliftIO (MockT m) where
  withRunInIO inner = MockT $ ReaderT $ \env ->
    withRunInIO $ \run -> inner (\(MockT r) -> run (runReaderT r env))

instance {-# OVERLAPPING #-} Monad m => MonadWithMockContext (MockT m) where
  askWithMockContext = MockT $ asks envWithMockContext

instance {-# OVERLAPPABLE #-} MonadReader r m => MonadReader r (MockT m) where
  ask = lift ask
  local f (MockT (ReaderT m)) = MockT $ ReaderT $ \env -> local f (m env)
  reader = lift . reader

instance {-# OVERLAPPABLE #-} MonadError e m => MonadError e (MockT m) where
  throwError = lift . throwError
  catchError (MockT m) h = MockT $ catchError m (unMockT . h)

instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (MockT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance {-# OVERLAPPABLE #-} MonadWriter w m => MonadWriter w (MockT m) where
  writer = lift . writer
  tell = lift . tell
  listen (MockT m) = MockT $ listen m
  pass (MockT m) = MockT $ pass m


data Definition =
  forall f params sym.
  ( KnownSymbol sym
  , Typeable f
  , Typeable params
  , params ~ ResolvableParamsOf f
  , Typeable (InvocationRecorder params)
  ) =>
  Definition {
  symbol :: Proxy sym,
  mockFunction :: f,  -- Restore to f for type safety
  verification :: Verification f
}

data Verification f
  = NoVerification
  | Verification (f -> IO ())

{- | Run MockT monad.
  After run, verification is performed to see if the stub function has been called.

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
        _readFile $ "input.txt" ~> pack "content"
        _writeFile $ "output.text" ~> pack "content" ~> ()
        operationProgram "input.txt" "output.text"

      result `shouldBe` ()
  @

-}
runMockT :: MonadIO m => MockT m a -> m a
runMockT (MockT r) = do
  liftIO Registry.resetMockHistory
  defsVar <- liftIO $ newTVarIO []
  expectsVar <- liftIO $ newTVarIO []
  fwdRef <- liftIO $ newIORef Map.empty
  let env =
        MockTEnv
          { envDefinitions = defsVar
          , envWithMockContext = WithMockContext expectsVar
          , envNameForwarders = fwdRef
          }
  -- Run user code with a per-run overlay registry active so registry writes/read
  -- during this MockT invocation are isolated to this run.
  overlay <- liftIO Registry.createOverlay
  liftIO $ Registry.installOverlay overlay
  a <- runReaderT r env
  actions <- liftIO $ readTVarIO expectsVar
  liftIO $ sequence_ actions
  liftIO Registry.clearOverlay
  pure a

instance MonadIO m => MonadMockDefs (MockT m) where
  addDefinition d = MockT $ ReaderT $ \env -> liftIO $ do
    atomically $ modifyTVar' (envDefinitions env) $ \xs ->
      case d of
        Definition sym _ _ ->
          let name = symbolVal sym
              exists = any (\(Definition sym' _ _) -> symbolVal sym' == name) xs
           in if exists then xs else xs ++ [d]
    pure ()
  getDefinitions = MockT $ ReaderT $ \env -> liftIO $ readTVarIO (envDefinitions env)

instance MonadIO m => MonadMockDefs (ReaderT MockTEnv m) where
  addDefinition d = ReaderT $ \env -> liftIO $ do
    atomically $ modifyTVar' (envDefinitions env) $ \xs ->
      case d of
        Definition sym _ _ ->
          let name = symbolVal sym
              exists = any (\(Definition sym' _ _) -> symbolVal sym' == name) xs
           in if exists then xs else xs ++ [d]
  getDefinitions = ReaderT $ \env -> liftIO $ readTVarIO (envDefinitions env)
  -- Note: ReaderT variant intentionally returns raw store (used by internal runners).