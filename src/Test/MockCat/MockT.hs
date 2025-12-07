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
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import GHC.TypeLits (KnownSymbol)
import Data.Data (Proxy, Typeable)
import UnliftIO (MonadUnliftIO(..))
import Test.MockCat.Internal.Types (Verifier)
import Test.MockCat.Verify (ResolvableParamsOf)
import Test.MockCat.WithMock (WithMockContext(..), MonadWithMockContext(..))

{- | MockT is a thin wrapper over @ReaderT MockTEnv@ providing
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
  * Perform verification (e.g. 'shouldApplyTimes', `expects`) after all
    parallel work has completed; running it mid-flight may observe fewer calls
    simply because some results are still lazy.
  * Each 'runMockT' call uses a fresh TVar store; mocks are not shared across
    separate 'runMockT' boundaries.
-}
data MockTEnv = MockTEnv
  { envDefinitions :: TVar [Definition]
  , envWithMockContext :: WithMockContext
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
  verification :: Verification f
}

data Verification f
  = NoVerification
  | Verification (f -> IO ())

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
  defsVar <- liftIO $ newTVarIO []
  expectsVar <- liftIO $ newTVarIO []
  let env =
        MockTEnv
          { envDefinitions = defsVar
          , envWithMockContext = WithMockContext expectsVar
          }
  a <- runReaderT r env
  actions <- liftIO $ readTVarIO expectsVar
  liftIO $ sequence_ actions
  pure a

instance MonadIO m => MonadMockDefs (MockT m) where
  addDefinition d = MockT $ ReaderT $ \env ->
    liftIO $ atomically $ modifyTVar' (envDefinitions env) (++ [d])
  getDefinitions = MockT $ ReaderT $ \env -> liftIO $ readTVarIO (envDefinitions env)

instance MonadIO m => MonadMockDefs (ReaderT MockTEnv m) where
  addDefinition d = ReaderT $ \env ->
    liftIO $ atomically $ modifyTVar' (envDefinitions env) (++ [d])
  getDefinitions = ReaderT $ \env -> liftIO $ readTVarIO (envDefinitions env)