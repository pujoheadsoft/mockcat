{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Test.MockCat.MockT (MockT(..), Definition(..), runMockT, applyTimesIs, neverApply) where
import Control.Monad.State
    ( StateT(..), MonadIO(..), MonadTrans(..), modify, execStateT, runStateT )
import GHC.TypeLits (KnownSymbol)
import Data.Data (Proxy)
import Test.MockCat.Mock (Mock, shouldApplyTimesToAnything)
import Data.Foldable (for_)
import UnliftIO (MonadUnliftIO(..))
import Data.Functor ((<&>))

newtype MockT m a = MockT { st :: StateT [Definition] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadUnliftIO m => MonadUnliftIO (MockT m) where
  withRunInIO inner = MockT $ StateT $ \s -> do
    a <- withRunInIO $ \runInIO ->
      inner (\(MockT m) -> runInIO (runStateT m s) <&> fst)
    pure (a, s)

data Definition = forall f p sym. KnownSymbol sym => Definition {
  symbol :: Proxy sym,
  mock :: Mock f p,
  verify :: Mock f p -> IO ()
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
runMockT (MockT s) = do
  r <- runStateT s []
  let
    !a = fst r
    defs = snd r
  for_ defs (\(Definition _ m v) -> liftIO $ v m)
  pure a

{- | Specify how many times a stub function should be applied.

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
        _writeFile ("output.text" |> pack "content" |> ()) `applyTimesIs` 0
        operationProgram "input.txt" "output.text"

      result `shouldBe` ()

  @

-}
applyTimesIs :: Monad m => MockT m () -> Int -> MockT m ()
applyTimesIs (MockT st) a = MockT do
  defs <- lift $ execStateT st []
  let newDefs = map (\(Definition s m _) -> Definition s m (`shouldApplyTimesToAnything` a)) defs
  modify (++ newDefs)

neverApply :: Monad m => MockT m () -> MockT m ()
neverApply (MockT st) = MockT do
  defs <- lift $ execStateT st []
  let newDefs = map (\(Definition s m _) -> Definition s m (`shouldApplyTimesToAnything` 0)) defs
  modify (++ newDefs)