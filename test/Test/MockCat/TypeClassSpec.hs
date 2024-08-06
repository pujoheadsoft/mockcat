{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (createMock, createStubFn, stubFn, (|>), shouldApplyTo, Param, (:>))
import Prelude hiding (readFile, writeFile)
import GHC.Generics (Generic)
import GHC.Base (Symbol, liftM)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Reader (ReaderT (..), ask, runReader, MonadReader)
import Control.Monad.State (StateT (runStateT), modify, modify', put, get, lift, state)
import Control.Monad.Trans
import Control.Monad.Reader.Class (MonadReader(local, reader))
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)

class (Monad m) => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

class (Monad m) => ApiOperation m where
  post :: Text -> m ()

program ::
  (FileOperation m, ApiOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent
  --post modifiedContent

--data MockT m a = MockT { run :: m a, definitions :: [Definition] } deriving (Generic)
newtype MockT m a = MockT { st :: StateT [Definition] m a }

-- newtype MockT m a where
--   MockT :: { unMockT :: ReaderT [Definition] m a } -> MockT m a
--data Hoge = Hoge { list :: [(Symbol, Exist a)] }
data Definition = forall a sym. KnownSymbol sym => Definition { symbol :: Proxy sym, value :: a }

instance (Functor m) => Functor (MockT m) where
  fmap f = undefined
instance (Applicative m) => Applicative (MockT m) where
  pure = undefined
instance (Monad m) => Monad (MockT m) where
  MockT x >>= f = MockT (x >>= st . f)


instance (MonadIO m, Monad m) => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- get
    let
      p :: (Param FilePath :> Param Text)
      p = unsafeCoerce $ fromJust $ findParam (Proxy :: Proxy "readFile") defs
    s <- createStubFn p
    pure $ s path

  writeFile path content = MockT do
    defs <- get
    let
      p :: (Param FilePath :> Param Text :> Param ())
      p = unsafeCoerce $ fromJust $ findParam (Proxy :: Proxy "writeFile") defs
    liftIO $ print p
    s <- createStubFn p
    pure $ s path content

instance Monad m => ApiOperation (MockT m) where
  post content = undefined

runMockT :: Monad m => MockT m a -> m a
runMockT (MockT s) = do
  r <- runStateT s []
  pure (fst r)


_readFile :: Monad m => (Param FilePath :> Param Text) -> MockT m ()
_readFile param = MockT $ do
  modify (++ [Definition (Proxy :: Proxy "readFile") param])

_writeFile :: Monad m => (Param FilePath :> Param Text :> Param ()) -> MockT m ()
_writeFile param = MockT $ do
  modify (++ [Definition (Proxy :: Proxy "writeFile") param])


findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition symbol value) -> symbolVal symbol == symbolVal pa) definitions
  fmap (\(Definition symbol value) -> unsafeCoerce value) definition

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()
      program "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
