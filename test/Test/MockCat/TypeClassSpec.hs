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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TypeClassSpec (spec) where

import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (Mock, createStubFn, stubFn, (|>), shouldApplyTo, Param, (:>), createNamedMock, any)
import Prelude hiding (readFile, writeFile)
import Data.Data
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (StateT (runStateT), modify, get)
import Control.Monad.Trans
import Data.Maybe (fromJust)
import GHC.IO (unsafePerformIO)
import Data.Foldable (for_)
import Test.MockCat.ParamDivider (args)

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
  post modifiedContent  

newtype MockT m a = MockT { st :: StateT [Definition] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

data Definition = forall f p sym. KnownSymbol sym => Definition { 
  symbol :: Proxy sym, 
  mock :: Mock f p,
  verify :: Mock f p -> IO ()
}

instance Monad m => FileOperation (MockT m) where
  readFile path = MockT do
    defs <- get
    let
      mock :: (Mock (FilePath -> Text) (Param FilePath :> Param Text))
      mock = fromJust $ findParam (Proxy :: Proxy "readFile") defs
      !result = stubFn mock path
    pure result

  writeFile path content = MockT do
    defs <- get
    let
      mock :: (Mock (FilePath -> Text -> ()) (Param FilePath :> Param Text :> Param ()))
      mock = fromJust $ findParam (Proxy :: Proxy "writeFile") defs
      !result = stubFn mock path content
    pure result

instance Monad m => ApiOperation (MockT m) where
  post content = MockT do
    defs <- get
    let 
      mock = unsafeCoerce $ maybe (error "postはスタブになってないよ") id $ findParam (Proxy :: Proxy "post") defs
      !result = stubFn mock content
    pure result

runMockT :: MonadIO m => MockT m a -> m a
runMockT (MockT s) = do
  r <- runStateT s []
  let
    !a = fst r
    defs = snd r
  for_ defs (\(Definition _ m v) -> liftIO $ v m)
  pure a


_readFile :: Monad m => (Param FilePath :> Param Text) -> MockT m ()
_readFile p = MockT $ do
  modify (++ [Definition 
    (Proxy :: Proxy "readFile")
    (unsafePerformIO $ createNamedMock "readFile" p)
    \m -> shouldApplyTo m (args p)])

_writeFile :: Monad m => (Param FilePath :> Param Text :> Param ()) -> MockT m ()
_writeFile p = MockT $ do
  modify (++ [Definition
    (Proxy :: Proxy "writeFile")
    (unsafePerformIO $ createNamedMock "writeFile" p)
    \m -> shouldApplyTo m (args p)])

_post :: Monad m => (Param Text :> Param ()) -> MockT m ()
_post p = MockT $ do
  modify (++ [Definition
    (Proxy :: Proxy "post")
    (unsafePerformIO $ createNamedMock "post" p)
    \m -> shouldApplyTo m (args p)])

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()
      _post $ pack "modifiedContent" |> ()
      program "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
