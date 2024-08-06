{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module TypeClassSpec (spec) where

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
import Control.Monad.Reader (ReaderT (runReaderT), ask, runReader)
import Control.Monad.State (StateT, modify, modify', put, get, lift, state)

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

--data MockT m a = MockT { run :: m a, definitions :: [Definition] } deriving (Generic)
data MockT m a = MockT (ReaderT [Definition] m a)

--data Hoge = Hoge { list :: [(Symbol, Exist a)] }
data Definition = forall a sym. KnownSymbol sym => Definition { symbol :: Proxy sym, value :: a }

instance (Functor m) => Functor (MockT m) where
  fmap f = undefined
instance (Applicative m) => Applicative (MockT m) where
  pure = undefined
instance (Monad m) => Monad (MockT m) where
  m >>= k = undefined

{-
スタブを呼び出さないといかんよな
だからMockTから取り出せる必要がある。
そして各スタブの識別ができないといかん。
readFileならreadFileのスタブを識別できないと。
何がくるかわからないのだから、MockTが持てる値は好きなようにかつ可変にしておかないといかんな。
bindではそれを蓄積していかんとならない。あとからフィールドって追加できるんか？
最初から内部的にレコードでも作るか？MockTが任意のレコードを受け取れるようになってれば解決できるよな？
Symbolでも使って、値は存在型で隠してしまうか？

-}
instance Monad m => FileOperation (MockT m) where
  readFile path = undefined
  writeFile path content = undefined

instance Monad m => ApiOperation (MockT m) where
  post content = undefined

runMockT :: MockT m a -> m a
runMockT = undefined

-- Paramを受け取ってMockTを作らないといけない
-- MockTには何をもたせればいい？
-- 任意のパラメーターを持たせないといかんよなあ？
_readFile :: Monad m => (Param FilePath :> Param Text) -> MockT m a
--_readFile param = MockT { run = undefined, definitions = [Definition (Proxy :: Proxy "readFile") param] }
_readFile param = MockT undefined



_writeFile :: (Param FilePath :> Param Text :> Param ()) -> MockT m a
--_writeFile param = MockT { run = undefined, definitions = [Definition (Proxy :: Proxy "writeFile") param] }
_writeFile param = undefined

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
