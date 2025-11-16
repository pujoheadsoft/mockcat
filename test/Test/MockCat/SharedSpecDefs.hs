{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.MockCat.SharedSpecDefs where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack, isInfixOf)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import qualified Control.Concurrent.Async as Async
import Control.Monad.State (MonadState)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Test.MockCat
import Data.List (find)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import qualified Data.Text.IO as TIO
import Data.Kind (Type)
import Data.Traversable (Traversable)


class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

program ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content

class Monad m => Finder a b m | a -> b, b -> a where
  findIds :: m [a]
  findById :: a -> m b

findValue :: Finder a b m => m [b]
findValue = do
  ids <- findIds
  mapM findById ids

class Monad m => ApiOperation m where
  post :: Text -> m ()

class (Eq s, Show s, MonadState s m) => MonadStateSub s m where
  fnState :: Maybe s -> m s

class (MonadState String m) => MonadStateSub2 s m where
  fnState2 :: String -> m ()

class Monad m => MonadVar2_1 m a where
class MonadVar2_1 m a => MonadVar2_1Sub m a where
  fn2_1Sub :: String -> m ()

class Monad m => MonadVar2_2 a m where
class MonadVar2_2 a m => MonadVar2_2Sub a m where
  fn2_2Sub :: String -> m ()

class MonadIO m => MonadVar3_1 m a b where
class MonadVar3_1 m a b => MonadVar3_1Sub m a b where
  fn3_1Sub :: String -> m ()

class MonadIO m => MonadVar3_2 a m b where
class MonadVar3_2 a m b => MonadVar3_2Sub a m b where
  fn3_2Sub :: String -> m ()

class MonadIO m => MonadVar3_3 a b m where
class MonadVar3_3 a b m => MonadVar3_3Sub a b m where
  fn3_3Sub :: String -> m ()

class Monad m => MultiApplyTest m where
  getValueBy :: String -> m String

getValues :: MultiApplyTest m => [String] -> m [String]
getValues = mapM getValueBy

class Monad m => ExplicitlyReturnMonadicValuesTest m where
  echoExplicit :: String -> m ()
  getByExplicit :: String -> m Int

class Monad m => ExplicitlyReturnMonadicValuesPartialTest m where
  echoExplicitPartial :: String -> m ()
  getByExplicitPartial :: String -> m Int

class Monad m => AssocTypeTest m where
  type ResultType m :: Type
  produce :: m (ResultType m)

class Monad m => DefaultMethodTest m where
  defaultAction :: m Int
  defaultAction = pure 0

class Monad m => ParamThreeMonad a b m | m -> a, m -> b where
  fnParam3_1 :: a -> b -> m String
  fnParam3_2 :: m a
  fnParam3_3 :: m b

class MonadUnliftIO m => MonadAsync m where
  mapConcurrently :: Traversable t => (a -> m b) -> t a -> m (t b)

class Monad m => TestClass m where
  echo :: String -> m ()
  getBy :: String -> m Int

class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()