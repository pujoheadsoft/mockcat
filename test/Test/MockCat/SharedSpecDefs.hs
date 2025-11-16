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
