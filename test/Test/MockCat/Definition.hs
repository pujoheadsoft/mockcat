module Test.MockCat.Definition where

import Data.Text
import Prelude hiding (readFile, writeFile)

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
