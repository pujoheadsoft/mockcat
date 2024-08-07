{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TyprClassTHSpec where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text, pack)
import Test.MockCat.TH (makeMock)
import Test.Hspec
import Test.MockCat

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

makeMock [''FileOperation]
makeMock [''ApiOperation]


spec :: Spec
spec = it "Read, edit, and output files" do
  modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

  result <- runMockT do
    _readFile [
      "input.txt" |> pack "content",
      "hoge.txt" |> pack "content"
      ]
    _writeFile $ "output.text" |> pack "modifiedContent" |> ()
    _post $ pack "modifiedContent" |> ()
    program "input.txt" "output.text" modifyContentStub

  result `shouldBe` ()