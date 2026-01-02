{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


module Test.MockCat.TypeClassSpec (spec) where

import Test.Hspec
import Test.MockCat
import Data.Text (Text, pack)
import Prelude hiding (readFile, writeFile)

import Data.Proxy (Proxy(..))
import Control.Exception (ErrorCall, displayException)
import Data.List (isInfixOf)

-- Helper for error matching
missingCallHandwritten :: String -> Selector ErrorCall
missingCallHandwritten name err =
  let needle1 = "function `" <> name <> "` was not called the expected number of times."
      needle2 = "function `_" <> name <> "` was not called the expected number of times."
   in (needle1 `isInfixOf` displayException err) || (needle2 `isInfixOf` displayException err)

-- Handwritten polymorphic return helpers
-- | No-op. Previously used StableName-based verification check, which breaks under HPC.
--   TH-generated code doesn't need this, so we make it a no-op for consistency.
ensureVerifiable :: Applicative m => a -> m ()
ensureVerifiable _ = pure ()

_readFile ::
  ( MockDispatch (IsMockSpec params) params (MockT IO) (FilePath -> Text)
  ) =>
  params ->
  MockT IO ()
_readFile p = MockT $ do
  fn <- unMockT (mock (label "readFile") p :: MockT IO (FilePath -> Text))
  ensureVerifiable fn
  addDefinition (Definition (Proxy :: Proxy "readFile") fn NoVerification)
  pure ()

_writeFile ::
  ( MockDispatch (IsMockSpec params) params (MockT IO) (FilePath -> Text -> ())
  ) =>
  params ->
  MockT IO ()
_writeFile p = MockT $ do
  fn <- unMockT (mock (label "writeFile") p :: MockT IO (FilePath -> Text -> ()))
  ensureVerifiable fn
  addDefinition (Definition (Proxy :: Proxy "writeFile") fn NoVerification)
  pure ()

spec :: Spec
spec = do
  describe "Polymorphic return tests (Clean)" do
    it "Program definition suppresses unused-do-bind warning (expects not used)" do
      -- Just verify that this compiles without warning
      runMockT do
        _readFile $ "input.txt" ~> pack "content"
        _writeFile $ "output.txt" ~> pack "content" ~> ()
        pure ()

    it "Error when read stub is defined but target function readFile is never called (expects used)" do
      (runMockT @IO do
        _readFile ("input.txt" ~> pack "content")
          `expects` do
            called once
        -- readFile is never called
        pure ()) `shouldThrow` missingCallHandwritten "readFile"
