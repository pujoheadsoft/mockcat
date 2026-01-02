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
import Test.MockCat.Verify (ResolvableParamsOf)
import Prelude hiding (readFile, writeFile, any)

import Data.Proxy (Proxy(..))
import Control.Exception (ErrorCall, displayException, evaluate)
import Data.List (isInfixOf)
import Data.Typeable (cast)
import GHC.TypeLits (symbolVal)
import Control.Monad.IO.Class (liftIO)

-- Helper for error matching
missingCallHandwritten :: String -> Selector ErrorCall
missingCallHandwritten name err =
  let needle1 = "function `" <> name <> "` was not called the expected number of times."
      needle2 = "function `_" <> name <> "` was not called the expected number of times."
   in (needle1 `isInfixOf` displayException err) || (needle2 `isInfixOf` displayException err)

-- Handwritten polymorphic return helpers
_readFile ::
  ( MockDispatch (IsMockSpec spec) spec (MockT IO) (FilePath -> Text)
  ) =>
  spec ->
  MockT IO (Unit' (ResolvableParamsOf (FilePath -> Text)))
_readFile p = MockT $ do
  fn <- unMockT (mock (label "readFile") p :: MockT IO (FilePath -> Text))
  addDefinition (Definition (Proxy :: Proxy "readFile") fn NoVerification)
  pure (Unit' ())

_writeFile ::
  ( MockDispatch (IsMockSpec spec) spec (MockT IO) (FilePath -> Text -> ())
  ) =>
  spec ->
  MockT IO (Unit' (ResolvableParamsOf (FilePath -> Text -> ())))
_writeFile p = MockT $ do
  fn <- unMockT (mock (label "writeFile") p :: MockT IO (FilePath -> Text -> ()))
  addDefinition (Definition (Proxy :: Proxy "writeFile") fn NoVerification)
  pure (Unit' ())

readFile :: FilePath -> MockT IO Text
readFile path = do
  defs <- getDefinitions
  let stubs = reverse [f | Definition (p :: Proxy sym) fn _ <- defs, symbolVal p == "readFile", Just f <- [cast fn :: Maybe (FilePath -> Text)]]
  case stubs of
    [] -> error "readFile: no stub defined or type mismatch"
    (f : _) -> pure (f path)

writeFile :: FilePath -> Text -> MockT IO ()
writeFile path content = do
  defs <- getDefinitions
  let stubs = reverse [f | Definition (p :: Proxy sym) fn _ <- defs, symbolVal p == "writeFile", Just f <- [cast fn :: Maybe (FilePath -> Text -> ())]]
  case stubs of
    [] -> error "writeFile: no stub defined or type mismatch"
    (f : _) -> pure (f path content)

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
          `expects` called once
        -- readFile is never called
        pure ()) `shouldThrow` missingCallHandwritten "readFile"

    it "Arguments can be verified even if the helper returns () (Dynamic Resolution + Coercion)" do
      runMockT do
        _readFile ("input.txt" ~> pack "content") `expects` called once
        _ <- readFile "input.txt" >>= liftIO . evaluate
        pure ()

    it "Arguments can be verified with 'with' (Dynamic Resolution + Coercion)" do
      runMockT do
        _readFile ("input.txt" ~> pack "content") `expects` (called once `with` "input.txt")
        _ <- readFile "input.txt" >>= liftIO . evaluate
        pure ()

    it "Can verify arguments for functions returning () without type annotations (writeFile)" do
      runMockT do
        _writeFile ("output.txt" ~> pack "content" ~> ()) `expects` called once
        -- Execute the function and force evaluation
        writeFile "output.txt" (pack "content") >>= liftIO . evaluate
        pure ()
