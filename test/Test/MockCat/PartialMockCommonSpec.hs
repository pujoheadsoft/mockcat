{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.MockCat.PartialMockCommonSpec
  ( specUserInputGetterPoly
  , specExplicitReturnPoly
  ) where

import Prelude hiding (readFile, writeFile)
import Test.Hspec (Spec, it, shouldBe, describe)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import qualified Test.MockCat.Verify as Verify
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)

-- Polymorphic entry for migrating from modules that expose more general builders
specUserInputGetterPoly
  :: UserInputGetter (MockT IO)
  => (forall r m. (Verify.ResolvableParamsOf r ~ (), MonadIO m, Typeable r) => r -> MockT m r)
  -> Spec
specUserInputGetterPoly getInputF = describe "UserInputGetter" do
  it "Get user input (has input)" do
    result <- runMockT do
      _ <- getInputF ("value" :: String)
      i <- getInput
      toUserInput i
    result `shouldBe` Just (UserInput "value")

  it "Get user input (no input)" do
    result <- runMockT do
      _ <- getInputF ("" :: String)
      i <- getInput
      toUserInput i
    result `shouldBe` Nothing


specExplicitReturnPoly
  :: ExplicitlyReturnMonadicValuesPartialTest (MockT IO)
  => ( forall params. ( MockBuilder params (String -> IO Int) (Param String)
                     , MonadIO IO
                     , Typeable (String -> IO Int)
                     , Verify.ResolvableParamsOf (String -> IO Int) ~ Param String
                     ) => params -> MockT IO (String -> IO Int))
  -> ( forall params. ( MockBuilder params (String -> IO ()) (Param String)
                     , MonadIO IO
                     , Typeable (String -> IO ())
                     , Verify.ResolvableParamsOf (String -> IO ()) ~ Param String
                     ) => params -> MockT IO (String -> IO ()))
  -> Spec
specExplicitReturnPoly getByF echoF = describe "ExplicitlyReturnMonadicValuesPartialTest" do
  it "Return monadic value test" do
    result <- runMockT $ do
      _ <- echoF $ ("3" :: String) |> pure @IO ()
      v <- getByExplicitPartial "abc"
      echoExplicitPartial (show v)
    result `shouldBe` ()

  it "Override getBy via stub" do
    result <- runMockT do
      _ <- getByF $ ("abc" :: String) |> pure @IO (123 :: Int)
      getByExplicitPartial "abc"
    result `shouldBe` 123
