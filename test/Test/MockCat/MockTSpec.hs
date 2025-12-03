{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCat.MockTSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import GHC.IO (evaluate)
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = describe "MockT + expects integration" do
  it "allows expects inside runMockT" do
    runMockT @IO do
      f <-
        mock (any @String |> True)
          `expects` do
            called once `with` "foo"
      _ <- liftIO $ evaluate (f "foo")
      pure ()

