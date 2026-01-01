{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test.MockCat.Internal.MockRegistrySpec (spec) where

import Test.Hspec
import Test.MockCat.Internal.MockRegistry
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Dynamic (fromDynamic)
import Test.MockCat.AssociationList (empty)
import Test.MockCat.Internal.Types (InvocationRecorder(..), FunctionNature(..), InvocationRecord(..))

spec :: Spec
spec = do
  describe "MockRegistry" do
    it "register and lookup" do
      let f = (+ 1) :: Int -> Int
      ref <- newTVarIO InvocationRecord { invocations = [] :: [Int], invocationCounts = empty }
      attachVerifierToFn f (Just "name", InvocationRecorder ref ParametricFunction)
      results <- lookupVerifierForFn f
      case results of
        [(mockName, dyn)] -> do
          mockName `shouldBe` Just "name"
          case (fromDynamic dyn :: Maybe (InvocationRecorder Int)) of
            Just (InvocationRecorder vref _) -> do
              r <- readTVarIO vref
              r `shouldBe` InvocationRecord { invocations = [] :: [Int], invocationCounts = empty }
            Nothing -> expectationFailure "payload dynamic mismatch"
        _ -> expectationFailure "lookupStubFn returned unexpected number of results"


