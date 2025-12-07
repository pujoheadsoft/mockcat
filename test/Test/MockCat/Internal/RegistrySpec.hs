{-# LANGUAGE BlockArguments #-}
module Test.MockCat.Internal.RegistrySpec (spec) where

import Test.Hspec
import Test.MockCat.Internal.Registry
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Dynamic (fromDynamic)
import Test.MockCat.AssociationList (empty)
import Test.MockCat.Internal.Types (InvocationRecorder(..), FunctionNature(..), InvocationRecord(..))

spec :: Spec
spec = do
  describe "Registry" do
    it "register and lookup" do
      let f = (+ 1) :: Int -> Int
      ref <- newTVarIO InvocationRecord { invocations = [] :: [Int], invocationCounts = empty }
      _ <- attachVerifierToFn f (Just "name", InvocationRecorder ref ParametricFunction)
      verifier <- lookupVerifierForFn f
      case verifier of
        Just (mockName, dyn) -> do
          mockName `shouldBe` Just "name"
          case (fromDynamic dyn :: Maybe (InvocationRecorder Int)) of
            Just (InvocationRecorder vref _) -> do
              r <- readTVarIO vref
              r `shouldBe` InvocationRecord { invocations = [] :: [Int], invocationCounts = empty }
            Nothing -> expectationFailure "payload dynamic mismatch"
        Nothing -> expectationFailure "lookupStubFn returned Nothing"