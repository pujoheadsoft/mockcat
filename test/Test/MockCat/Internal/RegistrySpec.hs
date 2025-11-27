{-# LANGUAGE BlockArguments #-}
module Test.MockCat.Internal.RegistrySpec (spec) where

import Test.Hspec
import Test.MockCat.Internal.Registry
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Data.Dynamic (fromDynamic)
import Test.MockCat.AssociationList (empty)
import Test.MockCat.Internal.Types (Verifier(..), VerifierKind(..), AppliedRecord(..))

spec :: Spec
spec = do
  describe "Registry" do
    it "register and lookup" do
      let f = (+ 1) :: Int -> Int
      ref <- newTVarIO AppliedRecord { appliedParamsList = [] :: [Int], appliedParamsCounter = empty }
      _ <- attachVerifierToFn f (Just "name", Verifier ref VerifierFunction)
      verifier <- lookupVerifierForFn f
      case verifier of
        Just (mockName, dyn) -> do
          mockName `shouldBe` Just "name"
          case (fromDynamic dyn :: Maybe (Verifier Int)) of
            Just (Verifier vref _) -> do
              r <- readTVarIO vref
              r `shouldBe` AppliedRecord { appliedParamsList = [] :: [Int], appliedParamsCounter = empty }
            Nothing -> expectationFailure "payload dynamic mismatch"
        Nothing -> expectationFailure "lookupStubFn returned Nothing"