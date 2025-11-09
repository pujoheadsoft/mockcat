{-# LANGUAGE BlockArguments #-}
module Test.MockCat.Internal.RegistrySpec (spec) where

import Test.Hspec
import Test.MockCat.Internal.Registry
import Data.Dynamic (fromDynamic)
import Data.IORef (newIORef, readIORef)
import Test.MockCat.Internal.Types (Verifier(..), AppliedRecord(..))
import Test.MockCat.AssociationList (empty)

spec :: Spec
spec = do
  describe "Registry" do
    it "register and lookup" do
      let f = (+ 1) :: Int -> Int
      ref <- newIORef AppliedRecord { appliedParamsList = [] :: [Int], appliedParamsCounter = empty }
      _ <- attachVerifierToFn f (Just "name", Verifier ref)
      verifier <- lookupVerifierForFn f
      case verifier of
        Just (mockName, dyn) -> do
          mockName `shouldBe` Just "name"
          case (fromDynamic dyn :: Maybe (Verifier Int)) of
            Just (Verifier vref) -> do
              r <- readIORef vref
              r `shouldBe` AppliedRecord { appliedParamsList = [] :: [Int], appliedParamsCounter = empty }
            Nothing -> expectationFailure "payload dynamic mismatch"
        Nothing -> expectationFailure "lookupStubFn returned Nothing"