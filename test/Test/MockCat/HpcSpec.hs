{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Test.MockCat.HpcSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (ErrorCall (..))
import Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "[HPC] Strict Verification Compatibility" $ do
    it "expects works correctly" $ do
      withMock $ do
        m <- mock ("arg" ~> "result") `expects` (called (times 1))
        let r = m "arg"
        liftIO $ r `shouldBe` "result"

    it "shouldBeCalled fails with specific error message when StableName lookup fails" $ do
      -- In HPC mode (or if wrapped), StableName lookup fails.
      -- To guarantee failure regardless of HPC trigger, we use a Wrapper.
      -- This simulates the condition we want to test: "Mock not recognized".
      
      m :: String -> String <- mock $ "arg" ~> "result"
      
      -- Wrap the mock. verifying 'wrapped' directly should fail
      -- because 'wrapped' is not registered in the Core registry.
      let wrapped x = m x
      
      -- Execute verification on the WRAPPED (unregistered) function.
      (wrapped `shouldBeCalled` "arg") `shouldThrow` \(ErrorCall msg) ->
        let expected = 
              "Error: Mock verification failed.\n" ++
              "\n" ++
              "The function passed to 'shouldBeCalled' could not be recognized as a registered mock.\n" ++
              "\n" ++
              "Possible causes:\n" ++
              "  1. You passed a raw wrapper function around the mock.\n" ++
              "  2. You passed a normal (non-mock) function.\n" ++
              "  3. HPC (Coverage) is enabled. (HPC instrumentation changes function identity)\n" ++
              "\n" ++
              "Solution:\n" ++
              "  - If you are using HPC or Wrappers, please use 'expects' or 'withMock' style.\n" ++
              "    'expects' is robust against HPC and wrappers as it does not rely on function identity lookup.\n" ++
              "  - Ensure you are passing the mock function directly created by 'mock'."
        in msg == expected
