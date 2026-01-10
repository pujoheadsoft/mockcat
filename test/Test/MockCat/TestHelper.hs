{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.TestHelper (checkStrictVerificationWorks) where

import Test.MockCat
import Test.Hspec (shouldBe)
import Control.Exception (try, ErrorCall)

-- | Checks if Strict Verification is working (i.e. not interfered by HPC/Coverage).
-- Returns True if verification succeeds.
-- Returns False if verification fails (indicating HPC is likely enabled).
checkStrictVerificationWorks :: IO Bool
checkStrictVerificationWorks = do
  result <- try $ do
    m <- mock ("ping" ~> "pong")
    m "ping" `shouldBe` "pong"
    m `shouldBeCalled` "ping"
  
  case result of
    Right _ -> pure True
    Left (_ :: ErrorCall) -> pure False
