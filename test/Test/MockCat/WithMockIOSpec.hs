{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.WithMockIOSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (try, ErrorCall(..), SomeException)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)

spec :: Spec
spec = do
  describe "withMockIO" $ do
    it "can run IO actions directly without liftIO" $ do
      withMockIO $ do
        f <- mock $ "hello" ~> "world"
        f "hello" `shouldBe` "world"
        f `shouldBeCalled` "hello"

    it "cleans up context even if an exception occurs" $ do
      -- Raise an exception in the first test
      void $ try @ErrorCall $ withMockIO $ do
        void $ mock $ "a" ~> "b"
        error "force fail"

      -- Verify that no remnants (like expectations) from the previous test remain in the second test
      withMockIO $ do
        f <- mock $ "x" ~> "y"
        f "x" `shouldBe` "y"
        -- Verify that the expectation "a" ~> "b" from the previous test is not verified here

    it "supports nested withMockIO" $ do
      withMockIO $ do
        f1 <- mock $ "outer" ~> "ok"
        withMockIO $ do
          f2 <- mock $ "inner" ~> "ok"
          f2 "inner" `shouldBe` "ok"
          f2 `shouldBeCalled` "inner"
        f1 "outer" `shouldBe` "ok"
        f1 `shouldBeCalled` "outer"

    it "isolates context between parent and child threads" $ do
      mvar <- newEmptyMVar
      withMockIO $ do
        f <- mock $ "parent" ~> "ok"
        void $ forkIO $ do
          res <- try @SomeException (mock ("child" ~> "ok") `expects` called once)
          putMVar mvar res

        f "parent" `shouldBe` "ok"
        f `shouldBeCalled` "parent"

      childRes <- takeMVar mvar
      case childRes of
        Left e -> show e `shouldContain` "No WithMockContext found"
        Right _ -> fail "Child thread should not have accessed parent context"
