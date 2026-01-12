{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.WithMockIOSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (try, ErrorCall(..), SomeException)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async, wait)
import Control.Monad (void, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "withMockIO" $ do
    it "can run IO actions directly without liftIO" $ do
      withMockIO $ do
        f <- mock ("hello" ~> "world") `expects` called once
        f "hello" `shouldBe` "world"

    it "cleans up context even if an exception occurs" $ do
      -- Raise an exception in the first test
      void $ try @ErrorCall $ withMockIO $ do
        _ <- mock ("a" ~> "b") `expects` called once
        error "force fail"

      -- Verify that no remnants (like expectations) from the previous test remain in the second test
      withMockIO $ do
        f <- mock ("x" ~> "y") `expects` called once
        f "x" `shouldBe` "y"
        -- Verify that the expectation "a" ~> "b" from the previous test is not verified here (implicit)

    it "supports nested withMockIO" $ do
      withMockIO $ do
        f1 <- mock ("outer" ~> "ok") `expects` called once
        withMockIO $ do
          f2 <- mock ("inner" ~> "ok") `expects` called once
          f2 "inner" `shouldBe` "ok"
        f1 "outer" `shouldBe` "ok"

    it "isolates context between parent and child threads" $ do
      mvar <- newEmptyMVar
      withMockIO $ do
        f <- mock ("parent" ~> "ok") `expects` called once
        void $ forkIO $ do
          -- This fails because child thread cannot find the parent's mock context
          res <- try @SomeException (mock ("child" ~> "ok") `expects` called once)
          putMVar mvar res

        f "parent" `shouldBe` "ok"

      childRes <- takeMVar mvar
      case childRes of
        Left e -> show e `shouldContain` "No WithMockContext found"
        Right _ -> fail "Child thread should not have accessed parent context"
    
    it "stress test: many threads with many calls" $ do
      let threads = 20 :: Int
          callsPerThread = 10 :: Int
          total = threads * callsPerThread :: Int

      withMockIO $ do
        mockFn <- mock ((any @String) ~> pure @IO True)
          `expects` called (times total)

        withRunInIO $ \runInIO -> do
          as <- replicateM threads (async $ runInIO $ do
              forM_ [1 .. callsPerThread] $ \_ -> do
                void $ mockFn "stress"
                liftIO $ threadDelay 1)
          mapM_ wait as