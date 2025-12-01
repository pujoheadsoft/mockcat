{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test.MockCat.WithMockSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (Text, pack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, anyErrorCall, shouldContain)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import GHC.IO (evaluate)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, replicateM_, void)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Exception (try, ErrorCall(..))
import qualified Test.MockCat.WithMock as WithMock
import Test.MockCat.WithMock (Expectation)
import Test.MockCat.Param (Param)
import Test.MockCat.Param (Param)

-- Generate mocks for FileOperation
makeMock [t|FileOperation|]

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content

spec :: Spec
spec = do
  describe "withMock basic functionality" $ do
    it "simple mock with expects" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called once `with` "a")
        liftIO $ mockFn "a" `shouldBe` True

    it "simple mock with expects using param" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called once `with` (param "a"))
        liftIO $ mockFn "a" `shouldBe` True

    it "fails when not called" $ do
      withMock (do
        _ <- mock (any |> True)
          `expects` (called once `with` "a")
        pure ()) `shouldThrow` anyErrorCall

    it "error message when not called" $ do
      result <- try $ withMock $ do
        _ <- mock (any |> True)
          `expects` (called once `with` "a")
        pure ()
      case result of
        Left (ErrorCall msg) -> do
          -- エラーメッセージを複数行で見やすく定義
          let expected =
                "function was not applied the expected number of times to the expected arguments.\n" <>
                "  expected: 1\n" <>
                "   but got: 0"
          msg `shouldBe` expected
        _ -> fail "Expected ErrorCall"

    it "atLeast expectation" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called (atLeast 2) `with` "a")

        void $ liftIO $ evaluate $ mockFn "a"
        void $ liftIO $ evaluate $ mockFn "a"
        void $ liftIO $ evaluate $ mockFn "a"

    it "anything expectation" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called once)

        void $ liftIO $ evaluate $ mockFn "a"

    it "anything expectation fails when not called" $ do
      (withMock $ do
        _ <- mock (any @String |> True)
          `expects` (called once)
        pure ()) `shouldThrow` anyErrorCall

    it "anything expectation error message when not called" $ do
      result <- try $ withMock $ do
        _ <- mock (any @String |> True)
          `expects` (called once)
        pure ()
      case result of
        Left (ErrorCall msg) -> do
          -- countMismatchMessage の形式に合わせる
          let expected =
                "function was not applied the expected number of times.\n" <>
                "  expected: 1\n" <>
                "   but got: 0"
          msg `shouldBe` expected
        _ -> fail "Expected ErrorCall"

    {-
    it "multiple expectations" $ do
      withMock $ do
        mockFn <- mock (any |> any |> True)
          `expects` do
            e1 <- registerExpectation mockFn $ called (times 3) `with` ("a" |> "b")
            e2 <- registerExpectation mockFn $ called once     `with` ("x" |> "x")
            e3 <- registerExpectation mockFn $ called never    `with` ("z" |> "z")
            pure [e1, e2, e3]
        
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "x" "x"

    it "anything expectation" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called once)
        
        liftIO $ evaluate $ mockFn "a"

    it "never expectation success" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called never `with` "z")
        
        void $ liftIO $ evaluate $ mockFn "a"

    it "never expectation fails when called" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called never `with` "z")
        
        void $ liftIO $ evaluate $ mockFn "z"
        pure ()) `shouldThrow` anyErrorCall

    it "never expectation error message when called" $ do
      result <- try $ withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called never `with` "z")
        
        void $ liftIO $ evaluate $ mockFn "z"
        pure ()
      case result of
        Left (ErrorCall msg) -> do
          -- countWithArgsMismatchMessage の形式に合わせる
          let expected = 
                "function was not applied the expected number of times to the expected arguments.\n" <>
                "  expected: 0\n" <>
                "   but got: 1"
          msg `shouldBe` expected
        _ -> fail "Expected ErrorCall"
    -}

    {-
    it "multiple expectations" $ do
      withMock $ do
        mockFn <- mock (any |> any |> True)
          `expects` do
            e1 <- registerExpectation mockFn $ called (times 3) `with` ("a" |> "b")
            e2 <- registerExpectation mockFn $ called once     `with` ("x" |> "x")
            e3 <- registerExpectation mockFn $ called never    `with` ("z" |> "z")
            pure [e1, e2, e3]
        
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "a" "b"
        liftIO $ evaluate $ mockFn "x" "x"

    it "atLeast expectation" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called (atLeast 2) `with` "a")
        
        liftIO $ evaluate $ mockFn "a"
        liftIO $ evaluate $ mockFn "a"
        liftIO $ evaluate $ mockFn "a"

    it "anything expectation" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called once)
        
        liftIO $ evaluate $ mockFn "a"

    it "never expectation success" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` (called never `with` "z")
        
        liftIO $ evaluate $ mockFn "a"

  describe "withMock verification failures" $ do
    it "fails when called fewer times than expected" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [called (times 3) `with` "a"]
        
        evaluate $ mockFn "a"
        evaluate $ mockFn "a"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called with unexpected arguments" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [called once `with` "a"]
        
        evaluate $ mockFn "b"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called but never expected" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [called never `with` "z"]
        
        evaluate $ mockFn "z"
        pure ()) `shouldThrow` anyErrorCall

  describe "withMock with runMockT" $ do
    it "can use runMockT inside withMock" $ do
      withMock $ do
        result <- runMockT do
          _readFile $ "input.txt" |> pack "content"
          _writeFile $ "output.txt" |> pack "content" |> ()
          operationProgram "input.txt" "output.txt"
        
        result `shouldBe` ()

  describe "order verification" $ do
    it "calledInOrder" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [calledInOrder ["a" |> True, "b" |> True, "c" |> True]]
        
        evaluate $ mockFn "a"
        evaluate $ mockFn "b"
        evaluate $ mockFn "c"

    it "calledInSequence" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [calledInSequence ["a" |> True, "c" |> True]]
        
        evaluate $ mockFn "a"
        evaluate $ mockFn "b"
        evaluate $ mockFn "c"

  describe "multiple mocks" $ do
    it "can define multiple mocks in withMock" $ do
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            pure [called once `with` "a"]
        
        fn2 <- mock (any |> any |> False)
          `expects` do
            pure [called once `with` ("x" |> "y")]
        
        evaluate $ fn1 "a"
        evaluate $ fn2 "x" "y"

  describe "withMock scope isolation" $ do
    it "mocks from different withMock blocks do not interfere" $ do
      -- First withMock block: expect one application
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            pure [called once `with` "a"]
        evaluate $ fn1 "a"
      
      -- Second withMock block: expect zero (if leaked, would see 1 and fail)
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            pure $ called never `with` "a"
        pure ()

    it "multiple sequential withMock blocks are independent" $ do
      -- Block 1: call with "x"
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            pure $ called once `with` "x"
        evaluate $ fn1 "x"
      
      -- Block 2: call with "y"
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            pure $ called once `with` "y"
        evaluate $ fn2 "y"
      
      -- Block 3: no calls
      withMock $ do
        fn3 <- mock (any |> True)
          `expects` do
            pure [called never `with` "z"]
        pure ()

    it "withMock blocks with same argument values are independent" $ do
      -- Both blocks use "same" as argument, but should be independent
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            pure $ called (times 2) `with` "same"
        evaluate $ fn1 "same"
        evaluate $ fn1 "same"
      
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            pure $ called (times 3) `with` "same"
        evaluate $ fn2 "same"
        evaluate $ fn2 "same"
        evaluate $ fn2 "same"

  describe "withMock concurrency" $ do
    it "counts calls across parallel threads" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [called (times 10)]
        
        withRunInIO $ \runInIO -> do
          as <- replicateM 10 (async $ runInIO $ do
            evaluate $ mockFn "a"
            pure ())
          mapM_ wait as

    it "handles concurrent calls with different arguments" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [called (times 5) `with` "a", called (times 5) `with` "b"]
        
        withRunInIO $ \runInIO -> do
          as1 <- replicateM 5 (async $ runInIO $ evaluate $ mockFn "a")
          as2 <- replicateM 5 (async $ runInIO $ evaluate $ mockFn "b")
          mapM_ wait (as1 ++ as2)

    it "stress test: many threads with many calls" $ do
      let threads = 20 :: Int
          callsPerThread = 10 :: Int
          total = threads * callsPerThread :: Int
      
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure (called (times total))
        
        withRunInIO $ \_runInIO -> do
          as <- replicateM threads (async $ do
            replicateM_ callsPerThread $ do
              evaluate $ mockFn "stress"
              threadDelay 1)
          mapM_ wait as

    it "concurrent calls preserve order expectations" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            pure [calledInOrder ["first" |> True, "second" |> True, "third" |> True]]
        
        withRunInIO $ \_runInIO -> do
          -- Sequential calls to preserve order
          evaluate $ mockFn "first"
          evaluate $ mockFn "second"
          evaluate $ mockFn "third"
    -}

