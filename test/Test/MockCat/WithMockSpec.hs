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

    it "never expectation without args succeeds when not called" $ do
      withMock $ do
        _ <- mock (any @String |> True)
          `expects` (called never)
        pure ()

    it "never expectation without args fails when called" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` (called never)
        liftIO $ mockFn "a" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "never expectation with args succeeds when not called with that arg" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` (called never `with` "z")
        liftIO $ mockFn "a" `shouldBe` True
        pure ()

    it "never expectation with args fails when called with that arg" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` (called never `with` "z")
        liftIO $ mockFn "z" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations in do block" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        pure ()

    it "multiple expectations in do block fails when not all satisfied" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        -- missing: mockFn "b"
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations in do block with never" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called once `with` "a"
            called never `with` "z"
        liftIO $ mockFn "a" `shouldBe` True
        pure ()

    it "multiple expectations in do block with never fails when violated" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called once `with` "a"
            called never `with` "z"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "z" `shouldBe` True  -- This should fail
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations with different counts" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 3) `with` "a"
            called (atLeast 2) `with` "b"
            called once `with` "c"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

    it "multiple expectations fails when count is insufficient" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 3) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        -- missing one more "a" call
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations fails when atLeast is not satisfied" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (atLeast 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        -- missing one more "a" call (need at least 2)
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations with mixed never and count" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 2) `with` "a"
            called never `with` "z"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        pure ()

    it "multiple expectations with never fails when never is violated" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            called (times 2) `with` "a"
            called never `with` "z"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "z" `shouldBe` True  -- This should fail
        pure ()) `shouldThrow` anyErrorCall


  describe "withMock verification failures" $ do
    it "fails when called fewer times than expected" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called (times 3) `with` "a"
        
        liftIO $ evaluate $ mockFn "a"
        liftIO $ evaluate $ mockFn "a"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called with unexpected arguments" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called once `with` "a"
        
        liftIO $ evaluate $ mockFn "b"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called but never expected" $ do
      (withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called never `with` "z"
        
        liftIO $ evaluate $ mockFn "z"
        pure ()) `shouldThrow` anyErrorCall

  describe "withMock with runMockT" $ do
    it "can use runMockT inside withMock" $ do
      withMock $ do
        result <- runMockT do
          _readFile $ "input.txt" |> pack "content"
          _writeFile $ "output.txt" |> pack "content" |> ()
          operationProgram "input.txt" "output.txt"
        
        liftIO $ result `shouldBe` ()

  describe "order verification" $ do
    it "calledInOrder succeeds when called in correct order" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInOrder ["a", "b", "c"]
        
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

    it "calledInOrder fails when called in wrong order" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInOrder ["a", "b", "c"]
        
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True  -- Wrong order: should be "b"
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "calledInOrder fails when not all calls are made" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInOrder ["a", "b", "c"]
        
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        -- missing: mockFn "c"
        pure ()) `shouldThrow` anyErrorCall

    it "calledInSequence succeeds when sequence is followed" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInSequence ["a", "c"]
        
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True  -- This is ignored
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

    it "calledInSequence fails when sequence is violated" $ do
      (withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInSequence ["a", "c"]
        
        liftIO $ mockFn "c" `shouldBe` True  -- Wrong: "a" should come first
        liftIO $ mockFn "a" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "calledInSequence succeeds with extra calls in between" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInSequence ["a", "c"]
        
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "x" `shouldBe` True
        liftIO $ mockFn "y" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

  describe "multiple mocks" $ do
    it "can define multiple mocks in withMock" $ do
      withMock $ do
        fn1 <- mock (any @String |> True)
          `expects` do
            called once `with` "a"
        
        fn2 <- mock (any @String |> any @String |> False)
          `expects` do
            called once `with` ("x" |> "y")
        
        liftIO $ fn1 "a" `shouldBe` True
        liftIO $ fn2 "x" "y" `shouldBe` False
        pure ()

  describe "withMock scope isolation" $ do
    it "mocks from different withMock blocks do not interfere" $ do
      -- First withMock block: expect one application
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            called once `with` "a"
        liftIO $ evaluate $ fn1 "a"
      
      -- Second withMock block: expect zero (if leaked, would see 1 and fail)
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            called never `with` "a"
        pure ()

    it "multiple sequential withMock blocks are independent" $ do
      -- Block 1: call with "x"
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            called once `with` "x"
        liftIO $ evaluate $ fn1 "x"
      
      -- Block 2: call with "y"
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            called once `with` "y"
        liftIO $ evaluate $ fn2 "y"
      
      -- Block 3: no calls
      withMock $ do
        fn3 <- mock (any |> True)
          `expects` do
            called never `with` "z"
        pure ()

    {-
    it "withMock blocks with same argument values are independent" $ do
      -- Both blocks use "same" as argument, but should be independent
      withMock $ do
        fn1 <- mock (any |> True)
          `expects` do
            called (times 2) `with` "same"
        liftIO $ evaluate $ fn1 "same"
        liftIO $ evaluate $ fn1 "same"
      
      withMock $ do
        fn2 <- mock (any |> True)
          `expects` do
            called (times 3) `with` "same"
        liftIO $ evaluate $ fn2 "same"
        liftIO $ evaluate $ fn2 "same"
        liftIO $ evaluate $ fn2 "same"
    -}

  describe "withMock concurrency" $ do
    it "counts calls across parallel threads" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called (times 10)
        
        withRunInIO $ \runInIO -> do
          as <- replicateM 10 (async $ runInIO $ do
            liftIO $ evaluate $ mockFn "a"
            pure ())
          mapM_ wait as

    it "handles concurrent calls with different arguments" $ do
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called (times 5) `with` "a"
            called (times 5) `with` "b"
        
        withRunInIO $ \runInIO -> do
          as1 <- replicateM 5 (async $ runInIO $ liftIO $ evaluate $ mockFn "a")
          as2 <- replicateM 5 (async $ runInIO $ liftIO $ evaluate $ mockFn "b")
          mapM_ wait (as1 ++ as2)

    it "stress test: many threads with many calls" $ do
      let threads = 20 :: Int
          callsPerThread = 10 :: Int
          total = threads * callsPerThread :: Int
      
      withMock $ do
        mockFn <- mock (any |> True)
          `expects` do
            called (times total)
        
        withRunInIO $ \runInIO -> do
          as <- replicateM threads (async $ runInIO $ do
            replicateM_ callsPerThread $ do
              liftIO $ evaluate $ mockFn "stress"
              liftIO $ threadDelay 1)
          mapM_ wait as

    it "concurrent calls preserve order expectations" $ do
      withMock $ do
        mockFn <- mock (any @String |> True)
          `expects` do
            calledInOrder ["first", "second", "third"]
        
        withRunInIO $ \runInIO -> do
          -- Sequential calls to preserve order
          runInIO $ void $ liftIO $ evaluate $ mockFn "first"
          runInIO $ void $ liftIO $ evaluate $ mockFn "second"
          runInIO $ void $ liftIO $ evaluate $ mockFn "third"

