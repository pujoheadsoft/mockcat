{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MockCat.WithMockSpec (spec) where

import Prelude hiding (readFile, writeFile, any)
import Data.Text (pack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, anyErrorCall)
import Test.MockCat
import Test.MockCat.SharedSpecDefs
import GHC.IO (evaluate)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay)
import Control.Monad (void, forM, forM_)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, ErrorCall(..))

-- Generate mocks for FileOperation
makeAutoLiftMock [t|FileOperation|]

perCall :: Int -> a -> a
perCall _ x = x

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
        mockFn <- mock (any ~> True)
          `expects` (called once `with` "a")
        liftIO $ mockFn "a" `shouldBe` True

    it "simple mock with expects using param" $ do
      withMock $ do 
        mockFn <- mock (any ~> True)
          `expects` (called once `with` param "a")
        liftIO $ mockFn "a" `shouldBe` True

    it "fails when not called" $ do
      withMock (do 
        _ <- mock (any ~> True)
          `expects` (called once `with` "a")
        pure ()) `shouldThrow` anyErrorCall

    it "error message when not called" $ do
      result <- try $ withMock $ do 
        _ <- mock (any ~> True)
          `expects` (called once `with` "a")
        pure ()
      case result of
        Left (ErrorCall msg) -> do
          let expected =
                "function was not called the expected number of times with the expected arguments.\n" <>
                "  expected: 1\n" <>
                "   but got: 0"
          msg `shouldBe` expected
        _ -> fail "Expected ErrorCall"

    it "atLeast expectation" $ do
      withMock $ do 
        mockFn <- mock (any ~> True)
          `expects` (called (atLeast 2) `with` "a")

        void $ liftIO $ evaluate $ mockFn "a"
        void $ liftIO $ evaluate $ mockFn "a"
        void $ liftIO $ evaluate $ mockFn "a"

    it "anything expectation" $ do
      withMock $ do 
        mockFn <- mock (any ~> True)
          `expects` called once

        void $ liftIO $ evaluate $ mockFn "a"

    it "anything expectation fails when not called" $ do
      withMock (do 
        _ <- mock (any @String ~> True)
          `expects` called once
        pure ()) `shouldThrow` anyErrorCall

    it "anything expectation error message when not called" $ do
      result <- try $ withMock $ do 
        _ <- mock (any @String ~> True)
          `expects` called once
        pure ()
      case result of
        Left (ErrorCall msg) -> do
          let expected =
                "function was not called the expected number of times.\n" <>
                "  expected: 1\n" <>
                "   but got: 0"
          msg `shouldBe` expected
        _ -> fail "Expected ErrorCall"

    it "never expectation without args succeeds when not called" $ do
      withMock $ do 
        _ <- mock (any @String ~> True)
          `expects` called never
        pure ()

    it "never expectation without args fails when called" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` called never
        liftIO $ mockFn "a" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "never expectation with args succeeds when not called with that arg" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` (called never `with` "z")
        liftIO $ mockFn "a" `shouldBe` True
        pure ()

    it "never expectation with args fails when called with that arg" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` (called never `with` "z")
        liftIO $ mockFn "z" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations in do block" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called (times 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        pure ()

    it "multiple expectations in do block fails when not all satisfied" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called (times 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        -- missing: mockFn "b"
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations in do block with never" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called once `with` "a"
            called never `with` "z"
        liftIO $ mockFn "a" `shouldBe` True
        pure ()

    it "multiple expectations in do block with never fails when violated" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called once `with` "a"
            called never `with` "z"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "z" `shouldBe` True  -- This should fail
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations with different counts" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
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
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called (times 3) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        -- missing one more "a" call
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations fails when atLeast is not satisfied" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called (atLeast 2) `with` "a"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        -- missing one more "a" call (need at least 2)
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "multiple expectations with mixed never and count" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            called (times 2) `with` "a"
            called never `with` "z"
            called once `with` "b"
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        pure ()

    it "multiple expectations with never fails when never is violated" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
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
      withMock (do 
        mockFn <- mock (any ~> True)
          `expects` do
            called (times 3) `with` "a"

        liftIO $ evaluate $ mockFn "a"
        liftIO $ evaluate $ mockFn "a"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called with unexpected arguments" $ do
      withMock (do 
        mockFn <- mock (any ~> True)
          `expects` do
            called once `with` "a"

        liftIO $ evaluate $ mockFn "b"
        pure ()) `shouldThrow` anyErrorCall

    it "fails when called but never expected" $ do
      withMock (do 
        mockFn <- mock (any ~> True)
          `expects` do
            called never `with` "z"

        liftIO $ evaluate $ mockFn "z"
        pure ()) `shouldThrow` anyErrorCall

  describe "withMock with runMockT" $ do
    it "can use runMockT inside withMock" $ do
      withMock $ do
        result <- runMockT do
          _readFile $ "input.txt" ~> pack "content"
          _writeFile $ "output.txt" ~> pack "content" ~> ()
          operationProgram "input.txt" "output.txt"

        liftIO $ result `shouldBe` ()

  describe "order verification" $ do
    it "calledInOrder succeeds when called in correct order" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInOrder ["a", "b", "c"]

        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

    it "calledInOrder fails when called in wrong order" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInOrder ["a", "b", "c"]

        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "c" `shouldBe` True  -- Wrong order: should be "b"
        liftIO $ mockFn "b" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "calledInOrder fails when not all calls are made" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInOrder ["a", "b", "c"]

        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True
        -- missing: mockFn "c"
        pure ()) `shouldThrow` anyErrorCall

    it "calledInSequence succeeds when sequence is followed" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInSequence ["a", "c"]

        liftIO $ mockFn "a" `shouldBe` True
        liftIO $ mockFn "b" `shouldBe` True  -- This is ignored
        liftIO $ mockFn "c" `shouldBe` True
        pure ()

    it "calledInSequence fails when sequence is violated" $ do
      withMock (do 
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInSequence ["a", "c"]

        liftIO $ mockFn "c" `shouldBe` True  -- Wrong: "a" should come first
        liftIO $ mockFn "a" `shouldBe` True
        pure ()) `shouldThrow` anyErrorCall

    it "calledInSequence succeeds with extra calls in between" $ do
      withMock $ do 
        mockFn <- mock (any @String ~> True)
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
        fn1 <- mock (any @String ~> True)
          `expects` do
            called once `with` "a" 

        fn2 <- mock (any @String ~> any @String ~> False)
          `expects` do
            called once `with` ("x" ~> "y")

        liftIO $ fn1 "a" `shouldBe` True
        liftIO $ fn2 "x" "y" `shouldBe` False
        pure ()

  describe "withMock scope isolation" $ do
    it "mocks from different withMock blocks do not interfere" $ do
      -- First withMock block: expect one call
      withMock $ do 
        fn1 <- mock (any ~> True)
          `expects` do
            called once `with` "a"
        liftIO $ evaluate $ fn1 "a"

      -- Second withMock block: expect zero (if leaked, would see 1 and fail)
      withMock $ do 
        _ <- mock (any ~> True)
          `expects` do
            called never `with` "a"
        pure ()

    it "multiple sequential withMock blocks are independent" $ do
      -- Block 1: call with "x"
      withMock $ do
        fn1 <- mock (any ~> True)
          `expects` do
            called once `with` "x"
        liftIO $ evaluate $ fn1 "x"

      -- Block 2: call with "y"
      withMock $ do
        fn2 <- mock (any ~> True)
          `expects` do
            called once `with` "y"
        liftIO $ evaluate $ fn2 "y"

      -- Block 3: no calls
      withMock $ do
        _ <- mock (any ~> True)
          `expects` do
            called never `with` "z"
        pure ()

  describe "withMock concurrency" $ do
    it "counts calls across parallel threads" $ do
      withMock $ do
        mockFn <- mock (any ~> True)
          `expects` do
            called (times 10)

        withRunInIO $ \runInIO -> do
          as <- forM [1 .. 10] $ \i ->
            async $ runInIO $ do
              liftIO $ evaluate $ perCall i (mockFn "a")
              pure ()
          mapM_ wait as

    it "handles concurrent calls with different arguments" $ do
      withMock $ do
        mockFn <- mock (any ~> True)
          `expects` do
            called (times 5) `with` "a"
            called (times 5) `with` "b"

        withRunInIO $ \runInIO -> do
          as1 <- forM [1 .. 5] $ \i ->
            async $ runInIO $ liftIO $ evaluate $ perCall i (mockFn "a")
          as2 <- forM [1 .. 5] $ \i ->
            async $ runInIO $ liftIO $ evaluate $ perCall (100 + i) (mockFn "b")
          mapM_ wait (as1 ++ as2)

    it "stress test: many threads with many calls" $ do
      let threads = 20 :: Int
          callsPerThread = 10 :: Int
          total = threads * callsPerThread :: Int

      withMock $ do
        mockFn <- mock (any ~> True)
          `expects` do
            called (times total)

        withRunInIO $ \runInIO -> do
          as <- forM [1 .. threads] $ \threadIx ->
            async $ runInIO $ do
              forM_ [1 .. callsPerThread] $ \callIx -> do
                let tag = threadIx * 1000 + callIx
                liftIO $ evaluate $ perCall tag (mockFn "stress")
                liftIO $ threadDelay 1
          mapM_ wait as

    it "concurrent calls preserve order expectations" $ do
      withMock $ do
        mockFn <- mock (any @String ~> True)
          `expects` do
            calledInOrder ["first", "second", "third"]

        withRunInIO $ \runInIO -> do
          -- Sequential calls to preserve order
          runInIO $ void $ liftIO $ evaluate $ mockFn "first"
          runInIO $ void $ liftIO $ evaluate $ mockFn "second"
          runInIO $ void $ liftIO $ evaluate $ mockFn "third"

