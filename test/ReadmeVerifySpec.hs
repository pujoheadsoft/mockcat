{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ReadmeVerifySpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (readFile, writeFile, any)
import Data.List (isPrefixOf)

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

makeMock [t|FileSystem|]

-- Dummy program for makeMock example
myProgram :: FileSystem m => FilePath -> m ()
myProgram _ = pure ()

spec :: Spec
spec = do
  describe "README Examples" $ do
    
    it "Quick Start" $ do
      result <- runMockT do
        -- 1. Create a mock
        f <- mock (("Hello" :: String) ~> (42 :: Int))
          `expects` called once

        -- 2. Use it
        let result = f "Hello"
        _ <- liftIO $ evaluate result
        pure result
      
      -- 3. Verify result
      result `shouldBe` 42

    it "Before / After (After)" $ do
      withMock $ do
        f <- mock (("a" :: String) ~> ("b" :: String))
          `expects` called once
        
        -- Execution
        let res = f "a"
        _ <- liftIO $ evaluate res
        pure ()

    it "User Guide 1. Declarative Verification" $ do
      withMock $ do
        f <- mock ((any :: Param String) ~> True)
          `expects` do
            called once `with` ("arg" :: String)
        
        -- Execution
        let res = f "arg"
        _ <- liftIO $ evaluate res
        pure ()

    it "User Guide 1. withMockIO" $ do
      withMockIO $ do
        f <- mock ((any :: Param String) ~> (pure @IO ("result" :: String)))
        -- res <- someIOCall f -- simulated
        -- In real test we would pass f. simulating usage:
        r <- f "arg"
        r `shouldBe` "result"

    it "User Guide 3. Function Mocking" $ do
      -- "a" -> "b" -> True
      f <- mock (("a" :: String) ~> ("b" :: String) ~> True)
      f "a" "b" `shouldBe` True
      f `shouldBeCalled` (("a" :: String) ~> ("b" :: String))
    
    it "User Guide 3. Flexible Matching" $ do
      f1 <- mock ((any :: Param String) ~> True)
      f1 "something" `shouldBe` True

      f2 <- mock (when (> (5 :: Int)) "> 5" ~> True)
      f2 6 `shouldBe` True

    it "User Guide 2. makeMock (Strict)" $ do
      result <- runMockT do
        _readFile $ ("config.txt" :: String) ~> (pure @IO "debug=true" :: IO String)
        _writeFile $ ("log.txt" :: String) ~> ("start" :: String) ~> (pure @IO () :: IO ())
        myProgram "config.txt"
      result `shouldBe` ()



    it "Matchers: when with label" $ do
       f <- mock do
         onCase $ when (\s -> "error" `isPrefixOf` s) "start with error" ~> False
         onCase $ any ~> True
       
       f "error code" `shouldBe` False
       f "ok value" `shouldBe` True

    it "Matchers: when_ without label" $ do
       f <- mock (when_ (> (5 :: Int)) ~> True)
       f 6 `shouldBe` True
