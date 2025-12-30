{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-hpc #-}
module ReadmeVerifySpec where

import Test.Hspec
import Test.MockCat
import Prelude hiding (readFile, writeFile, any)
import Control.Monad (when)

-- -------------------------------------------------------
-- 2. Typeclass Mocking Definitions
-- -------------------------------------------------------
class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Strict Mode]
makeMock [t|FileSystem|]

-- Dummy program for Typeclass Mocking
myProgram :: FileSystem m => FilePath -> m ()
myProgram path = do
  content <- readFile path
  when (content == "debug=true") $ writeFile "log.txt" "start"


spec :: Spec
spec = do
  describe "Quick Start" do
    it "Quick Start Demo" do
      -- 1. Create a mock
      f <- mock $ "Hello" ~> (42 :: Int)

      -- 2. Use it
      let result = f "Hello"
      result `shouldBe` 42

      {- 
        Note: The README includes:
        -- 3. Verify
        f `shouldBeCalled` "Hello"

        Without OverloadedStrings, "Hello" is String.
        f :: String -> Int.
        shouldBeCalled expects Matcher (String -> Int).
        "Hello" works as matcher implies `Param String`.
      -}
      f `shouldBeCalled` "Hello"

  describe "Function Mocking" do
    it "Basic" do
      f <- mock $ "a" ~> "b" ~> True
      let r = f "a" "b"
      r `shouldBe` True

    it "Condition" do
      f <- mock $ expect (> 5) "> 5" ~> True
      let r = f (6 :: Int)
      r `shouldBe` True

    it "Any" do
      f <- mock $ any ~> True
      let r = f ("something" :: String)
      r `shouldBe` True

  describe "Typeclass Mocking" do
    it "filesystem test (Strict)" do
      result <- runMockT do
        _readFile $ "config.txt" ~> pure @IO "debug=true"
        _writeFile $ "log.txt" ~> "start" ~> pure @IO ()
        myProgram "config.txt"
      result `shouldBe` ()
