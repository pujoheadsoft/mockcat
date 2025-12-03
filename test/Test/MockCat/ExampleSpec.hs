{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (writeFile, readFile, and, any, not, or)
import GHC.IO (evaluate)
import Data.Text hiding (any)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Test.MockCat.SharedSpecDefs

makeMock [t|FileOperation|]

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content

echoProgram :: Teletype m => m ()
echoProgram = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echoProgram

makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }

spec :: Spec
spec = do
  it "function arg" do
    let
      f :: String -> String -> String
      f a b = a <> b
    stub <- mock $ "a" |> f |> True
    stub "a" f `shouldBe` True

  it "function arg2" do
    let
      -- Do not allow optimization to remove duplicates.
      {-# NOINLINE f #-}
      f :: String -> String -> String
      f a b = a <> b
      
      {-# NOINLINE g #-}
      g :: String -> String -> String
      g a b = a <> b

    stub <- mock $ "a" |> f |> g |> True

    -- Verify that calling with wrong function order raises an error
    evaluate (stub "a" g f) `shouldThrow` anyErrorCall

  it "echo1" do
    result <- runMockT do
      _readTTY $ pure @IO ""
      echoProgram
    result `shouldBe` ()

  it "echo2" do
    result <- runMockT do
      _readTTY $ do
        onCase $ pure @IO "a"
        onCase $ pure @IO ""

      _writeTTY $ "a" |> pure @IO ()
      echoProgram
    result `shouldBe` ()

  it "echo3" do
    result <- runMockT do
      _readTTY $ casesIO ["a", ""]
      _writeTTY $ "a" |> pure @IO ()
      echoProgram
    result `shouldBe` ()

  it "echo4" do
    result <- runMockT do
      _readTTY $ cases [ pure @IO "a", pure @IO "" ]
      _writeTTY $ "a" |> pure @IO ()
      echoProgram
    result `shouldBe` ()

  it "read & write" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "Content"
      _writeFile $ "output.text" |> pack "Content" |> ()
      operationProgram "input.txt" "output.text"

    result `shouldBe` ()

  it "stub" do
    -- create a stub function
    stubFn <- mock $ "value" |> True
    -- assert
    stubFn "value" `shouldBe` True

  it "stub & verify" do
    -- create a mock
    mockFn <- mock $ "value" |> True
    -- assert
    mockFn "value" `shouldBe` True

  it "how to use" do
    f <- mock $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()

  it "named stub" do
    f <- mock (label "named stub") $ "x" |> "y" |> True
    f "x" "y" `shouldBe` True

  it "mock returns monadic stub (IO)" do
    f <- mock $ "a" |> (11 :: Int) |> pure @IO False
    f "a" (11 :: Int) `shouldReturn` False

  it "mock returns monadic stub (MaybeT)" do
    mm <- runMaybeT do
      -- create a mock inside MaybeT
      mock $ True |> pure @(MaybeT IO) False
    case mm of
      Nothing -> expectationFailure "mock returned Nothing"
      Just f -> do
        -- f :: Bool -> MaybeT IO Bool
        res <- runMaybeT $ f True
        res `shouldBe` Just False

  it "named mock" do
    f <- mock (label "mock") $ "value" |> "a" |> True
    f "value" "a" `shouldBe` True

  it "stub function" do
    f <- mock $ "value" |> True
    f "value" `shouldBe` True

  it "any" do
    f <- mock $ any |> "return value"
    f "something" `shouldBe` "return value"

  it "expect" do
    f <- mock $ expect (> (5 :: Int)) "> 5" |> "return value"
    f 6 `shouldBe` "return value"

  it "expect_" do
    f <- mock $ expect_ (> (5 :: Int)) |> "return value"
    f 6 `shouldBe` "return value"

  it "expectByExpr" do
    f <- mock $ $(expectByExpr [|(> (5 :: Int))|]) |> "return value"
    f 6 `shouldBe` "return value"

  it "multi" do
    f <- mock do
      onCase $ "a" |> "return x"
      onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"

  it "Return different values for the same argument" do
    f <- mock $ do
      onCase $ "arg" |> "x"
      onCase $ "arg" |> "y"

    -- Do not allow optimization to remove duplicates.
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, “y” is returned.
