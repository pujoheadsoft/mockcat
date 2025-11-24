{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
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
import Control.Monad.Trans.Maybe (runMaybeT)
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
    stub <- createMockFn $ "a" |> f |> True
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

    stub <- createMockFn $ "a" |> f |> g |> True

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
    stubFn <- createMockFn $ "value" |> True
    -- assert
    stubFn "value" `shouldBe` True

  it "stub & verify" do
    -- create a mock
    mockFn <- createMockFn $ "value" |> True
    -- assert
    mockFn "value" `shouldBe` True
    -- verify
    mockFn `shouldApplyTo` "value"

  it "how to use" do
    f <- createMockFn $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()

  it "named stub" do
    f <- createNamedMockFn "named stub" $ "x" |> "y" |> True
    f "x" "y" `shouldBe` True

  it "createMockFnIO returns monadic stub (IO)" do
    f <- createMockFnIO $ "a" |> (11 :: Int) |> False
    f "a" (11 :: Int) `shouldReturn` False

  it "createMockFnIO returns monadic stub (MaybeT)" do
    mm <- runMaybeT do
      -- create a mock inside MaybeT
      createMockFnIO $ True |> False
    case mm of
      Nothing -> expectationFailure "createMockFnIO returned Nothing"
      Just f -> do
        -- f :: Bool -> MaybeT IO Bool
        res <- runMaybeT $ f True
        res `shouldBe` Just False

  it "named mock" do
    f <- createNamedMockFn "mock" $ "value" |> "a" |> True
    f "value" "a" `shouldBe` True

  it "stub function" do
    f <- createMockFn $ "value" |> True
    f "value" `shouldBe` True

  it "shouldApplyTimes" do
    f <- createMockFn $ "value" |> True
    print $ f "value"
    print $ f "value"
    f `shouldApplyTimes` (2 :: Int) `to` "value"

  it "shouldApplyInOrder" do
    f <- createMockFn $ any |> True |> ()
    print $ f "a" True
    print $ f "b" True
    f
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]

  it "shouldApplyInPartialOrder" do
    f <- createMockFn $ any |> True |> ()
    print $ f "a" True
    print $ f "b" True
    print $ f "c" True
    f
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]

  it "any" do
    f <- createMockFn $ any |> "return value"
    f "something" `shouldBe` "return value"

  it "expect" do
    f <- createMockFn $ expect (> (5 :: Int)) "> 5" |> "return value"
    f 6 `shouldBe` "return value"

  it "expect_" do
    f <- createMockFn $ expect_ (> (5 :: Int)) |> "return value"
    f 6 `shouldBe` "return value"

  it "expectByExpr" do
    f <- createMockFn $ $(expectByExpr [|(> (5 :: Int))|]) |> "return value"
    f 6 `shouldBe` "return value"

  it "multi" do
    f <- createMockFn do
      onCase $ "a" |> "return x"
      onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"

  it "Return different values for the same argument" do
    f <- createMockFn $ do
      onCase $ "arg" |> "x"
      onCase $ "arg" |> "y"

    -- Do not allow optimization to remove duplicates.
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, “y” is returned.
