{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Test.MockCat.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat
import Prelude hiding (writeFile, readFile, and, any, not, or)
import GHC.IO (evaluate)
import Data.Text hiding (any)
import Control.Monad.Trans.Maybe (runMaybeT)

class (Monad m) => FileOperation m where
  writeFile :: FilePath -> Text -> m ()
  readFile :: FilePath -> m Text

makeMock [t|FileOperation|]

operationProgram ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
operationProgram inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content

class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

echo :: Teletype m => m ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }

spec :: Spec
spec = do
  it "echo1" do
    result <- runMockT do
      _readTTY $ pure @IO ""
      echo
    result `shouldBe` ()

  it "echo2" do
    result <- runMockT do
      _readTTY $ do
        onCase $ pure @IO "a"
        onCase $ pure @IO ""

      _writeTTY $ "a" |> pure @IO ()
      echo
    result `shouldBe` ()

  it "echo3" do
    result <- runMockT do
      _readTTY $ casesIO ["a", ""]
      _writeTTY $ "a" |> pure @IO ()
      echo
    result `shouldBe` ()

  it "echo4" do
    result <- runMockT do
      _readTTY $ cases [ pure @IO "a", pure @IO "" ]
      _writeTTY $ "a" |> pure @IO ()
      echo
    result `shouldBe` ()

  it "read & write" do
    result <- runMockT do
      _readFile $ "input.txt" |> pack "Content"
      _writeFile $ "output.text" |> pack "Content" |> ()
      operationProgram "input.txt" "output.text"

    result `shouldBe` ()

  it "stub" do
    -- create a stub function
    stubFn <- createStubFn $ "value" |> True
    -- assert
    stubFn "value" `shouldBe` True

  it "stub & verify" do
    -- create a mock
    mockFn <- createStubFn $ "value" |> True
    -- assert
    mockFn "value" `shouldBe` True
    -- verify
    mockFn `shouldApplyTo` "value"

  it "how to use" do
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()

  it "named stub" do
    f <- createNamedStubFn "named stub" $ "x" |> "y" |> True
    f "x" "y" `shouldBe` True

  it "createStubFnIO returns monadic stub (IO)" do
    f <- createStubFnIO $ "a" |> (11 :: Int) |> False
    f "a" (11 :: Int) `shouldReturn` False

  it "createStubFnIO returns monadic stub (MaybeT)" do
    mm <- runMaybeT do
      -- create a mock inside MaybeT
      createStubFnIO $ True |> False
    case mm of
      Nothing -> expectationFailure "createStubFnIO returned Nothing"
      Just f -> do
        -- f :: Bool -> MaybeT IO Bool
        res <- runMaybeT $ f True
        res `shouldBe` Just False

  it "named mock" do
    f <- createNamedStubFn "mock" $ "value" |> "a" |> True
    f "value" "a" `shouldBe` True

  it "stub function" do
    f <- createStubFn $ "value" |> True
    f "value" `shouldBe` True

  it "shouldApplyTimes" do
    f <- createStubFn $ "value" |> True
    print $ f "value"
    print $ f "value"
    f `shouldApplyTimes` (2 :: Int) `to` "value"

  it "shouldApplyInOrder" do
    f <- createStubFn $ any |> True |> ()
    print $ f "a" True
    print $ f "b" True
    f
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]

  it "shouldApplyInPartialOrder" do
    f <- createStubFn $ any |> True |> ()
    print $ f "a" True
    print $ f "b" True
    print $ f "c" True
    f
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]

  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"

  it "expect" do
    f <- createStubFn $ expect (> (5 :: Int)) "> 5" |> "return value"
    f 6 `shouldBe` "return value"

  it "expect_" do
    f <- createStubFn $ expect_ (> (5 :: Int)) |> "return value"
    f 6 `shouldBe` "return value"

  it "expectByExpr" do
    f <- createStubFn $ $(expectByExpr [|(> (5 :: Int))|]) |> "return value"
    f 6 `shouldBe` "return value"

  it "multi" do
    f <- createStubFn do
      onCase $ "a" |> "return x"
      onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"

  it "Return different values for the same argument" do
    f <- createStubFn $ do
      onCase $ "arg" |> "x"
      onCase $ "arg" |> "y"

    -- Do not allow optimization to remove duplicates.
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, “y” is returned.
