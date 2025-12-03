{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Test.MockCat.UnsafeCheck where

import GHC.IO (unsafePerformIO)
import Test.Inspection (inspect, doesNotUse)
import Test.MockCat

simpleStubExample :: String -> Bool
simpleStubExample = stub $ "value" |> True

monadicMockExample :: IO (String -> IO Bool)
monadicMockExample = mockM $ "value" |> True

pureMockExample :: IO (String -> Bool)
pureMockExample = mock $ "value" |> True

inspect $ 'simpleStubExample `doesNotUse` 'unsafePerformIO
inspect $ 'monadicMockExample `doesNotUse` 'unsafePerformIO
inspect $ 'pureMockExample `doesNotUse` 'unsafePerformIO

{-
-- The following check demonstrates that applying the inspection to a pure
-- binding built via `mock` indeed fails (because `mock` relies on
-- unsafePerformIO internally). We keep it commented out so the suite passes.
{-# NOINLINE pureMockPure #-}
pureMockPure :: String -> Bool
pureMockPure = unsafePerformIO $ do
  f <- mock $ "value" |> True
  pure f

inspect $ 'pureMockPure `doesNotUse` 'unsafePerformIO
-}

