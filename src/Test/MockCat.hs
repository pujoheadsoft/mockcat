{-|
Module      : Test.MockCat
Description : Type-safe mocking with a single arrow '~>'
Copyright   : (c) Setup, 2025
License     : MIT
Maintainer  : kenji

MOCKCAT is a lightweight, declarative mocking library for Haskell.
It provides a unified way to create stubs and mocks using the dedicated 'Mock Arrow' operator ('~>').

= Quick Start

> {-# LANGUAGE BlockArguments #-}
> import Test.Hspec
> import Test.MockCat
>
> spec :: Spec
> spec = do
>   it "Quick Start" do
>     -- 1. Create a mock ('Hello' -> 42)
>     f <- mock $ "Hello" ~> (42 :: Int)
>
>     -- 2. Use it
>     f "Hello" `shouldBe` 42
>
>     -- 3. Verify it was called
>     f `shouldBeCalled` "Hello"

= Concepts

* __Stub__: A pure function ('stub') that stands in for a dependency. It returns fixed values but does not record calls.
* __Mock__: A verifiable function ('mock' / 'mockM') that records its interactions. Use 'shouldBeCalled' to verify them.
* __Mock Arrow ('~>')__: The core operator for defining expectations. @arg1 ~> arg2 ~> result@.

= Main Components

* "Test.MockCat.Mock": Core functions for creating mocks and stubs ('mock', 'stub').
* "Test.MockCat.Param": Parameter matchers ('any', 'expect', '~>').
* "Test.MockCat.Verify": Verification functions ('shouldBeCalled', 'once', 'times').
* "Test.MockCat.TH": Template Haskell generators for typeclasses ('makeMock').

-}
module Test.MockCat
  ( module Test.MockCat.Mock,
    module Test.MockCat.Cons,
    module Test.MockCat.Param,
    module Test.MockCat.MockT,
    module Test.MockCat.TH,
    module Test.MockCat.WithMock
  )
where

import Test.MockCat.Mock
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.MockT
import Test.MockCat.TH
import Test.MockCat.WithMock