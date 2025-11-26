{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeOperators #-}

module Test.MockCat.MockSpec (spec) where

import qualified Control.Exception as E
import Data.Function ((&))
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Test of Mock" do
    describe "combination test" do
      it "arity = 1" do
        f <- createMockFn $ True |> False
        f True `shouldBe` False

      it "arity = 2" do
        f <- createMockFn $ True |> False |> True
        f True False `shouldBe` True

      it "arity = 3" do
        f <- createMockFn $ True |> "False" |> True |> "False"
        f True "False" True `shouldBe` "False"

      it "arity = 4" do
        f <- createMockFn $ True |> "False" |> True |> "False" |> True
        f True "False" True "False" `shouldBe` True

      it "Param |> a" do
        f <- createMockFn $ any |> False
        f True `shouldBe` False

      it "Param |> (a |> b)" do
        f <- createMockFn $ any |> False |> True
        f True False `shouldBe` True

      it "a     |> (Param |> b)" do
        f <- createMockFn $ True |> any |> True
        f True False `shouldBe` True

      it "Param |> (Param |> a)" do
        f <- createMockFn $ any |> any |> True
        f True False `shouldBe` True

      it "a     |> (Param |> (Param |> a))" do
        f <- createMockFn $ "any" |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

      it "param |> (Param |> (Param |> a))" do
        f <- createMockFn $ any |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

    mockTest
      Fixture
        { name = "arity = 1",
          create = createMockFn $ "a" |> False,
          apply = \f -> f "a",
          applyFailed = Just (\f -> f "x"),
          expected = False,
          verifyApply = (`shouldApplyTo` "a"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = (`shouldApplyTo` "2"),
          verifyApplyCount = \f c -> f `shouldApplyTimes` c `to` "a"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create = createMockFn $ "a" |> "b" |> True,
          apply = \f -> f "a" "b",
          applyFailed = Just (\f -> f "a" "x"),
          expected = True,
          verifyApply = \f -> shouldApplyTo f $ "a" |> "b",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \f -> shouldApplyTo f $ "2" |> "b",
          verifyApplyCount = \f c -> f `shouldApplyTimes` c `to` ("a" |> "b")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create = createMockFn $ "a" |> "b" |> "c" |> False,
          apply = \f -> f "a" "b" "c",
          applyFailed = Just (\f -> f "a" "b" "x"),
          expected = False,
          verifyApply = \f -> shouldApplyTo f $ "a" |> "b" |> "c",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \f -> shouldApplyTo f $ "a" |> "b" |> "d",
          verifyApplyCount = \f c -> f `shouldApplyTimes` c `to` ("a" |> "b" |> "c")
        }


    mockTest
      Fixture
        { name = "arity = 9",
          create = createMockFn $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i" |> False,
          apply = \f -> f "a" "b" "c" "d" "e" "f" "g" "h" "i",
          applyFailed = Just (\f -> f "a" "b" "c" "d" "e" "f" "g" "h" "x"),
          expected = False,
          verifyApply = \f -> shouldApplyTo f $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = \f -> shouldApplyTo f $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "x",
          verifyApplyCount = \f c -> f `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i")
        }

  describe "Test of Multi Mock" do
    mockTest
      Fixture
        { name = "arity = 1",
          create =
            createMockFn $ do
              onCase $ "1" |> True
              onCase $ "2" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \f -> [f "1", f "2"],
          applyFailed = Just \f -> [f "3"],
          verifyApply = \f -> do
            f `shouldApplyTo` "1"
            f `shouldApplyTo` "2",
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyFailed = (`shouldApplyTo` "3"),
          verifyApplyCount = \f c -> do
            f `shouldApplyTimes` c `to` "1"
            f `shouldApplyTimes` c `to` "2"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create =
            createMockFn $ do
              onCase $ "1" |> "2" |> True
              onCase $ "2" |> "3" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \f ->
            [ f "1" "2",
              f "2" "3"
            ],
          applyFailed = Just \f -> [f "1" "x"],
          verifyApply = \f -> do
            f `shouldApplyTo` ("1" |> "2")
            f `shouldApplyTo` ("2" |> "3"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \f c -> do
            f `shouldApplyTimes` c `to` ("1" |> "2")
            f `shouldApplyTimes` c `to` ("2" |> "3"),
          verifyApplyFailed = \f -> f `shouldApplyTo` ("1" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create =
            createMockFn $ do
              onCase $ "1" |> "2" |> "3" |> True
              onCase $ "2" |> "3" |> "4" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \f ->
            [ f "1" "2" "3",
              f "2" "3" "4"
            ],
          applyFailed = Just \f -> [f "1" "2" "x"],
          verifyApply = \f -> do
            f `shouldApplyTo` ("1" |> "2" |> "3")
            f `shouldApplyTo` ("2" |> "3" |> "4"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \f c -> do
            f `shouldApplyTimes` c `to` ("1" |> "2" |> "3")
            f `shouldApplyTimes` c `to` ("2" |> "3" |> "4"),
          verifyApplyFailed = \f -> f `shouldApplyTo` ("1" |> "2" |> "x")
        }


    mockTest
      Fixture
        { name = "arity = 9",
          create =
            createMockFn $ do
              onCase $ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> True
              onCase $ "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10" |> False
              ,
          expected =
            [ True,
              False
            ],
          apply = \f ->
            [ f "1" "2" "3" "4" "5" "6" "7" "8" "9",
              f "2" "3" "4" "5" "6" "7" "8" "9" "10"
            ],
          applyFailed = Just \f -> [f "1" "2" "3" "4" "5" "6" "7" "8" "x"],
          verifyApply = \f -> do
            f `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            f `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyApplyAny = shouldApplyToAnything,
          verifyApplyCount = \f c -> do
            f `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            f `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyApplyFailed = \f -> f `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "x")
        }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMockFn $ any |> (),
            apply = \f -> do
              evaluate $ f "a"
              evaluate $ f "b"
              evaluate $ f "c",
            verifyApply = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "c"
                                       ],
            verifyApplyFailed = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "b"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMockFn $ any |> any |> any |> any |> any |> any |> any |> any |> (),
            apply = \f -> do
              evaluate $ f "1" "2" "3" "4" "5" "6" "7" "8"
              evaluate $ f "2" "3" "4" "5" "6" "7" "8" "9"
              evaluate $ f "3" "4" "5" "6" "7" "8" "9" "0",
            verifyApply = \f ->
              f
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "0"
                                       ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "y",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "z"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params.",
            create = createMockFn $ any |> (),
            apply = \f -> do
              evaluate $ f "a"
              pure (),
            verifyApply = \f ->
              f
                `shouldApplyInOrder` [ "a"
                                       ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInOrder` [ "a",
                                         "b"
                                       ]
          }

    describe "partially sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMockFn $ any |> (),
            apply = \f -> do
              evaluate $ f "a"
              evaluate $ f "b"
              evaluate $ f "c"
              pure (),
            verifyApply = \f ->
              f
                `shouldApplyInPartialOrder` [ "a",
                                                "c"
                                              ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInPartialOrder` [ "b",
                                                "a"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMockFn $ any |> any |> any |> any |> any |> any |> any |> any |> any |> (),
            apply = \f -> do
              evaluate $ f "a" "" "" "" "" "" "" "" ""
              evaluate $ f "b" "" "" "" "" "" "" "" ""
              evaluate $ f "c" "" "" "" "" "" "" "" "",
            verifyApply = \f ->
              f
                `shouldApplyInPartialOrder` [ "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "c" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInPartialOrder` [ "b" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "Uncalled value specified.",
            create = createMockFn $ any |> (),
            apply = \f -> do
              evaluate $ f "a"
              evaluate $ f "b"
              evaluate $ f "c"
              pure (),
            verifyApply = \f ->
              f
                `shouldApplyInPartialOrder` [ "b",
                                                "c"
                                              ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInPartialOrder` [ "a",
                                                "d"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params",
            create = createMockFn $ any |> (),
            apply = \f -> do
              evaluate $ f "a"
              pure (),
            verifyApply = \f ->
              f
                `shouldApplyInPartialOrder` [ "a"
                                              ],
            verifyApplyFailed = \f ->
              f
                `shouldApplyInPartialOrder` [ "a",
                                                "b"
                                              ]
          }

  describe "The number of times applied can also be verified by specifying conditions." do
    describe "createMockFn" do
      it "greater than equal (success)" do
        f <- createMockFn $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThanEqual` 3 `to` "a"

      it "greater than equal (failure)" do
        f <- createMockFn $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThanEqual` 3 `to` "a" `shouldThrow` anyErrorCall

      it "less than equal (success)" do
        f <- createMockFn $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThanEqual` 3 `to` "a"

      it "less than equal (failure)" do
        f <- createMockFn $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThanEqual` 2 `to` "a" `shouldThrow` anyErrorCall

      it "greater than (success)" do
        f <- createMockFn $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThan` 2 `to` "a"

      it "greater than (failure)" do
        f <- createMockFn $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThan` 2 `to` "a" `shouldThrow` anyErrorCall

      it "less than (success)" do
        f <- createMockFn $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThan` 4 `to` "a"

      it "less than (failure)" do
        f <- createMockFn $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThan` 3 `to` "a" `shouldThrow` anyErrorCall

    describe "createNamedMockFn" do
      it "greater than equal (success)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThanEqual` 3 `to` "a"

      it "greater than equal (failure)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThanEqual` 3 `to` "a" `shouldThrow` anyErrorCall

      it "less than equal (success)" do
        f <- createMockFn (label "test") $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThanEqual` 3 `to` "a"

      it "less than equal (failure)" do
        f <- createMockFn (label "test") $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThanEqual` 2 `to` "a" `shouldThrow` anyErrorCall

      it "greater than (success)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThan` 2 `to` "a"

      it "greater than (failure)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesGreaterThan` 2 `to` "a" `shouldThrow` anyErrorCall

      it "less than (success)" do
        f <- createMockFn (label "test") $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThan` 4 `to` "a"

      it "less than (failure)" do
        f <- createMockFn (label "test") $ "a" |> False
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesLessThan` 3 `to` "a" `shouldThrow` anyErrorCall

      it "shouldApplyTimesToAnything (success)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesToAnything` 3

      it "shouldApplyTimesToAnything (failure)" do
        f <- createMockFn (label "test") $ "a" |> True
        evaluate $ f "a"
        evaluate $ f "a"
        f `shouldApplyTimesToAnything` 3 `shouldThrow` anyErrorCall

  describe "Monad" do
    it "Return IO Monad." do
      f <- createMockFn $ "Article Id" |> pure @IO "Article Title"

      result <- f "Article Id"

      result `shouldBe` "Article Title"

      f `shouldApplyTo` "Article Id"

  describe "createMockFnIO" do
    describe "shouldApplyTo" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        f `shouldApplyTo` ("a" |> (1 :: Int))
      
      it "failure (arguments mismatch)" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        f "b" (1 :: Int) `shouldThrow` anyErrorCall

      it "failure" do
        f <- createMockFnIO $ any |> pure @IO True
        _ <- f "A"
        f `shouldApplyTo` "X"
          `shouldThrow` errorCall
            "function was not applied to the expected arguments.\n\
            \  expected: \"X\"\n\
            \   but got: \"A\""

    describe "shouldApplyTimes" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimes` (3 :: Int) `to` ("a" |> (1 :: Int))

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimes` (3 :: Int) `to` ("a" |> (1 :: Int)) `shouldThrow` anyErrorCall

    describe "shouldApplyInOrder" do
      it "success" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldApplyInOrder` ["a" |> (1 :: Int), "b" |> (2 :: Int), "c" |> (3 :: Int)]

      it "failure" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldApplyInOrder` ["a" |> (1 :: Int), "b" |> (2 :: Int), "c" |> (3 :: Int)] `shouldThrow` anyErrorCall

    describe "shouldApplyInPartialOrder" do
      it "success" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldApplyInPartialOrder` ["a" |> (1 :: Int), "c" |> (3 :: Int)]

      it "failure" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyInPartialOrder` ["a" |> (1 :: Int), "b" |> (2 :: Int)] `shouldThrow` anyErrorCall

    describe "shouldApplyTimesGreaterThanEqual" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesGreaterThanEqual` 3 `to` ("a" |> (1 :: Int))

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesGreaterThanEqual` 3 `to` ("a" |> (1 :: Int)) `shouldThrow` anyErrorCall

    describe "shouldApplyTimesLessThanEqual" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesLessThanEqual` 3 `to` ("a" |> (1 :: Int))

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesLessThanEqual` 2 `to` ("a" |> (1 :: Int)) `shouldThrow` anyErrorCall

    describe "shouldApplyTimesGreaterThan" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesGreaterThan` 2 `to` ("a" |> (1 :: Int))

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesGreaterThan` 2 `to` ("a" |> (1 :: Int)) `shouldThrow` anyErrorCall

    describe "shouldApplyTimesLessThan" do
      it "success" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesLessThan` 4 `to` ("a" |> (1 :: Int))

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        f `shouldApplyTimesLessThan` 3 `to` ("a" |> (1 :: Int)) `shouldThrow` anyErrorCall

    describe "shouldApplyToAnything" do
      it "success" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "a" (1 :: Int)
        shouldApplyToAnything f

      it "failure" do
        f <- createMockFnIO $ "a" |> (1 :: Int) |> True
        shouldApplyToAnything f `shouldThrow` anyErrorCall

    describe "shouldApplyTimesToAnything" do
      it "success" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        _ <- f "c" (3 :: Int)
        f `shouldApplyTimesToAnything` 3

      it "failure" do
        f <- createMockFnIO $ any |> any |> True
        _ <- f "a" (1 :: Int)
        _ <- f "b" (2 :: Int)
        f `shouldApplyTimesToAnything` 3 `shouldThrow` anyErrorCall

    describe "named mock" do
      it "shouldApplyTo with name in error message" do
        f <- createMockFnIO (label "mockIO function") $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        f `shouldApplyTo` ("b" |> (1 :: Int)) `shouldThrow` anyErrorCall

      it "shouldApplyTimes with name in error message" do
        f <- createMockFnIO (label "mockIO function") $ "a" |> (1 :: Int) |> True
        _ <- f "a" (1 :: Int)
        _ <- f "a" (1 :: Int)
        let e =
              "function `mockIO function` was not applied the expected number of times to the expected arguments.\n\
              \  expected: 3\n\
              \   but got: 2"
        f `shouldApplyTimes` (3 :: Int) `to` ("a" |> (1 :: Int)) `shouldThrow` errorCall e

      it "shouldApplyInOrder with name in error message" do
        f <- createMockFnIO (label "mockIO function") $ any |> any |> True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        _ <- f "c" (3 :: Int)
        let e =
              "function `mockIO function` was not applied to the expected arguments in the expected order.\n\
              \  expected 1st applied: a,1\n\
              \   but got 1st applied: b,2\n\
              \  expected 2nd applied: b,2\n\
              \   but got 2nd applied: a,1"
        f `shouldApplyInOrder` ["a" |> (1 :: Int), "b" |> (2 :: Int), "c" |> (3 :: Int)] `shouldThrow` errorCall e

      it "shouldApplyInPartialOrder with name in error message" do
        f <- createMockFnIO (label "mockIO function") $ any |> any |> True
        _ <- f "b" (2 :: Int)
        _ <- f "a" (1 :: Int)
        let e =
              "function `mockIO function` was not applied to the expected arguments in the expected order.\n\
              \  expected order:\n\
              \    a,1\n\
              \    c,3\n\
              \  but got:\n\
              \    b,2\n\
              \    a,1"
        f `shouldApplyInPartialOrder` ["a" |> (1 :: Int), "c" |> (3 :: Int)] `shouldThrow` errorCall e

      it "shouldApplyToAnything with name in error message" do
        f <- createMockFnIO (label "mockIO function") $ "a" |> (1 :: Int) |> True
        shouldApplyToAnything f `shouldThrow` errorCall "It has never been applied function `mockIO function`"

  describe "Appropriate message when a test fails." do
    describe "anonymous mock" do
      describe "apply" do
        it "simple mock" do
          f <- createMockFn $ "a" |> pure @IO True
          f "b"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"a\"\n\
              \   but got: \"b\""

        it "multi mock" do
          f <-
            createMockFn $ do
              onCase $ "aaa" |> (100 :: Int) |> pure @IO True
              onCase $ "bbb" |> (200 :: Int) |> pure @IO False

          f "aaa" 200
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected one of the following:\n\
              \    \"aaa\",100\n\
              \    \"bbb\",200\n\
              \  but got:\n\
              \    \"aaa\",200"

      describe "verify" do
        it "simple mock verify" do
          f <- createMockFn $ any |> pure @IO True
          evaluate $ f "A"
          f `shouldApplyTo` "X"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"X\"\n\
              \   but got: \"A\""

        it "It has never been applied." do
          f <- createMockFn $ "X" |> pure @IO True
          f `shouldApplyTo` "X"
            `shouldThrow` errorCall
              "function was not applied to the expected arguments.\n\
              \  expected: \"X\"\n\
              \   but got: It has never been applied"

        it "count" do
          f <- createMockFn $ any |> pure @IO True
          evaluate $ f "A"
          let e =
                "function was not applied the expected number of times to the expected arguments.\n\
                \  expected: 2\n\
                \   but got: 1"
          f `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          f <- createMockFn $ any |> pure @IO False
          evaluate $ f "B"
          evaluate $ f "C"
          evaluate $ f "A"
          let e =
                "function was not applied to the expected arguments in the expected order.\n\
                \  expected 1st applied: \"A\"\n\
                \   but got 1st applied: \"B\"\n\
                \  expected 2nd applied: \"B\"\n\
                \   but got 2nd applied: \"C\"\n\
                \  expected 3rd applied: \"C\"\n\
                \   but got 3rd applied: \"A\""
          f `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          f <- createMockFn $ any |> True
          evaluate $ f "B"
          evaluate $ f "C"
          let e =
                "function was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 3\n\
                \   but got: 2"
          f `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          f <- createMockFn $ any |> True
          evaluate $ f "B"
          evaluate $ f "A"
          let e =
                "function was not applied to the expected arguments in the expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          f `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          f <- createMockFn $ any |> False
          evaluate $ f "B"
          let e =
                "function was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 2\n\
                \   but got: 1"
          f `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify applied anything" do
          f <- createMockFn $ "X" |> True
          shouldApplyToAnything f `shouldThrow` errorCall "It has never been applied function"

    describe "named mock" do
      describe "aply" do
        it "simple mock" do
          f <- createMockFn (label "mock function") $ "a" |> pure @IO ()
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected: \"a\"\n\
                \   but got: \"b\""
          f "b" `shouldThrow` errorCall e

        it "multi mock" do
          f <-
            createMockFn (label "mock function")
              do 
                onCase $ "aaa" |> True |> pure @IO True
                onCase $ "bbb" |> False |> pure @IO False
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected one of the following:\n\
                \    \"aaa\",True\n\
                \    \"bbb\",False\n\
                \  but got:\n\
                \    \"aaa\",False"
          f "aaa" False `shouldThrow` errorCall e

      describe "verify" do
        it "simple mock verify" do
          f <- createMockFn (label "mock function") $ any |> pure @IO ()
          evaluate $ f "A"
          let e =
                "function `mock function` was not applied to the expected arguments.\n\
                \  expected: \"X\"\n\
                \   but got: \"A\""
          f `shouldApplyTo` "X" `shouldThrow` errorCall e

        it "count" do
          f <- createMockFn (label "mock function") $ any |> pure @IO ()
          evaluate $ f "A"
          let e =
                "function `mock function` was not applied the expected number of times to the expected arguments.\n\
                \  expected: 2\n\
                \   but got: 1"
          f `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          f <- createMockFn (label "mock function") $ any |> pure @IO ()
          evaluate $ f "B"
          evaluate $ f "C"
          evaluate $ f "A"
          let e =
                "function `mock function` was not applied to the expected arguments in the expected order.\n\
                \  expected 1st applied: \"A\"\n\
                \   but got 1st applied: \"B\"\n\
                \  expected 2nd applied: \"B\"\n\
                \   but got 2nd applied: \"C\"\n\
                \  expected 3rd applied: \"C\"\n\
                \   but got 3rd applied: \"A\""
          f `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          f <- createMockFn (label "createMockFnc") $ any |> pure @IO ()
          evaluate $ f "B"
          evaluate $ f "C"
          let e =
                "function `createMockFnc` was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 3\n\
                \   but got: 2"
          f `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          f <- createMockFn (label "mock function") $ any |> pure @IO ()
          evaluate $ f "B"
          evaluate $ f "A"
          let e =
                "function `mock function` was not applied to the expected arguments in the expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          f `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          f <- createMockFn (label "createMockFnc") $ any |> pure @IO ()
          evaluate $ f "B"
          let e =
                "function `createMockFnc` was not applied to the expected arguments in the expected order (count mismatch).\n\
                \  expected: 2\n\
                \   but got: 1"
          f `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify applied anything" do
          f <- createMockFn (label "mock") $ "X" |> True
          shouldApplyToAnything f `shouldThrow` errorCall "It has never been applied function `mock`"

  describe "use expectation" do
    it "expectByExpr" do
      f <- createMockFn $ $(expectByExpr [|\x -> x == "y" || x == "z"|]) |> True
      f "y" `shouldBe` True

  describe "repeatable" do
    it "arity = 1" do
      f <- createMockFn $ do
        onCase $ "a" |> True
        onCase $ "b" |> False
        onCase $ "a" |> False
        onCase $ "b" |> True
        
      v1 <- evaluate $ f "a"
      v2 <- evaluate $ f "a"
      v3 <- evaluate $ f "b"
      v4 <- evaluate $ f "b"
      v1 `shouldBe` True
      v2 `shouldBe` False
      v3 `shouldBe` False
      v4 `shouldBe` True

    it "arity = 2" do
      f <- createMockFn $ do
        onCase $ "a" |> "b" |> (0 :: Int)
        onCase $ "a" |> "c" |> (1 :: Int)
        onCase $ "a" |> "b" |> (2 :: Int)
        onCase $ "a" |> "c" |> (3 :: Int)

      v1 <- evaluate $ f "a" "b"
      v2 <- evaluate $ f "a" "b"
      v3 <- evaluate $ f "a" "c"
      v4 <- evaluate $ f "a" "c"
      v5 <- evaluate $ f "a" "b"
      v1 `shouldBe` (0 :: Int)
      v2 `shouldBe` (2 :: Int)
      v3 `shouldBe` (1 :: Int)
      v4 `shouldBe` (3 :: Int)
      v5 `shouldBe` (2 :: Int)

  describe "constant" do
    it "createConstantMock" do
      f <- createMockFn "foo"
      f `shouldBe` "foo"
      shouldApplyToAnything f

    it "createNamedConstantMock" do
      f <- createMockFn (label "const") "foo"
      f `shouldBe` "foo"
      shouldApplyToAnything f

    it "createConstantMock (error message)" do
      f <- createMockFn "foo"
      shouldApplyToAnything f `shouldThrow` errorCall "It has never been applied function"

    it "createNamedConstantMock (error message)" do
      f <- createMockFn (label "constant") "foo"
      shouldApplyToAnything f `shouldThrow` errorCall "It has never been applied function `constant`"

    -- TODO: Fix this test
    -- it "createConstantMock with shouldApplyTimesToAnything (success)" do
    --   f <- createMockFn "foo"
    --   _ <- print f
    --   _ <- print f
    --   _ <- print f
    --   f `shouldApplyTimesToAnything` 3

    it "createConstantMock with shouldApplyTimesToAnything (failure)" do
      f <- createMockFn "foo"
      _ <- evaluate f
      _ <- evaluate f
      f `shouldApplyTimesToAnything` 3 `shouldThrow` anyErrorCall

    -- TODO: Fix this test
    -- it "createNamedConstantMock with shouldApplyTimesToAnything (success)" do
    --   f <- createNamedMockFn "const" "foo"
    --   _ <- f `shouldBe` "foo"
    --   _ <- f `shouldBe` "foo"
    --   _ <- f `shouldBe` "foo"
    --   f `shouldApplyTimesToAnything` 3

    it "createNamedConstantMock with shouldApplyTimesToAnything (failure)" do
      f <- createMockFn (label "const") "foo"
      _ <- evaluate f
      _ <- evaluate f
      f `shouldApplyTimesToAnything` 3 `shouldThrow` anyErrorCall

    it "verify constant IO mock" do
      f <- createMockFn $ pure @IO "foo"
      f `shouldReturn` "foo"
      f `shouldReturn` "foo"
      f `shouldReturn` "foo"
      f `shouldApplyTimesToAnything` 3

    it "verify constant IO mock (failure)" do
      f <- createMockFn $ pure @IO "foo"
      f `shouldReturn` "foo"
      f `shouldReturn` "foo"
      f `shouldApplyTimesToAnything` 3 `shouldThrow` anyErrorCall

    it "verify constant multi IO mock" do
      f <- createMockFn $ do
        onCase $ pure @IO "foo"
        onCase $ pure @IO "bar"
        onCase $ pure @IO "baz"

      f `shouldReturn` "foo"
      f `shouldReturn` "bar"
      f `shouldReturn` "baz"
      f `shouldApplyTimesToAnything` 3

data Fixture mock r = Fixture
  { name :: String,
    create :: IO mock,
    apply :: mock -> r,
    applyFailed :: Maybe (mock -> r),
    expected :: r,
    verifyApply :: mock -> IO (),
    verifyApplyAny :: mock -> IO (),
    verifyApplyFailed :: mock -> IO (),
    verifyApplyCount :: mock -> Int -> IO ()
  }

data VerifyOrderFixture mock r = VerifyOrderFixture
  { name :: String,
    create :: IO mock,
    apply :: mock -> IO r,
    verifyApply :: mock -> IO (),
    verifyApplyFailed :: mock -> IO ()
  }

class Eval a where
  evaluate :: a -> IO a

instance Eval [a] where
  evaluate v = do
    mapM_ (\x -> E.evaluate (x `seq` ())) v
    pure v

instance {-# OVERLAPPABLE #-} Eval a where
  evaluate = E.evaluate

-- mock test template
mockTest :: (Eq r, Show r, Eval r) => Fixture mock r -> SpecWith (Arg Expectation)
mockTest f = describe f.name do
  it "Expected argument is applied, the expected value is returned." do
    m <- f.create
    f.apply m `shouldBe` f.expected

  it "Unexpected argument is applied, an exception is thrown." do
    f.applyFailed & maybe mempty \func -> do
      m <- f.create
      evaluate (func m) `shouldThrow` anyErrorCall

  it "Expected arguments are applied, the verification succeeds." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApply m

  it "Unexpected arguments are applied, the verification fails." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyFailed m `shouldThrow` anyErrorCall

  it "The number of times a function has been applied can be verification (0 times)." do
    m <- f.create
    f.verifyApplyCount m 0

  it "The number of times a function has been applied can be verification (3 times)." do
    m <- f.create
    evaluate $ f.apply m
    evaluate $ f.apply m
    evaluate $ f.apply m
    f.verifyApplyCount m 3

  it "Fails to verification the number of times it has been applied, an exception is thrown." do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyCount m 3 `shouldThrow` anyErrorCall

  it "verify any" do
    m <- f.create
    evaluate $ f.apply m
    f.verifyApplyAny m

mockOrderTest :: VerifyOrderFixture mock r -> SpecWith (Arg Expectation)
mockOrderTest f = describe f.name do
  it "If the functions are applied in the expected order, the verification succeeds." do
    m <- f.create
    f.apply m
    f.verifyApply m

  it "If the functions are not applied in the expected order, verification fails." do
    m <- f.create
    f.apply m
    f.verifyApplyFailed m `shouldThrow` anyErrorCall
