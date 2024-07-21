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
import Test.MockCat.Mock
  ( any,
    expectByExpr,
    stubFn,
    shouldApplyInOrder,
    shouldApplyInPartialOrder,
    shouldApplyTimes,
    shouldApplyTimesGreaterThan,
    shouldApplyTimesGreaterThanEqual,
    shouldApplyTimesLessThan,
    shouldApplyTimesLessThanEqual,
    shouldApplyTo,
    createMock,
    createStubFn,
    createNamedMock,
    to,
    (|>)
  )
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Test of Mock" do
    describe "combination test" do
      it "arity = 1" do
        f <- createStubFn $ True |> False
        f True `shouldBe` False

      it "arity = 2" do
        f <- createStubFn $ True |> False |> True 
        f True False `shouldBe` True

      it "arity = 3" do
        f <- createStubFn $ True |> "False" |> True |> "False"
        f True "False" True `shouldBe` "False"

      it "arity = 4" do
        f <- createStubFn $ True |> "False" |> True |> "False" |> True
        f True "False" True "False" `shouldBe` True

      it "Param |> a" do
        f <- createStubFn $ any |> False
        f True `shouldBe` False

      it "Param |> (a |> b)" do
        f <- createStubFn $ any |> False |> True
        f True False `shouldBe` True

      it "a     |> (Param |> b)" do
        f <- createStubFn $ True |> any |> True
        f True False `shouldBe` True

      it "Param |> (Param |> a)" do
        f <- createStubFn $ any |> any |> True
        f True False `shouldBe` True

      it "a     |> (Param |> (Param |> a))" do
        f <- createStubFn $ "any" |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

      it "param |> (Param |> (Param |> a))" do
        f <- createStubFn $ any |> any |> any |> True
        f "any" "any" "any" `shouldBe` True

    mockTest
      Fixture
        { name = "arity = 1",
          create = createMock $ "a" |> False,
          execute = (`stubFn` "a"),
          executeFailed = Just (`stubFn` "x"),
          expected = False,
          verifyMock = (`shouldApplyTo` "a"),
          verifyFailed = (`shouldApplyTo` "2"),
          verifyCount = \m c -> m `shouldApplyTimes` c `to` "a"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create = createMock $ "a" |> "b" |> True,
          execute = \m -> stubFn m "a" "b",
          executeFailed = Just (\m -> stubFn m "a" "x"),
          expected = True,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b",
          verifyFailed = \m -> shouldApplyTo m $ "2" |> "b",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create = createMock $ "a" |> "b" |> "c" |> False,
          execute = \m -> stubFn m "a" "b" "c",
          executeFailed = Just (\m -> stubFn m "a" "b" "x"),
          expected = False,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "d",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c")
        }

    mockTest
      Fixture
        { name = "arity = 4",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> True,
          execute = \m -> stubFn m "a" "b" "c" "d",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "x"),
          expected = True,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "x",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d")
        }

    mockTest
      Fixture
        { name = "arity = 5",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> False,
          execute = \m -> stubFn m "a" "b" "c" "d" "e",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "d" "x"),
          expected = False,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "x",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e")
        }

    mockTest
      Fixture
        { name = "arity = 6",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> True,
          execute = \m -> stubFn m "a" "b" "c" "d" "e" "f",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "x"),
          expected = True,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "x",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f")
        }

    mockTest
      Fixture
        { name = "arity = 7",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> False,
          execute = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "x"),
          expected = False,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "y",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g")
        }

    mockTest
      Fixture
        { name = "arity = 8",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> False,
          execute = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "x"),
          expected = False,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "x",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h")
        }

    mockTest
      Fixture
        { name = "arity = 9",
          create = createMock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i" |> False,
          execute = \m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h" "i",
          executeFailed = Just (\m -> stubFn m "a" "b" "c" "d" "e" "f" "g" "h" "x"),
          expected = False,
          verifyMock = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i",
          verifyFailed = \m -> shouldApplyTo m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "x",
          verifyCount = \m c -> m `shouldApplyTimes` c `to` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i")
        }

  describe "Test of Multi Mock" do
    mockTest
      Fixture
        { name = "arity = 1",
          create =
            createMock
              [ "1" |> True,
                "2" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m -> [stubFn m "1", stubFn m "2"],
          executeFailed = Just \m -> [stubFn m "3"],
          verifyMock = \m -> do
            m `shouldApplyTo` "1"
            m `shouldApplyTo` "2",
          verifyFailed = (`shouldApplyTo` "3"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` "1"
            m `shouldApplyTimes` c `to` "2"
        }

    mockTest
      Fixture
        { name = "arity = 2",
          create =
            createMock
              [ "1" |> "2" |> True,
                "2" |> "3" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2",
              stubFn m "2" "3"
            ],
          executeFailed = Just \m -> [stubFn m "1" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2")
            m `shouldApplyTo` ("2" |> "3"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2")
            m `shouldApplyTimes` c `to` ("2" |> "3"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 3",
          create =
            createMock
              [ "1" |> "2" |> "3" |> True,
                "2" |> "3" |> "4" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3",
              stubFn m "2" "3" "4"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3")
            m `shouldApplyTo` ("2" |> "3" |> "4"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 4",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> True,
                "2" |> "3" |> "4" |> "5" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4",
              stubFn m "2" "3" "4" "5"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 5",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> "5" |> True,
                "2" |> "3" |> "4" |> "5" |> "6" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4" "5",
              stubFn m "2" "3" "4" "5" "6"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "4" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 6",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> True,
                "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6",
              stubFn m "2" "3" "4" "5" "6" "7"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 7",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> True,
                "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7",
              stubFn m "2" "3" "4" "5" "6" "7" "8"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 8",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> True,
                "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7" "8",
              stubFn m "2" "3" "4" "5" "6" "7" "8" "9"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "7" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x")
        }

    mockTest
      Fixture
        { name = "arity = 9",
          create =
            createMock
              [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> True,
                "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10" |> False
              ],
          expected =
            [ True,
              False
            ],
          execute = \m ->
            [ stubFn m "1" "2" "3" "4" "5" "6" "7" "8" "9",
              stubFn m "2" "3" "4" "5" "6" "7" "8" "9" "10"
            ],
          executeFailed = Just \m -> [stubFn m "1" "2" "3" "4" "5" "6" "7" "8" "x"],
          verifyMock = \m -> do
            m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            m `shouldApplyTo` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyCount = \m c -> do
            m `shouldApplyTimes` c `to` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
            m `shouldApplyTimes` c `to` ("2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "10"),
          verifyFailed = \m -> m `shouldApplyTo` ("1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "x")
        }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMock $ any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c",
            verifyMock = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "c"
                                       ],
            verifyFailed = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b",
                                         "b"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMock $ any |> any |> any |> any |> any |> any |> any |> any |> (),
            execute = \m -> do
              evaluate $ stubFn m "1" "2" "3" "4" "5" "6" "7" "8"
              evaluate $ stubFn m "2" "3" "4" "5" "6" "7" "8" "9"
              evaluate $ stubFn m "3" "4" "5" "6" "7" "8" "9" "0",
            verifyMock = \m ->
              m
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "0"
                                       ],
            verifyFailed = \m ->
              m
                `shouldApplyInOrder` [ "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x",
                                         "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "y",
                                         "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "z"
                                       ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params.",
            create = createMock $ any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a"
              pure (),
            verifyMock = \m ->
              m
                `shouldApplyInOrder` [ "a"
                                       ],
            verifyFailed = \m ->
              m
                `shouldApplyInOrder` [ "a",
                                         "b"
                                       ]
          }

    describe "partially sequential order." do
      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 1",
            create = createMock $ any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c"
              pure (),
            verifyMock = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "c"
                                              ],
            verifyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "b",
                                                "a"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "arity = 9",
            create = createMock $ any |> any |> any |> any |> any |> any |> any |> any |> any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a" "" "" "" "" "" "" "" ""
              evaluate $ stubFn m "b" "" "" "" "" "" "" "" ""
              evaluate $ stubFn m "c" "" "" "" "" "" "" "" ""
              pure (),
            verifyMock = \m ->
              m
                `shouldApplyInPartialOrder` [ "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "c" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ],
            verifyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "b" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
                                                "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "Uncalled value specified.",
            create = createMock $ any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a"
              evaluate $ stubFn m "b"
              evaluate $ stubFn m "c"
              pure (),
            verifyMock = \m ->
              m
                `shouldApplyInPartialOrder` [ "b",
                                                "c"
                                              ],
            verifyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "d"
                                              ]
          }

      mockOrderTest
        VerifyOrderFixture
          { name = "number of function calls doesn't match the number of params",
            create = createMock $ any |> (),
            execute = \m -> do
              evaluate $ stubFn m "a"
              pure (),
            verifyMock = \m ->
              m
                `shouldApplyInPartialOrder` [ "a"
                                              ],
            verifyFailed = \m ->
              m
                `shouldApplyInPartialOrder` [ "a",
                                                "b"
                                              ]
          }

  describe "The number of times applied can also be verified by specifying conditions." do
    it "greater than equal" do
      m <- createMock $ "a" |> True
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesGreaterThanEqual` 3 `to` "a"

    it "less than equal" do
      m <- createMock $ "a" |> False
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesLessThanEqual` 3 `to` "a"

    it "greater than" do
      m <- createMock $ "a" |> True
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesGreaterThan` 2 `to` "a"

    it "less than" do
      m <- createMock $ "a" |> False
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      evaluate $ stubFn m "a"
      m `shouldApplyTimesLessThan` 4 `to` "a"

  describe "Monad" do
    it "Return IO Monad." do
      m <- createMock $ "Article Id" |> pure @IO "Article Title"

      result <- stubFn m "Article Id"

      result `shouldBe` "Article Title"

      m `shouldApplyTo` "Article Id"

  describe "Appropriate message when a test fails." do
    describe "anonymous mock" do
      describe "apply" do
        it "simple mock" do
          m <- createMock $ "a" |> pure @IO True
          stubFn m "b"
            `shouldThrow` errorCall
              "expected arguments were not applied to the function.\n\
              \  expected: \"a\"\n\
              \   but got: \"b\""

        it "multi mock" do
          m <-
            createMock
              [ "aaa" |> (100 :: Int) |> pure @IO True,
                "bbb" |> (200 :: Int) |> pure @IO False
              ]
          stubFn m "aaa" 200
            `shouldThrow` errorCall
              "expected arguments were not applied to the function.\n\
              \  expected one of the following:\n\
              \    \"aaa\",100\n\
              \    \"bbb\",200\n\
              \  but got:\n\
              \    \"aaa\",200"

      describe "verify" do
        it "simple mock verify" do
          m <- createMock $ any |> pure @IO True
          evaluate $ stubFn m "A"
          m `shouldApplyTo` "X"
            `shouldThrow` errorCall
              "expected arguments were not applied to the function.\n\
              \  expected: \"X\"\n\
              \   but got: \"A\""

        it "count" do
          m <- createMock $ any |> pure @IO True
          evaluate $ stubFn m "A"
          let e =
                "function was not applied the expected number of times.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          m <- createMock $ any |> pure @IO False
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          evaluate $ stubFn m "A"
          let e =
                "function was not applied with expected order.\n\
                \  expected 1st call: \"A\"\n\
                \   but got 1st call: \"B\"\n\
                \  expected 2nd call: \"B\"\n\
                \   but got 2nd call: \"C\"\n\
                \  expected 3rd call: \"C\"\n\
                \   but got 3rd call: \"A\""
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          m <- createMock $ any |> True
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          let e =
                "function was not applied the expected number of times.\n\
                \  expected: 3\n\
                \   but got: 2"
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          m <- createMock $ any |> True
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "A"
          let e =
                "function was not applied with expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          m <- createMock $ any |> False
          evaluate $ stubFn m "B"
          let e =
                "function was not applied the expected number of times.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

    describe "named mock" do
      describe "aply" do
        it "simple mock" do
          m <- createNamedMock "mock function" $ "a" |> pure @IO ()
          let e =
                "expected arguments were not applied to the function `mock function`.\n\
                \  expected: \"a\"\n\
                \   but got: \"b\""
          stubFn m "b" `shouldThrow` errorCall e

        it "multi mock" do
          m <-
            createNamedMock
              "mock function"
              [ "aaa" |> True |> pure @IO True,
                "bbb" |> False |> pure @IO False
              ]
          let e =
                "expected arguments were not applied to the function `mock function`.\n\
                \  expected one of the following:\n\
                \    \"aaa\",True\n\
                \    \"bbb\",False\n\
                \  but got:\n\
                \    \"aaa\",False"
          stubFn m "aaa" False `shouldThrow` errorCall e

      describe "verify" do
        it "simple mock verify" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "A"
          let e =
                "expected arguments were not applied to the function `mock function`.\n\
                \  expected: \"X\"\n\
                \   but got: \"A\""
          m `shouldApplyTo` "X" `shouldThrow` errorCall e

        it "count" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied the expected number of times.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyTimes` (2 :: Int) `to` "A" `shouldThrow` errorCall e

        it "verify sequence" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied with expected order.\n\
                \  expected 1st call: \"A\"\n\
                \   but got 1st call: \"B\"\n\
                \  expected 2nd call: \"B\"\n\
                \   but got 2nd call: \"C\"\n\
                \  expected 3rd call: \"C\"\n\
                \   but got 3rd call: \"A\""
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify sequence (count mismatch)" do
          m <- createNamedMock "createStubFnc" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "C"
          let e =
                "function `createStubFnc` was not applied the expected number of times.\n\
                \  expected: 3\n\
                \   but got: 2"
          m `shouldApplyInOrder` ["A", "B", "C"] `shouldThrow` errorCall e

        it "verify partially sequence" do
          m <- createNamedMock "mock function" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          evaluate $ stubFn m "A"
          let e =
                "function `mock function` was not applied with expected order.\n\
                \  expected order:\n\
                \    \"A\"\n\
                \    \"C\"\n\
                \  but got:\n\
                \    \"B\"\n\
                \    \"A\""
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

        it "verify partially sequence (count mismatch)" do
          m <- createNamedMock "createStubFnc" $ any |> pure @IO ()
          evaluate $ stubFn m "B"
          let e =
                "function `createStubFnc` was not applied the expected number of times.\n\
                \  expected: 2\n\
                \   but got: 1"
          m `shouldApplyInPartialOrder` ["A", "C"] `shouldThrow` errorCall e

  describe "use expectation" do
    it "expectByExpr" do
      f <- createStubFn $ $(expectByExpr [|\x -> x == "y" || x == "z"|]) |> True
      f "y" `shouldBe` True

data Fixture mock r = Fixture
  { name :: String,
    create :: IO mock,
    execute :: mock -> r,
    executeFailed :: Maybe (mock -> r),
    expected :: r,
    verifyMock :: mock -> IO (),
    verifyFailed :: mock -> IO (),
    verifyCount :: mock -> Int -> IO ()
  }

data VerifyOrderFixture mock r = VerifyOrderFixture
  { name :: String,
    create :: IO mock,
    execute :: mock -> IO r,
    verifyMock :: mock -> IO (),
    verifyFailed :: mock -> IO ()
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
    f.execute m `shouldBe` f.expected

  it "Unexpected argument is applied, an exception is thrown." do
    f.executeFailed & maybe mempty \func -> do
      m <- f.create
      evaluate (func m) `shouldThrow` anyErrorCall

  it "Expected arguments are applied, the verification succeeds." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyMock m

  it "Unexpected arguments are applied, the verification fails." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyFailed m `shouldThrow` anyErrorCall

  it "The number of times a function has been applied can be verification (0 times)." do
    m <- f.create
    f.verifyCount m 0

  it "The number of times a function has been applied can be verification (3 times)." do
    m <- f.create
    evaluate $ f.execute m
    evaluate $ f.execute m
    evaluate $ f.execute m
    f.verifyCount m 3

  it "Fails to verification the number of times it has been applied, an exception is thrown." do
    m <- f.create
    evaluate $ f.execute m
    f.verifyCount m 3 `shouldThrow` anyErrorCall

mockOrderTest :: VerifyOrderFixture mock r -> SpecWith (Arg Expectation)
mockOrderTest f = describe f.name do
  it "If the functions are applied in the expected order, the verification succeeds." do
    m <- f.create
    f.execute m
    f.verifyMock m

  it "If the functions are not applied in the expected order, verification fails." do
    m <- f.create
    f.execute m
    f.verifyFailed m `shouldThrow` anyErrorCall
