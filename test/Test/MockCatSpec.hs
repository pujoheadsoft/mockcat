{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCatSpec (spec) where

import Prelude hiding (any)
import Test.Hspec
import Test.MockCat (
  mock,
  fun, hasBeenCalledWith, hasBeenCalledTimes, with, hasBeenCalledInOrder, hasBeenCalledTimesGreaterThanEqual,
    hasBeenCalledTimesLessThanEqual,
    hasBeenCalledTimesGreaterThan,
    hasBeenCalledTimesLessThan, hasBeenCalledInPartialOrder)
import Test.MockCat.Param (any, (|>))
import Data.Function ((&))
import qualified Control.Exception as E

spec :: Spec
spec = do
  describe "Test of Mock" do
    mockTest Fixture {
      name = "arity = 1",
      create = mock $ "a" |> False,
      execute = (`fun` "a"),
      executeFailed = Just (`fun` "x"),
      expected = False,
      verifyMock = (`hasBeenCalledWith` "a"),
      verifyFailed = (`hasBeenCalledWith` "2"),
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` "a"
    }

    mockTest Fixture {
      name = "arity = 2",
      create = mock $ "a" |> "b" |> True,
      execute = \m -> fun m "a" "b",
      executeFailed = Just (\m -> fun m "a" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b",
      verifyFailed = \m -> hasBeenCalledWith m $ "2" |> "b",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b")
    }

    mockTest Fixture {
      name = "arity = 3",
      create = mock $ "a" |> "b" |> "c" |> False,
      execute = \m -> fun m "a" "b" "c",
      executeFailed = Just (\m -> fun m "a" "b" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "d",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c")
    }

    mockTest Fixture {
      name = "arity = 4",
      create = mock $ "a" |> "b" |> "c" |> "d" |> True,
      execute = \m -> fun m "a" "b" "c" "d",
      executeFailed = Just (\m -> fun m "a" "b" "c" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d")
    }

    mockTest Fixture {
      name = "arity = 5",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e")
    }

    mockTest Fixture {
      name = "arity = 6",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> True,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "x"),
      expected = True,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f")
    }

    mockTest Fixture {
      name = "arity = 7",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f" "g",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "f" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "y",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g")
    }

    mockTest Fixture {
      name = "arity = 8",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f" "g" "h",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "f" "g" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h")
    }

    mockTest Fixture {
      name = "arity = 9",
      create = mock $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i" |> False,
      execute = \m -> fun m "a" "b" "c" "d" "e" "f" "g" "h" "i",
      executeFailed = Just (\m -> fun m "a" "b" "c" "d" "e" "f" "g" "h" "x"),
      expected = False,
      verifyMock = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i",
      verifyFailed = \m -> hasBeenCalledWith m $ "a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "x",
      verifyCount = \m c -> m `hasBeenCalledTimes` c `with` ("a" |> "b" |> "c" |> "d" |> "e" |> "f" |> "g" |> "h" |> "i")
    }

  describe "Test of Multi Mock" do
    mockTest Fixture {
      name = "arity = 1",
      create = mock [
        "1" |> True,
        "2" |> False
      ],
      expected = [
        True,
        False
      ],
      execute = \m -> [fun m "1", fun m "2"],
      executeFailed = Just \m -> [ fun m "3" ],
      verifyMock = \m -> do
        m `hasBeenCalledWith` "1"
        m `hasBeenCalledWith` "2"
      ,
      verifyFailed = (`hasBeenCalledWith` "3"),
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` "1"
        m `hasBeenCalledTimes` c `with` "2"
    }

    mockTest Fixture {
      name = "arity = 2", 
      create = mock [
        "1" |> "2" |> True,
        "2" |> "3" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2", 
        fun m "2" "3"
      ],
      executeFailed = Just \m -> [ fun m "1" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2")
        m `hasBeenCalledWith` ("2" |> "3")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2")
        m `hasBeenCalledTimes` c `with` ("2" |> "3")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "x")
    }

    mockTest Fixture {
      name = "arity = 3", 
      create = mock [
        "1" |> "2" |> "3" |> True,
        "2" |> "3" |> "4" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3", 
        fun m "2" "3" "4"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3")
        m `hasBeenCalledWith` ("2" |> "3" |> "4")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "x")
    }

    mockTest Fixture {
      name = "arity = 4", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> True,
        "2" |> "3" |> "4"  |> "5" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4", 
        fun m "2" "3" "4" "5"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3" |> "4")
        m `hasBeenCalledWith` ("2" |> "3" |> "4" |> "5")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3" |> "4")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4" |> "5")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3" |> "x")
    }

    mockTest Fixture {
      name = "arity = 5", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> "5" |> True,
        "2" |> "3" |> "4"  |> "5" |> "6" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4" "5", 
        fun m "2" "3" "4" "5" "6"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "4" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5")
        m `hasBeenCalledWith` ("2" |> "3" |> "4"  |> "5" |> "6")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3"  |> "4" |> "5")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4"  |> "5" |> "6")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "x")
    }

    mockTest Fixture {
      name = "arity = 6", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> True,
        "2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4" "5" "6", 
        fun m "2" "3" "4" "5" "6" "7"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "4" "5" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6")
        m `hasBeenCalledWith` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "x")
    }

    mockTest Fixture {
      name = "arity = 7", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> True,
        "2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4" "5" "6" "7", 
        fun m "2" "3" "4" "5" "6" "7" "8"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "4" "5" "6" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7")
        m `hasBeenCalledWith` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "x")
    }

    mockTest Fixture {
      name = "arity = 8", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8" |> True,
        "2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4" "5" "6" "7" "8", 
        fun m "2" "3" "4" "5" "6" "7" "8" "9"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "4" "5" "6" "7" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8")
        m `hasBeenCalledWith` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "x")
    }

    mockTest Fixture {
      name = "arity = 9", 
      create = mock [
        "1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8" |> "9"  |> True,
        "2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9" |> "10" |> False
      ],
      expected = [
        True, 
        False
      ], 
      execute = \m -> [
        fun m "1" "2" "3" "4" "5" "6" "7" "8" "9", 
        fun m "2" "3" "4" "5" "6" "7" "8" "9" "10"
      ],
      executeFailed = Just \m -> [ fun m "1" "2" "3" "4" "5" "6" "7" "8" "x" ],
      verifyMock = \m -> do 
        m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
        m `hasBeenCalledWith` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9" |> "10")
      ,
      verifyCount = \m c -> do
        m `hasBeenCalledTimes` c `with` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8" |> "9")
        m `hasBeenCalledTimes` c `with` ("2" |> "3" |> "4"  |> "5" |> "6" |> "7" |> "8" |> "9" |> "10")
      ,
      verifyFailed = \m -> m `hasBeenCalledWith` ("1" |> "2" |> "3"  |> "4" |> "5" |> "6" |> "7" |> "8" |> "x")
    }

  describe "Order Verification" do
    describe "exactly sequential order." do
      mockOrderTest VerifyOrderFixture {
        name = "arity = 1",
        create = mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          evaluate $ fun m "b"
          evaluate $ fun m "c",
        verifyMock = \m -> m `hasBeenCalledInOrder` [
          "a",
          "b",
          "c"
        ],
        verifyFailed = \m -> m `hasBeenCalledInOrder` [
          "a",
          "b",
          "b"
        ]
      }

      mockOrderTest VerifyOrderFixture {
        name = "arity = 9",
        create = mock $ any |> any |> any |> any |> any |> any |> any |> any |> (),
        execute = \m -> do
          evaluate $ fun m "1" "2" "3" "4" "5" "6" "7" "8"
          evaluate $ fun m "2" "3" "4" "5" "6" "7" "8" "9"
          evaluate $ fun m "3" "4" "5" "6" "7" "8" "9" "0",
        verifyMock = \m -> m `hasBeenCalledInOrder` [
          "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8",
          "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9",
          "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "0"
        ],
        verifyFailed = \m -> m `hasBeenCalledInOrder` [
          "1" |> "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "x",
          "2" |> "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "y",
          "3" |> "4" |> "5" |> "6" |> "7" |> "8" |> "9" |> "z"
        ]
      }

      mockOrderTest VerifyOrderFixture {
        name = "number of function calls doesn't match the number of params.", 
        create = mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          pure (),
        verifyMock = \m -> m `hasBeenCalledInOrder` [
          "a"
        ],
        verifyFailed = \m -> m `hasBeenCalledInOrder` [
          "a",
          "b"
        ]
      }

    describe "partially sequential order." do
      mockOrderTest VerifyOrderFixture {
        name = "arity = 1", 
        create = mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          evaluate $ fun m "b"
          evaluate $ fun m "c"
          pure (),
        verifyMock = \m -> m `hasBeenCalledInPartialOrder` [
          "a",
          "c"
        ],
        verifyFailed = \m -> m `hasBeenCalledInPartialOrder` [
          "b",
          "a"
        ]
      }

      mockOrderTest VerifyOrderFixture {
        name = "arity = 9", 
        create = mock $ any |> any |> any |> any |> any |> any |> any |> any |> any |> (),
        execute = \m -> do
          evaluate $ fun m "a" "" "" "" "" "" "" "" ""
          evaluate $ fun m "b" "" "" "" "" "" "" "" ""
          evaluate $ fun m "c" "" "" "" "" "" "" "" ""
          pure (),
        verifyMock = \m -> m `hasBeenCalledInPartialOrder` [
          "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
          "c" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
        ],
        verifyFailed = \m -> m `hasBeenCalledInPartialOrder` [
          "b" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> "",
          "a" |> "" |> "" |> "" |> "" |> "" |> "" |> "" |> ""
        ]
      }

      mockOrderTest VerifyOrderFixture {
        name = "Uncalled value specified.", 
        create =  mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          evaluate $ fun m "b"
          evaluate $ fun m "c"
          pure (),
        verifyMock = \m -> m `hasBeenCalledInPartialOrder` [
          "b",
          "c"
        ],
        verifyFailed = \m -> m `hasBeenCalledInPartialOrder` [
          "a",
          "d"
        ]
      }

      mockOrderTest VerifyOrderFixture {
        name = "number of function calls doesn't match the number of params", 
        create = mock $ any |> (),
        execute = \m -> do
          evaluate $ fun m "a"
          pure (),
        verifyMock = \m -> m `hasBeenCalledInPartialOrder` [
          "a"
        ],
        verifyFailed = \m -> m `hasBeenCalledInPartialOrder` [
          "a",
          "b"
        ]
      }

  describe "The number of times applied can also be verified by specifying conditions." do
    it "greater than equal" do
      m <- mock $ "a" |> True
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      m `hasBeenCalledTimesGreaterThanEqual` 3 `with` "a"

    it "less than equal" do
      m <- mock $ "a" |> False
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      m `hasBeenCalledTimesLessThanEqual` 3 `with` "a"

    it "greater than" do
      m <- mock $ "a" |> True
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      m `hasBeenCalledTimesGreaterThan` 2 `with` "a"

    it "less than" do
      m <- mock $ "a" |> False
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      evaluate $ fun m "a"
      m `hasBeenCalledTimesLessThan` 4 `with` "a"

  describe "Monad" do
    it "Return IO Monad." do
      m <- mock $ "Article Id" |> pure @IO "Article Title"

      result <- fun m "Article Id"

      result `shouldBe` "Article Title"
      
      m `hasBeenCalledWith` "Article Id"
          
  -- describe "mock" do
  --   it "fn" do
  --     m <- mock $ "a" |> "x"
  --     let
  --       f = fun m
  --       v = f "c"
  --     v `shouldBe` "x"
  --     m `hasBeenCalledWith` "a"
  -- describe "mock" do
  --   it "fn" do
  --     m <- mock ["a" |> "x", "b" |> "y"]
  --     let
  --       f = fun m
  --       v = [f "a", f "b"]
  --     mapM_ (\x -> evaluate (x `seq` ())) v
  --     --v `shouldBe` ["x", "y"]
  --     m `hasBeenCalledWith` "a"
  --     m `hasBeenCalledWith` "b"


data Fixture mock r = Fixture {
  name :: String,
  create :: IO mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> IO (),
  verifyFailed :: mock -> IO (),
  verifyCount :: mock -> Int -> IO ()
}

data VerifyOrderFixture mock r = VerifyOrderFixture {
  name :: String,
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
