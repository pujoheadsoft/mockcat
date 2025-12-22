{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.MockCat.ErrorDiffSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldThrow, errorCall)
import Test.MockCat
import Control.Exception (evaluate)
import Prelude hiding (any)
import GHC.Generics (Generic)

data User = User { name :: String, age :: Int } deriving (Show, Eq, Generic)

spec :: Spec
spec = do
  describe "Error Message Diff" do
    it "shows diff for string arguments" do
      f <- mock $ (any :: Param String) |> "ok"
      _ <- evaluate $ f "hello haskell"
      let expectedError = 
            "function was not called with the expected arguments.\n\
            \  expected: hello world\n\
            \   but got: hello haskell\n\
            \                  ^^^^^^^"
      f `shouldBeCalled` "hello world" `shouldThrow` errorCall expectedError

    it "shows diff for long list" do
      f <- mock $ (any :: Param [Int]) |> "ok"
      _ <- evaluate $ f [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]
      let expectedError = 
            "function was not called with the expected arguments.\n\
            \  expected: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\
            \   but got: [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]\n\
            \                            ^^^^^^^^^^^^^^^"
      f `shouldBeCalled` [1..10] `shouldThrow` errorCall expectedError

    it "shows diff for record" do
      f <- mock $ (any :: Param User) |> "ok"
      _ <- evaluate $ f (User "Fagen" 20)
      let expectedError = 
            "function was not called with the expected arguments.\n\
            \  Specific difference in `age`:\n\
            \    expected: 30\n\
            \     but got: 20\n\
            \             ^^\n\
            \\n\
            \Full context:\n\
            \  expected: User {name = \"Fagen\", age = 30}\n\
            \   but got: User {name = \"Fagen\", age = 20}\n\
            \                                        ^^^"
      f `shouldBeCalled` User "Fagen" 30 `shouldThrow` errorCall expectedError

    it "shows diff for inOrderWith" do
      f <- mock $ (any :: Param String) |> "ok"
      _ <- evaluate $ f "a"
      _ <- evaluate $ f "b"
      let expectedError = 
            "function was not called with the expected arguments in the expected order.\n\
            \  expected 2nd call: \"c\"\n\
            \   but got 2nd call: \"b\"\n\
            \                      ^^"
      f `shouldBeCalled` inOrderWith ["a", "c"] `shouldThrow` errorCall expectedError

    it "chooses nearest neighbor in multi-case mock" do
      f <- mock $ do
        onCase $ "aaa" |> (100 :: Int) |> "ok"
        onCase $ "bbb" |> (200 :: Int) |> "ok"
      
      let expectedError = 
            "function was not called with the expected arguments.\n\
            \  expected one of the following:\n\
            \    \"aaa\", 100\n\
            \    \"bbb\", 200\n\
            \  but got:\n\
            \    \"aaa\", 200\n\
            \           ^^^"
      evaluate (f "aaa" 200) `shouldThrow` errorCall expectedError
