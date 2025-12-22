{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.MockCat.ErrorDiffSpec (spec) where

import Test.Hspec
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
            "function was not applied to the expected arguments.\n\
            \  expected: \"hello world\"\n\
            \   but got: \"hello haskell\"\n\
            \                   ^^^^^^^^"
      f `shouldBeCalled` "hello world" `shouldThrow` errorCall expectedError

    it "shows diff for long list" do
      f <- mock $ (any :: Param [Int]) |> "ok"
      _ <- evaluate $ f [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]
      let expectedError = 
            "function was not applied to the expected arguments.\n\
            \  expected: [1,2,3,4,5,6,7,8,9,10]\n\
            \   but got: [1,2,3,4,5,0,7,8,9,10]\n\
            \                       ^^^^^^^^^^^"
      f `shouldBeCalled` [1..10] `shouldThrow` errorCall expectedError

    it "shows diff for record" do
      f <- mock $ (any :: Param User) |> "ok"
      _ <- evaluate $ f (User "kenji" 20)
      let expectedError = 
            "function was not applied to the expected arguments.\n\
            \  expected: User {name = \"kenji\", age = 30}\n\
            \   but got: User {name = \"kenji\",\"age = 20}\"\n\
            \                                 ^^^^^^^^^^^"
      f `shouldBeCalled` User "kenji" 30 `shouldThrow` errorCall expectedError

    it "shows diff for inOrderWith" do
      f <- mock $ (any :: Param String) |> "ok"
      _ <- evaluate $ f "a"
      _ <- evaluate $ f "b"
      let expectedError = 
            "function was not applied to the expected arguments in the expected order.\n\
            \  expected 2nd applied: \"c\"\n\
            \   but got 2nd applied: \"b\"\n\
            \                         ^^"
      f `shouldBeCalled` inOrderWith ["a", "c"] `shouldThrow` errorCall expectedError
