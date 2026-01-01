{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-hpc #-}

module Test.MockCat.ShouldBeCalledErrorDiffSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldThrow, errorCall)
import Test.MockCat
import Test.MockCat.SharedSpecDefs (Post(..))
import Control.Exception (evaluate)
import Prelude hiding (any)
import GHC.Generics (Generic)

data User = User { name :: String, age :: Int } deriving (Show, Eq, Generic)
data Config = Config { theme :: String, level :: Int } deriving (Show, Eq, Generic)
data ComplexUser = ComplexUser { name :: String, config :: Config } deriving (Show, Eq, Generic)
data DeepNode = Leaf Int | Node { val :: Int, next :: DeepNode } deriving (Show, Eq, Generic)
data MultiLayer = MultiLayer
  { layer1 :: String,
    sub :: SubLayer
  } deriving (Show, Eq, Generic)
data SubLayer = SubLayer
  { layer2 :: String,
    items :: [DeepNode]
  } deriving (Show, Eq, Generic)

spec :: Spec
spec = do
  describe "Error Message Diff" do
    it "shows diff for string arguments" do
      f <- mock ((any :: Param String) ~> "ok")
      evaluate $ f "hello haskell"
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: \"hello world\"\n\
            \     but got: \"hello haskell\"\n\
            \            " <> replicate 9 ' ' <> "^^^^^^^^\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. \"hello haskell\""
      f `shouldBeCalled` "hello world" `shouldThrow` errorCall expectedError

    it "shows diff for user-defined type (Post)" do
      f <- mock (Post 2 "wrong" ~> "ok")
      evaluate $ f (Post 2 "wrong")
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: Post {postId = 1, title = \"title\"}\n\
            \     but got: Post {postId = 2, title = \"wrong\"}\n\
            \              " <> replicate 15 ' ' <> "^^^^^^^^^^^^^^^^^^^\n\
            \  Specific differences:\n\
            \    - `postId`:\n\
            \        expected: 1\n\
            \         but got: 2\n\
            \    - `title`:\n\
            \        expected: \"title\"\n\
            \         but got: \"wrong\"\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. Post {postId = 2, title = \"wrong\"}"
      f `shouldBeCalled` Post 1 "title" `shouldThrow` errorCall expectedError

    it "shows diff for long list" do
      f <- mock ((any :: Param [Int]) ~> "ok")
      evaluate $ f [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\
            \     but got: [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]\n\
            \            " <> replicate 18 ' ' <> "^^^^^^^^^^^^^^^\n\
            \  Specific difference in `[5]`:\n\
            \    expected: 6\n\
            \     but got: 0\n\
            \              ^\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]"
      f `shouldBeCalled` [1..10] `shouldThrow` errorCall expectedError

    it "shows diff for record" do
      f <- mock ((any :: Param User) ~> "ok")
      evaluate $ f (User "Fagen" 20)
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: User {name = \"Fagen\", age = 30}\n\
            \     but got: User {name = \"Fagen\", age = 20}\n\
            \            " <> replicate 30 ' ' <> "^^^\n\
            \  Specific difference in `age`:\n\
            \    expected: 30\n\
            \     but got: 20\n\
            \              ^^\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. User {name = \"Fagen\", age = 20}"
      f `shouldBeCalled` User "Fagen" 30 `shouldThrow` errorCall expectedError

    it "shows diff for inOrderWith" do
      f <- mock ((any :: Param String) ~> "ok")
      evaluate $ f "a"
      evaluate $ f "b"
      let expectedError =
            "function was not called with the expected arguments in the expected order.\n\
            \  expected 2nd call: \"c\"\n\
            \   but got 2nd call: \"b\"\n\
            \                      ^^"
      f `shouldBeCalled` inOrderWith ["a", "c"] `shouldThrow` errorCall expectedError

    it "chooses nearest neighbor in multi-case mock" do
      f <- mock $ do
        onCase $ "aaa" ~> (100 :: Int) ~> "ok"
        onCase $ "bbb" ~> (200 :: Int) ~> "ok"

      let expectedError =
            "function was not called with the expected arguments.\n\
            \  expected one of the following:\n\
            \    \"aaa\", 100\n\
            \    \"bbb\", 200\n\
            \  but got:\n\
            \    \"aaa\", 200\n\
            \           ^^^"
      evaluate (f "aaa" 200) `shouldThrow` errorCall expectedError

    it "shows diff for nested record" do
      f <- mock ((any :: Param ComplexUser) ~> "ok")
      evaluate $ f (ComplexUser "Alice" (Config "Light" 1))
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: ComplexUser {name = \"Alice\", config = Config {theme = \"Dark\", level = 1}}\n\
            \     but got: ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 1}}\n\
            \            " <> replicate 57 ' ' <> replicate 19 '^' <> "\n\
            \  Specific difference in `config.theme`:\n\
            \    expected: \"Dark\"\n\
            \     but got: \"Light\"\n\
            \               ^^^^^^\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 1}}"
      f `shouldBeCalled` ComplexUser "Alice" (Config "Dark" 1) `shouldThrow` errorCall expectedError

    it "shows diff for nested list" do
      f <- mock ((any :: Param [[Int]]) ~> "ok")
      evaluate $ f [[1, 2], [3, 4]]
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: [[1,2], [3,5]]\n\
            \     but got: [[1,2], [3,4]]\n\
            \            " <> replicate 13 ' ' <> "^^^\n\
            \  Specific difference in `[1][1]`:\n\
            \    expected: 5\n\
            \     but got: 4\n\
            \              ^\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. [[1,2], [3,4]]"
      f `shouldBeCalled` [[1, 2], [3, 5]] `shouldThrow` errorCall expectedError

    it "shows multiple differences in nested record" do
      f <- mock ((any :: Param ComplexUser) ~> "ok")
      let actual = ComplexUser "Alice" (Config "Light" 2)
          expected = ComplexUser "Bob" (Config "Dark" 1)
      evaluate $ f actual
      let expectedError =
            "function was not called with the expected arguments.\n\
            \\n\
            \  Closest match:\n\
            \    expected: ComplexUser {name = \"Bob\", config = Config {theme = \"Dark\", level = 1}}\n\
            \     but got: ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 2}}\n\
            \            " <> replicate 23 ' ' <> replicate 53 '^' <> "\n\
            \  Specific differences:\n\
            \    - `name`:\n\
            \        expected: \"Bob\"\n\
            \         but got: \"Alice\"\n\
            \    - `config.theme`:\n\
            \        expected: \"Dark\"\n\
            \         but got: \"Light\"\n\
            \    - `config.level`:\n\
            \        expected: 1\n\
            \         but got: 2\n\
            \\n\
            \  Call history (1 calls):\n\
            \    [Closest] 1. ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 2}}"
      f `shouldBeCalled` expected `shouldThrow` errorCall expectedError

    describe "robustness (broken formats)" do
      it "handles unbalanced braces gracefully (fallback to standard message)" do
         f <- mock ((any :: Param String) ~> "ok")
         let actual = "{ name = \"Alice\""
             expected = "{ name = \"Bob\" }"
         evaluate $ f actual
         let expectedError =
               "function was not called with the expected arguments.\n\
               \\n\
               \  Closest match:\n\
               \    expected: \"{ name = \\\"Bob\\\" }\"\n\
               \     but got: \"{ name = \\\"Alice\\\"\"\n\
               \            " <> replicate 14 ' ' <> "^^^^^^^^\n\
               \\n\
               \  Call history (1 calls):\n\
               \    [Closest] 1. \"{ name = \\\"Alice\\\"\""
         f `shouldBeCalled` expected `shouldThrow` errorCall expectedError

      it "handles completely broken structure strings" do
         f <- mock ((any :: Param String) ~> "ok")
         let actual = "NotARecord {,,,,,}"
         let expected = "NotARecord { a = 1 }"
         evaluate $ f actual
         let expectedError =
               "function was not called with the expected arguments.\n\
               \\n\
               \  Closest match:\n\
               \    expected: \"NotARecord { a = 1 }\"\n\
               \     but got: \"NotARecord {,,,,,}\"\n\
               \            " <> replicate 15 ' ' <> "^^^^^^^^^\n\
               \\n\
               \  Call history (1 calls):\n\
               \    [Closest] 1. \"NotARecord {,,,,,}\""
         f `shouldBeCalled` expected `shouldThrow` errorCall expectedError

    describe "extreme structural cases" do
      it "handles extremely deep nesting" do
        f <- mock ((any :: Param DeepNode) ~> "ok")
        -- 5 layers deep
        let actual = Node{val = 1, next = Node{val = 2, next = Node{val = 3, next = Node{val = 4, next = Node{val = 5, next = Leaf 0}}}}}
            expected = Node{val = 1, next = Node{val = 2, next = Node{val = 3, next = Node{val = 4, next = Node{val = 5, next = Leaf 1}}}}}
        evaluate $ f actual
        let expectedError =
              "function was not called with the expected arguments.\n" <>
              "\n" <>
              "  Closest match:\n" <>
              "    expected: Node {val = 1, next = Node {val = 2, next = Node {val = 3, next = Node {val = 4, next = Node {val = 5, next = Leaf 1}}}}}\n" <>
              "     but got: Node {val = 1, next = Node {val = 2, next = Node {val = 3, next = Node {val = 4, next = Node {val = 5, next = Leaf 0}}}}}\n" <>
              "            " <> replicate 117 ' ' <> "^^^^^^\n" <>
              "  Specific difference in `next.next.next.next.next`:\n" <>
              "    expected: Leaf 1\n" <>
              "     but got: Leaf 0\n" <>
              "              " <> replicate 5 ' ' <> "^\n" <>
              "\n" <>
              "  Call history (1 calls):\n" <>
              "    [Closest] 1. Node {val = 1, next = Node {val = 2, next = Node {val = 3, next = Node {val = 4, next = Node {val = 5, next = Leaf 0}}}}}"
        f `shouldBeCalled` expected `shouldThrow` errorCall expectedError

      it "shows mismatches across multiple layers" do
        f <- mock ((any :: Param MultiLayer) ~> "ok")
        let actual = MultiLayer {layer1 = "A", sub = SubLayer {layer2 = "B", items = [Node {val = 1, next = Leaf 2}]}}
            expected = MultiLayer {layer1 = "X", sub = SubLayer {layer2 = "Y", items = [Node {val = 1, next = Leaf 3}]}}
        evaluate $ f actual
        let expectedError =
              "function was not called with the expected arguments.\n" <>
              "\n" <>
              "  Closest match:\n" <>
              "    expected: MultiLayer {layer1 = \"X\", sub = SubLayer {layer2 = \"Y\", items = [Node {val = 1, next = Leaf 3}]}}\n" <>
              "     but got: MultiLayer {layer1 = \"A\", sub = SubLayer {layer2 = \"B\", items = [Node {val = 1, next = Leaf 2}]}}\n" <>
              "            " <> replicate 24 ' ' <> replicate 75 '^' <> "\n" <>
              "  Specific differences:\n" <>
              "    - `layer1`:\n" <>
              "        expected: \"X\"\n" <>
              "         but got: \"A\"\n" <>
              "    - `sub.layer2`:\n" <>
              "        expected: \"Y\"\n" <>
              "         but got: \"B\"\n" <>
              "    - `sub.items[0].next`:\n" <>
              "        expected: Leaf 3\n" <>
              "         but got: Leaf 2\n" <>
              "\n" <>
              "  Call history (1 calls):\n" <>
              "    [Closest] 1. MultiLayer {layer1 = \"A\", sub = SubLayer {layer2 = \"B\", items = [Node {val = 1, next = Leaf 2}]}}"
        f `shouldBeCalled` expected `shouldThrow` errorCall expectedError

      it "handles cases where almost everything is different" do
        f <- mock ((any :: Param Config) ~> "ok")
        let actual = Config "Light" 1
            expected = Config "Dark" 99
        evaluate $ f actual
        let expectedError =
              "function was not called with the expected arguments.\n" <>
              "\n" <>
              "  Closest match:\n" <>
              "    expected: Config {theme = \"Dark\", level = 99}\n" <>
              "     but got: Config {theme = \"Light\", level = 1}\n" <>
              "            " <> replicate 19 ' ' <> replicate 18 '^' <> "\n" <>
              "  Specific differences:\n" <>
              "    - `theme`:\n" <>
              "        expected: \"Dark\"\n" <>
              "         but got: \"Light\"\n" <>
              "    - `level`:\n" <>
              "        expected: 99\n" <>
              "         but got: 1\n" <>
              "\n" <>
              "  Call history (1 calls):\n" <>
              "    [Closest] 1. Config {theme = \"Light\", level = 1}"
        f `shouldBeCalled` expected `shouldThrow` errorCall expectedError
