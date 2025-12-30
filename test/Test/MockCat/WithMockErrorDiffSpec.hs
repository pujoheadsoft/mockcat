{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Test.MockCat.WithMockErrorDiffSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldThrow, errorCall, xdescribe, xit)
import Test.MockCat
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
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

instance WrapParam User where wrap v = ExpectValue v (show v)
instance WrapParam Config where wrap v = ExpectValue v (show v)
instance WrapParam ComplexUser where wrap v = ExpectValue v (show v)
instance WrapParam DeepNode where wrap v = ExpectValue v (show v)
instance WrapParam MultiLayer where wrap v = ExpectValue v (show v)
instance WrapParam SubLayer where wrap v = ExpectValue v (show v)

spec :: Spec
spec = do
  xdescribe "Error Message Diff" do
    xit "shows diff for string arguments" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  expected: \"hello world\"\n\
            \   but got: \"hello haskell\"\n\
            \                   ^^^^^^^^"
      withMock (do
        f <- mock $ (any :: Param String) ~> "ok" `expects` (called once `with` "hello world")
        _ <- liftIO $ evaluate $ f "hello haskell"
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows diff for long list" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  Specific difference in `[5]`:\n\
            \    expected: 6\n\
            \     but got: 0\n\
            \              ^\n\
            \\n\
            \Full context:\n\
            \  expected: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\
            \   but got: [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]\n\
            \                            ^^^^^^^^^^^^^^^"
      withMock (do
        f <- mock $ (any :: Param [Int]) ~> "ok" `expects` (called once `with` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        _ <- liftIO $ evaluate $ f [1, 2, 3, 4, 5, 0, 7, 8, 9, 10]
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows diff for record" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  Specific difference in `age`:\n\
            \    expected: 30\n\
            \     but got: 20\n\
            \              ^^\n\
            \\n\
            \Full context:\n\
            \  expected: User {name = \"Fagen\", age = 30}\n\
            \   but got: User {name = \"Fagen\", age = 20}\n\
            \                                        ^^^"
      withMock (do
        f <- mock $ User "Fagen" 30 ~> "ok" `expects` called once
        _ <- liftIO $ evaluate $ f (User "Fagen" 20)
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows diff for inOrderWith" do
      let expectedError =
            "function was not called with the expected arguments in the expected order.\n\
            \  expected 2nd call: \"c\"\n\
            \   but got 2nd call: \"b\"\n\
            \                      ^^"
      withMock (do
        f <- mock $ (any :: Param String) ~> "ok" `expects` calledInOrder ["a", "c"]
        _ <- liftIO $ evaluate $ f "a"
        _ <- liftIO $ evaluate $ f "b"
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "chooses nearest neighbor in multi-case mock" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  expected one of the following:\n\
            \    \"aaa\", 100\n\
            \    \"bbb\", 200\n\
            \  but got:\n\
            \    \"aaa\", 200\n\
            \           ^^^"
      withMock (do
        f <- mock $ do
          onCase $ "aaa" ~> (100 :: Int) ~> "ok"
          onCase $ "bbb" ~> (200 :: Int) ~> "ok"
        _ <- liftIO $ evaluate (f "aaa" 200)
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows diff for nested record" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  Specific difference in `config.theme`:\n\
            \    expected: \"Dark\"\n\
            \     but got: \"Light\"\n\
            \               ^^^^^^\n\
            \\n\
            \Full context:\n\
            \  expected: ComplexUser {name = \"Alice\", config = Config {theme = \"Dark\", level = 1}}\n\
            \   but got: ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 1}}\n\
            \                                                                   ^^^^^^^^^^^^^^^^^^^"
      withMock (do
        f <- mock $ (any :: Param ComplexUser) ~> "ok" `expects` (called once `with` ComplexUser "Alice" (Config "Dark" 1))
        _ <- liftIO $ evaluate $ f (ComplexUser "Alice" (Config "Light" 1))
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows diff for nested list" do
      let expectedError =
            "function was not called with the expected arguments.\n\
            \  Specific difference in `[1][1]`:\n\
            \    expected: 5\n\
            \     but got: 4\n\
            \              ^\n\
            \\n\
            \Full context:\n\
            \  expected: [[1,2], [3,5]]\n\
            \   but got: [[1,2], [3,4]]\n\
            \                       ^^^"
      withMock (do
        f <- mock $ (any :: Param [[Int]]) ~> "ok" `expects` (called once `with` [[1, 2], [3, 5]])
        _ <- liftIO $ evaluate $ f [[1, 2], [3, 4]]
        pure ()
        ) `shouldThrow` errorCall expectedError

    it "shows multiple differences in nested record" do
      let actual = ComplexUser "Alice" (Config "Light" 2)
          expected = ComplexUser "Bob" (Config "Dark" 1)
          expectedError =
            "function was not called with the expected arguments.\n\
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
            \Full context:\n\
            \  expected: ComplexUser {name = \"Bob\", config = Config {theme = \"Dark\", level = 1}}\n\
            \   but got: ComplexUser {name = \"Alice\", config = Config {theme = \"Light\", level = 2}}\n\
            \                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      withMock (do
        f <- mock $ (any :: Param ComplexUser) ~> "ok" `expects` (called once `with` expected)
        _ <- liftIO $ evaluate $ f actual
        pure ()
        ) `shouldThrow` errorCall expectedError

    describe "robustness (broken formats)" do
      it "handles unbalanced braces gracefully (fallback to standard message)" do
         let actual = "{ name = \"Alice\""
             expected = "{ name = \"Bob\" }"
             expectedError =
                "function was not called with the expected arguments.\n\
                \  expected: \"{ name = \\\"Bob\\\" }\"\n\
                \   but got: \"{ name = \\\"Alice\\\"\"\n\
                \                        ^^^^^^^^"
         withMock (do
           f <- mock $ (any :: Param String) ~> "ok" `expects` (called once `with` expected)
           _ <- liftIO $ evaluate $ f actual
           pure ()
           ) `shouldThrow` errorCall expectedError

      it "handles completely broken structure strings" do
         let actual = "NotARecord {,,,,,}"
             expected = "NotARecord { a = 1 }"
             expectedError =
                "function was not called with the expected arguments.\n\
                \  expected: \"NotARecord { a = 1 }\"\n\
                \   but got: \"NotARecord {,,,,,}\"\n\
                \                         ^^^^^^^^^"
         withMock (do
           f <- mock $ (any :: Param String) ~> "ok" `expects` (called once `with` expected)
           _ <- liftIO $ evaluate $ f actual
           pure ()
           ) `shouldThrow` errorCall expectedError

    describe "extreme structural cases" do
      it "handles extremely deep nesting" do
        -- 5 layers deep
        let actual = Node{val = 1, next = Node{val = 2, next = Node{val = 3, next = Node{val = 4, next = Node{val = 5, next = Leaf 0}}}}}
            expected = Node{val = 1, next = Node{val = 2, next = Node{val = 3, next = Node{val = 4, next = Node{val = 5, next = Leaf 1}}}}}
            expectedError =
              "function was not called with the expected arguments.\n" <>
              "  Specific difference in `next.next.next.next.next`:\n" <>
              "    expected: Leaf 1\n" <>
              "     but got: Leaf 0\n" <>
              "              " <> replicate 5 ' ' <> "^\n" <>
              "\n" <>
              "Full context:\n" <>
              "  expected: Node {val = 1, next = Node {val = 2, next = Node {val = 3, next = Node {val = 4, next = Node {val = 5, next = Leaf 1}}}}}\n" <>
              "   but got: Node {val = 1, next = Node {val = 2, next = Node {val = 3, next = Node {val = 4, next = Node {val = 5, next = Leaf 0}}}}}\n" <>
              "            " <> replicate 115 ' ' <> "^^^^^^"
        withMock (do
          f <- mock $ (any :: Param DeepNode) ~> "ok" `expects` (called once `with` expected)
          _ <- liftIO $ evaluate $ f actual
          pure ()
          ) `shouldThrow` errorCall expectedError

      it "shows mismatches across multiple layers" do
        let actual = MultiLayer {layer1 = "A", sub = SubLayer {layer2 = "B", items = [Node {val = 1, next = Leaf 2}]}}
            expected = MultiLayer {layer1 = "X", sub = SubLayer {layer2 = "Y", items = [Node {val = 1, next = Leaf 3}]}}
            expectedError =
              "function was not called with the expected arguments.\n" <>
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
              "Full context:\n" <>
              "  expected: MultiLayer {layer1 = \"X\", sub = SubLayer {layer2 = \"Y\", items = [Node {val = 1, next = Leaf 3}]}}\n" <>
              "   but got: MultiLayer {layer1 = \"A\", sub = SubLayer {layer2 = \"B\", items = [Node {val = 1, next = Leaf 2}]}}\n" <>
              "            " <> replicate 22 ' ' <> replicate 75 '^'
        withMock (do
          f <- mock $ (any :: Param MultiLayer) ~> "ok" `expects` (called once `with` expected)
          _ <- liftIO $ evaluate $ f actual
          pure ()
          ) `shouldThrow` errorCall expectedError

      it "handles cases where almost everything is different" do
        let actual = Config "Light" 1
            expected = Config "Dark" 99
            expectedError =
              "function was not called with the expected arguments.\n" <>
              "  Specific differences:\n" <>
              "    - `theme`:\n" <>
              "        expected: \"Dark\"\n" <>
              "         but got: \"Light\"\n" <>
              "    - `level`:\n" <>
              "        expected: 99\n" <>
              "         but got: 1\n" <>
              "\n" <>
              "Full context:\n" <>
              "  expected: Config {theme = \"Dark\", level = 99}\n" <>
              "   but got: Config {theme = \"Light\", level = 1}\n" <>
              "            " <> replicate 17 ' ' <> replicate 18 '^'
        withMock (do
          f <- mock $ (any :: Param Config) ~> "ok" `expects` (called once `with` expected)
          _ <- liftIO $ evaluate $ f actual
          pure ()
          ) `shouldThrow` errorCall expectedError
