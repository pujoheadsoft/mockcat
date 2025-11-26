# Diff report: feature/mock-io vs origin/main

Generated: 2025-11-24T11:34:49Z

## Summary

- Current branch: feature/mock-io
- Base branch: origin/main

### Commits on feature/mock-io not in origin/main

d774380 integrated mock function.
e3c1569 wip
b92c29c wip
9b91ed8 rename
d3ca454 wip
169c803 wip
16c25e9 wip
244b954 wip
d21ba26 support function param
cb793d1 wip
82a6afc wip
7def6e5 wip
41eba41 wip
40f8def wip
4f2ace0 wip
33292de wip
0454cb8 th refactor
11ae8ed th refactor
e071837 wip: refactor
a586a19 wip
b846d57 wip
1502439 wip
e4e69c2 wip
176b23c wip
50879db wip
a87a026 wip
705af1a wip
d408019 wip
1f90e93 support type families
db05af8 support default function
36af92e wip
9011d99 wip
d5fe622 wip
1935d27 wip
75da48f wip
04ae4b9 wip
b48e03c wip
dbba864 wip
ff7324c wip
88d2c8f wip
708053a wip
9853dfe wip
cbf3e17 wip
38cfeec wip
15f02a2 wip
ff658ba wip
a74fec1 wip
93a33ec wip
35d1fa5 wip
576bd47 wip
53d6ad3 remove mock & mockio objects
4044904 wip
b43d789 wip
6696aee wip: mock to stub
5728a46 wip
85703d7 wip
144c8a1 wip
5a168f1 wip
4829f0c wip
ca7d3d5 wip
24435eb add type family and class
08dc492 wip
d3e65ed add Registry
37123a8 wip
62fa3be wip
28608a5 wip
74fb2a4 wip
ef74424 wip
c8f0f28 wip
721194e wip
8436772 wip
5df9310 wip
8b23ed4 wip
cbf9764 wip
3c31772 wip
840765b wip
7bc1ead wip
517213a wip
6f0cc8e wip
ce44e42 wip
749a9ea wip
575c979 wip
44f871f wip
ccb9799 wip
e88f3c4 wip
35cd3b7 refactoring: polymorphic
1c4f39b add type class and instances
a256d7c format
734dfff wip
f2a2ec0 wip
94b584b added io mock

### Files changed (status)

A	.cursor/rules/answer.mdc
M	.github/workflows/test.yml
M	README-ja.md
M	README.md
A	diff.patch
M	mockcat.cabal
M	package.yaml
M	src/Test/MockCat/Cons.hs
A	src/Test/MockCat/Internal/Builder.hs
A	src/Test/MockCat/Internal/Message.hs
A	src/Test/MockCat/Internal/Registry.hs
A	src/Test/MockCat/Internal/Types.hs
M	src/Test/MockCat/Mock.hs
M	src/Test/MockCat/MockT.hs
M	src/Test/MockCat/Param.hs
M	src/Test/MockCat/TH.hs
A	src/Test/MockCat/TH/ClassAnalysis.hs
A	src/Test/MockCat/TH/ContextBuilder.hs
A	src/Test/MockCat/TH/FunctionBuilder.hs
A	src/Test/MockCat/TH/TypeUtils.hs
A	src/Test/MockCat/TH/Types.hs
A	src/Test/MockCat/Verify.hs
M	stack.yaml
M	stack.yaml.lock
A	test.sh
M	test/Property/AdditionalProps.hs
M	test/Property/ConcurrentCountProp.hs
M	test/Property/Generators.hs
M	test/Property/LazyEvalProp.hs
M	test/Property/OrderProps.hs
M	test/Property/ParamSpecNormalizeProp.hs
M	test/Property/ParamSpecRangeMergeRandomProp.hs
M	test/Property/ReinforcementProps.hs
M	test/Property/ScriptProps.hs
M	test/Spec.hs
M	test/Test/MockCat/ConcurrencySpec.hs
D	test/Test/MockCat/Definition.hs
M	test/Test/MockCat/ExampleSpec.hs
M	test/Test/MockCat/Impl.hs
A	test/Test/MockCat/Internal/RegistrySpec.hs
M	test/Test/MockCat/MockSpec.hs
M	test/Test/MockCat/ParamSpec.hs
M	test/Test/MockCat/PartialMockSpec.hs
M	test/Test/MockCat/PartialMockTHSpec.hs
A	test/Test/MockCat/SharedSpecDefs.hs
A	test/Test/MockCat/StubSpec.hs
A	test/Test/MockCat/TH/ClassAnalysisSpec.hs
A	test/Test/MockCat/TH/ContextBuilderSpec.hs
A	test/Test/MockCat/TH/FunctionBuilderSpec.hs
A	test/Test/MockCat/TH/TypeUtilsSpec.hs
A	test/Test/MockCat/THCompareSpec.hs
M	test/Test/MockCat/TypeClassSpec.hs
M	test/Test/MockCat/TypeClassTHSpec.hs

### Diffstat

 .cursor/rules/answer.mdc                       |    5 +
 .github/workflows/test.yml                     |    4 +-
 README-ja.md                                   |  188 +--
 README.md                                      |  158 +-
 diff.patch                                     | 2020 ++++++++++++++++++++++++
 mockcat.cabal                                  |   21 +-
 package.yaml                                   |    1 +
 src/Test/MockCat/Cons.hs                       |    8 +-
 src/Test/MockCat/Internal/Builder.hs           |  416 +++++
 src/Test/MockCat/Internal/Message.hs           |  114 ++
 src/Test/MockCat/Internal/Registry.hs          |  193 +++
 src/Test/MockCat/Internal/Types.hs             |   86 +
 src/Test/MockCat/Mock.hs                       | 1085 ++++---------
 src/Test/MockCat/MockT.hs                      |   49 +-
 src/Test/MockCat/Param.hs                      |   46 +-
 src/Test/MockCat/TH.hs                         |  596 +++----
 src/Test/MockCat/TH/ClassAnalysis.hs           |  133 ++
 src/Test/MockCat/TH/ContextBuilder.hs          |  102 ++
 src/Test/MockCat/TH/FunctionBuilder.hs         |  443 ++++++
 src/Test/MockCat/TH/TypeUtils.hs               |   64 +
 src/Test/MockCat/TH/Types.hs                   |   20 +
 src/Test/MockCat/Verify.hs                     |  501 ++++++
 stack.yaml                                     |    2 +-
 stack.yaml.lock                                |    8 +-
 test.sh                                        |   12 +
 test/Property/AdditionalProps.hs               |   43 +-
 test/Property/ConcurrentCountProp.hs           |    7 +-
 test/Property/Generators.hs                    |    6 +-
 test/Property/LazyEvalProp.hs                  |    8 +-
 test/Property/OrderProps.hs                    |   24 +-
 test/Property/ParamSpecNormalizeProp.hs        |    8 +-
 test/Property/ParamSpecRangeMergeRandomProp.hs |    8 +-
 test/Property/ReinforcementProps.hs            |   24 +-
 test/Property/ScriptProps.hs                   |    7 +-
 test/Spec.hs                                   |   14 +
 test/Test/MockCat/ConcurrencySpec.hs           |    1 +
 test/Test/MockCat/Definition.hs                |   27 -
 test/Test/MockCat/ExampleSpec.hs               |  124 +-
 test/Test/MockCat/Impl.hs                      |    2 +-
 test/Test/MockCat/Internal/RegistrySpec.hs     |   27 +
 test/Test/MockCat/MockSpec.hs                  |  967 ++++++------
 test/Test/MockCat/ParamSpec.hs                 |   12 +-
 test/Test/MockCat/PartialMockSpec.hs           |  245 ++-
 test/Test/MockCat/PartialMockTHSpec.hs         |   68 +-
 test/Test/MockCat/SharedSpecDefs.hs            |  148 ++
 test/Test/MockCat/StubSpec.hs                  |   42 +
 test/Test/MockCat/TH/ClassAnalysisSpec.hs      |   81 +
 test/Test/MockCat/TH/ContextBuilderSpec.hs     |   63 +
 test/Test/MockCat/TH/FunctionBuilderSpec.hs    |   99 ++
 test/Test/MockCat/TH/TypeUtilsSpec.hs          |   84 +
 test/Test/MockCat/THCompareSpec.hs             |  507 ++++++
 test/Test/MockCat/TypeClassSpec.hs             |  755 ++++++++-
 test/Test/MockCat/TypeClassTHSpec.hs           |   84 +-
 53 files changed, 7622 insertions(+), 2138 deletions(-)

### Top diffs (hunks, up to 500 lines)

diff --git a/.cursor/rules/answer.mdc b/.cursor/rules/answer.mdc
new file mode 100644
index 0000000..16f6646
--- /dev/null
+++ b/.cursor/rules/answer.mdc
@@ -0,0 +1,5 @@
+---
+alwaysApply: true
+---
+- 回答末尾でこちらに聞き返す行為は禁止する。
+- 作業の開始を宣言したら、必ず作業を開始する。
\ No newline at end of file
diff --git a/.github/workflows/test.yml b/.github/workflows/test.yml
index a919643..5029782 100644
--- a/.github/workflows/test.yml
+++ b/.github/workflows/test.yml
@@ -5,9 +5,9 @@ jobs:
     runs-on: ${{ matrix.os }}
     strategy:
       matrix:
-        ghc: ['9.2.8', '9.4.8', '9.6.3', '9.8.2', '9.10.1']
+        ghc: ['9.2.8', '9.4.8', '9.6.7', '9.8.4', '9.10.3', '9.12.2']
         cabal: ['3.10.3.0', '3.12.1.0']
-        os: [ubuntu-latest, macOS-latest, windows-latest]
+        os: [ubuntu-latest, macOS-latest]
 
     name: Haskell GHC ${{ matrix.ghc }} Test
     steps:
diff --git a/README-ja.md b/README-ja.md
index 583562e..dab91ca 100644
--- a/README-ja.md
+++ b/README-ja.md
@@ -42,7 +42,7 @@ mockcat は Haskell 向けの小さなモック / スタブ DSL です。
 * 侵襲的なアーキテクチャ変更を要求しない。既存テストへ差し込める。
 
 #### 導入 (手書きスタブから段階的移行) 手順例
1. 既存の手書きスタブを `createMockFn` に置換  
   - 既存の型シグネチャはそのまま保持してください。  
   - 置換例: `createMock` + `stubFn` → `createMockFn`

2. 必要なテストにのみ `shouldApplyTo` / `shouldApplyTimes` を追加  
   - すべてのスタブに無差別に付けるのではなく、意図する振る舞いを検証したい箇所に限定してください。

3. 重複呼び出し判定や順序がテストの意図である場合は `shouldApplyInOrder` を使用

4. 将来的に fuzz / property ベースの検証を行う場合は、PoC モジュール（例: `ParamSpec` / `Scenario`）の導入を検討してください。
@@ -86,23 +86,21 @@ mockcat は Haskell 向けの小さなモック / スタブ DSL です。
 </details>
 
 ## 例
-スタブ関数
+スタブ関数（検証機能なし）
 ```haskell
--- create a stub function
-stubFn <- createStubFn $ "value" |> True
+-- create a stub function without verification
+let stubFn = createStubFn $ "value" |> True
 -- assert
 stubFn "value" `shouldBe` True
 ```
-適用の検証
+モック関数（検証機能付き）
 ```haskell
--- create a mock
-mock <- createMock $ "value" |> True
--- stub function
-let stubFunction = stubFn mock
+-- create a verifiable mock function
+stubFunction <- createMockFn $ "value" |> True
 -- assert
 stubFunction "value" `shouldBe` True
 -- verify
-mock `shouldApplyTo` "value"
+stubFunction `shouldApplyTo` "value"
 ```
 型クラス
 ```haskell
@@ -115,12 +113,29 @@ result <- runMockT do
 
 result `shouldBe` ()
 ```
-## スタブ関数の概要
+## スタブ関数とモック関数の概要
+
+mockcatは2種類の関数を提供します：
+
+1. **スタブ関数** (`createStubFn`): 検証機能を持たない純粋なスタブ関数
+2. **モック関数** (`createMockFn`): 検証機能を持つモック関数（内部で`unsafePerformIO`を使用）
+
+### スタブ関数（検証機能なし）
+
 スタブ関数は`createStubFn`関数で生成することができます。
 `createStubFn`の引数は、適用が期待される引数を `|>` で連結したもので、`|>` の最後の値が関数の返り値となります。
 ```haskell
-createStubFn $ (10 :: Int) |> "return value"
+let stubFn = createStubFn $ (10 :: Int) |> "return value"
+```
+
+### モック関数（検証機能付き）
+
+モック関数は`createMockFn`関数で生成することができます。
+`createMockFn`の引数は、適用が期待される引数を `|>` で連結したもので、`|>` の最後の値が関数の返り値となります。
+```haskell
+mockFn <- createMockFn $ (10 :: Int) |> "return value"
 ```
+
 これは型クラスのモックにおけるスタブ関数の場合も同様です。
 ```haskell
 runMockT do
@@ -129,50 +144,45 @@ runMockT do
 期待される引数は、条件として指定することもできます。
 ```haskell
 -- Conditions other than exact match
-createStubFn $ any |> "return value"
-createStubFn $ expect (> 5) "> 5" |> "return value"
-createStubFn $ expect_ (> 5) |> "return value"
-createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
+mockFn <- createMockFn $ any |> "return value"
+mockFn <- createMockFn $ expect (> 5) "> 5" |> "return value"
+mockFn <- createMockFn $ expect_ (> 5) |> "return value"
+mockFn <- createMockFn $ $(expectByExpr [|(> 5)|]) |> "return value"
 ```
 また、引数に応じて返す値を変えることも可能です。
 （同じ引数に対して、別の値を返すことも可能できます。）
 ```haskell
--- Parameterized Stub
-createStubFn do
+-- Parameterized Mock
+mockFn <- createMockFn do
   onCase $ "a" |> "return x"
   onCase $ "b" |> "return y"
-createStubFn do
+mockFn <- createMockFn do
   onCase $ "arg" |> "x"
   onCase $ "arg" |> "y"
 ```
 ## 検証の概要
-スタブ関数の適用を検証するには、まず`createMock`関数でモックを作ります。
-スタブ関数はモックから`stubFn`関数で取り出して使います。
-検証はモックに対して行います。
-```haskell
--- create a mock
-mock <- createMock $ "value" |> True
--- stub function
-let stubFunction = stubFn mock
+モック関数の適用を検証するには、`createMockFn` で生成したモック関数に対して直接検証関数を適用します。
+```haskell
+stubFunction <- createMockFn $ "value" |> True
 -- assert
 stubFunction "value" `shouldBe` True
 -- verify
-mock `shouldApplyTo` "value"
+stubFunction `shouldApplyTo` "value"
 ```
 スタブ関数と同様に検証の場合も条件を指定することができます。
 ```haskell
-mock `shouldApplyTo` any @String
-mock `shouldApplyTo` expect_ (/= "not value")
-mock `shouldApplyTo` $(expectByExpr [|(/= "not value")|])
+stubFunction `shouldApplyTo` any @String
+stubFunction `shouldApplyTo` expect_ (/= "not value")
+stubFunction `shouldApplyTo` $(expectByExpr [|(/= "not value")|])
 ```
 また適用された回数を検証することもできます。
 ```haskell
-mock `shouldApplyTimes` (1 :: Int) `to` "value"
-mock `shouldApplyTimesGreaterThan` (0 :: Int) `to` "value"
-mock `shouldApplyTimesGreaterThanEqual` (1 :: Int) `to` "value"
-mock `shouldApplyTimesLessThan` (2 :: Int) `to` "value"
-mock `shouldApplyTimesLessThanEqual` (1 :: Int) `to` "value"
-mock `shouldApplyTimesToAnything` (1 :: Int)
+stubFunction `shouldApplyTimes` (1 :: Int) `to` "value"
+stubFunction `shouldApplyTimesGreaterThan` (0 :: Int) `to` "value"
+stubFunction `shouldApplyTimesGreaterThanEqual` (1 :: Int) `to` "value"
+stubFunction `shouldApplyTimesLessThan` (2 :: Int) `to` "value"
+stubFunction `shouldApplyTimesLessThanEqual` (1 :: Int) `to` "value"
+stubFunction `shouldApplyTimesToAnything` (1 :: Int)
 ```
 型クラスのモックの場合は、`runMockT`を適用した際、用意したスタブ関数の適用が行われたかの検証が自動で行われます。
 ```haskell
@@ -441,21 +451,18 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "使い方の例" do
-    -- モックの生成("value"を適用すると、純粋な値Trueを返す)
-    mock <- createMock $ "value" |> True
-
-    -- モックからスタブ関数を取り出す
-    let stubFunction = stubFn mock
+    -- モック関数の生成("value"を適用すると、純粋な値Trueを返す)
+    stubFunction <- createMockFn $ "value" |> True
 
     -- 関数の適用結果を検証
     stubFunction "value" `shouldBe` True
 
     -- 期待される値("value")が適用されたかを検証
-    mock `shouldApplyTo` "value"
+    stubFunction `shouldApplyTo` "value"
 
 ```
 
-### スタブ関数
+### スタブ関数（検証機能なし）
 スタブ関数を直接作るには `createStubFn` 関数を使います。  
 検証が不要な場合は、こちらを使うとよいでしょう。
 ```haskell
@@ -467,14 +474,11 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "スタブ関数を生成することができる" do
-    -- 生成
-    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()
+    -- 生成（検証機能なしの純粋なスタブ）
+    let f = createStubFn $ "param1" |> "param2" |> True
 
     -- 適用
-    actual <- f "param1" "param2"
-
-    -- 検証
-    actual `shouldBe` ()
+    f "param1" "param2" `shouldBe` True
 ```
 `createStubFn` 関数には、関数が適用されることを期待する引数を `|>` で連結して渡します。
 `|>` の最後の値が関数の返り値となります。
@@ -486,8 +490,8 @@ Expected arguments were not applied to the function.
   expected: "value"
   but got: "valuo"
 ```
-### 名前付きスタブ関数
-スタブ関数には名前を付けることができます。
+### 名前付きモック関数
+モック関数には名前を付けることができます。
 ```haskell
 {-# LANGUAGE BlockArguments #-}
 {-# LANGUAGE TypeApplications #-}
@@ -496,8 +500,8 @@ import Test.MockCat
 
 spec :: Spec
 spec = do
-  it "named stub" do
-    f <- createNamedStubFn "named stub" $ "x" |> "y" |> True
+  it "named mock" do
+    f <- createNamedMockFn "named mock" $ "x" |> "y" |> True
     f "x" "z" `shouldBe` True
 ```
 期待した引数に適用されなかった場合に出力されるエラーメッセージには、この名前が含まれるようになります。
@@ -525,8 +529,8 @@ spec = do
     shouldApplyToAnything m
 ```
 
-### 柔軟なスタブ関数
-`createStubFn` 関数に具体的な値ではなく、条件式を与えることで、柔軟なスタブ関数を生成できます。  
+### 柔軟なモック関数
+`createMockFn` 関数に具体的な値ではなく、条件式を与えることで、柔軟なモック関数を生成できます。  
 これを使うと、任意の値や、特定のパターンに合致する文字列などに対して期待値を返すことができます。  
 これはモナド型のモックを生成した際のスタブ関数も同様です。
 ### any
@@ -541,7 +545,7 @@ import Prelude hiding (any)
 spec :: Spec
 spec = do
   it "any" do
-    f <- createStubFn $ any |> "return value"
+    f <- createMockFn $ any |> "return value"
     f "something" `shouldBe` "return value"
 ```
 Preludeに同名の関数が定義されているため、`import Prelude hiding (any)`としています。
@@ -559,7 +563,7 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "expect" do
-    f <- createStubFn $ expect (> 5) "> 5" |> "return value"
+    f <- createMockFn $ expect (> 5) "> 5" |> "return value"
     f 6 `shouldBe` "return value"
 ```
 
@@ -576,7 +580,7 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "expect_" do
-    f <- createStubFn $ expect_ (> 5) |> "return value"
+    f <- createMockFn $ expect_ (> 5) |> "return value"
     f 6 `shouldBe` "return value"
 ```
 
@@ -593,12 +597,12 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "expectByExpr" do
-    f <- createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
+    f <- createMockFn $ $(expectByExpr [|(> 5)|]) |> "return value"
     f 6 `shouldBe` "return value"
 ```
 
-### 適用される引数ごとに異なる値を返すスタブ関数
-`onCase`関数を使うと引数ごとに異なる値を返すスタブ関数を作れます。
+### 適用される引数ごとに異なる値を返すモック関数
+`onCase`関数を使うと引数ごとに異なる値を返すモック関数を作れます。
 ```haskell
 {-# LANGUAGE BlockArguments #-}
 {-# LANGUAGE TypeApplications #-}
@@ -608,7 +612,7 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "multi" do
-    f <- createStubFn do
+    f <- createMockFn do
       onCase $ "a" |> "return x"
       onCase $ "b" |> "return y"
 
@@ -616,8 +620,8 @@ spec = do
     f "b" `shouldBe` "return y"
 ```
 
-### 同じ引数に適用されたとき異なる値を返すスタブ関数
-`onCase`関数を使うとき、引数が同じで返り値が異なるようにすると、同じ引数に適用しても異なる値を返すスタブ関数を作れます。
+### 同じ引数に適用されたとき異なる値を返すモック関数
+`onCase`関数を使うとき、引数が同じで返り値が異なるようにすると、同じ引数に適用しても異なる値を返すモック関数を作れます。
 ```haskell
 {-# LANGUAGE BlockArguments #-}
 {-# LANGUAGE TypeApplications #-}
@@ -628,7 +632,7 @@ import GHC.IO (evaluate)
 spec :: Spec
 spec = do
   it "Return different values for the same argument" do
-    f <- createStubFn do
+    f <- createMockFn do
       onCase $ "arg" |> "x"
       onCase $ "arg" |> "y"
 
@@ -638,12 +642,12 @@ spec = do
     v3 <- evaluate $ f "arg"
     v1 `shouldBe` "x"
     v2 `shouldBe` "y"
-    v3 `shouldBe` "y" -- After the second time, “y” is returned.
+    v3 `shouldBe` "y" -- After the second time, "y" is returned.
 ```
 あるいは`cases`関数を使うこともできます。
 ```haskell
 f <-
-  createStubFn $
+  createMockFn $
     cases
       [ "a" |> "return x",
         "b" |> "return y"
@@ -656,8 +660,7 @@ f "b" `shouldBe` "return y"
 ## 検証
 ### 期待される引数に適用されたか検証する
 期待される引数に適用されたかは `shouldApplyTo` 関数で検証することができます。  
-検証を行う場合は、`createStubFn` 関数ではなく `createMock` 関数でモックを作る必要があります。
-この場合スタブ関数は `stubFn` 関数でモックから取り出して使います。
+`createMockFn` で生成したモック関数に対して直接検証を行います。
 ```haskell
 {-# LANGUAGE BlockArguments #-}
 {-# LANGUAGE TypeApplications #-}
@@ -666,18 +669,17 @@ import Test.MockCat
 
 spec :: Spec
 spec = do
-  it "stub & verify" do
-    -- create a mock
-    mock <- createMock $ "value" |> True
-    -- stub function
-    let stubFunction = stubFn mock
+  it "mock & verify" do
+    -- create a verifiable mock
+    let args = "value" |> True
+    stubFunction <- createMockFn args
     -- assert
     stubFunction "value" `shouldBe` True
     -- verify
-    mock `shouldApplyTo` "value"
+    stubFunction `shouldApplyTo` "value"
 ```
 ### 注
-適用されたという記録は、スタブ関数の返り値が評価される時点で行われます。  
+適用されたという記録は、モック関数の返り値が評価される時点で行われます。  
 したがって、検証は返り値の評価後に行う必要があります。
 ```haskell
 {-# LANGUAGE BlockArguments #-}
@@ -688,10 +690,10 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "Verification does not work" do
-    mock <- createMock $ "expect arg" |> "return value"
+    f <- createMockFn $ "expect arg" |> "return value"
     -- 引数の適用は行うが返り値は評価しない
-    let _ = stubFn mock "expect arg"
-    mock `shouldApplyTo` "expect arg"
+    let _ = f "expect arg"
+    f `shouldApplyTo` "expect arg"
 ```
 ```console
 uncaught exception: ErrorCall
@@ -711,10 +713,10 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "shouldApplyTimes" do
-    m <- createMock $ "value" |> True
-    print $ stubFn m "value"
-    print $ stubFn m "value"
-    m `shouldApplyTimes` (2 :: Int) `to` "value"
+    f <- createStubFn $ "value" |> True
+    print $ f "value"
+    print $ f "value"
+    f `shouldApplyTimes` (2 :: Int) `to` "value"
 ```
 
 ### 何かしらに適用されたかを検証する
@@ -734,10 +736,10 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "shouldApplyInOrder" do
-    m <- createMock $ any |> True |> ()
-    print $ stubFn m "a" True
-    print $ stubFn m "b" True
-    m
+    f <- createStubFn $ any |> True |> ()
+    print $ f "a" True
+    print $ f "b" True
+    f
       `shouldApplyInOrder` [ "a" |> True,
                              "b" |> True
                            ]
@@ -755,11 +757,11 @@ import Test.MockCat
 spec :: Spec
 spec = do
   it "shouldApplyInPartialOrder" do
-    m <- createMock $ any |> True |> ()
-    print $ stubFn m "a" True
-    print $ stubFn m "b" True
-    print $ stubFn m "c" True
-    m
+    f <- createStubFn $ any |> True |> ()
+    print $ f "a" True
+    print $ f "b" True
+    print $ f "c" True
+    f
       `shouldApplyInPartialOrder` [ "a" |> True,
                                     "c" |> True
                                   ]
diff --git a/README.md b/README.md
index 923fee4..8277a56 100644
--- a/README.md
+++ b/README.md
@@ -82,8 +82,8 @@ Consider alternatives if:
 
 ### Migrating from handwritten stubs
 If you already write tiny handmade stubs:
-1. Replace the stub body with `createMock` + `stubFn`.
-2. Inline expectation in the test (keep original type signature).
+1. Replace the stub body with `createMockFn`.
+2. Inline expectation in the test (keep the original type signature).
 3. Add verification only where it increases confidence (don’t verify everything by habit).
 
 ### Interoperability notes
@@ -114,16 +114,16 @@ Failure message lists expected vs actual (or sequence diff). Add labels via `exp
 
 ### Quick Start
 
-Function mock:
+Function stub:
 ```haskell
 import Test.Hspec
 import Test.MockCat
 
 spec :: Spec
-spec = it "simple function mock" do
-  m <- createMock $ "input" |> 42
-  stubFn m "input" `shouldBe` 42
-  m `shouldApplyTo` "input"
+spec = it "simple function stub" do
+  f <- createStubFn $ "input" |> 42
+  f "input" `shouldBe` 42
+  f `shouldApplyTo` "input"
 ```
 
 Typeclass mock:
@@ -214,14 +214,12 @@ stubFn "value" `shouldBe` True
 ```
 Verification
 ```haskell
--- create a mock
-mock <- createMock $ "value" |> True
--- stub function
-let stubFunction = stubFn mock
+-- create a verifiable stub
+stubFunction <- createStubFn $ "value" |> True
 -- assert
 stubFunction "value" `shouldBe` True
 -- verify
-mock `shouldApplyTo` "value"
+stubFunction `shouldApplyTo` "value"
 ```
 Mock of Type Class
 ```haskell
@@ -235,10 +233,15 @@ result <- runMockT do
 result `shouldBe` ()
 ```
 ## Stub Function Overview
-Stub functions can be created with the `createStubFn` function.  
-The arguments of `createStubFn` are the arguments expected to be applied, concatenated by `|>`, where the last value of `|>` is the return value of the function.
+Mock functions (with verification) can be created with the `createMockFn` function.  
+The arguments of `createMockFn` are the arguments expected to be applied, concatenated by `|>`, where the last value of `|>` is the return value of the function.
 ```haskell
-createStubFn $ (10 :: Int) |> "return value"

\n---\n
## 設計差分サマリ
Generated: 2025-11-24T11:43:50Z

差分に含まれる主なファイルと推定される設計上の影響：

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

## 設計差分（詳細まとめ）

- **API/呼び名の変更と追加**  
  - `createMock` / `createStubFn` 系から、関数向けに `createMockFn` / `createNamedMockFnWithParams` / `createMockFnIO` のような新しい作成APIへ移行しています。テストコードや README も新APIに合わせて更新されています。

- **パラメータ表現の一般化**  
  - `Param` の GADT 拡張（例: `ExpectValue` / `ExpectCondition` / `ExpectRef` 等）と、`ToParam` / `ParamsEq` / `Target` といった型クラス・型族が導入され、パラメータの等値判定・変換を型クラスで制御する設計に変わっています。これにより複雑なパラメータ（関数・IO 等）に対する比較や一致判定が柔軟になっています。
  - 実装上は `OVERLAPPING` / `OVERLAPPABLE` / `INCOHERENT` を慎重に使っており、インスタンス解決のトレードオフに留意する必要があります。

- **Template Haskell（TH）周りの大規模改修**  
  - `src/Test/MockCat/TH/*`（`FunctionBuilder`, `ContextBuilder`, `ClassAnalysis`, `TypeUtils`, `Types` など）が再設計され、生成されるインスタンス/関数の堅牢性や型制約の扱いが改善されています。`createMockFnDec` や `createMockBody` 等シグネチャの変更が見られます。  
  - `Language.Haskell.TH.Ppr` / `Type.Reflection` / `Language.Haskell.TH.Syntax` 等を用い、型情報の抽出や生成コードの整形が強化されています。

- **モード分離とビルダ層の導入**  
  - `ModeSpec` / `ApplyMode` / `BuildCurriedGeneric` といった抽象化により、純粋関数・IO 関数・モナド持ち戻り等を意識したモック生成が可能になっています。`MockBuilder` と `MockIOBuilder` の分離によりビルド処理が整理されています。`LiftFunTo` の導入で IO を別のモナドへリフトする仕組みが提供されています。

- **ランタイム識別とレジストリの強化**  
  - `StableName` と `Dynamic` を使ったレジストリ実装（関数識別子の安定化、動的値の格納）が導入され、ランタイムでの関数照合や検証がより堅牢になっています。また Unit 用のガードやメタデータ管理も追加されています。

- **検証（Verify）API の拡張**  
  - `Verify` 系（`VerifyCount` / `VerifyOrder` 等）の型やインスタンスが更新され、`shouldApplyTo` / `shouldApplyTimes` / `shouldApplyInOrder` などの振る舞いがより一般化・型安全になっています。パラメータ解決（ResolvableParams 等）を通した検証フローに変更があります。

- **テストと仕様の更新**  
  - 多くのテストが新API（`createMockFn` 系）に合わせて書き換えられており、TH 生成コードの挙動を検証する Spec モジュールが追加・拡張されています（`TH.*` 系の Spec）。

- **ビルド設定／依存の変更**  
  - `package.yaml` / `mockcat.cabal` / `stack.yaml` や CI ワークフロー（`.github/workflows/test.yml`）が更新されています。外部依存の追加や GHC オプションの調整が含まれている可能性があるため、CI 通過と依存解決を確認してください。

- **技術的に特筆すべき工夫**  
  - TH 側で型制約や Typeable 情報を積極的に扱い、生成コードの型安全性を高めている点。  
  - `ParamsEq` によるパラメータ等価性の抽象化は、関数や複雑な構造体の比較を一般化する実用的な設計判断です。  
  - `StableName` + `Dynamic` の組合せは、ランタイムの関数識別を速く・確実にするための実装で、モック/検証の正確性を高めます。  
  - モード分離（Pure / IO / モナド）により、生成される API の利用感を損なわずに多様な呼び出しシグネチャをサポートしています。

- **注意点 / リスクと推奨対応**  
  - 公開APIの互換性に関わる変更（関数名・シグネチャ・モジュールのエクスポート）を要点ごとに手動レビューしてください。自動検出はヒューリスティックであり見落としがあります。  
  - TH 生成コードは GHC のバージョン差や拡張の影響を受けやすいので、ターゲット環境でのビルド確認（CI 流し直し）を行ってください。  
  - `OVERLAPPING` 系のインスタンスは将来的な相互作用（他ライブラリ/ユーザコードとの衝突）を生む可能性があるため、ドキュメント化と最小化を推奨します。

（以上は差分のコード解析に基づく要約です。個別ファイルの深掘りが必要であれば、特定ファイルを指定してください。）
- \
  - 推定カテゴリ: ドキュメント
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: ドキュメント
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: 実装/API
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: 実装/その他
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 削除または変更された型/シグネチャが検出されました。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 実装内部の変更が主（公開APIの型/シグネチャ変更は検出されていません）。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 削除または変更された型/シグネチャが検出されました。 関数シグネチャの削除あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 新しい関数シグネチャの追加あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。

- \
  - 推定カテゴリ: テスト
  - 影響の可能性: 追加された型/シグネチャの変更が検出されました。 削除または変更された型/シグネチャが検出されました。 新しい関数シグネチャの追加あり。 関数シグネチャの削除あり。
  - 注記: Template Haskell を使う変更が含まれている可能性があります。ビルド順序やspliceの影響を確認してください。

