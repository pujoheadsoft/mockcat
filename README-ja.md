# 🐈Mocking library for Haskell🐈‍
[![Test](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions?query=workflow%3ATest+branch%3Amain)

mockcatは、Haskellのテストをサポートするシンプルなモック・ライブラリです。

できることは主に2つあります。
1. スタブ関数を作る
2. 引数が期待通り適用されたかを検証する

スタブ関数はモナディックな値だけでなく、純粋な型の値も返すことができます。

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "使い方の例" do
    -- モックの生成("value"を適用すると、純粋な値Trueを返す)
    mock <- createMock $ "value" |> True

    -- モックからスタブ関数を取り出す
    let stubFunction = stubFn mock

    -- 関数の適用結果を検証
    stubFunction "value" `shouldBe` True

    -- 期待される値("value")が適用されたかを検証
    mock `shouldApplyTo` "value"

```

# スタブ関数
## 単純なスタブ関数
スタブ関数の生成には `createStubFn` 関数を使います。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "スタブ関数を生成することができる" do
    -- 生成
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()

    -- 適用
    actual <- f "param1" "param2"

    -- 検証
    actual `shouldBe` ()
```
`createStubFn` 関数には、適用されることが期待する引数を `|>` で連結して渡します。
`|>` の最後の値が関数の返り値となります。

スタブ関数に対して期待されていない引数が適用された場合はエラーとなります。
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "value"
  but got: "valuo"
```
## 名前付きスタブ関数
スタブ関数には名前を付けることができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "named stub" do
    f <- createNamedStubFun "named stub" $ "x" |> "y" |> True
    f "x" "z" `shouldBe` True
```
期待した引数が適用されなかった場合に出力されるエラーメッセージには、この名前が含まれるようになります。
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function `named stub`.
  expected: "x","y"
  but got: "x","z"
```

## 柔軟なスタブ関数
`createStubFn` 関数に具体的な値ではなく、条件式を与えることで、柔軟なスタブ関数を生成できます。  
これを使うと、任意の値や、特定のパターンに合致する文字列などに対して期待値を返すことができます。
### any
`any` は任意の値にマッチします。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"
```
Preludeに同名の関数が定義されているため、`import Prelude hiding (any)`としています。

### 条件式
`expect`関数を使うと任意の条件式を扱えます。  
`expect`関数は条件式とラベルをとります。  
ラベルは条件式にマッチしなかった場合のエラーメッセージに使われます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect" do
    f <- createStubFn $ expect (> 5) "> 5" |> "return value"
    f 6 `shouldBe` "return value"
```

### ラベルなし条件式
`expect_` は `expect` のラベルなし版です。  
エラーメッセージには [some condition] と表示されます。

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect_" do
    f <- createStubFn $ expect_ (> 5) |> "return value"
    f 6 `shouldBe` "return value"
```

### Template Haskellを使った条件式
`expectByExp`を使うと、`Q Exp`型の値として条件式を扱えます。  
エラーメッセージには条件式を文字列化したものが使われます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expectByExpr" do
    f <- createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
    f 6 `shouldBe` "return value"
```

## 適用される引数ごとに異なる値を返すスタブ関数
`createStubFn`関数に、x |> y 形式のリストを適用させると、適用される引数ごとに異なる値を返すスタブ関数を作れます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "multi" do
    f <-
      createStubFn
        [ "a" |> "return x",
          "b" |> "return y"
        ]
    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
```

## 同じ引数が適用されても異なる値を返すスタブ関数
`createStubFn`関数に、x |> y 形式のリストを適用させるとき、引数が同じで返り値が異なるようにすると、同じ引数が適用されても異なる値を返すスタブ関数を作れます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import GHC.IO (evaluate)

spec :: Spec
spec = do
  it "Return different values for the same argument" do
    f <- createStubFn [
        "arg" |> "x",
        "arg" |> "y"
      ]
    -- Do not allow optimization to remove duplicates.
    v1 <- evaluate $ f "arg"
    v2 <- evaluate $ f "arg"
    v3 <- evaluate $ f "arg"
    v1 `shouldBe` "x"
    v2 `shouldBe` "y"
    v3 `shouldBe` "y" -- After the second time, “y” is returned.
```

# 検証
## 期待される引数が適用されたか検証する
期待される引数が適用されたかは `shouldApplyTo` 関数で検証することができます。  
検証を行う場合は、`createStubFn` 関数ではなく `createMock` 関数でモックを作る必要があります。
この場合スタブ関数は `stubFn` 関数でモックから取り出して使います。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "stub & verify" do
    -- create a mock
    mock <- createMock $ "value" |> True
    -- stub function
    let stubFunction = stubFn mock
    -- assert
    stubFunction "value" `shouldBe` True
    -- verify
    mock `shouldApplyTo` "value"
```
### 注
引数が適用されたという記録は、スタブ関数の返り値が評価される時点で行われます。  
したがって、検証は返り値の評価後に行う必要があります。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Verification does not work" do
    mock <- createMock $ "expect arg" |> "return value"
    -- 引数の適用は行うが返り値は評価しない
    let _ = stubFn mock "expect arg"
    mock `shouldApplyTo` "expect arg"
```
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "expect arg"
  but got: Never been called.
```

## 期待される引数が適用された回数を検証する
期待される引数が適用された回数は `shouldApplyTimes` 関数で検証することができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyTimes" do
    m <- createMock $ "value" |> True
    print $ stubFn m "value"
    print $ stubFn m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value"
```

## 期待される順序で適用されたかを検証する
期待される順序で適用されたかは `shouldApplyInOrder` 関数で検証することができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    m
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]
```

## 期待される順序で適用されたかを検証する(部分一致)
`shouldApplyInOrder` 関数は適用の順序を厳密に検証しますが、  
`shouldApplyInPartialOrder` 関数は適用の順序が部分的に一致しているかを検証することができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInPartialOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    print $ stubFn m "c" True
    m
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]
```