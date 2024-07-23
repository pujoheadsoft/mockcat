# Mocking library for Haskell (=^･ｪ･^=)

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
    -- モックの生成("value"に適用されたら、純粋な値Trueを返す)
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

## 柔軟なスタブ関数
`createStubFn` 関数に具体的な値ではなく、条件式を与えることで、柔軟なスタブ関数を生成できます。
これを使うと、任意の値や、特定のパターンに合致する文字列などに対して期待値を返すことができます。
### any
`any` は組み込みの条件式で、任意の値にマッチします。
```haskell
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

###

# 検証
スタブ関数に対して期待される引数が適用されたか検証することができます。

 `shouldApplyTo` 関数を使います。
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
