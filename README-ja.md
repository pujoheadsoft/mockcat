# Mocking library for Haskell (=^･ｪ･^=)

mockcatは、Haskellのテストをサポートするシンプルなモック・ライブラリです。

できることは主に2つあります。
1. スタブ関数を作る
2. スタブ関数が期待通り適用されたかを検証する

スタブ関数はモナディックな値だけでなく、純粋な型の値も返すことができます。

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "スタブ関数の生成と検証" do
    -- モックの生成(純粋な値 True を返す)
    m <- createMock $ "value" |> True

    -- モックからスタブ関数を取り出す
    let f = stubFn m

    -- 関数の適用結果を検証
    f "value" `shouldBe` True

    -- 期待される値("value")で関数が適用されたかを検証
    m `shouldApplyTo` "value"

```


## mock関数を使う
`createStubFn` を使うとスタブ関数を生成することができます。
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "スタブ関数を生成することができる" do
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()
    actual <- f "param1" "param2"
    actual `shouldBe` ()
```
`createStubFn` 関数には、関数が適用されることを期待する引数を `|>` で連結して渡します。
`|>` の最後の値が関数の戻り値となります。
