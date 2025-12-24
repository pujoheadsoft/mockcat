<div align="center">
    <img src="https://raw.githubusercontent.com/pujoheadsoft/mockcat/main/logo.png" width="600px" alt="Mockcat Logo">
    <h1>Declarative mocking with a single arrow <code>~&gt;</code></h1>
</div>

<div align="center">

[![Hackage](https://img.shields.io/hackage/v/mockcat.svg)](https://hackage.haskell.org/package/mockcat)
[![Stackage LTS](http://stackage.org/package/mockcat/badge/lts)](http://stackage.org/lts/package/mockcat)
[![Build Status](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions)

</div>

**Mockcat** は、Haskell のための直感的で宣言的なモックライブラリです。
専用の演算子 **Mock Arrow (`~>`)** を使うことで、関数定義と同じような感覚でモックの振る舞いを記述できます。

```haskell
-- 定義 (Define)
f <- mock $ "input" ~> "output"

-- 検証 (Verify)
f `shouldBeCalled` "input"
```

---

## 概念と用語 (Concepts & Terminology)

Mockcat は、一般的に混同されがちな「スタブ」と「モック」を明確に区別し、目的に応じた最適な道具を提供します。

| 用語 | 役割 | Mockcat での対応関数 |
| :--- | :--- | :--- |
| **Stub (スタブ)** | テスト対象を動かすために、決まった値を返すだけの存在。<br>副作用を持たず、「どう呼ばれたか」に関心を持ちません。 | **`stub`**<br>純粋な関数を返します。<br>検証機能なし。副作用なし。<br>最も軽量です。 |
| **Mock (モック)** | スタブの機能に加え、「期待通りに呼び出されたか」を記録・検証する存在。<br>オブジェクト間の相互作用（Interaction）テストに使用します。 | **`mock`** / **`mockM`**<br>値を返しつつ、呼び出しを記録します。<br>`shouldBeCalled` 等で検証できます。 |

---

## Why Mockcat?

Haskell でモックを書くのは、本来もっと簡単であるべきです。
Mockcat は、ボイラープレート（定型コード）を徹底的に排除し、テストの本質だけに集中できるように設計されています。

| | **Before: 手書き...** 😫 | **After: Mockcat** 🐱✨ |
| :--- | :--- | :--- |
| **定義 (Stub)**<br>「この引数には<br>この値を返したい」 | <pre lang="haskell">f :: String -> IO String<br>f arg = case arg of<br>  "a" -> pure "b"<br>  _   -> error "unexpected"</pre><br>_単純な分岐を書くだけでも行数を消費します。_ | <pre lang="haskell">let f = stub $<br>  "a" ~> "b"<br><br><br></pre><br>_検証不要なら `stub` で十分。<br>完全に純粋な関数として振る舞います。_ |
| **検証 (Verify)**<br>「正しく呼ばれたか<br>テストしたい」 | <pre lang="haskell">-- 記録の仕組みから作る必要がある<br>ref <- newIORef []<br>let f arg = do<br>      modifyIORef ref (arg:)<br>      ...<br><br>-- 検証ロジック<br>calls <- readIORef ref<br>calls \`shouldBe\` ["a"]</pre><br>_検証のための「仕掛け作り」に時間を取られます。_ | <pre lang="haskell">-- 1行で定義と監視を開始<br>f <- mock $ "a" ~> "b"<br><br>-- 検証したい内容を書くだけ<br>f \`shouldBeCalled\` "a"<br><br><br><br></pre><br>_記録は自動。<br>「何を検証するか」という本質に集中できます。_ |

### 主な特徴

*   **直感的な DSL**: 引数と戻り値を矢印 (`~>`) で順につなぐ、関数定義のような記法。
*   **モック自動生成**: 型クラスから Template Haskell でモックを一行で生成 (`makeMock`)。
*   **圧倒的に親切なエラー**: テスト失敗時、どこが違うのかを「構造差分」で表示します。
    ```text
    Expected arguments were not called.
      expected: [Record { name = "Alice", age = 20 }]
       but got: [Record { name = "Alice", age = 21 }]
                                                ^^
    ```
*   **堅牢な設計**: スレッドセーフな呼び出しカウントと、遅延評価を尊重した記録メカニズム。


---

## クイックスタート

以下のコードをコピペすれば、今すぐ Mockcat を体験できます。

### インストール

`package.yaml`:
```yaml
dependencies:
  - mockcat
```

または `.cabal`:
```cabal
build-depends:
    mockcat
```

### 最初のテスト (`Main.hs` / `Spec.hs`)

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.MockCat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Quick Start Demo" do
    -- 1. モックを作成 ("Hello" を受け取ったら 42 を返す)
    f <- mock $ "Hello" ~> (42 :: Int)

    -- 2. 関数として使う
    result <- f "Hello"
    result `shouldBe` 42

    -- 3. 呼び出されたことを検証
    f `shouldBeCalled` "Hello"
```

---

## 使い方ガイド (User Guide)

### 1. 関数のモック (`mock`)

最も基本的な使い方です。特定の引数に対して値を返す関数を作ります。

```haskell
-- "a" -> "b" -> True を返す関数
f <- mock $ "a" ~> "b" ~> True
```

**柔軟なマッチング**:
具体的な値だけでなく、条件（述語）を指定することもできます。

```haskell
-- 任意の文字列 (param any)
f <- mock $ any ~> True

-- 条件式 (expect)
f <- mock $ expect (> 5) "> 5" ~> True
```

### 2. 型クラスのモック (`makeMock`)

実務で最もよく使う機能です。Template Haskell を使って、既存の型クラスからモックを自動生成します。

```haskell
class FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Strict Mode] デフォルトの動作。「mock」関数と挙動が一致します。
-- スタブの戻り値には、明示的に pure / monadic な値を渡す必要があります。
makeMock [t|FileSystem|]

-- [Auto-Lift Mode] これまでの挙動（便利なラッパー）。
-- 純粋な値を自動的にモナド（m String など）に包んで返します。
makeAutoLiftMock [t|FileSystem|]
```

テストコード内では `runMockT` ブロックを使用します。

```haskell
spec :: Spec
spec = do
  it "filesytem test" do
    result <- runMockT do
      -- [Strict Mode] (makeMock 使用時): 明示的に pure で包む
      _readFile $ "config.txt" ~> pure "debug=true"
      _writeFile $ "log.txt" ~> "start" ~> pure ()

      -- [Auto-Lift Mode] (makeAutoLiftMock 使用時): 値は自動的に包まれる (便利)
      -- _readFile $ "config.txt" ~> "debug=true"

      -- テスト対象コードの実行（モックが注入される）
      myProgram "config.txt"
    
    result `shouldBe` ()
```

### 3. 宣言的な検証 (`withMock` / `expects`)

定義と同時に期待値を記述するスタイルです。スコープを抜ける時に自動的に検証が走ります。
「定義」と「検証」を近くに書きたい場合に便利です。

```haskell
withMock $ do
  -- 定義と同時に期待値(expects)を書く
  f <- mock (any ~> True)
    `expects` do
      called once `with` "arg"

  -- 実行
  f "arg"
```

> [!NOTE]
> `runMockT` ブロックの中でも、同様に `expects` を使った宣言的検証が可能です。
> つまり、「モック生成」と「期待値宣言」が１つのブロック内で完結する統一された体験を提供します。

### 4. 高度な機能

#### mock vs stub vs mockM の使い分け

Mockcat には 3種類の関数作成方法があります。用途に合わせて使い分けてください。

| 関数 | 検証 (`shouldBeCalled`) | IO依存 | 特徴 |
| :--- | :---: | :---: | :--- |
| **`stub`** | ❌ | なし | **純粋なスタブ**。検証不要ならこれ一択。IO を汚染しません。 |
| **`mock`** | ✅ | あり(隠蔽) | **モック**。内部で `unsafePerformIO` を使い、純粋関数のふりをしつつ呼び出しを記録します。 |
| **`mockM`** | ✅ | あり(明示) | **Monadic モック**。`MockT` や `IO` の中で使い、副作用（ロギングなど）を明示的に扱えます。 |

#### 部分モック (Partial Mock): 本物の関数と混ぜて使う

一部のメソッドだけモックに差し替え、残りは本物の実装を使いたい場合に便利です。

```haskell
makePartialMock [t|FileSystem|] -- makeMock の代わりにこれを使う

instance FileSystem IO where ... -- 本物のインスタンスも必要

test = runMockT do
  _readFile $ "test" ~> "content" -- readFile だけモック化
  program -- writeFile は本物の IO インスタンスが走る
```

#### IO アクションを返す (Monadic Return)

`IO` を返す関数で、呼び出しごとに副作用（結果）を変えたい場合に使います。

```haskell
f <- mock $ do
  onCase $ "get" ~> pure @IO 1 -- 1回目
  onCase $ "get" ~> pure @IO 2 -- 2回目
```

#### 名前付きモック

エラーメッセージに関数名を表示させたい場合は、ラベルを付けられます。

```haskell
f <- mock (label "myAPI") $ "arg" ~> True
```

---

## リファレンス & レシピ (Encyclopedia)

困ったときはここを参照してください。

### 検証マッチャ一覧 (`shouldBeCalled`)

| マッチャ | 説明 | 例 |
| :--- | :--- | :--- |
| `x` (値そのもの) | その値で呼ばれたか | ``f `shouldBeCalled` (10 :: Int)`` |
| `times n` | 回数指定 | ``f `shouldBeCalled` (times 3 `with` "arg")`` |
| `once` | 1回だけ | ``f `shouldBeCalled` (once `with` "arg")`` |
| `never` | 呼ばれていない | ``f `shouldBeCalled` never`` |
| `atLeast n` | n回以上 | ``f `shouldBeCalled` atLeast 2`` |
| `atMost n` | n回以下 | ``f `shouldBeCalled` atMost 5`` |
| `anything` | 引数は何でも良い(回数不問) | ``f `shouldBeCalled` anything`` |
| `inOrderWith [...]` | 厳密な順序 | ``f `shouldBeCalled` inOrderWith ["a", "b"]`` |
| `inPartialOrderWith [...]` | 部分的順序（間飛びOK） | ``f `shouldBeCalled` inPartialOrderWith ["a", "c"]`` |

### パラメータマッチャ一覧（引数定義）

| マッチャ | 説明 | 例 |
| :--- | :--- | :--- |
| `any` | 任意の値 | `any ~> True` |
| `expect pred label` | 条件式 | `expect (>0) "positive" ~> True` |
| `expect_ pred` | ラベルなし | `expect_ (>0) ~> True` |

### よくある質問 (FAQ)

<details>
<summary><strong>Q. 未評価の遅延評価はどう扱われますか？</strong></summary>
A. カウントされません。Mockcat は「結果が評価された時点」で呼び出しを記録します (Honest Laziness)。これにより、不要な計算による誤検知を防ぎます。
</details>

<details>
<summary><strong>Q. 並列テストで使えますか？</strong></summary>
A. はい。内部で `TVar` を使用してアトミックにカウントしているため、`mapConcurrently` などで並列に呼ばれても正確に記録されます。
</details>

<details>
<summary><strong>Q. `makeMock` が生成するコードは何ですか？</strong></summary>
A. 指定された型クラスの `MockT m` インスタンスと、各メソッドに対応する `_メソッド名` というスタブ生成関数定義です。
</details>

<details>
<summary><strong>Q. 厳密な定義では Spy ではないですか？</strong></summary>
A. はい、xUnit Patterns 等の定義に従えば、事後検証を行う Mockcat のモックは **Test Spy** に分類されます。<br>
しかし、近年の多くのライブラリ（Jest, Mockito 等）がこれらを包括して「モック」と呼称していること、および用語の乱立による混乱を避けるため、本ライブラリでは **"Mock"** という用語で統一しています。
</details>

---

_Happy Mocking!_ 🐱