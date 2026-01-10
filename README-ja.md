<div align="center">
    <img src="https://raw.githubusercontent.com/pujoheadsoft/mockcat/main/logo.png" width="600px" alt="Mockcat Logo">
    <h1>Declarative mocking with a single arrow <code>~&gt;</code></h1>
</div>

<div align="center">

[![Hackage](https://img.shields.io/hackage/v/mockcat.svg)](https://hackage.haskell.org/package/mockcat)
[![Stackage LTS](http://stackage.org/package/mockcat/badge/lts)](http://stackage.org/lts/package/mockcat)
[![Build Status](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions)

[🇺🇸 English](README.md)

</div>

**Mockcat** は、Haskell のための直感的で宣言的なモックライブラリです。
2つの検証スタイルをサポートしています。

*   **事前宣言による検証 (`expects`)**: 【推奨】 モック定義時に期待される振る舞いを宣言します。
*   **事後検証 (`shouldBeCalled`)**: テスト実行後に呼び出しを検証します。

```haskell
-- 定義と同時に検証内容を宣言 ("input" で1回呼ばれることを期待)
f <- mock ("input" ~> "output")
  `expects` called once

-- 実行
f "input"
```

---

## 概念と用語 (Concepts & Terminology)

Mockcat は、「実行時に検証を行いますが、定義時に『満たすべき条件』を宣言できる」という設計を採用しています。

*   **Stub (スタブ)**:
    テストを進めるために値を返すだけの存在。「どう呼ばれたか」に関心を持ちません。
    `stub` 関数は完全に純粋な関数を返します。

*   **Mock (モック)**:
    スタブの機能に加え、「期待通りに呼び出されたか」を記録・検証する存在。
    `mock` 関数は、呼び出しを記録しながら値を返します。検証はテストの最後に行うことも、モック定義時に「この条件で呼ばれるはずだ」と宣言することも可能です（`expects` による宣言的検証）。

---

## Why Mockcat?

Haskell におけるモック記述を、できるだけ自然な形で行えるよう設計されています。

Mockcat は、**「アーキテクチャに依存せず、関数の "振る舞いと意図" を宣言的に記述できる」** モックライブラリです。

「型クラス (MTL) を導入しないとテストできない」
「モックのために専用のデータ型を定義しなければならない」
（例: 型クラスを増やす／Service Handle 用のレコードを別途用意する、など）

そんな制約から解放されます。既存の関数をそのままモックし、設計が固まりきっていない段階でもテストを書き進めることができます。

**Mockcat は、テストのために設計を固定するのではなく、設計を試すためにテストを書けることを目指しています。**

### Before / After

Mockcat を使うことで、テスト記述は次のようになります。

| | **Before: 手書き...** 😫 | **After: Mockcat** 🐱✨ |
| :--- | :--- | :--- |
| **定義 (Stub)**<br />「この引数には<br />この値を返したい」 | <pre>f :: String -> IO String<br />f arg = case arg of<br />  "a" -> pure "b"<br />  _   -> error "unexpected"</pre><br />_単純な分岐を書くだけでも行数を消費します。_ | <pre>-- 検証不要なら stub (純粋)<br />let f = stub ("a" ~> "b")</pre><br />_完全な純粋関数として振る舞います。_ |
| **検証 (Verify)**<br />「正しく呼ばれたか<br />テストしたい」 | <pre>-- 記録の仕組みから作る必要がある<br />ref <- newIORef []<br />let f arg = do<br />      modifyIORef ref (arg:)<br />      ...<br /><br />-- 検証ロジック<br />calls <- readIORef ref<br />calls `shouldBe` ["a"]</pre><br />_※ これはよくある一例です。実際にはさらに補助コードが増えがちです。_ | <pre>-- 定義と同時に期待値を宣言<br />f <- mock ("a" ~> "b")<br />  `expects` called once<br /><br />-- 実行するだけ (自動検証)</pre><br />_記録は自動。<br />「何を検証するか」という本質に集中できます。_ |

### 主な特徴

*   **Haskell ネイティブな DSL**: 冗長なデータコンストラクタや専用の記法を覚えなくても、関数定義と同じ感覚 (`引数 ~> 戻り値`) で自然に記述できます。
*   **アーキテクチャ非依存**: MTL (型クラス)、Service Handle (レコード)、あるいは純粋な関数。どのような設計パターンを採用していても、最小単位で導入可能です。
*   **値ではなく「条件」で検証**: 引数が `Eq` インスタンスを持っていなくても問題ありません。値の一致だけでなく、「どのような性質を満たすべきか」という条件 (Predicate) で検証できます。
*   **圧倒的に親切なエラー**: テスト失敗時、どこが違うのかを「構造差分」で表示します。
    ```text
    function was not called with the expected arguments.

      Closest match:
        expected: Record { name = "Alice", age = 20 }
         but got: Record { name = "Alice", age = 21 }
                                             ^^^
      Specific difference in `age`:
        expected: 20
         but got: 21
                  ^^
    ```
*   **意図を導く型設計**: 型はあなたの記述を縛るものではなく、テストの意図（何を期待しているか）を自然に表現させるために存在します。


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
import Test.Hspec
import Test.MockCat

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Quick Start Demo" do
    result <- runMockT do
      -- 1. モックを作成 ("Hello" を受け取ったら 42 を返す)
      --    同時に「1回呼ばれるはずだ」と期待を宣言
      f <- mock ("Hello" ~> (42 :: Int))
        `expects` called once

      -- 2. 関数として使う
      let result = f "Hello"
      
      pure result
    
    -- 3. 結果の検証
    result `shouldBe` 42
```

---

### At a Glance: Matchers
| Matcher | Description | Example |
| :--- | :--- | :--- |
| **`any`** | どんな値でも許可 | `f <- mock (any ~> True)` |
| **`when`** | 条件(述語)で検証 | `f <- mock (when (> 5) "gt 5" ~> True)` |
| **`"val"`** | 値の一致 (Eq) | `f <- mock ("val" ~> True)` |
| **`inOrder`** | 順序検証 | `expects` ブロック内で使用 (後述) |
| **`inPartial`**| 部分順序 | `expects` ブロック内で使用 (後述) |

---

## 使い方ガイド (User Guide)

Mockcat は、テストの目的や環境に応じて 2 つの検証スタイルをサポートしています。

### 1. 宣言的な検証 (`withMock` / `expects`) - [推奨]

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

#### `withMockIO`: IO テストの簡略化
`withMockIO` は `withMock` を IO に特化させたバージョンです。`liftIO` を使わずにモックコンテキスト内で直接 IO アクションを実行できます。

```haskell
it "IO test" $ withMockIO do
  f <- mock (any ~> pure "result")
  res <- someIOCall f
  res `shouldBe` "result"
```

> [!IMPORTANT]
> `expects`（宣言的検証）を使用する場合、モック定義部分は必ず **括弧 `(...)`** で囲んでください。
> 以前のバージョンで使用できた `$` 演算子 (`mock $ ... expected ...`) は、優先順位の関係でコンパイルエラーになります。
>
> ❌ `mock $ any ~> True expects ...`
> ✅ `mock (any ~> True) expects ...`

> [!NOTE]
> `runMockT` ブロックの中でも、同様に `expects` を使った宣言的検証が可能です。
> 生成された型クラスのモック関数（`_xxx`）に対してもそのまま使用できます。
>
> ```haskell
> runMockT do
>   _readFile "config.txt" ~> pure "value"
>     `expects` called once
> ```

### 2. 型クラスのモック (`makeMock`)

既存の型クラスをそのままテストに持ち込みたい場合に使います。Template Haskell を使って、既存の型クラスからモックを自動生成します。

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Strict Mode] デフォルトの動作。「mock」関数と挙動が一致します。
-- 戻り値の型が `m a` の場合、スタブ定義の右辺には `m a` 型の値（例: `pure @IO "value"`, `throwIO Error`）を記述する必要があります。
-- Haskell の型システムに対して正直で、明示的な記述を好む場合に推奨されます。
makeMock [t|FileSystem|]

-- [Auto-Lift Mode] 利便性重視のモード。
-- 純粋な値を自動的にモナド（m String など）に包んで返します。
makeAutoLiftMock [t|FileSystem|]
```

> [!NOTE]
> クラスの定義に応じてさらなる言語拡張（`MultiParamTypeClasses` や `UndecidableInstances` など）が必要な場合、Mockcat はコンパイル時に詳細なエラーメッセージを表示して通知します。

テストコード内では `runMockT` ブロックを使用します。

```haskell
spec :: Spec
spec = do
  it "filesystem test" do
    result <- runMockT do
      -- [Strict Mode] (makeMock 使用時): 明示的に pure で包む
      _readFile $ "config.txt" ~> pure @IO "debug=true"
      _writeFile $ "log.txt" ~> "start" ~> pure @IO ()

      -- [Auto-Lift Mode] (makeAutoLiftMock 使用時): 値は自動的に包まれる (便利)
      -- _readFile $ "config.txt" ~> "debug=true"

      -- テスト対象コードの実行（モックが注入される）
      myProgram "config.txt"
    
    result `shouldBe` ()
```

### 3. 関数のモックと事後検証 (`mock` / `shouldBeCalled`)

最も基本的な使い方です。特定の引数に対して値を返す関数を作ります。
事後検証 (`shouldBeCalled`) を組み合わせることで、探索的なテストやプロトタイピングに適しています。

```haskell
-- "a" -> "b" -> True を返す関数
f <- mock ("a" ~> "b" ~> True)

-- 検証
f `shouldBeCalled` "a"
```

> [!WARNING]
> **HPC (コードカバレッジ) 環境での制限**
> `stack test --coverage` 等を使用する場合、`shouldBeCalled` は使用しないでください。
> GHC のカバレッジ計測機能が関数をラップするため、関数の同一性が失われ、検証に失敗します。
> カバレッジ計測が必要な場合は、**`expects`** スタイル (Section 1) を使用してください。

**柔軟なマッチング**:
具体的な値だけでなく、条件（述語）を指定することもできます。

```haskell
-- 任意の文字列 (param any)
f <- mock (any ~> True)

-- 条件式 (when)
f <- mock (when (> 5) "> 5" ~> True)
```

### 4. 柔軟な検証（マッチャー）

引数が `Eq` インスタンスを持っていなくても、あるいは特定の値に依存したくない場合でも、「どのような条件を満たすべきか」という**意図**で検証できます。
Mockcat は、値の一致だけでなく、関数の性質を検証するための**マッチャー**を提供しています。

#### 任意の値を許可 (`any`)

```haskell
-- どんな引数で呼ばれても True を返す
f <- mock (any ~> True)

-- 何でもいいから呼ばれたことを検証
f `shouldBeCalled` any
```

#### 条件を指定して検証 (`when`)

任意の値ではなく、「条件（述語）」を使って検証できます。
`Eq` を持たない型（関数など）や、部分的な一致を確認したい場合に強力です。

```haskell
-- 引数が "error" で始まる場合のみ False を返す
f <- mock do
  onCase $ when (\s -> "error" `isPrefixOf` s) "start with error" ~> False
  onCase $ any ~> True
```

ラベル（エラー時に表示される説明）が不要な場合は、`when_` を使用することもできます。

```haskell
f <- mock (when_ (> 5) ~> True)
```

### 5. 高度な機能 - [応用]

#### mock vs stub vs mockM の使い分け

基本的には **`mock`** だけ覚えれば十分です。
より細かい制御が必要になった場合に、他の関数を検討してください。

| 関数 | 検証 (`shouldBeCalled`) | IO依存 | 特徴 |
| :--- | :---: | :---: | :--- |
| **`stub`** | ❌ | なし | **純粋なスタブ**。IO に依存しません。検証不要ならこれで十分です。 |
| **`mock`** | ✅ | あり(隠蔽) | **モック**。純粋関数として振る舞いますが、内部的には IO を介して呼び出し履歴を管理します。 |
| **`mockM`** | ✅ | あり(明示) | **Monadic モック**。`MockT` や `IO` の中で使い、副作用（ロギングなど）を明示的に扱えます。 |

#### 部分モック (Partial Mock): 本物の関数と混ぜて使う

一部のメソッドだけモックに差し替え、残りは本物の実装を使いたい場合に便利です。

```haskell
-- [Strict Mode]
makePartialMock [t|FileSystem|]

-- [Auto-Lift Mode]
-- makeAutoLiftMock と同様に、Partial Mock にも Auto-Lift 版があります。
makeAutoLiftPartialMock [t|FileSystem|]

instance FileSystem IO where ... -- 本物のインスタンスも必要

test = runMockT do
  _readFile $ "test" ~> pure @IO "content" -- readFile だけモック化 (Strict)
  -- or
  -- _readFile $ "test" ~> "content" -- (Auto-Lift)

  program -- writeFile は本物の IO インスタンスが走る
```

#### 派生とカスタムインスタンス (Derivation and Custom Instances)

`MockT` を使用する際、モック対象の副作用とは直接関係のない型クラスを扱わなければならないことがあります。Mockcat は、これらのケースを補助するためのマクロを提供しています。

##### MTL インスタンス (`MonadReader`, `MonadError` 等)
`MockT` は、標準的な `mtl` の型クラス（`MonadReader`, `MonadError`, `MonadState`, `MonadWriter`）のインスタンスを標準で備えています。これらのインスタンスは、操作を自動的にベースモナドへリフト（持ち上げ）します。

##### カスタム型クラスの派生 (`deriveMockInstances`)
ベースモナドへリフトするだけでよいカスタムの "Capability" 型クラス（`MonadLogger`, `MonadConfig` 等）については、`deriveMockInstances` を使用できます。

```haskell
class Monad m => MonadLogger m where
  logInfo :: String -> m ()

deriveMockInstances [t|MonadLogger|]
```
これにより、`lift . logInfo` を呼び出す `MockT m` のインスタンスが自動生成されます。

##### 明示的な No-op インスタンス (`deriveNoopInstance`)
メソッド（特に `m ()` を返すもの）に対して、明示的なスタブ定義やベース実装を用意することなく、「何もしない」モックを作成したい場合があります。

```haskell
class Monad m => MonadAuditor m where
  audit :: String -> m ()

deriveNoopInstance [t|MonadAuditor|]
```
これにより、`audit` が単に `pure ()` を返す `MockT m` のインスタンスが生成されます。


---

#### 設計思想: Capability vs. Control

Mockcat は、型クラスの派生において **Capability (能力)** と **Control (制御)** を区別します。

*   **Capability (注入/リフト)**: コンテキストやツールを提供する型クラス（例：`MonadReader`, `MonadLogger`）。
    *   **アプローチ**: `deriveMockInstances` や標準の `mtl` インスタンスを使用します。環境の一貫性を保つため、これらはベースモナドへリフトされるべきです。
*   **Control (モック)**: 外部への副作用やビジネスロジックの境界を表す型クラス（例：`UserRepository`, `PaymentGateway`）。
    *   **アプローチ**: `makeMock` を使用します。テスト対象のロジックを隔離するため、これらは明示的にスタブ定義や検証が行われる必要があります。

---


#### IO アクションを返す (Monadic Return)

`IO` を返す関数で、呼び出しごとに副作用（結果）を変えたい場合に使います。

```haskell
f <- mock do
  onCase $ "get" ~> pure @IO 1 -- 1回目
  onCase $ "get" ~> pure @IO 2 -- 2回目
```

#### 名前付きモック

エラーメッセージに関数名を表示させたい場合は、ラベルを付けられます。

```haskell
f <- mock (label "myAPI") ("arg" ~> True)
```

---

## リファレンス & レシピ (Encyclopedia)

※ このセクションは、困ったときの辞書として使ってください。

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
| `when pred label` | 条件式 | `when (>0) "positive" ~> True` |
| `when_ pred` | ラベルなし | `when_ (>0) ~> True` |

### 宣言的検証 DSL (`expects`)

`expects` ブロックでは、ビルダースタイルの構文を使って宣言的に期待値を記述できます。
`shouldBeCalled` と共通の語彙を使用しています。

#### 基本的な使い方

`called` で開始し、条件を連鎖させて記述します。

```haskell
-- 回数のみ
mock (any ~> True) `expects` called once

-- 引数を指定
mock (any ~> True) `expects` (called once `with` "arg")

-- 複数の期待値 (do ブロック)
mock (any ~> True) `expects` do
  called once `with` "A"
  called once `with` "B"
```

#### 構文リファレンス

| Builder | 説明 | 例 |
| :--- | :--- | :--- |
| **`called`** | **[必須]** 期待値ビルダーを開始します。 | `called ...` |
| **`times n`** | 回数を指定します。 | `called . times 2` |
| **`once`** | `times 1` のエイリアス。 | `called . once` |
| **`never`** | 0回を期待します。 | `called . never` |
| **`with arg`** | 引数を指定します。 | `called `with` "value"` |
| **`with matcher`** | マッチャを使って引数を検証します。 | `called `with` when (>5) "gt 5"` |
| **`inOrder`** | 呼び出し順序を検証 (リスト内で使用) | (順序検証の項を参照) |

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
<summary><strong>Q. コードカバレッジ (HPC) を有効にしてテストを実行できますか？</strong></summary>
A. はい (v1.1.0.0 以降)。Mockcat の `expects` スタイルは、HPC による関数のラップの影響を受けない設計になっているため、HPC下でも安全に動作します。
ただし、前述の理由により **`expects`** スタイル (または `withMock`) の使用を推奨します。
`shouldBeCalled` スタイルは、HPC の仕組み上、モックの同一性を特定できないため使用できません。
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

## ヒントとトラブルシューティング

### `any` と `Prelude.any` の名前衝突
`Test.MockCat` をインポートすると、パラメータマッチャの `any` が `Prelude.any` と衝突することがあります。
その場合は `Prelude` の `any` を隠すか、修飾名を使用してください。

```haskell
import Prelude hiding (any)
-- または
import qualified Test.MockCat as MC
```

### `when` と `Control.Monad.when` の名前衝突
`Test.MockCat` は `when` (パラメータマッチャ) をエクスポートするため、`Control.Monad` の `when` (条件分岐) と衝突することがあります。
その場合は `Test.MockCat` からの `when` を隠すか、修飾名を使用してください。

```haskell
import Test.MockCat hiding (when)
-- または
import Control.Monad hiding (when) -- モックの方を使いたい場合
```

### `OverloadedStrings` 使用時の型推論エラー
`OverloadedStrings` 拡張を有効にしている場合、文字列リテラルの型が曖昧になり、エラーが発生することがあります。
その場合は明示的に型注釈を付けてください。

```haskell
mock (("value" :: String) ~> True)
```

---

_Happy Mocking!_ 🐱