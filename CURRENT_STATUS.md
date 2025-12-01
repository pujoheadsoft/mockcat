# 現在の状況まとめ

## 元々の目的

Haskellのモックライブラリ（mockcat）に新しいDSLを追加する。以下の機能を実現：

1. **`expects` DSL**: モック関数の期待値を宣言的に記述
2. **`withMock` ブロック**: モックを生成し、自動検証を実行
3. **`do` 構文サポート**: 複数の期待値を `do` ブロックで記述可能
4. **自然な記述**: `called once` や `called once `with` "a"` のような自然な記述を可能に

## ゴール

以下のようなコードが動作すること：

### 基本的なパターン

1. **単一の期待値（引数あり）**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called once `with` "a")
  liftIO $ mockFn "a" `shouldBe` True
```

2. **単一の期待値（引数なし、回数のみ）**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called once)
  void $ liftIO $ evaluate $ mockFn "a"
```

3. **`param` を使った期待値**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called once `with` (param "a"))
  liftIO $ mockFn "a" `shouldBe` True
```

4. **`atLeast` などの他の回数指定**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called (atLeast 2) `with` "a")
  void $ liftIO $ evaluate $ mockFn "a"
  void $ liftIO $ evaluate $ mockFn "a"
  void $ liftIO $ evaluate $ mockFn "a"
```

### 複数の期待値（`do` ブロック）

5. **`do` ブロックで複数の期待値を記述**:
```haskell
withMock $ do
  mockFn <- mock (any |> any |> True)
    `expects` do
      called (times 3) `with` ("a" |> "b")
      called once     `with` ("x" |> "x")
      called never    `with` ("z" |> "z")
  
  liftIO $ evaluate $ mockFn "a" "b"
  liftIO $ evaluate $ mockFn "a" "b"
  liftIO $ evaluate $ mockFn "a" "b"
  liftIO $ evaluate $ mockFn "x" "x"
```

### 失敗ケースとエラーメッセージ

6. **呼ばれなかった場合のエラー**:
```haskell
(withMock $ do
  _ <- mock (any |> True)
    `expects` (called once `with` "a")
  pure ()) `shouldThrow` anyErrorCall
```

7. **エラーメッセージの検証**:
```haskell
result <- try $ withMock $ do
  _ <- mock (any |> True)
    `expects` (called once `with` "a")
  pure ()
case result of
  Left (ErrorCall msg) -> do
    let expected = "function was not applied the expected number of times to the expected arguments.\n" <>
                   "  expected: 1\n" <>
                   "   but got: 0"
    msg `shouldBe` expected
```

8. **`never` 期待値（呼ばれてはいけない）**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called never `with` "z")
  void $ liftIO $ evaluate $ mockFn "a"  -- "z" 以外ならOK
```

9. **`never` 期待値が失敗する場合**:
```haskell
(withMock $ do
  mockFn <- mock (any |> True)
    `expects` (called never `with` "z")
  void $ liftIO $ evaluate $ mockFn "z"  -- エラーになる
  pure ()) `shouldThrow` anyErrorCall
```

### 順序検証

10. **完全な順序検証（`calledInOrder`）**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` do
      calledInOrder ["a" |> True, "b" |> True, "c" |> True]
  
  evaluate $ mockFn "a"
  evaluate $ mockFn "b"
  evaluate $ mockFn "c"
```

11. **部分的な順序検証（`calledInSequence`）**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` do
      calledInSequence ["a" |> True, "c" |> True]
  
  evaluate $ mockFn "a"
  evaluate $ mockFn "b"  -- これは無視される
  evaluate $ mockFn "c"
```

### 複数のモック関数

12. **複数のモック関数を定義**:
```haskell
withMock $ do
  fn1 <- mock (any |> True)
    `expects` do
      called once `with` "a"
  
  fn2 <- mock (any |> any |> False)
    `expects` do
      called once `with` ("x" |> "y")
  
  evaluate $ fn1 "a"
  evaluate $ fn2 "x" "y"
```

### `runMockT` との連携

13. **`withMock` 内で `runMockT` を使用**:
```haskell
withMock $ do
  result <- runMockT do
    _readFile $ "input.txt" |> pack "content"
    _writeFile $ "output.txt" |> pack "content" |> ()
    operationProgram "input.txt" "output.txt"
  
  result `shouldBe` ()
```

### スコープ分離

14. **複数の `withMock` ブロックが独立している**:
```haskell
-- 最初のブロック
withMock $ do
  fn1 <- mock (any |> True)
    `expects` do
      called once `with` "a"
  evaluate $ fn1 "a"

-- 2番目のブロック（独立している）
withMock $ do
  fn2 <- mock (any |> True)
    `expects` do
      called never `with` "a"  -- 最初のブロックの影響を受けない
  pure ()
```

### 並行性

15. **並行スレッドでの呼び出しカウント**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` do
      called (times 10)
  
  withRunInIO $ \runInIO -> do
    as <- replicateM 10 (async $ runInIO $ do
      evaluate $ mockFn "a"
      pure ())
    mapM_ wait as
```

16. **異なる引数での並行呼び出し**:
```haskell
withMock $ do
  mockFn <- mock (any |> True)
    `expects` do
      called (times 5) `with` "a"
      called (times 5) `with` "b"
  
  withRunInIO $ \runInIO -> do
    as1 <- replicateM 5 (async $ runInIO $ evaluate $ mockFn "a")
    as2 <- replicateM 5 (async $ runInIO $ evaluate $ mockFn "b")
    mapM_ wait (as1 ++ as2)
```

## 現在の状況

### 完了していること

1. **`Expectations` モナドの実装**
   - `newtype Expectations params a = Expectations (State [Expectation params] a)`
   - `Functor`, `Applicative`, `Monad` インスタンス（`GeneralizedNewtypeDeriving` で導出）
   - `do` 構文で複数の期待値を記述可能

2. **`withMock` の基本実装**
   - `ReaderT` ベースのコンテキスト管理
   - 自動検証の実装

3. **`expects` の基本実装**
   - 型シグネチャ: `m fn -> Expectations params () -> m fn`
   - `ResolvableParamsOf fn ~ params` で `params` を推論

4. **`called` 関数の実装**
   - 型: `forall params. (Typeable params) => TimesSpec -> Expectations params ()`
   - `CountAnyExpectation` を生成

5. **`with` 関数の実装**
   - `WithArgs` 型クラスで実装
   - 生の値（`"a"`）と `Param` 値（`param "a"`）の両方を受け入れ可能

6. **テストケースの記述**
   - `test/Test/MockCat/WithMockSpec.hs` に多数のテストケースを記述済み
   - ただし、多くのテストケースがコメントアウトされている（実装待ち）

### テストファイルの状況

- **有効なテストケース**: 基本的なパターン（単一期待値、エラーメッセージなど）
- **コメントアウトされているテストケース**:
  - 複数の期待値（`do` ブロック）
  - `never` 期待値
  - 順序検証（`calledInOrder`, `calledInSequence`）
  - 複数のモック関数
  - `runMockT` との連携
  - スコープ分離
  - 並行性テスト

### 現在の課題

**型推論の問題**: `called once` の型推論ができない

- **エラー**: `No instance for 'Typeable params0' arising from a use of 'called'`
- **原因**: 
  - `called` は `forall params. (Typeable params) => TimesSpec -> Expectations params ()` という型
  - `expects` のコンテキストで `params` が推論できない
  - `expects` の型シグネチャでは `ResolvableParamsOf fn ~ params` という制約があるが、`called once` は `expects` の外で評価されるため、`params` が推論できない

- **具体例**:
  ```haskell
  mockFn <- mock (any |> True)
    `expects` (called once)  -- ここで型推論エラー
  ```

### 次にやるべきこと

#### 最優先: 型推論の問題を解決

1. **型推論の問題を解決**
   - `called once` の型推論ができない問題を解決
   - `called` を `expects` のコンテキストで使えるようにする
   - または、`expects` の型シグネチャを調整して `params` を推論可能にする
   - または、`called` を型クラスベースにして、`expects` のコンテキストで解決できるようにする

#### 基本機能の実装とテスト

2. **基本的な期待値パターンの実装**
   - `called once` が動作することを確認
   - `called once `with` "a"` が動作することを確認
   - `called (atLeast n)` が動作することを確認
   - `called never` が動作することを確認

3. **`do` ブロックでの複数期待値**
   - `do` ブロックで複数の `called` を記述できることを確認
   - 複数の期待値が正しく検証されることを確認

4. **エラーメッセージの実装**
   - 呼ばれなかった場合のエラーメッセージ
   - 呼び出し回数が不一致の場合のエラーメッセージ
   - 引数が不一致の場合のエラーメッセージ
   - `never` 期待値が失敗した場合のエラーメッセージ

#### 高度な機能の実装

5. **順序検証の実装**
   - `calledInOrder` の実装とテスト
   - `calledInSequence` の実装とテスト
   - 並行環境での順序検証

6. **複数のモック関数**
   - 複数のモック関数を定義できることを確認
   - 各モック関数が独立して検証されることを確認

7. **`runMockT` との連携**
   - `withMock` 内で `runMockT` が使用できることを確認
   - `runMockT` の検証が `withMock` の検証と統合されることを確認

#### スコープ分離と並行性

8. **スコープ分離の実装**
   - 複数の `withMock` ブロックが独立していることを確認
   - 各ブロックのモック状態が分離されていることを確認

9. **並行性の実装**
   - 並行スレッドでの呼び出しカウントが正しく動作することを確認
   - 異なる引数での並行呼び出しが正しく動作することを確認
   - ストレステスト（多数のスレッドと多数の呼び出し）

#### その他

10. **`unsafePerformIO` の除去**
    - 可能な箇所で `unsafePerformIO` を除去
    - スレッドセーフティを確保

11. **コメントアウトされているテストケースの有効化**
    - テストファイル内のコメントアウトされているテストケースを有効化
    - すべてのテストケースが通ることを確認

## 技術的な詳細

### ファイル構成

- **実装**: `src/Test/MockCat/WithMock.hs`
- **テスト**: `test/Test/MockCat/WithMockSpec.hs`

### 主要な型

```haskell
newtype Expectations params a = Expectations (State [Expectation params] a)

expects :: 
  forall m fn params.
  ( MonadIO m
  , MonadReader WithMockContext m
  , ResolvableMock fn
  , ResolvableParamsOf fn ~ params
  , Typeable params
  , Typeable (Verifier params)
  , Show params
  , Eq params
  ) =>
  m fn ->
  Expectations params () ->
  m fn

called :: 
  forall params.
  (Typeable params) =>
  TimesSpec -> Expectations params ()
```

### 型推論の問題の詳細

`expects` の型シグネチャでは `ResolvableParamsOf fn ~ params` という制約があるため、`fn` から `params` を推論できます。しかし、`called once` は `expects` の外で評価されるため、`params` が推論できません。

解決策の候補：
1. `called` を型クラスにして、`expects` のコンテキストで解決できるようにする
2. `expects` の中で `called` を使えるように、`expects` の実装を変更する
3. `called` に型注釈を必要としないように、型推論を改善する

## ビルド状況

- **ライブラリ**: ビルド成功（警告あり）
- **テスト**: 型推論エラーでコンパイル失敗

## 参考情報

- `ResolvableParamsOf` は `src/Test/MockCat/Verify.hs` で定義されている型ファミリー
- `mock` 関数は `src/Test/MockCat/Mock.hs` で定義されている
- `Param` 型は `src/Test/MockCat/Param.hs` で定義されている

