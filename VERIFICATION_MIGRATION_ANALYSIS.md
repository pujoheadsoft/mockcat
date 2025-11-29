# 既存検証関数の `shouldBeCalled` への置き換え可能性分析

## 既存の検証関数一覧

1. `shouldApplyTo` - 引数に適用されたか検証（最低1回）
2. `shouldApplyTimes` - 引数に適用された回数を検証
3. `shouldApplyInOrder` - 順序検証（完全一致）
4. `shouldApplyInPartialOrder` - 順序検証（部分一致）
5. `shouldApplyToAnything` - 何かに適用されたか検証（最低1回）
6. `shouldApplyTimesToAnything` - 何かに適用された回数を検証
7. `shouldApplyTimesGreaterThanEqual` - 以上回数検証
8. `shouldApplyTimesLessThanEqual` - 以下回数検証
9. `shouldApplyTimesGreaterThan` - より大きい回数検証
10. `shouldApplyTimesLessThan` - より小さい回数検証

## 使用箇所の分析

### 1. `shouldApplyTo` の使用パターン

**使用箇所:**
- `f `shouldApplyTo` "value"` - 単一引数
- `f `shouldApplyTo` ("a" |> "b")` - 複数引数（Param チェーン）
- `f `shouldApplyTo` ("a" |> (1 :: Int))` - 型付き引数

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTo` "value"
-- 新: f `shouldBeCalled` "value"
-- または: f `shouldBeCalled` calledWith "value"

-- 旧: f `shouldApplyTo` ("a" |> "b")
-- 新: f `shouldBeCalled` ("a" |> "b")
-- または: f `shouldBeCalled` calledWith ("a" |> "b")
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 2. `shouldApplyTimes` の使用パターン

**使用箇所:**
- `f `shouldApplyTimes` (2 :: Int) `to` "value"` - 正確な回数
- `f `shouldApplyTimes` (3 :: Int) `to` ("a" |> (1 :: Int))` - 複数引数

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimes` (2 :: Int) `to` "value"
-- 新: f `shouldBeCalled` (times 2 `withArgs` "value")

-- 旧: f `shouldApplyTimes` (3 :: Int) `to` ("a" |> (1 :: Int))
-- 新: f `shouldBeCalled` (times 3 `withArgs` ("a" |> (1 :: Int)))
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 3. `shouldApplyInOrder` の使用パターン

**使用箇所:**
- `f `shouldApplyInOrder` ["a", "b", "c"]` - 生の値のリスト
- `f `shouldApplyInOrder` ["a" |> True, "b" |> True]` - Param チェーンのリスト
- `f `shouldApplyInOrder` (param <$> xs)` - Param のリスト

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyInOrder` ["a", "b", "c"]
-- 新: f `shouldBeCalled` inOrderWith ["a", "b", "c"]

-- 旧: f `shouldApplyInOrder` ["a" |> True, "b" |> True]
-- 新: f `shouldBeCalled` inOrderWith ["a" |> True, "b" |> True]

-- 旧: f `shouldApplyInOrder` (param <$> xs)
-- 新: f `shouldBeCalled` inOrderWith (param <$> xs)
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 4. `shouldApplyInPartialOrder` の使用パターン

**使用箇所:**
- `f `shouldApplyInPartialOrder` ["a", "c"]` - 生の値のリスト
- `f `shouldApplyInPartialOrder` ["a" |> True, "c" |> True]` - Param チェーンのリスト
- `f `shouldApplyInPartialOrder` (param <$> uniques)` - Param のリスト

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyInPartialOrder` ["a", "c"]
-- 新: f `shouldBeCalled` inPartialOrderWith ["a", "c"]

-- 旧: f `shouldApplyInPartialOrder` ["a" |> True, "c" |> True]
-- 新: f `shouldBeCalled` inPartialOrderWith ["a" |> True, "c" |> True]

-- 旧: f `shouldApplyInPartialOrder` (param <$> uniques)
-- 新: f `shouldBeCalled` inPartialOrderWith (param <$> uniques)
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 5. `shouldApplyToAnything` の使用パターン

**使用箇所:**
- `shouldApplyToAnything f` - 何かに適用されたか検証

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: shouldApplyToAnything f
-- 新: f `shouldBeCalled` anything
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 6. `shouldApplyTimesToAnything` の使用パターン

**使用箇所:**
- `f `shouldApplyTimesToAnything` 3` - 正確な回数
- `f `shouldApplyTimesToAnything` length xs` - 動的な回数

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimesToAnything` 3
-- 新: f `shouldBeCalled` (times 3)
-- または: f `shouldBeCalled` (CountAnyVerification 3) - 内部実装

-- 旧: f `shouldApplyTimesToAnything` length xs
-- 新: f `shouldBeCalled` (times (length xs))
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 7. `shouldApplyTimesGreaterThanEqual` の使用パターン

**使用箇所:**
- `f `shouldApplyTimesGreaterThanEqual` 3 `to` "a"` - 以上回数検証

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimesGreaterThanEqual` 3 `to` "a"
-- 新: f `shouldBeCalled` (atLeast 3 `withArgs` "a")
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 8. `shouldApplyTimesLessThanEqual` の使用パターン

**使用箇所:**
- `f `shouldApplyTimesLessThanEqual` 3 `to` "a"` - 以下回数検証

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimesLessThanEqual` 3 `to` "a"
-- 新: f `shouldBeCalled` (atMost 3 `withArgs` "a")
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 9. `shouldApplyTimesGreaterThan` の使用パターン

**使用箇所:**
- `f `shouldApplyTimesGreaterThan` 2 `to` "a"` - より大きい回数検証

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimesGreaterThan` 2 `to` "a"
-- 新: f `shouldBeCalled` (greaterThan 2 `withArgs` "a")
```

**置き換え可能性:** ✅ 完全に置き換え可能

### 10. `shouldApplyTimesLessThan` の使用パターン

**使用箇所:**
- `f `shouldApplyTimesLessThan` 4 `to` "a"` - より小さい回数検証

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: f `shouldApplyTimesLessThan` 4 `to` "a"
-- 新: f `shouldBeCalled` (lessThan 4 `withArgs` "a")
```

**置き換え可能性:** ✅ 完全に置き換え可能

## 特殊な使用パターン

### `shouldApplyToAnythingResolved` の使用

**使用箇所:**
- `Verify.shouldApplyToAnythingResolved resolved` - 内部実装での使用

**`shouldBeCalled` での置き換え:**
```haskell
-- 旧: Verify.shouldApplyToAnythingResolved resolved
-- 新: shouldBeCalled (ResolvedMock name verifier) anything
-- ただし、これは内部実装なので、shouldBeCalled の実装を確認する必要がある
```

**置き換え可能性:** ⚠️ 内部実装のため、`shouldBeCalled` の実装を確認する必要がある

## まとめ

### 置き換え可能な関数（10個中10個）

すべての既存検証関数は `shouldBeCalled` で置き換え可能です。

### 置き換えマッピング表

| 既存関数 | shouldBeCalled での置き換え |
|---------|---------------------------|
| `f `shouldApplyTo` arg` | `f `shouldBeCalled` arg` |
| `f `shouldApplyTimes` n `to` arg` | `f `shouldBeCalled` (times n `withArgs` arg)` |
| `f `shouldApplyInOrder` args` | `f `shouldBeCalled` inOrderWith args` |
| `f `shouldApplyInPartialOrder` args` | `f `shouldBeCalled` inPartialOrderWith args` |
| `shouldApplyToAnything f` | `f `shouldBeCalled` anything` |
| `f `shouldApplyTimesToAnything` n` | `f `shouldBeCalled` (times n)` |
| `f `shouldApplyTimesGreaterThanEqual` n `to` arg` | `f `shouldBeCalled` (atLeast n `withArgs` arg)` |
| `f `shouldApplyTimesLessThanEqual` n `to` arg` | `f `shouldBeCalled` (atMost n `withArgs` arg)` |
| `f `shouldApplyTimesGreaterThan` n `to` arg` | `f `shouldBeCalled` (greaterThan n `withArgs` arg)` |
| `f `shouldApplyTimesLessThan` n `to` arg` | `f `shouldBeCalled` (lessThan n `withArgs` arg)` |

### 注意点

1. **内部実装 (`shouldApplyToAnythingResolved`)**: これは内部実装で使用されているため、`shouldBeCalled` の実装を確認する必要があります。

2. **`to` 演算子**: `shouldApplyTimes` では `to` 演算子を使用していますが、`shouldBeCalled` では `withArgs` を使用します。

3. **エラーメッセージ**: 置き換え後もエラーメッセージが同じかどうかを確認する必要があります。

## 結論

**すべての既存検証関数は `shouldBeCalled` で完全に置き換え可能です。**

ただし、以下の点を確認する必要があります：
1. エラーメッセージの一貫性
2. 内部実装 (`shouldApplyToAnythingResolved`) の扱い
3. 型推論の動作

