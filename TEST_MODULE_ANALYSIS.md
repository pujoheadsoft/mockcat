# テストモジュール構造分析レポート

## 概要
このレポートは、mockcatプロジェクトのテストモジュールの分け方について、責務、一貫性、重複、欠落などの観点から分析したものです。

## テストファイル一覧

### 基本モジュールテスト
1. **ConsSpec.hs** - `Test.MockCat.Cons` のテスト
2. **ParamSpec.hs** - `Test.MockCat.Param` のテスト
3. **AssociationListSpec.hs** - `Test.MockCat.AssociationList` のテスト
4. **StubSpec.hs** - `stub` 関数のテスト
5. **MockSpec.hs** - `mock` 関数のテスト
6. **ShouldBeCalledSpec.hs** - `shouldBeCalled` API のテスト

### 統合・機能テスト
7. **ExampleSpec.hs** - 使用例と統合テスト
8. **ConcurrencySpec.hs** - 並行実行のテスト
9. **TypeClassSpec.hs** - 型クラスモック（手書き実装）のテスト
10. **TypeClassTHSpec.hs** - 型クラスモック（TH生成）のテスト
11. **PartialMockSpec.hs** - 部分モック（手書き実装）のテスト
12. **PartialMockTHSpec.hs** - 部分モック（TH生成）のテスト

### Template Haskell関連テスト
13. **THCompareSpec.hs** - TH生成コードと手書きコードの比較テスト
14. **TH/ClassAnalysisSpec.hs** - `Test.MockCat.TH.ClassAnalysis` のテスト
15. **TH/ContextBuilderSpec.hs** - `Test.MockCat.TH.ContextBuilder` のテスト
16. **TH/FunctionBuilderSpec.hs** - `Test.MockCat.TH.FunctionBuilder` のテスト
17. **TH/TypeUtilsSpec.hs** - `Test.MockCat.TH.TypeUtils` のテスト

### 内部モジュールテスト
18. **Internal/RegistrySpec.hs** - `Test.MockCat.Internal.Registry` のテスト

### 共有定義
19. **SharedSpecDefs.hs** - テストで使用する型クラス定義の共有
20. **Impl.hs** - テストで使用する実装の共有

## 分析結果

### ✅ 良い点

#### 1. 責務の分離が明確
- **基本モジュール**: 各モジュール（Cons, Param, AssociationList）に個別のテストファイル
- **機能別**: Mock, Stub, Verify（ShouldBeCalled）が分離
- **TH関連**: THモジュールごとに個別のテストファイル
- **統合テスト**: ExampleSpecで使用例を集約

#### 2. 命名規則の一貫性
- すべてのテストファイルが `*Spec.hs` という命名規則に従っている
- モジュール名が `Test.MockCat.*Spec` で統一されている
- ソースモジュールとテストモジュールの対応が明確

#### 3. 階層構造の一貫性
- `Test.MockCat.TH.*` に対応して `test/Test/MockCat/TH/*Spec.hs` が存在
- `Test.MockCat.Internal.*` に対応して `test/Test/MockCat/Internal/*Spec.hs` が存在

### ⚠️ 改善の余地がある点

#### 1. 責務の重複と境界の曖昧さ

**問題点:**
- **MockSpec.hs** と **StubSpec.hs** の境界が曖昧
  - `MockSpec.hs` には `mock` 関数のテストが含まれているが、`stub` 関数の基本テストも含まれている可能性
  - `StubSpec.hs` は `stub` 関数専用だが、`MockSpec.hs` にも関連テストがある

**推奨:**
- `MockSpec.hs` は `mock` 関数（検証可能なモック）に特化
- `StubSpec.hs` は `stub` 関数（検証不可のスタブ）に特化
- 重複しているテストを整理

**問題点:**
- **TypeClassSpec.hs** と **TypeClassTHSpec.hs** の役割分担が不明確
  - `TypeClassSpec.hs`: 手書き実装のテスト（985行）
  - `TypeClassTHSpec.hs`: TH生成実装のテスト（204行）
  - しかし、`TypeClassSpec.hs` には手書き実装だけでなく、TH生成コードも使用している可能性

**推奨:**
- `TypeClassSpec.hs` を手書き実装のテストに特化
- `TypeClassTHSpec.hs` をTH生成実装のテストに特化
- または、統合して `TypeClassSpec.hs` にまとめ、TH生成と手書きの両方をテスト

**問題点:**
- **PartialMockSpec.hs** と **PartialMockTHSpec.hs** の役割分担
  - 同様に、手書き実装とTH生成実装のテストが分離されているが、境界が不明確

#### 2. ファイルサイズの不均衡

**問題点:**
- **TypeClassSpec.hs**: 985行（非常に大きい）
- **PartialMockSpec.hs**: 355行（大きい）
- **ShouldBeCalledSpec.hs**: 736行（大きい）
- **MockSpec.hs**: 197行（中程度）
- 他のファイル: 50-200行程度

**推奨:**
- 大きなファイルを機能別に分割
  - `TypeClassSpec.hs` → `TypeClassBasicSpec.hs`, `TypeClassAdvancedSpec.hs` など
  - `ShouldBeCalledSpec.hs` → `ShouldBeCalledBasicSpec.hs`, `ShouldBeCalledOrderSpec.hs` など
- または、`describe` ブロックで整理し、ファイル内の構造を明確化

#### 3. テストの重複

**問題点:**
- **ExampleSpec.hs** と **MockSpec.hs** に重複するテストがある可能性
  - 例: `ExampleSpec.hs` の "stub" テストと `MockSpec.hs` の基本テスト
  - 例: `ExampleSpec.hs` の "how to use" テストと `MockSpec.hs` の類似テスト

**推奨:**
- `ExampleSpec.hs` は使用例と統合テストに特化
- 基本機能のテストは各機能別のSpecファイルに集約
- 重複を削減し、メンテナンス性を向上

#### 4. 内部モジュールのテスト不足

**問題点:**
- `Test.MockCat.Internal.Builder` の直接テストがない
- `Test.MockCat.Internal.Message` の直接テストがない
- `Test.MockCat.Internal.Types` の直接テストがない

**推奨:**
- 内部モジュールの直接テストを追加
- または、間接テストで十分であることを明記

#### 5. SharedSpecDefs.hs の役割

**良い点:**
- テストで使用する型クラス定義を共有している
- 重複を避けている

**改善点:**
- `Impl.hs` との役割分担が不明確
- ドキュメント化が必要

#### 6. THCompareSpec.hs の位置づけ

**良い点:**
- TH生成コードと手書きコードの比較テストという明確な目的

**改善点:**
- ファイル名が `THCompareSpec.hs` で、THモジュールのテストと混同しやすい
- `TH/` ディレクトリに配置するか、`TH/CompareSpec.hs` にリネームを検討

### 📊 モジュール構造との対応関係

| ソースモジュール | テストファイル | 対応状況 |
|----------------|---------------|---------|
| Test.MockCat.Cons | ConsSpec.hs | ✅ 完全対応 |
| Test.MockCat.Param | ParamSpec.hs | ✅ 完全対応 |
| Test.MockCat.AssociationList | AssociationListSpec.hs | ✅ 完全対応 |
| Test.MockCat.Mock | MockSpec.hs | ✅ 対応（一部StubSpecと重複） |
| Test.MockCat (stub) | StubSpec.hs | ✅ 対応 |
| Test.MockCat.Verify | ShouldBeCalledSpec.hs | ✅ 対応 |
| Test.MockCat.MockT | MockSpec.hs, PartialMockSpec.hs | ⚠️ 間接テスト |
| Test.MockCat.TH | TypeClassTHSpec.hs, PartialMockTHSpec.hs | ✅ 対応 |
| Test.MockCat.TH.ClassAnalysis | TH/ClassAnalysisSpec.hs | ✅ 完全対応 |
| Test.MockCat.TH.ContextBuilder | TH/ContextBuilderSpec.hs | ✅ 完全対応 |
| Test.MockCat.TH.FunctionBuilder | TH/FunctionBuilderSpec.hs | ✅ 完全対応 |
| Test.MockCat.TH.TypeUtils | TH/TypeUtilsSpec.hs | ✅ 完全対応 |
| Test.MockCat.Internal.Registry | Internal/RegistrySpec.hs | ✅ 完全対応 |
| Test.MockCat.Internal.Builder | なし | ❌ 直接テストなし |
| Test.MockCat.Internal.Message | なし | ❌ 直接テストなし |
| Test.MockCat.Internal.Types | なし | ❌ 直接テストなし |

## 推奨される改善策

### 高優先度

1. **MockSpec.hs と StubSpec.hs の整理**
   - 重複テストを削除
   - 各ファイルの責務を明確化
   - `MockSpec.hs`: `mock` 関数（検証可能）に特化
   - `StubSpec.hs`: `stub` 関数（検証不可）に特化

2. **TypeClassSpec.hs の分割**
   - 985行は大きすぎる
   - 機能別に分割するか、`describe` ブロックで整理
   - 例: `TypeClassBasicSpec.hs`, `TypeClassAdvancedSpec.hs`

3. **内部モジュールのテスト追加**
   - `Internal/BuilderSpec.hs` の追加
   - `Internal/MessageSpec.hs` の追加
   - または、間接テストで十分であることを明記

### 中優先度

4. **ExampleSpec.hs の整理**
   - 基本機能のテストを各機能別Specに移動
   - 使用例と統合テストに特化

5. **THCompareSpec.hs の位置づけ**
   - `TH/CompareSpec.hs` にリネーム
   - または、THモジュールのテストとして明確化

6. **SharedSpecDefs.hs と Impl.hs の整理**
   - 役割分担を明確化
   - ドキュメント化

### 低優先度

7. **ShouldBeCalledSpec.hs の分割検討**
   - 736行は大きいが、機能が明確に分かれている
   - `describe` ブロックで整理されているため、分割は任意

8. **命名規則の統一**
   - すべて `*Spec.hs` で統一されているため問題なし
   - ただし、TH関連のテストファイル名を検討

## 総合評価

### スコア: 80/100

**良い点:**
- ✅ 基本的なモジュール構造との対応が良好
- ✅ 命名規則が一貫している
- ✅ 責務の分離が概ね明確
- ✅ TH関連モジュールのテストが充実

**改善点:**
- ⚠️ 一部のファイルが大きすぎる（TypeClassSpec.hs）
- ⚠️ 重複テストがある（MockSpec.hs と StubSpec.hs）
- ⚠️ 内部モジュールの直接テストが不足
- ⚠️ 一部の責務分担が不明確（TypeClassSpec vs TypeClassTHSpec）

### 推奨される構造

```
test/Test/MockCat/
├── Basic/
│   ├── ConsSpec.hs
│   ├── ParamSpec.hs
│   └── AssociationListSpec.hs
├── Core/
│   ├── MockSpec.hs          # mock関数（検証可能）
│   ├── StubSpec.hs          # stub関数（検証不可）
│   └── ShouldBeCalledSpec.hs
├── MockT/
│   ├── MockTSpec.hs         # MockTの基本機能
│   ├── ConcurrencySpec.hs   # 並行実行
│   └── PartialMockSpec.hs   # 部分モック（手書き）
├── TypeClass/
│   ├── TypeClassBasicSpec.hs      # 基本機能
│   ├── TypeClassAdvancedSpec.hs   # 高度な機能
│   └── TypeClassTHSpec.hs         # TH生成
├── TH/
│   ├── ClassAnalysisSpec.hs
│   ├── ContextBuilderSpec.hs
│   ├── FunctionBuilderSpec.hs
│   ├── TypeUtilsSpec.hs
│   └── CompareSpec.hs      # TH生成と手書きの比較
├── Internal/
│   ├── RegistrySpec.hs
│   ├── BuilderSpec.hs      # 追加推奨
│   └── MessageSpec.hs      # 追加推奨
├── Integration/
│   └── ExampleSpec.hs      # 使用例と統合テスト
└── Shared/
    ├── SharedSpecDefs.hs
    └── Impl.hs
```

この構造により、責務がより明確になり、メンテナンス性が向上します。

