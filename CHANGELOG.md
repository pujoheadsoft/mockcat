# Changelog for `mockcat`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.3.0.0] - 2026-01-03
### Added
- **Type-Safe Verification Result**: Generated mock helpers now return `MockResult params`. This type carries parameter information, enabling robust type inference and compile-time safety checks when using declarative verification (`expects`).
    - *This change is part of an ongoing effort to make misuse of mocks impossible at the type level.*

### Changed
- **Breaking Change**: Due to the introduction of `MockResult`, generated mock helpers (e.g., `_myMethod`) no longer return the mock function itself (`MockT m (FunctionType)`).
    - Code that previously relied on capturing the returned function (e.g., `fn <- _myMethod ...`) will need to be updated.
- **Refactoring**: Reorganized internal test verification logic to utilize `MockResult` properties.
- **Internal**: Refactored test suite organization for better maintainability.

## [1.2.1.0] - 2026-01-01
### Added
- **Dynamic Language Extension Detection**: `mockcat` now automatically identifies and requests only necessary extensions (e.g., `MultiParamTypeClasses`, `UndecidableInstances`) based on the target class definition.
- **Granular Extension Optimization**: Simple multi-parameter type classes no longer require `AllowAmbiguousTypes` or `FunctionalDependencies` unless they are actually used.

### Fixed
- Resolved `NoVerification` scope issues in Template Haskell generated code.
- Resolved ambiguous `any` occurrences in internal Template Haskell logic.
- Fixed several typos in Template Haskell error messages.

## [1.2.0.0] - 2025-12-31
### Changed
- **Breaking Change**: Changed the fixity of `expects` operator to `infixl 0` to resolve precedence issues with `$`.
    - **Impact**: Code using `mock $ ... expects ...` will now fail to compile.
    - **Migration**: Wrap the mock definition in parentheses: `mock (... ~> ...) expects ...`.
- **Breaking Change**: `expects` now strictly enforces that it can only be applied to a mock creation action (`m fn`).
    - Attempting to apply `expects` directly to a `MockSpec` (e.g. `(any ~> 1) expects ...`) or an already instantiated function will result in a compile-time `TypeError`.
- Removed redundant `Typeable` constraints from `expects`, enabling cleaner builds on GHC 9.8+.

## [1.1.0.0] - 2025-12-29
### Added
- **HPC Coverage Support**: Verification logic now robustly handles unstable `StableNames` caused by HPC instrumentation (`stack test --coverage`).
- **Optimization Hardening**: Protected verification logic against GHC optimizations (CSE/LICM) to ensure stable tests in optimized builds.

### Changed
- **Automatic History Reset**: `runMockT` and `withMock` now strictly scope mock history. History is automatically reset to prevent interference between sequential tests or Property-Based Testing iterations.

## [1.0.0.0] - 2025-12-24
### Changed
- **DSL Reboot**: Replaced `|>` with `~>` as the primary parameter chain operator (representing the "mock arrow").
- **Terminology Shift**: Standardized terminology to "called" instead of "applied" throughout the library and error messages.
- Simplified creating/stubbing API: `f <- mock $ ...` is now the canonical way.
- Expanded structural diffing support for nested records and lists.
- Unified verification API: All verification is now handled via `shouldBeCalled`.
- **Strict by Default**: `makeMock` and `makePartialMock` now default to strict return values (implicit monadic return is disabled). `makeAutoLiftMock` was introduced for the previous behavior.

### Added
- Deep Structural Diff: Enhanced error messages with precise caret pointers for complex nested data structures.
- STM-based concurrency for mock registration and call recording.
- Infinite arity support for mock/stub building.

### Removed
- Backward compatibility with 0.x.x APIs (`stubFn`, `createMock`, `applied`, etc.).
- `makeMockWithOptions`, `makePartialMockWithOptions`, and `MockOptions` (internalized to simplify API).

### Migration Guide (0.x -> 1.0)
This release is a complete reboot. Previous code **will break**.

1.  **Operator Change**: Replace `|>` with `~>`.
    ```haskell
    -- Old
    createStubFn $ "arg" |> "result"
    
    -- New
    stub $ "arg" ~> "result"
    ```

2.  **Mock Creation**: Use `mock` / `stub` instead of `createMock` / `createStubFn`.
    ```haskell
    -- Old
    f <- createMock $ "arg" |> "result"
    
    -- New
    f <- mock $ "arg" ~> "result"
    ```

3.  **Verification**: Use `shouldBeCalled` (unified API).
    ```haskell
    -- Old
    f `shouldApplyTo` "arg"
    
    -- New
    f `shouldBeCalled` "arg"
    ```

4.  **Template Haskell Generics**:
    `makeMock` is now strict by default (requires explicit `pure` for IO actions).
    - Use `makeAutoLiftMock` for old implicit behavior.
    - Or stick to `makeMock` and add `pure` to your return values.

## 0.6.0.0
### Changed
- Removed the upper limit on variable arguments when creating stub functions. Previously, there was a restriction on the maximum number of arguments, but this limitation has been removed, allowing stub functions to accept an arbitrary number of arguments.

## 0.5.5.0
### Added
- Aliases `expectApplyTimes` and `expectNever` (preferred names) for pre-run expectation declarations.

### Documentation
- README (EN/JA) now recommends `expectApplyTimes` / `expectNever` over legacy `applyTimesIs` / `neverApply`.
- Clarified that `expectApplyTimes n` is the canonical form; `expectNever` is sugar for `expectApplyTimes 0`.

### Notes
- Legacy names remain exported for backward compatibility (no deprecation pragma yet). They may receive a soft deprecation notice in a future minor release after community feedback.


## 0.5.4.0
### Added
- Parallel execution support (verified counting under concurrency, stress tests).
- Verification helpers: `applyTimesIs`, `neverApply`.

### Changed
- Refactored `MockT` from `StateT` to `ReaderT (TVar [Definition])` architecture.
- Simplified Template Haskell generated constraints.

### Fixed
- Race causing lost/double count in concurrent stub applications (strict `modifyTVar'`).

### Removed
- `unsafePerformIO` in TH-generated code.

### Internal
- Introduced `MonadMockDefs` abstraction.

## 0.5.3.0
### Added
- `MonadUnliftIO` instance for `MockT` (initial groundwork for later parallel support).
