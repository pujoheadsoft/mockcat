# Changelog for `mockcat`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [1.0.0.0] - 2024-12-23
### Changed
- **DSL Reboot**: Replaced `|>` with `~>` as the primary parameter chain operator (representing the "mock arrow").
- **Terminology Shift**: Standardized terminology to "called" instead of "applied" throughout the library and error messages.
- Simplified creating/stubbing API: `f <- mock $ ...` is now the canonical way.
- Expanded structural diffing support for nested records and lists.
- Unified verification API: All verification is now handled via `shouldBeCalled`.

### Added
- Deep Structural Diff: Enhanced error messages with precise caret pointers for complex nested data structures.
- STM-based concurrency for mock registration and call recording.
- Infinite arity support for mock/stub building.

### Removed
- Backward compatibility with 0.x.x APIs (`stubFn`, `createMock`, `applied`, etc.).

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
