# Changelog for `mockcat`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.5.4.0
### Added
- Parallel execution support (verified counting under concurrency, stress tests).
- Verification helpers: `applyTimesIs`, `neverApply`.

### Changed
- Refactored `MockT` from `StateT` to `ReaderT (IORef [Definition])` architecture.
- Simplified Template Haskell generated constraints.

### Fixed
- Race causing lost/double count in concurrent stub applications (strict `atomicModifyIORef'`).

### Removed
- `unsafePerformIO` in TH-generated code.

### Internal
- Introduced `MonadMockDefs` abstraction.

## 0.5.3.0
### Added
- `MonadUnliftIO` instance for `MockT` (initial groundwork for later parallel support).
