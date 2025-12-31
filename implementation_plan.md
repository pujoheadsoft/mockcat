# Implementation Plan - Closest Match Error Messages

The goal is to provide helpful error messages when a mock is expected to be called with specific arguments but isn't (count = 0), yet other calls were recorded. The message should show the diff against the closest matching call and list the call history.

# Unify Error Message Diffs

## User Review Required
None. This is a consistency improvement.

## Proposed Changes

### [Unify Error Formats]
Unify `shouldBeCalled` error messages with the new "Closest match" format used by `withMock`.

#### [MODIFY] [Message.hs](file:///home/kenji/source/haskell/mockcat/src/Test/MockCat/Internal/Message.hs)
- Refactor `verifyFailedMessage` to use `countWithArgsMismatchMessageWithDiff`, enabling detailed diffs for explicit verification.

#### [MODIFY] [ShouldBeCalledErrorDiffSpec.hs](file:///home/kenji/source/haskell/mockcat/test/Test/MockCat/ShouldBeCalledErrorDiffSpec.hs)
- Update test expectations to match the new "Closest match" / "Call history" format.

### [Clean Up]
Fix unused import warnings causing build failures on GHC 9.8.4.

#### [MODIFY] [Verify.hs](file:///home/kenji/source/haskell/mockcat/src/Test/MockCat/Internal/Verify.hs)
- Remove redundant imports (`listToMaybe`, `AssociationList`).

#### [MODIFY] [Builder.hs](file:///home/kenji/source/haskell/mockcat/src/Test/MockCat/Internal/Builder.hs)
- Remove redundant import (`Test.MockCat.Internal.Message`).

## Verification Plan
### Automated Tests
- `stack test --ta '--match "Error Message Diff"'` (checks both specs)
- `./test.sh 9.8.4` (verifies clean build)

### [Post-Verification Cleanup]
Remove the temporary `-Wno-redundant-constraints` pragmas and properly remove the redundant constraints from the code.

#### [MODIFY] [PartialMockSpec.hs](file:///home/kenji/source/haskell/mockcat/test/Test/MockCat/PartialMockSpec.hs)
#### [MODIFY] [TypeClassSpec.hs](file:///home/kenji/source/haskell/mockcat/test/Test/MockCat/TypeClassSpec.hs)
#### [MODIFY] [ReadmeVerifySpec.hs](file:///home/kenji/source/haskell/mockcat/test/ReadmeVerifySpec.hs)
- Remove `{-# OPTIONS_GHC -Wno-redundant-constraints #-}`.
- Remove the specific constraints flagged as redundant by GHC 9.8.4.

## User Review Required

> [!IMPORTANT]
> This changes the error message format for `countWithArgsMismatchMessage` when `callCount` is 0 and `invocationList` is not empty. Existing tests checking for strict "expected: N, got: 0" messages might need updates (specifically `WithMockErrorDiffSpec.hs` which we are fixing).

## Proposed Changes

### [Test.MockCat]

#### [MODIFY] [Verify.hs](file:///home/kenji/source/haskell/mockcat/src/Test/MockCat/Internal/Verify.hs)
- Update `verifyResolvedCount` to:
    - Capture the `invocationList`
    - When verification fails (specifically when expected > 0 but actual == 0):
        - Call a new error formatting function `countWithArgsMismatchMessageWithDiff` instead of the simple `countWithArgsMismatchMessage`.
        - Pass `invocationList` and the expected `v` (value) to this function.

#### [MODIFY] [Message.hs](file:///home/kenji/source/haskell/mockcat/src/Test/MockCat/Internal/Message.hs)
- Implement `countWithArgsMismatchMessageWithDiff`:
    - Logic to find the "closest" match from `invocationList` relative to the expected argument.
    - Use `structuralDiff` (existing) to generate the diff for the closest match.
    - Format the "Call history" section, marking the closest match with `[Closest]`.
- Refactor/Expose `chooseNearest` or similar logic if needed to support `Eq/Show` generic types (currently `messageForMultiMock` uses string-based logic).

#### [MODIFY] [WithMockErrorDiffSpec.hs](file:///home/kenji/source/haskell/mockcat/test/Test/MockCat/WithMockErrorDiffSpec.hs)
- Update the expected error strings to match the new format approved by the user.
- Re-enable the pending tests (remove `xdescribe`).

## Verification Plan

### Automated Tests
- Run `stack test --ta '--match "Error Message Diff"'` to verify `WithMockErrorDiffSpec` passes with the new format.
- Run full `stack test` to ensure no regressions in other count verification tests (e.g. simple count mismatches).

### Manual Verification
- None required beyond automated tests as this is a text formatting change covered by the spec.
