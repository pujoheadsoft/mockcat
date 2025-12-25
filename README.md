<div align="center">
    <img src="https://raw.githubusercontent.com/pujoheadsoft/mockcat/main/logo.png" width="600px" alt="Mockcat Logo">
    <h1>Declarative mocking with a single arrow <code>~&gt;</code></h1>
</div>

<div align="center">

[![Hackage](https://img.shields.io/hackage/v/mockcat.svg)](https://hackage.haskell.org/package/mockcat)
[![Stackage LTS](http://stackage.org/package/mockcat/badge/lts)](http://stackage.org/lts/package/mockcat)
[![Build Status](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions)

</div>

**Mockcat** is a lightweight, declarative mocking library for Haskell.  
By using the dedicated **Mock Arrow (`~>`)** operator, you can describe mock behavior with the same ease as defining standard functions.

```haskell
-- Define
f <- mock $ "input" ~> "output"

-- Verify
f `shouldBeCalled` "input"
```

---

## Concepts & Terminology

Mockcat clearly distinguishes between "Stubs" and "Mocks" (terms often confused), providing the best tool for each purpose.

| Term | Role | Corresponding Function |
| :--- | :--- | :--- |
| **Stub** | **"The Stand-in"**<br>Exists solely to keep the test moving by returning fixed values.<br>Has no side effects and doesn't care "how it was called". | **`stub`**<br>Returns a pure function.<br>No verification hooks. No side effects.<br>Extremely lightweight. |
| **Mock** | **"Mimic & Verify"**<br>In addition to stubbing, it records calls to verify "was it called as expected?".<br>Used for Interaction Testing between objects. | **`mock`** / **`mockM`**<br>Returns values while recording calls.<br>Can be verified with `shouldBeCalled`, etc. |

---

## Why Mockcat?

Writing mocks in Haskell should be simpler.  
Mockcat is designed to thoroughly eliminate boilerplate, allowing you to focus purely on the essence of your tests.

| | **Before: Handwritten...** 😫 | **After: Mockcat** 🐱✨ |
| :--- | :--- | :--- |
| **Definition (Stub)**<br>"I want to return<br>this value for this arg" | <pre lang="haskell">f :: String -> IO String<br>f arg = case arg of<br>  "a" -> pure "b"<br>  _   -> error "unexpected"</pre><br>_Even simple branching consumes many lines._ | <pre lang="haskell">let f = stub $<br>  "a" ~> "b"<br><br><br></pre><br>_If you don't need verification, `stub` is enough.<br>Behaves as a completely pure function._ |
| **Verification (Verify)**<br>"I want to test<br>if it was called correctly" | <pre lang="haskell">-- Need to implement recording logic<br>ref <- newIORef []<br>let f arg = do<br>      modifyIORef ref (arg:)<br>      ...<br><br>-- Verification logic<br>calls <- readIORef ref<br>calls \`shouldBe\` ["a"]</pre><br>_Building the "verification machinery" takes time and hides the intent._ | <pre lang="haskell">-- Define & start monitoring in 1 line<br>f <- mock $ "a" ~> "b"<br><br>-- Just state what you want to verify<br>f \`shouldBeCalled\` "a"<br><br><br><br></pre><br>_Recording is automatic.<br>Focus on the "Why" and "What", not the "How"._ |

### Key Features

*   **Intuitive DSL**: Chain arguments and return values with `~>`. It reads just like a function signature.
*   **Automatic Mock Generation**: Generate mocks from typeclasses in one line using Template Haskell (`makeMock`).
*   **Helpful Error Messages**: Shows "structural diffs" on failure, highlighting exactly what didn't match.
    ```text
    Expected arguments were not called.
      expected: [Record { name = "Alice", age = 20 }]
       but got: [Record { name = "Alice", age = 21 }]
                                                ^^
    ```
*   **Robust Design**: Thread-safe invocation counting and lazy-evaluation friendly.


---

## Quick Start

Copy and paste the code below to experience Mockcat right now.

### Installation

`package.yaml`:
```yaml
dependencies:
  - mockcat
```

Or `.cabal`:
```cabal
build-depends:
    mockcat
```

### First Test (`Main.hs` / `Spec.hs`)

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
    -- 1. Create a mock (Return 42 when receiving "Hello")
    f <- mock $ "Hello" ~> (42 :: Int)

    -- 2. Use it as a function
    let result = f "Hello"
    result `shouldBe` 42

    -- 3. Verify it was called
    f `shouldBeCalled` "Hello"
```

---

## User Guide

### 1. Function Mocking (`mock`)

The most basic usage. Creates a function that returns values for specific arguments.

```haskell
-- Function that returns True for "a" -> "b"
f <- mock $ "a" ~> "b" ~> True
```

**Flexible Matching**:
You can specify conditions (predicates) instead of concrete values.

```haskell
-- Arbitrary string (param any)
f <- mock $ any ~> True

-- Condition (expect)
f <- mock $ expect (> 5) "> 5" ~> True
```

### 2. Typeclass Mocking (`makeMock`)

The most common use case in real-world applications. Generates mocks from existing typeclasses using Template Haskell.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Strict Mode] Default behavior. Consistent with 'mock'.
-- If the return type is `m a`, the stub definition must return a value of type `m a` (e.g., `pure @IO "value"`, `throwIO Error`).
-- Recommended when you prefer explicit descriptions faithful to Haskell's type system.
makeMock [t|FileSystem|]

-- [Auto-Lift Mode] Convenience-focused mode.
-- Automatically wraps pure values into the monad (m String).
makeAutoLiftMock [t|FileSystem|]
```

Use `runMockT` block in your tests.

```haskell
spec :: Spec
spec = do
  it "filesystem test" do
    result <- runMockT do
      -- [Strict Mode] (if using makeMock)
      _readFile $ "config.txt" ~> pure @IO "debug=true"
      _writeFile $ "log.txt" ~> "start" ~> pure @IO ()

      -- [Auto-Lift Mode] (if using makeAutoLiftMock)
      -- _readFile $ "config.txt" ~> "debug=true"

      -- Run code under test (mock injected)
      myProgram "config.txt"
    
    result `shouldBe` ()
```

### 3. Declarative Verification (`withMock` / `expects`)

A style where you describe expectations at definition time. Verification runs automatically when exiting the scope.
Useful when you want "Definition" and "Verification" to be written close together.

```haskell
withMock $ do
  -- Write expectations (expects) at definition time
  f <- mock (any ~> True)
    `expects` do
      called once `with` "arg"

  -- Execution
  f "arg"
```

> [!NOTE]
> You can also use `expects` for declarative verification inside `runMockT` blocks.
> This provides a unified experience where "Mock Creation" and "Expectation Declaration" complete within a single block.

### 4. Advanced Features

#### mock vs stub vs mockM

Mockcat offers 3 ways to create functions. Choose the one that fits your needs.

| Function | Verification (`shouldBeCalled`) | IO Dependency | Characteristics |
| :--- | :---: | :---: | :--- |
| **`stub`** | ❌ | None | **Pure Stub**. No IO dependency. Sufficient if verification isn't needed. |
| **`mock`** | ✅ | Yes (Hidden) | **Mock**. Behaves as a pure function, but internally manages call history via IO. |
| **`mockM`** | ✅ | Yes (Explicit) | **Monadic Mock**. Used within `MockT` or `IO`, allowing explicit handling of side effects (e.g., logging). |

#### Partial Mocking: Mixing with Real Functions

Useful when you want to replace only some methods with mocks while using real implementations for others.

```haskell
-- [Strict Mode]
makePartialMock [t|FileSystem|]

-- [Auto-Lift Mode]
-- Just like makeAutoLiftMock, there is an Auto-Lift version for Partial Mock.
makeAutoLiftPartialMock [t|FileSystem|]

instance FileSystem IO where ... -- Real instance is also required

test = runMockT do
  _readFile $ "test" ~> pure @IO "content" -- Only mock readFile (Strict)
  -- or
  -- _readFile $ "test" ~> "content" -- (Auto-Lift)

  program -- writeFile runs the real IO instance
```

#### Monadic Return (`IO a`)

Used when you want a function returning `IO` to have different side effects (results) for each call.

```haskell
f <- mock $ do
  onCase $ "get" ~> pure @IO 1 -- 1st call
  onCase $ "get" ~> pure @IO 2 -- 2nd call
```

#### Named Mocks

You can attach labels to display function names in error messages.

```haskell
f <- mock (label "myAPI") $ "arg" ~> True
```

---

## Encyclopedia

Refer to this section when in doubt.

### Verification Matchers (`shouldBeCalled`)

| Matcher | Description | Example |
| :--- | :--- | :--- |
| `x` (Value itself) | Was called with that value | ``f `shouldBeCalled` (10 :: Int)`` |
| `times n` | Exact count | ``f `shouldBeCalled` (times 3 `with` "arg")`` |
| `once` | Exactly once | ``f `shouldBeCalled` (once `with` "arg")`` |
| `never` | Never called | ``f `shouldBeCalled` never`` |
| `atLeast n` | n or more times | ``f `shouldBeCalled` atLeast 2`` |
| `atMost n` | n or fewer times | ``f `shouldBeCalled` atMost 5`` |
| `anything` | Any argument (count ignored) | ``f `shouldBeCalled` anything`` |
| `inOrderWith [...]` | Strict order | ``f `shouldBeCalled` inOrderWith ["a", "b"]`` |
| `inPartialOrderWith [...]` | Partial order (skips allowed) | ``f `shouldBeCalled` inPartialOrderWith ["a", "c"]`` |

### Parameter Matchers (Definition)

| Matcher | Description | Example |
| :--- | :--- | :--- |
| `any` | Any value | `any ~> True` |
| `expect pred label` | Condition | `expect (>0) "positive" ~> True` |
| `expect_ pred` | No label | `expect_ (>0) ~> True` |

### FAQ

<details>
<summary><strong>Q. How are unevaluated lazy values handled?</strong></summary>
A. They are not counted. Mockcat records calls only "when the result is evaluated" (Honest Laziness). This prevents false positives from unneeded calculations.
</details>

<details>
<summary><strong>Q. Can I use it in parallel tests?</strong></summary>
A. Yes. Internally uses `TVar` to count atomically, so it records accurately even when called in parallel via `mapConcurrently`, etc.
</details>

<details>
<summary><strong>Q. What code does `makeMock` generate?</strong></summary>
A. It generates a `MockT m` instance for the specified typeclass, and stub generation function definitions named `_methodName` corresponding to each method.
</details>

<details>
<summary><strong>Q. Isn't this strictly a Spy?</strong></summary>
A. Yes, according to definitions like xUnit Patterns, Mockcat's mocks which verify after execution are classified as **Test Spies**.<br>
However, since many modern libraries (Jest, Mockito, etc.) group these under "Mock", and to avoid confusion from terminology proliferation, this library unifies them under the term **"Mock"**.
</details>

## Tips and Troubleshooting

### Name collision with `Prelude.any`
The `any` parameter matcher from `Test.MockCat` may conflict with `Prelude.any`.
To resolve this, hide `any` from Prelude or use a qualified name.

```haskell
import Prelude hiding (any)
-- or
import qualified Test.MockCat as MC
```

### Ambiguous types with `OverloadedStrings`
If you have `OverloadedStrings` enabled, string literals may cause ambiguity errors.
Add explicit type annotations to resolve this.

```haskell
mock $ ("value" :: String) ~> True
```

---

## Tested Versions
mockcat is continuously tested in CI across these configurations:

| GHC | Cabal | OS |
|-----|-------|----|
| 9.2.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.4.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.6.3 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.8.2 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.10.1 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |

_Happy Mocking!_ 🐱