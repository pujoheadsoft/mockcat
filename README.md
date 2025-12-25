<div align="center">
    <img src="https://raw.githubusercontent.com/pujoheadsoft/mockcat/main/logo.png" width="600px" alt="Mockcat Logo">
    <h1>Declarative mocking with a single arrow <code>~&gt;</code></h1>
</div>

<div align="center">

[![Hackage](https://img.shields.io/hackage/v/mockcat.svg)](https://hackage.haskell.org/package/mockcat)
[![Stackage LTS](http://stackage.org/package/mockcat/badge/lts)](http://stackage.org/lts/package/mockcat)
[![Build Status](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions)

[🇯🇵 Japanese (日本語)](README-ja.md)

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

Mockcat adopts a design where "verification happens at runtime, but 'conditions to be met' can be declared at definition time."

*   **Stub**:
    Exists solely to keep the test moving by returning values. It does not care "how it was called".
    The `stub` function returns a completely pure function.

*   **Mock**:
    In addition to stubbing, it records and verifies "was it called as expected?".
    The `mock` function returns a value while recording calls. Verification can be done at the end of the test, or declared as "it must be called this way" at definition time.

---

## Why Mockcat?

There's no need to brace yourself when writing mocks in Haskell.

Mockcat is a mocking library that **"allows you to declaratively describe function behavior and intent without depending on specific architectures."**

"I can't test unless I introduce Typeclasses (MTL)."
"I have to define dedicated data types just for mocking."
(e.g., adding extra Typeclasses or Service Handle records just for testing)

You are freed from such constraints. You can mock existing functions as they are, and start writing tests even when the design isn't fully solidified.

**Mockcat aims to let you write tests to explore design, rather than forcing you to fixate the design just for testing.**

### Before / After

See how simple writing tests in Haskell can be.

| | **Before: Handwritten...** 😫 | **After: Mockcat** 🐱✨ |
| :--- | :--- | :--- |
| **Definition (Stub)**<br>"I want to return<br>this value for this arg" | <pre lang="haskell">f :: String -> IO String<br>f arg = case arg of<br>  "a" -> pure "b"<br>  _   -> error "unexpected"</pre><br>_Even simple branching consumes many lines._ | <pre lang="haskell">-- Use stub if verification is unneeded (Pure)<br>let f = stub $<br>  "a" ~> "b"<br><br><br></pre><br>_Behaves as a completely pure function._ |
| **Verification (Verify)**<br>"I want to test<br>if it was called correctly" | <pre lang="haskell">-- Need to implement recording logic<br>ref <- newIORef []<br>let f arg = do<br>      modifyIORef ref (arg:)<br>      ...<br><br>-- Verification logic<br>calls <- readIORef ref<br>calls \`shouldBe\` ["a"]</pre><br>_※ This is just one example. Real-world setups often require even more boilerplate._ | <pre lang="haskell">-- Use mock if verification is needed (Recorded internally)<br>f <- mock $ "a" ~> "b"<br><br>-- Just state what you want to verify<br>f \`shouldBeCalled\` "a"</pre><br>_Recording is automatic.<br>Focus on the "Why" and "What", not the "How"._ |

### Key Features

*   **Haskell Native DSL**: No need to memorize redundant data constructors or specialized notation. Write mocks naturally, just like function definitions (`arg ~> return`).
*   **Architecture Agnostic**: Whether using MTL (Typeclasses), Service Handle (Records), or pure functions—Mockcat adapts to your design choice with minimal friction.
*   **Verify by "Condition", not just Value**: Works even if arguments lack `Eq` instances. You can verify based on "what properties it should satisfy" (Predicates) rather than just strict equality.
*   **Helpful Error Messages**: Shows "structural diffs" on failure, highlighting exactly what didn't match.
    ```text
    Expected arguments were not called.
      expected: [Record { name = "Alice", age = 20 }]
       but got: [Record { name = "Alice", age = 21 }]
                                                ^^
    ```
*   **Intent-Driven Types**: Types exist not to restrict you, but to naturally guide you in expressing your testing intent.


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

### At a Glance: Matchers
| Matcher | Description | Example |
| :--- | :--- | :--- |
| **`any`** | Matches any value | `f <- mock $ any ~> True` |
| **`expect`** | Matches condition | `f <- mock $ expect (> 5) "gt 5" ~> True` |
| **`"val"`** | Matches value (Eq) | `f <- mock $ "val" ~> True` |
| **`inOrder`** | Order verification | ``f `shouldBeCalled` inOrderWith ["a", "b"]`` |
| **`inPartial`**| Partial order | ``f `shouldBeCalled` inPartialOrderWith ["a", "c"]`` |

---

## User Guide

Mockcat supports two verification styles depending on your testing needs and preferences.

1.  **Post-Verification Style (Spy)**:
    Define mock behavior, run the code, and verify afterwards using `shouldBeCalled`.  
    Ideal for exploratory testing or simple setups. (Used mainly in Sections 1 & 2 below)
2.  **Pre-Expectation Style (Declarative/Expectation)**:
    Describe "how it should be called" at the definition time.  
    Ideal for strict interaction testing. (Explained in Section 3)

### 1. Function Mocking (`mock`) - [Basic]

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

Useful when you want to bring existing typeclasses directly into your tests. Generates mocks from existing typeclasses using Template Haskell.

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

### 4. Flexible Verification (Matchers)

Even if arguments don't have `Eq` instances, or you don't want to depend on specific values, you can verify based on **intent**—"what condition should be met".
Mockcat provides **matchers** to verify properties of functions, not just value equality.

#### Allow Any Value (`any`)

```haskell
-- Return True regardless of the argument
f <- mock $ any ~> True

-- Verify that it was called (arguments don't matter)
f `shouldBeCalled` any
```

#### Verify with Conditions (`expect`)

You can verify using "conditions (predicates)" instead of arbitrary values.
Powerfully useful for types without `Eq` (like functions) or when checking partial matches.

```haskell
-- Return False only if the argument starts with "error"
f <- mock $ do
  onCase $ expect (\s -> "error" `isPrefixOf` s) "start with error" ~> False
  onCase $ any ~> True
```

### 5. Advanced Features - [Advanced]

#### mock vs stub vs mockM

In most cases, **`mock`** is all you need.
Consider other functions only when you need finer control.

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

## Encyclopedia (Feature Reference)

※ Use this section as a dictionary when you get stuck.

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

### Declarative Verification DSL (`expects`)

In `expects` blocks, you can describe expectations declaratively.
The syntax used in `expects` shares the same vocabulary as `shouldBeCalled`.

| Syntax | Description |
| :--- | :--- |
| `called` | Start expectation |
| `once` | Called exactly once |
| `times n` | Called n times |
| `never` | Never called |
| `with arg` | Expected argument |
| `with matcher` | Argument verification with matcher |

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