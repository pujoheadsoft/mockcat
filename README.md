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

```haskell
-- Stub: Returns True when given "a"
let f :: String -> Bool
    f = stub ("a" ~> True)

f "a"  -- => True
```

That's it. Write the argument on the left of `~>`, the return value on the right.
No verification—just a pure function.

**To verify calls**, use `mock` with `expects`:

```haskell
withMockIO $ do
  f <- mock ("a" ~> True)
    `expects` called once   -- Declare "should be called exactly once"

  f "a" `shouldBe` True
  -- Verification runs when exiting the withMockIO scope
```

> **When to use which:**
> - Just need a return value? Use `stub` (pure, no verification)
> - Need to verify calls? Use `mock` (always with `expects`)

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
| **Definition (Stub)**<br />"I want to return<br />this value for this arg" | <pre>f :: String -> IO String<br />f arg = case arg of<br />  "a" -> pure "b"<br />  _   -> error "unexpected"</pre><br />_Even simple branching consumes many lines._ | <pre>-- Use stub if verification is unneeded (Pure)<br />let f = stub ("a" ~> "b")</pre><br />_Behaves as a completely pure function._ |
| **Verify**<br />(Did it get called<br />correctly?) | <pre>-- Need manual recording mechanism<br />ref <- newIORef []<br />let f arg = do<br />      modifyIORef ref (arg:)<br />      ...<br /><br />-- Verification Logic<br />calls <- readIORef ref<br />calls `shouldBe` ["a"]</pre><br />_ This is just one example. Boilerplate often grows._ | <pre>withMock $ do<br />  -- Declare expected values at definition<br />  f <- mock ("a" ~> "b")<br />    &#96;expects&#96; called once<br /><br />  -- Just execute (Automatic verification)</pre><br />_Recording is automatic.<br />Focus on the "Why" and "What", not the "How"._ |

### Key Features

*   **Haskell Native DSL**: No need to memorize redundant data constructors or specialized notation. Write mocks naturally, just like function definitions (`arg ~> return`).
*   **Architecture Agnostic**: Whether using MTL (Typeclasses), Service Handle (Records), or pure functions—Mockcat adapts to your design choice with minimal friction.
*   **Verify by "Condition", not just Value**: Works even if arguments lack `Eq` instances. You can verify based on "what properties it should satisfy" (Predicates) rather than just strict equality.
*   **Helpful Error Messages**: Shows "structural diffs" on failure, highlighting exactly what didn't match.
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

### First Test (No Verification)

```haskell
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "stub demo" $ do
    let f :: String -> Int
        f = stub ("Hello" ~> 42)

    f "Hello" `shouldBe` 42
```

### First Test (With Verification)

```haskell
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "mock demo" $ do
    withMockIO $ do
      -- Declare "should be called exactly once"
      f <- mock ("Hello" ~> (42 :: Int))
        `expects` called once

      f "Hello" `shouldBe` 42
      -- Verification runs when exiting the withMockIO scope
```



## User Guide

Mockcat supports two verification styles depending on your testing needs and preferences.

### 1. Declarative Verification (`withMock` / `expects`) - [Recommended]

A style where you describe expectations at definition time. Verification runs automatically when exiting the scope.
Useful when you want "Definition" and "Verification" to be written close together.

```haskell
import Test.Hspec
import Test.MockCat
import Control.Monad.IO.Class (MonadIO(liftIO))

spec :: Spec
spec = do
  it "User Guide (withMock)" $ do
    withMock $ do
      -- Define a mock that returns True for "Hello"
      f <- mock ("Hello" ~> True)
        `expects` called once

      -- Execution
      let result = f "Hello"

      liftIO $ result `shouldBe` True
```

#### `withMockIO`: Simplified IO Testing
`withMockIO` is an IO-specialized version of `withMock`. It allows you to run IO actions directly within the mock context without needing `liftIO`.

```haskell
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "User Guide (withMockIO)" $ do
    withMockIO $ do
      f <- mock ("Hello" ~> True)
        `expects` called once

      let result = f "Hello"

      result `shouldBe` True
```

> [!IMPORTANT]
> When using `expects` (declarative verification), you MUST wrap the mock definition in **parentheses `(...)`**.
> The `$` operator pattern used in previous versions (`mock $ ... expected ...`) will cause compilation errors due to precedence changes.
>
> ❌ `mock $ any ~> True expects ...`
> ✅ `mock (any ~> True) expects ...`

> [!NOTE]
> You can also use `expects` for declarative verification inside `runMockT` blocks.
> This works seamlessly with generated typeclass mocks as well.
>
> ```haskell
> runMockT do
>   _readFile ("config.txt" ~> pure "value")
>     `expects` called once
> ```

### 2. Mocking with Typeclass-Based Designs (`makeMock`)

For designs that express dependencies via typeclasses (MTL style or Capability pattern),
you can bring them directly into tests. Generates mocks from typeclasses using Template Haskell.

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

-- [Strict Mode] Default behavior. Consistent with 'mock'.
-- If the return type is `m a`, the stub definition must return a value of type `m a` (e.g., `pure @IO "value"`, `throwIO Error`).
-- Recommended when you prefer explicit descriptions faithful to Haskell's type system.
makeMock [t|FileSystem|]

-- [Auto-Lift Mode] Convenience-focused mode.
-- Automatically wraps pure values into the monad (m String).
makeAutoLiftMock [t|FileSystem|]
```

> [!NOTE]
> If the class definition requires additional extensions (e.g., `MultiParamTypeClasses`, `UndecidableInstances`), Mockcat will display a detailed error message during compilation to guide you.

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

### 3. Function Mocking and Post-Verification (`mock` / `shouldBeCalled`)

The most basic usage. Creates a function that returns values for specific arguments.
Combining it with Post-Verification (`shouldBeCalled`) makes it suitable for exploratory testing or prototyping.

```haskell
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Function Mocking" $ do
    -- Define a mock that returns True for "Hello" (No 'expects' here)
    f <- mock ("Hello" ~> True)
    
    -- Execution
    f "Hello" `shouldBe` True

    -- Post-Verification (shouldBeCalled)
    f `shouldBeCalled` "Hello"
```

> [!WARNING]
> **Limitation in HPC (Code Coverage) Environments**
> Do not use `shouldBeCalled` when running tests with `stack test --coverage` or similar.
> The code coverage instrumentation by GHC wraps functions, which changes their identity and causes verification to fail.
> If you need code coverage, please use the **`expects`** style (Section 1).

**Flexible Matching**:
You can specify conditions (predicates) instead of concrete values.

```haskell
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "Matcher Examples" $ do
    -- Arbitrary string (param any)
    f <- mock (any @String ~> True)
    f "foo" `shouldBe` True

    -- Condition (when)
    g <- mock (when (> (5 :: Int)) "> 5" ~> True)
    g 6 `shouldBe` True
```

### 4. Flexible Verification (Matchers)

Even if arguments don't have `Eq` instances, or you don't want to depend on specific values, you can verify based on **intent**—"what condition should be met".
Mockcat provides **matchers** to verify properties of functions, not just value equality.

#### Allow Any Value (`any`)

```haskell
-- Return True regardless of the argument
f <- mock (any ~> True)

-- Verify that it was called (arguments don't matter)
f `shouldBeCalled` any
```

#### Verify with Conditions (`when`)

You can verify using "conditions (predicates)" instead of arbitrary values.
Powerfully useful for types without `Eq` (like functions) or when checking partial matches.

```haskell
-- Return False only if the argument starts with "error"
f <- mock do
  onCase $ when (\s -> "error" `isPrefixOf` s) "start with error" ~> False
  onCase $ any ~> True
```

If you don't need a label (description shown on error), you can use `when_`.

```haskell
f <- mock (when_ (> 5) ~> True)
```

### 5. Advanced Features - [Advanced]

#### mock vs stub vs mockM

Choose the function based on the nature of your test target.
See the table below for details.

| Function | Verification (`shouldBeCalled`) | IO Dependency | Characteristics |
| :--- | :---: | :---: | :--- |
| **`stub`** | ❌ | None | **Pure Stub**. No IO dependency. Sufficient if verification isn't needed. |
| **`mock`** | ✅ | None (External) | **Mock**. Behaves as a pure function. Automatically records history. |
| **`mockM`** | ✅ | Yes (Explicit) | **Monadic Mock**. Used within `MockT` or `IO`, allowing explicit handling of side effects (e.g., logging). |

#### Choosing between `mock` and `mockM`

Choose the function according to the **return type** of the target function.

*   **`mock` (For Pure Functions)**:
    *   Use this when mocking **pure functions** like `String -> Int`.
    *   It respects Haskell's lazy evaluation and records the call only when the result is actually evaluated. This prevents counting unnecessary calls that were never executed.

> [!IMPORTANT]
> Since `mock` behaves as a pure function (`a -> b`),
> **it is subject to GHC's optimizations (CSE / CAF / full laziness)**.
>
> As a result, even if an expression appears multiple times in your source code,
> **it may be evaluated only once** after compilation.
>
> Mockcat records and verifies **the actual number of evaluations**,
> not the number of times the expression appears in the source code.


*   **`mockM` (For IO/Monadic Functions)**:
    *   Use this when mocking functions that return **`IO` or other `MonadIO` instances** (such as `ReaderT IO`), like `String -> IO Int`.
    *   Since the recording logic is built directly into the returned action (`IO`), it provides extremely high predictability even in highly concurrent tests or environments with heavy GHC optimizations.

> [!TIP]
> When in doubt, remember: **"If the function returns IO, use `mockM`. Otherwise, use `mock`."**

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

#### Derivation and Custom Instances

When using `MockT`, you might need to handle type classes that are not directly related to the side effects you are mocking. Mockcat provides macros to help with these cases.

##### MTL Instances (`MonadReader`, `MonadError`, etc.)
`MockT` provides standard `mtl` instances (`MonadReader`, `MonadError`, `MonadState`, `MonadWriter`) out of the box. These instances automatically lift operations to the base monad.

##### Custom Type Class Derivation (`deriveMockInstances`)
For custom "Capability" type classes (like `MonadLogger`, `MonadConfig`) that should just be lifted to the base monad, use `deriveMockInstances`.

```haskell
class Monad m => MonadLogger m where
  logInfo :: String -> m ()

deriveMockInstances [t|MonadLogger|]
```
This generates an instance for `MockT m` that calls `lift . logInfo`.

##### Explicit No-op Instances (`deriveNoopInstance`)
Sometimes you want a mock to do nothing for certain methods (especially those returning `m ()`) without having to define explicit stubs or provide a base implementation.

```haskell
class Monad m => MonadAuditor m where
  audit :: String -> m ()

deriveNoopInstance [t|MonadAuditor|]
```
This generates an instance for `MockT m` where `audit` simply returns `pure ()`.


---

#### Design Philosophy: Capability vs. Control

Mockcat makes a distinction between **Capability** and **Control** when it comes to type class derivation.

*   **Capability (Inject/Lift)**: Type classes that provide context or tools (e.g., `MonadReader`, `MonadLogger`).
    *   **Approach**: Use `deriveMockInstances` or standard `mtl` instances. These should be lifted to the base monad to keep the environment consistent.
*   **Control (Mock)**: Type classes that represent external side effects or business logic boundaries (e.g., `UserRepository`, `PaymentGateway`).
    *   **Approach**: Use `makeMock`. These must be explicitly stubbed or verified to ensure the test isolates the logic under test.

---


#### Monadic Return (`IO a`)

Used when you want a function returning `IO` to have different side effects (results) for each call.

```haskell
f <- mock do
  onCase $ "get" ~> pure @IO 1 -- 1st call
  onCase $ "get" ~> pure @IO 2 -- 2nd call
```

#### Named Mocks

You can attach labels to display function names in error messages.

```haskell
f <- mock (label "myAPI") ("arg" ~> True)
```

---

## Encyclopedia (Feature Reference)

※ Use this section as a dictionary when you get stuck.

### Declarative Verification DSL (`expects`)

In `expects` blocks, you can describe expectations declaratively using a builder-style syntax.
It shares the same vocabulary as `shouldBeCalled`.

#### Basic Usage

Start with `called` and chain conditions.

```haskell
-- Call count only
mock (any ~> True) `expects` called once

-- With arguments
mock (any ~> True) `expects` (called once `with` "arg")

-- Multiple expectations (in do block)
mock (any ~> True) `expects` do
  called once `with` "A"
  called once `with` "B"
```

#### Syntax Reference

| Builder | Description | Example |
| :--- | :--- | :--- |
| **`called`** | **[Required]** Starts the expectation builder. | `called ...` |
| **`times n`** | Expects exact call count. | `called . times 2` |
| **`once`** | Alias for `times 1`. | `called . once` |
| **`never`** | Expects 0 calls. | `called . never` |
| **`with arg`** | Expects specific argument(s). | `called `with` "value"` |
| **`with matcher`** | Uses a matcher for argument verification. | `called `with` when (>5) "gt 5"` |
| **`inOrder`** | Verify usage order (when used in a list) | (See "Order Verification" section) |

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
| `when pred label` | Condition | `when (>0) "positive" ~> True` |
| `when_ pred` | No label | `when_ (>0) ~> True` |

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
<summary><strong>Q. Can I run tests with code coverage (HPC)?</strong></summary>
A. Yes (since v1.1.0.0). Mockcat's `expects` style is designed to be unaffected by the function wrapping performed by HPC, so it operates safely even under HPC.
However, for the reasons mentioned above, we strongly recommend using the **`expects`** style (or `withMock`).
The `shouldBeCalled` style cannot be used because HPC's mechanism makes it impossible to identify mock identity.
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

<details>
<summary><strong>Q. Call counts are lower than expected in tests</strong></summary>

A. Since `mock` is treated as a pure function, GHC's optimizations may cause evaluations to be shared.
This is Mockcat's intended behavior—it accurately reflects the actual execution result after compilation.

Mockcat records **"the actual number of evaluations"** at runtime.
Therefore, if the same expression is shared due to optimization, it is correctly counted as "1 call".

If you want to suppress evaluation sharing for testing purposes, you can add the following
**GHC pragmas** to your test file.

```haskell
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
```

This is a **test-only setting**, useful when you want to verify behavior closer to the source-level call count.
</details>

## Real-World Examples

Mockcat does not assume any specific architecture.
It can be used as the core of your testing foundation, or as a lightweight helper within an existing framework.

Here are real-world test suites using mockcat:

- **MTL + Capability pattern**: Port-based application tests (using `MockT` / `ExceptT`)
  👉 [UsecaseSpec.hs (cli-mtl)](https://github.com/pujoheadsoft/haskell-layered-examples/blob/main/cli-mtl/test/Application/UsecaseSpec.hs)

- **Polysemy effects**: Use stub for data flow, mock only where verification is needed
  👉 [UsecaseSpec.hs (cli-effect-polysemy)](https://github.com/pujoheadsoft/haskell-layered-examples/blob/main/cli-effect-polysemy/test/Application/UsecaseSpec.hs)

## Tips and Troubleshooting

### Name collision with `Prelude.any`
The `any` parameter matcher from `Test.MockCat` may conflict with `Prelude.any`.
To resolve this, hide `any` from Prelude or use a qualified name.

```haskell
import Prelude hiding (any)
-- or
import qualified Test.MockCat as MC
```

### Name collision with `Control.Monad.when`
`Test.MockCat` exports `when` (parameter matcher), which may conflict with `Control.Monad.when`.
To avoid this, hide `when` from `Test.MockCat` or use qualified import.

```haskell
import Test.MockCat hiding (when)
-- or
import Control.Monad hiding (when) -- if you want to use the matcher
```

### Ambiguous types with `OverloadedStrings`
If you have `OverloadedStrings` enabled, string literals may cause ambiguity errors.
Add explicit type annotations to resolve this.

```haskell
mock (("value" :: String) ~> True)
```

---

## Tested Versions
mockcat is continuously tested in CI across these configurations:

| GHC | Cabal | OS |
|-----|-------|----|
| 9.2.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.4.8 | 3.10.3.0 / 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.6.7 | 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.8.4 | 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.10.3 | 3.12.1.0 | Ubuntu, macOS, Windows |
| 9.12.2 | 3.12.1.0 | Ubuntu, macOS, Windows |

_Happy Mocking!_ 🐱