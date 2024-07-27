# 🐈Mocking library for Haskell🐈‍

[![Test](https://github.com/pujoheadsoft/mockcat/workflows/Test/badge.svg)](https://github.com/pujoheadsoft/mockcat/actions?query=workflow%3ATest+branch%3Amain)

[日本語版 README はこちら](https://github.com/pujoheadsoft/mockcat/blob/master/README-ja.md)

mockcat is a simple mocking library that supports testing in Haskell.

It mainly provides two features:
- Creating stub functions
- Verifying if the expected arguments were applied

Stub functions can return not only monadic values but also pure values.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Example of usage" do
    -- Create a Mock (applying “value” returns the pure value True)
    mock <- createMock $ "value" |> True

    -- Extract the stub function from the mock
    let stubFunction = stubFn mock

    -- Verify the results of applying an argument
    stubFunction "value" `shouldBe` True

    -- Verify if the expected value ("value") was applied
    mock `shouldApplyTo` "value"
```

# Stub Functions
## Simple Stub Functions
To create stub functions, use the createStubFn function.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "can create a stub function" do
    -- Create
    f <- createStubFn $ "param1" |> "param2" |> pure @IO ()

    -- Apply
    actual <- f "param1" "param2"

    -- Verify
    actual `shouldBe` ()
```
To createStubFn, you pass the expected arguments concatenated with |>.
The final value after |> is the return value of the function.

If unexpected arguments are applied to the stub function, an error occurs.
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "value"
  but got: "valuo"
```
## Named Stub Functions
You can name stub functions.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "named stub" do
    f <- createNamedStubFun "named stub" $ "x" |> "y" |> True
    f "x" "z" `shouldBe` True
```
If the expected arguments are not applied, the error message will include this name.
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function `named stub`.
  expected: "x","y"
  but got: "x","z"
```
## Flexible Stub Functions
You can create a flexible stub function by giving the `createStubFn` function a conditional expression instead of a specific value.  
This allows you to return expected values for arbitrary values, strings matching specific patterns, etc.

### any
any matches any value.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (any)

spec :: Spec
spec = do
  it "any" do
    f <- createStubFn $ any |> "return value"
    f "something" `shouldBe` "return value"
```
Since a function with the same name is defined in Prelude, we use import Prelude hiding (any).

### Condition Expressions
Using the expect function, you can handle arbitrary condition expressions.  
The expect function takes a condition expression and a label.  
The label is used in the error message if the condition is not met.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect" do
    f <- createStubFn $ expect (> 5) "> 5" |> "return value"
    f 6 `shouldBe` "return value"
```

### Condition Expressions without Labels
`expect_` is a label-free version of expect.  
The error message will show [some condition].
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expect_" do
    f <- createStubFn $ expect_ (> 5) |> "return value"
    f 6 `shouldBe` "return value"
```

### Condition Expressions using Template Haskell
Using expectByExp, you can handle condition expressions as values of type Q Exp.  
The error message will include the string representation of the condition expression.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "expectByExpr" do
    f <- createStubFn $ $(expectByExpr [|(> 5)|]) |> "return value"
    f 6 `shouldBe` "return value"
```

## Stub Functions that Return Different Values for Each Applied Argument
By applying a list in the form of x |> y to the `createStubFn` function,   
you can create stub functions that return different values for each applied argument.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat
import Prelude hiding (and)

spec :: Spec
spec = do
  it "multi" do
    f <-
      createStubFn
        [ "a" |> "return x",
          "b" |> "return y"
        ]
    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
```

# Verification
## Verify if the Expected Arguments were Applied
You can verify if the expected arguments were applied using the `shouldApplyTo` function.
To perform the verification, create a mock using the `createMock` function instead of the `createStubFn` function.
In this case, use the `stubFn` function to extract the stub function from the mock.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "stub & verify" do
    -- create a mock
    mock <- createMock $ "value" |> True
    -- stub function
    let stubFunction = stubFn mock
    -- assert
    stubFunction "value" `shouldBe` True
    -- verify
    mock `shouldApplyTo` "value"
```
### Note
The recording of the application of arguments is done at the time the return value of the stub function is evaluated.  
Therefore, verification must be done after evaluation.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "Verification does not work" do
    mock <- createMock $ "expect arg" |> "return value"
    -- Apply arguments to stub functions but do not evaluate values
    let _ = stubFn mock "expect arg"
    mock `shouldApplyTo` "expect arg"
```
```console
uncaught exception: ErrorCall
Expected arguments were not applied to the function.
  expected: "expect arg"
  but got: Never been called.
```

## Verify the Number of Times the Expected Arguments were Applied
You can verify the number of times the expected arguments were applied using the `shouldApplyTimes` function.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyTimes" do
    m <- createMock $ "value" |> True
    print $ stubFn m "value"
    print $ stubFn m "value"
    m `shouldApplyTimes` (2 :: Int) `to` "value"
```
## Verify if the Arguments were Applied in the Expected Order
You can verify if the arguments were applied in the expected order using the `shouldApplyInOrder` function.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    m
      `shouldApplyInOrder` [ "a" |> True,
                             "b" |> True
                           ]
```

## Verify if the Arguments were Applied in the Expected Partial Order
The `shouldApplyInOrder` function strictly verifies the order of application,  
but the `shouldApplyInPartialOrder` function can verify if the order of application matches partially.
```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Test.Hspec
import Test.MockCat

spec :: Spec
spec = do
  it "shouldApplyInPartialOrder" do
    m <- createMock $ any |> True |> ()
    print $ stubFn m "a" True
    print $ stubFn m "b" True
    print $ stubFn m "c" True
    m
      `shouldApplyInPartialOrder` [ "a" |> True,
                                    "c" |> True
                                  ]
```