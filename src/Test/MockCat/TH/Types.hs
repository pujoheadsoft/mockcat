module Test.MockCat.TH.Types
  ( MockOptions(..)
  , options
  )
where

-- | Options for generating mocks.
--
--  - prefix: Stub function prefix
--  - suffix: stub function suffix
--  - implicitMonadicReturn: If True, the return value of the stub function is wrapped in a monad automatically.
data MockOptions = MockOptions {prefix :: String, suffix :: String, implicitMonadicReturn :: Bool}

-- | Default Options.
--
--  Stub function names are prefixed with "_".
options :: MockOptions
options = MockOptions {prefix = "_", suffix = "", implicitMonadicReturn = True}


