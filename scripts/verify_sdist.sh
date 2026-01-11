#!/bin/bash
set -e

# Cleanup previous run
rm -rf verification
mkdir -p verification

# Build sdist
echo "Building sdist..."
stack sdist .

# Find the sdist tarball
SDIST_PATH=$(find .stack-work/dist -name "mockcat-*.tar.gz" | head -n 1)
echo "Found sdist at: $SDIST_PATH"

# Unpack specific version for consumer
tar -xzf "$SDIST_PATH" -C verification
UNPACKED_DIR=$(find verification -maxdepth 1 -type d -name "mockcat-*" | head -n 1)

# Setup consumer project structure
CONSUMER_DIR="verification/consumer"
mkdir -p "$CONSUMER_DIR/app"

# Generate stack.yaml
cat <<EOF > "$CONSUMER_DIR/stack.yaml"
resolver: lts-22.44
packages:
- .
- ../$(basename "$UNPACKED_DIR")
extra-deps: []
EOF

# Generate package.yaml
cat <<EOF > "$CONSUMER_DIR/package.yaml"
name: consumer
version: 0.1.0.0
dependencies:
- base
- mockcat
executables:
  consumer-exe:
    main: Main.hs
    source-dirs: app
    ghc-options:
    - -threaded
EOF

# Generate Main.hs from ReadmeVerifySpec content (simplified for brevity/robustness)
cat <<EOF > "$CONSUMER_DIR/app/Main.hs"
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.MockCat
import Prelude hiding (any, readFile)

class Monad m => FileSystem m where
  readFile :: FilePath -> m String

makeMock [t|FileSystem|]

instance FileSystem IO where
  readFile path = pure \$ "Real: " ++ path

main :: IO ()
main = do
  putStrLn "Running Consumer Verification..."

  res <- runMockT \$ do
    _readFile (("config" :: String) ~> pure @IO ("mocked content" :: String))
    readFile "config"
  
  putStrLn \$ "Filesystem result: " ++ res
  if res == "mocked content" then putStrLn "Verification Passed!" else error "Verification failed"
EOF

# Run verification
echo "Running consumer verification..."
cd "$CONSUMER_DIR"
stack run

echo "SUCCESS: Package is consumable from sdist."
