#!/bin/bash
set -e

DEFAULT_VERSIONS=("9.2.8" "9.4.8" "9.6.7" "9.8.4" "9.10.3" "9.12.2")
VERSIONS=("${1:-${DEFAULT_VERSIONS[@]}}")

for v in "${VERSIONS[@]}"; do
  echo "Testing with GHC $v..."
  cabal v2-test --disable-optimization -w ~/.ghcup/bin/ghc-$v
done

echo "All tests passed!"