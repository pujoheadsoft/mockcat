#!/bin/bash
set -e

# Case 1: THFailureNoop.hs
echo "Verifying THFailureNoop.hs (deriveNoopInstance)..."
OUTPUT1=$(echo ":load test-failure-cases/THFailureNoop.hs" | stack repl --no-load --work-dir .stack-work-verify 2>&1 || true)
EXPECTED_ERR_1='deriveNoopInstance: Function `bad` does not return `m ()`'

if echo "$OUTPUT1" | grep -qF "$EXPECTED_ERR_1"; then
  echo "✔ Case 1 PASSED: deriveNoopInstance failure detected."
else
  echo "✘ Case 1 FAILED: Expected error not found."
  echo "Output was:"
  echo "$OUTPUT1"
  exit 1
fi

echo ""
echo "All TH failure cases verified successfully."
exit 0
