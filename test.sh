#!/bin/bash
set -o pipefail

DEFAULT_VERSIONS=("9.2.8" "9.4.8" "9.6.7" "9.8.4" "9.10.3" "9.12.2")

if [ "$#" -gt 0 ]; then
  VERSIONS=("$@")
else
  VERSIONS=("${DEFAULT_VERSIONS[@]}")
fi

echo "Starting parallel tests for versions: ${VERSIONS[*]}"

PIDS=()
declare -A PID_TO_VERSION

for v in "${VERSIONS[@]}"; do
  echo "  [GHC $v] Launching..."
  # Run in background, piping output to sed to add a prefix
  # set -o pipefail ensures that if cabal fails, the pipeline fails
  (
    cabal v2-test --enable-tests --enable-coverage --disable-optimization --ghc-options="-Werror" -w ~/.ghcup/bin/ghc-$v 2>&1 | tee "build_log_$v.txt" | sed -u "s/^/[$v] /"
  ) &
  pid=$!
  PIDS+=("$pid")
  PID_TO_VERSION["$pid"]=$v
done

echo "Waiting for ${#PIDS[@]} jobs to complete..."

FAILURE=0
declare -A PID_RESULTS

# First, wait for all processes to finish and collect exit codes
for pid in "${PIDS[@]}"; do
  if wait "$pid"; then
    PID_RESULTS["$pid"]=0
  else
    PID_RESULTS["$pid"]=1
    FAILURE=1
  fi
done

# If there were failures, print error details before the summary
if [ "$FAILURE" -eq 1 ]; then
  echo ""
  echo "========================================"
  echo "FAILURE DETAILS (Last 20 lines)"
  echo "========================================"
  for pid in "${PIDS[@]}"; do
    if [ "${PID_RESULTS[$pid]}" -ne 0 ]; then
      v="${PID_TO_VERSION[$pid]}"
      echo ""
      echo "--- [GHC $v] Error Log ---"
      if [ -f "build_log_$v.txt" ]; then
        # improved error extraction: look for "error:" or "FAIL" but fallback to tail
        # actually tail is safest
        tail -n 20 "build_log_$v.txt"
      else
        echo "Log file not found."
      fi
    fi
  done
fi

# Clean up logs
rm -f build_log_*.txt

# Now output the summary
echo ""
echo "========================================"
echo "TEST RESULTS Summary"
echo "========================================"

FAILED_VERSIONS=()

for pid in "${PIDS[@]}"; do
  v="${PID_TO_VERSION[$pid]}"
  if [ "${PID_RESULTS[$pid]}" -eq 0 ]; then
    echo "[$v] ✅ PASSED"
  else
    echo "[$v] ❌ FAILED"
    FAILED_VERSIONS+=("$v")
  fi
done

if [ "$FAILURE" -eq 1 ]; then
  echo ""
  echo "========================================"
  echo "BUILD FAILURES DETECTED"
  echo "========================================"
  echo "Failed versions: ${FAILED_VERSIONS[*]}"
  exit 1
else
  echo ""
  echo "🎉 All tests passed across all versions!"
  exit 0
fi