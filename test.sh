#!/bin/bash
set -o pipefail

DEFAULT_VERSIONS=("9.2.8" "9.4.8" "9.6.7" "9.8.4" "9.10.3" "9.12.2")

if [ "$#" -gt 0 ]; then
  VERSIONS=("$@")
else
  VERSIONS=("${DEFAULT_VERSIONS[@]}")
fi

echo "Starting tests for versions: ${VERSIONS[*]}"

FAILURE=0
declare -A RESULTS

# Helper function to run tests
run_test_phase() {
    local phase_name=$1
    local enable_coverage=$2
    local extra_args=$3
    local log_prefix=$4
    
    echo "=== Starting $phase_name Phase ==="
    
    PIDS=()
    declare -A PID_TO_VERSION
    
    for v in "${VERSIONS[@]}"; do
        echo "  [GHC $v] Launching $phase_name..."
        (
            cabal v2-test --enable-tests $enable_coverage --disable-optimization --ghc-options="-Werror" $extra_args -w ~/.ghcup/bin/ghc-$v 2>&1 | tee "build_log_${v}_${log_prefix}.txt" | sed -u "s/^/[$v][$log_prefix] /"
        ) &
        PID=$!
        PIDS+=("$PID")
        PID_TO_VERSION["$PID"]="$v"
    done
    
    # Wait and collect results
    for pid in "${PIDS[@]}"; do
        wait "$pid"
        STATUS=$?
        v="${PID_TO_VERSION[$pid]}"
        
        if [ $STATUS -ne 0 ]; then
            FAILURE=1
            RESULTS["$v"]=1
            echo ""
            echo "--- [GHC $v] $phase_name FAILED ---"
            if [ -f "build_log_${v}_${log_prefix}.txt" ]; then
                tail -n 20 "build_log_${v}_${log_prefix}.txt"
            fi
        fi
    done
    
    # Cleanup logs
    rm -f build_log_*_${log_prefix}.txt
}

# --- Phase 1: Standard (No Coverage) ---
# Validates that all tests pass normally when HPC is disabled.
# (Automatic skipping mechanism in Spec.hs ensures this runs full suite)
run_test_phase "Standard" "--disable-coverage" "" "std"

if [ "$FAILURE" -eq 1 ]; then
    echo "Standard Phase failed. Aborting HPC Phase."
else
    # --- Phase 2: HPC (With Coverage) ---
    # Validates that Strict Verification detects HPC and gracefully skips standard tests,
    # while STILL running the critical HpcSpec.
    run_test_phase "HPC" "--enable-coverage" "" "hpc"
fi

echo ""
echo "========================================"
echo "TEST RESULTS Summary"
echo "========================================"

FAILED_VERSIONS=()
ANY_FAILURE=0

for v in "${VERSIONS[@]}"; do
  if [ "${RESULTS[$v]}" ]; then
    echo "[$v] ❌ FAILED"
    FAILED_VERSIONS+=("$v")
    ANY_FAILURE=1
  else
    echo "[$v] ✅ PASSED"
  fi
done

if [ "$ANY_FAILURE" -eq 1 ]; then
  echo ""
  echo "BUILD FAILURES DETECTED"
  echo "Failed versions: ${FAILED_VERSIONS[*]}"
  exit 1
else
  echo ""
  echo "running TH error verification..."
  if ./verify_th_errors.sh; then
      echo "TH verification passed."
  else
      echo "TH verification FAILED."
      exit 1
  fi

  echo ""
  echo "🎉 All tests passed across all versions (Standard & HPC)!"
  exit 0
fi
