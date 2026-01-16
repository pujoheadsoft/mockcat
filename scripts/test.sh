#!/bin/bash
set -o pipefail

DEFAULT_VERSIONS=("9.2.8" "9.4.8" "9.6.7" "9.8.4" "9.10.3" "9.12.2")

# Parse arguments
CLEAN=0
VERSIONS=()
for arg in "$@"; do
  if [ "$arg" == "--clean" ]; then
    CLEAN=1
  else
    VERSIONS+=("$arg")
  fi
done

# Default versions if none specified
if [ ${#VERSIONS[@]} -eq 0 ]; then
  VERSIONS=("${DEFAULT_VERSIONS[@]}")
fi

# Clean if requested
if [ "$CLEAN" -eq 1 ]; then
  echo "Cleaning build artifacts (dist-newstyle/)..."
  rm -rf dist-newstyle/
fi

echo "Starting tests for versions: ${VERSIONS[*]}"

FAILURE=0
declare -A RESULTS

# Helper function to run tests
run_test_phase() {
    local phase_name=$1
    local enable_coverage=$2
    local optimization=$3
    local extra_args=$4
    local log_prefix=$5
    
    echo "=== Starting $phase_name Phase ==="
    
    PIDS=()
    declare -A PID_TO_VERSION
    
    for v in "${VERSIONS[@]}"; do
        echo "  [GHC $v] Launching $phase_name..."
        (
            cabal v2-test test:mockcat-test --enable-tests $enable_coverage $optimization --ghc-options="-Werror" $extra_args -w ~/.ghcup/bin/ghc-$v 2>&1 | tee "build_log_${v}_${log_prefix}.txt" | sed -u "s/^/[$v][$log_prefix] /"
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

# --- Phase 1: Standard (No Coverage, No Optimization) ---
# Fast test run to validate basic correctness.
run_test_phase "Standard" "--disable-coverage" "--disable-optimization" "" "std"

if [ "$FAILURE" -eq 1 ]; then
    echo "Standard Phase failed. Aborting remaining phases."
else
    # --- Phase 2: Optimized (No Coverage, With Optimization) ---
    # Validates tests pass with optimization enabled (matches Stackage/Hackage conditions).
    # This catches GHC optimization-related issues like LICM affecting unsafePerformIO.
    run_test_phase "Optimized" "--disable-coverage" "--enable-optimization" "" "opt"
fi

if [ "$FAILURE" -eq 1 ]; then
    echo "Optimized Phase failed. Aborting HPC Phase."
else
    # --- Phase 3: HPC (With Coverage) ---
    # Validates that Strict Verification detects HPC and gracefully skips standard tests,
    # while STILL running the critical HpcSpec.
    run_test_phase "HPC" "--enable-coverage" "--disable-optimization" "" "hpc"
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
  if ./scripts/verify_th_errors.sh; then
      echo "TH verification passed."
  else
      echo "TH verification FAILED."
      exit 1
  fi

  echo ""
  echo "=== Starting Safety Analysis Phase ==="
  # Run safety analysis on GHC 9.6.7 (proven to work with Core analysis)
  TARGET_V="9.6.7"
  echo "  [GHC $TARGET_V] Generating Core dumps for safety analysis..."
  # We need optimization and specific GHC flags for Core dumping.
  # Use sed trick to swap NOINLINE -> INLINE temporarily to improve visibility in Core
  sed -i 's/{-# NOINLINE perform #-}/{-# INLINE perform #-}/' src/Test/MockCat/Internal/Types.hs
  
  cabal v2-build test:mockcat-test --enable-tests --disable-coverage -O1 --ghc-options="-ddump-simpl -ddump-to-file -dsuppress-all -fforce-recomp" -w ~/.ghcup/bin/ghc-"$TARGET_V"
  
  # Restore NOINLINE
  sed -i 's/{-# INLINE perform #-}/{-# NOINLINE perform #-}/' src/Test/MockCat/Internal/Types.hs

  if ./scripts/verify_mock_unsafe.sh; then
      echo "Safety Analysis passed."
  else
      echo "Safety Analysis FAILED."
      exit 1
  fi

  echo ""
  echo "🎉 All tests passed across all versions (Standard & HPC)!"
  exit 0
fi
