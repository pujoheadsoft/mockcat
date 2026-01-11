#!/bin/bash

# scripts/verify_mock_unsafe.sh
# Analyzes GHC Core dump for unsafePerformIO usage in SafetyAnalysis.hs patterns.
# Uses transitive call analysis to handle shared implementations and GHC worker functions.

set -e

# Find the most recent Core dump
CORE_FILE=$(find .stack-work dist-newstyle -name "*SafetyAnalysis*.dump-simpl" 2>/dev/null | xargs ls -t 2>/dev/null | head -n 1)

if [ -z "$CORE_FILE" ]; then
  echo "Error: Core dump file not found."
  echo "Run 'stack build' or 'cabal build' with '-ddump-simpl -ddump-to-file' first."
  exit 1
fi

echo "=== GHC Core Safety Analysis ==="
echo "Analyzing Core dump: $CORE_FILE"
echo ""

# Functions to check (all 26 patterns)
FUNCTIONS=(
  "path_plainIO_stub"
  "path_plainIO_mock"
  "path_plainIO_mock_shouldBeCalled"
  "path_plainIO_mockM"
  "path_plainIO_mockM_shouldBeCalled"
  "path_withMock_stub"
  "path_withMock_mock"
  "path_withMock_mock_expects"
  "path_withMock_mock_shouldBeCalled"
  "path_withMock_mockM"
  "path_withMock_mockM_expects"
  "path_withMock_mockM_shouldBeCalled"
  "path_withMockIO_stub"
  "path_withMockIO_mock"
  "path_withMockIO_mock_expects"
  "path_withMockIO_mock_shouldBeCalled"
  "path_withMockIO_mockM"
  "path_withMockIO_mockM_expects"
  "path_withMockIO_mockM_shouldBeCalled"
  "path_runMockT_stub"
  "path_runMockT_mock"
  "path_runMockT_mock_expects"
  "path_runMockT_mock_shouldBeCalled"
  "path_runMockT_mockM"
  "path_runMockT_mockM_expects"
  "path_runMockT_mockM_shouldBeCalled"
)

# Expected results
declare -A EXPECTED
EXPECTED["path_plainIO_stub"]="SAFE"
EXPECTED["path_plainIO_mock"]="UNSAFE"
EXPECTED["path_plainIO_mock_shouldBeCalled"]="UNSAFE"
EXPECTED["path_plainIO_mockM"]="SAFE"
EXPECTED["path_plainIO_mockM_shouldBeCalled"]="SAFE"
EXPECTED["path_withMock_stub"]="SAFE"
EXPECTED["path_withMock_mock"]="UNSAFE"
EXPECTED["path_withMock_mock_expects"]="UNSAFE"
EXPECTED["path_withMock_mock_shouldBeCalled"]="UNSAFE"
EXPECTED["path_withMock_mockM"]="SAFE"
EXPECTED["path_withMock_mockM_expects"]="SAFE"
EXPECTED["path_withMock_mockM_shouldBeCalled"]="SAFE"
EXPECTED["path_withMockIO_stub"]="SAFE"
EXPECTED["path_withMockIO_mock"]="UNSAFE"
EXPECTED["path_withMockIO_mock_expects"]="UNSAFE"
EXPECTED["path_withMockIO_mock_shouldBeCalled"]="UNSAFE"
EXPECTED["path_withMockIO_mockM"]="SAFE"
EXPECTED["path_withMockIO_mockM_expects"]="SAFE"
EXPECTED["path_withMockIO_mockM_shouldBeCalled"]="SAFE"
EXPECTED["path_runMockT_stub"]="SAFE"
EXPECTED["path_runMockT_mock"]="UNSAFE"
EXPECTED["path_runMockT_mock_expects"]="UNSAFE"
EXPECTED["path_runMockT_mock_shouldBeCalled"]="UNSAFE"
EXPECTED["path_runMockT_mockM"]="SAFE"
EXPECTED["path_runMockT_mockM_expects"]="SAFE"
EXPECTED["path_runMockT_mockM_shouldBeCalled"]="SAFE"

# Transitive analysis logic
check_safety() {
  local entry_sym=$1
  local check_queue=("$entry_sym")
  local visited=""
  
  while [ ${#check_queue[@]} -gt 0 ]; do
    local current=${check_queue[0]}
    check_queue=("${check_queue[@]:1}")
    
    # Skip if already visited
    if [[ " $visited " =~ " $current " ]]; then continue; fi
    visited="$visited $current"
    
    # Extract block for the current symbol (including GHC prefixes)
    # Regex handles things like path_foo, $wpath_foo, $cpath_foo, etc.
    local block=$(awk -v sym="$current" '
      BEGIN { printing = 0; regex = "^([$][a-z0-9])*[*]*" sym "[0-9]*($|[ =])" }
      /^[^ ]/ { if ($0 ~ regex) { printing = 1 } else { printing = 0 } }
      { if (printing) print }
    ' "$CORE_FILE")
    
    if [ -z "$block" ]; then continue; fi
    
    # Core detection: presence of unsafe primitives in THIS block
    if echo "$block" | grep -q "runRW#" && echo "$block" | grep -q "noDuplicate#"; then
      echo "UNSAFE"
      return
    fi
    
    # Find all path-related symbols called within this block and add to queue
    # This follows transitive calls across generated bindings
    local children=$(echo "$block" | grep -oE '([$][a-z0-9])*path_[a-zA-Z0-9_]+' | sort -u)
    for child in $children; do
      check_queue+=("$child")
    done
  done
  
  echo "SAFE"
}

mismatches=0
echo "$(printf "%-40s" "Function Path") | $(printf "%-10s" "Expected") | $(printf "%-10s" "Actual") | Result"
echo "------------------------------------------------------------"

for func in "${FUNCTIONS[@]}"; do
  actual=$(check_safety "$func")
  expected=${EXPECTED[$func]}
  
  if [ "$actual" == "$expected" ]; then
    res="✅  "
  else
    res="❌  "
    mismatches=$((mismatches + 1))
  fi

  echo "$(printf "%-40s" "$func") | $(printf "%-10s" "$expected") | $(printf "%-10s" "$actual") | $res"
done

echo "------------------------------------------------------------"
echo "Summary:"
echo "  Total: ${#FUNCTIONS[@]}"
echo "  Mismatches: $mismatches"
echo ""

if [ "$mismatches" -eq 0 ]; then
  echo "✅ Safety Analysis PASSED (All results match expectations)"
  exit 0
else
  echo "❌ Safety Analysis FAILED ($mismatches mismatches)"
  exit 1
fi
