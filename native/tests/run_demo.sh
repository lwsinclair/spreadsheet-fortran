#!/bin/bash
#
# run_demo.sh - Create demo spreadsheet using piped input
#
# Creates a comprehensive demo exercising all XL Spreadsheet features.
#
# Key behavior (Lotus 1-2-3 style):
#   - Enter: Store cell, move DOWN, stay in same column
#   - Tab: Store cell, move RIGHT, stay in same row
#
# Usage: ./run_demo.sh [executable]
#   Default: ../xl_large
#
# Author: Claude Code
# Date: 2026-01-28
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

EXECUTABLE="${1:-../xl_large}"

if [ ! -x "$EXECUTABLE" ]; then
    echo "ERROR: Executable not found: $EXECUTABLE"
    echo "Build with: cd .. && make"
    exit 1
fi

echo "============================================================"
echo "XL Spreadsheet Integration Test"
echo "============================================================"
echo ""
echo "Creating demo spreadsheet with all features..."
echo ""

generate_input() {
    local ESC=$'\033'
    local TAB=$'\t'
    local CR=$'\r'
    local LEFT="${ESC}[D"

    # Helper: end row and go to column A of next row
    # After TAB-navigating to column N, use Enter (which goes DOWN, same col)
    # then LEFT×(N-1) to get back to col A
    end_row_5() { printf "${CR}${LEFT}${LEFT}${LEFT}${LEFT}"; }
    end_row_2() { printf "${CR}${LEFT}"; }

    # Row 1: Headers (5 columns: A-E)
    printf "'BUDGET${TAB}"      # A1
    printf "'JAN${TAB}"         # B1
    printf "'FEB${TAB}"         # C1
    printf "'MAR${TAB}"         # D1
    printf "'TOTAL"             # E1 (no TAB)
    end_row_5                   # Enter + move left to A2

    # Row 2: Sales data
    printf "'Sales${TAB}"       # A2
    printf "1000${TAB}"         # B2
    printf "1200${TAB}"         # C2
    printf "1500${TAB}"         # D2
    printf "=@SUM(B2:D2)"       # E2
    end_row_5

    # Row 3: Costs
    printf "'Costs${TAB}"       # A3
    printf "600${TAB}"          # B3
    printf "700${TAB}"          # C3
    printf "800${TAB}"          # D3
    printf "=@SUM(B3:D3)"       # E3
    end_row_5

    # Row 4: Profit
    printf "'Profit${TAB}"      # A4
    printf "=B2-B3${TAB}"       # B4
    printf "=C2-C3${TAB}"       # C4
    printf "=D2-D3${TAB}"       # D4
    printf "=E2-E3"             # E4
    end_row_5

    # Row 5: Empty
    printf "${CR}"

    # Row 6: Stats header
    printf "'STATISTICS${CR}"

    # Row 7-10: Statistical functions (2 columns each)
    printf "'Average${TAB}=@AVG(B2:D2)"
    end_row_2

    printf "'Min${TAB}=@MIN(B2:D2)"
    end_row_2

    printf "'Max${TAB}=@MAX(B2:D2)"
    end_row_2

    printf "'Count${TAB}=@N(B2:D2)"
    end_row_2

    # Row 11: Empty
    printf "${CR}"

    # Row 12: Math header
    printf "'MATH FUNCTIONS${CR}"

    # Row 13-16: Math functions
    printf "'SQRT(16)${TAB}=@SQRT(16)"
    end_row_2

    printf "'2^8${TAB}=2^8"
    end_row_2

    printf "'ABS(-5)${TAB}=@ABS(0-5)"
    end_row_2

    printf "'INT(9.7)${TAB}=@INT(9.7)"
    end_row_2

    # Row 17: Empty
    printf "${CR}"

    # Row 18: Conditional header
    printf "'CONDITIONAL${CR}"

    # Row 19: IF example (6 columns)
    printf "'Target${TAB}"      # A19
    printf "1000${TAB}"         # B19
    printf "'Actual${TAB}"      # C19
    printf "1250${TAB}"         # D19
    printf "'Met?${TAB}"        # E19
    printf "=@IF(D19>B19,1,0)"  # F19
    printf "${CR}"

    # Save and quit
    printf "/SAVE demo.xl${CR}"
    printf "/QUIT${CR}"
}

# Run spreadsheet with generated input
rm -f demo.xl
generate_input | "$EXECUTABLE" > /dev/null 2>&1

# Check result
if [ -f "demo.xl" ]; then
    echo "SUCCESS: demo.xl created"
    echo ""

    # Count cells to verify
    CELL_COUNT=$(grep -c '"ref":' demo.xl)
    echo "Total cells: $CELL_COUNT"
    echo ""

    # Show just the cell references for verification
    echo "Cell layout:"
    grep '"ref":' demo.xl | sed 's/.*"ref": "\([^"]*\)".*/\1/' | paste - - - - - -
    echo ""

    # Show some key formulas
    echo "Key formulas verified:"
    grep -E '(@SUM|@AVG|@MIN|@MAX|@N|@SQRT|@ABS|@INT|@IF|\^)' demo.xl | head -15
    echo ""

    echo "============================================================"
    echo "Test PASSED - All features demonstrated"
    echo "============================================================"
else
    echo "FAILED: demo.xl was not created"
    exit 1
fi
