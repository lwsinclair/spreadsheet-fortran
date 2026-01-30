#!/bin/bash
#
# gen_demo_input.sh - Generate demo input file with escape sequences
#
# Creates demo_input.bin with actual ANSI escape codes for arrow keys
#
# Author: Claude Code
# Date: 2026-01-28
#

# Output file
OUT="demo_input.bin"

# Escape sequences
ESC=$'\x1b'
UP="${ESC}[A"
DOWN="${ESC}[B"
RIGHT="${ESC}[C"
LEFT="${ESC}[D"
CR=$'\r'

# Function to output text with newline (Enter key)
enter() {
    printf '%s\r' "$1"
}

# Function to move cursor
move_right() { printf '%s' "$RIGHT"; }
move_left()  { printf '%s' "$LEFT"; }
move_down()  { printf '%s' "$DOWN"; }
move_up()    { printf '%s' "$UP"; }

{
    # Row 1: Headers
    enter "'QUARTER"
    move_right; enter "'Q1"
    move_right; enter "'Q2"
    move_right; enter "'Q3"
    move_right; enter "'Q4"
    move_right; enter "'TOTAL"

    # Move to A2
    move_left; move_left; move_left; move_left; move_left
    move_down

    # Row 2: Sales data
    enter "'Sales"
    move_right; enter "100"
    move_right; enter "150"
    move_right; enter "200"
    move_right; enter "250"
    move_right; enter "=@SUM(B2:E2)"

    # Move to A3
    move_left; move_left; move_left; move_left; move_left
    move_down

    # Row 3: Expenses
    enter "'Expense"
    move_right; enter "80"
    move_right; enter "90"
    move_right; enter "100"
    move_right; enter "110"
    move_right; enter "=@SUM(B3:E3)"

    # Move to A4
    move_left; move_left; move_left; move_left; move_left
    move_down

    # Row 4: Profit
    enter "'Profit"
    move_right; enter "=B2-B3"
    move_right; enter "=C2-C3"
    move_right; enter "=D2-D3"
    move_right; enter "=E2-E3"
    move_right; enter "=@SUM(B4:E4)"

    # Skip row, Stats header
    move_left; move_left; move_left; move_left; move_left
    move_down; move_down
    enter "'STATS"

    # Row 7: Average
    move_left
    move_down
    enter "'Average"
    move_right; enter "=@AVG(B2:E2)"

    # Row 8: Min
    move_left
    move_down
    enter "'Min"
    move_right; enter "=@MIN(B2:E2)"

    # Row 9: Max
    move_left
    move_down
    enter "'Max"
    move_right; enter "=@MAX(B2:E2)"

    # Row 10: Count
    move_left
    move_down
    enter "'Count"
    move_right; enter "=@N(B2:E2)"

    # Skip row, Math header
    move_left
    move_down; move_down
    enter "'MATH"

    # Row 13: SQRT
    move_left
    move_down
    enter "'SQRT16"
    move_right; enter "=@SQRT(16)"

    # Row 14: Power
    move_left
    move_down
    enter "'3^2"
    move_right; enter "=3^2"

    # Row 15: ABS
    move_left
    move_down
    enter "'ABS"
    move_right; enter "=@ABS(0-5)"

    # Row 16: INT
    move_left
    move_down
    enter "'INT"
    move_right; enter "=@INT(7.9)"

    # Skip row, IF header
    move_left
    move_down; move_down
    enter "'IF TEST"

    # Row 19: IF example
    move_left
    move_down
    enter "100"
    move_right; enter "50"
    move_right; enter "=@IF(A19>B19,1,0)"

    # Save and Quit
    enter "/SAVE demo.xl"
    enter "/QUIT"

} > "$OUT"

echo "Generated: $OUT"
echo "Size: $(wc -c < "$OUT") bytes"
echo ""
echo "To run: ../xl_large < $OUT"
