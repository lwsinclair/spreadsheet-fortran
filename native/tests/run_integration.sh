#!/bin/bash
#
# run_integration.sh - Run the XL Spreadsheet integration test
#
# This script runs the expect-based integration test that creates
# a demo spreadsheet exercising all features.
#
# Usage: ./run_integration.sh [executable]
#   Default: ../xl_large
#
# Author: Claude Code
# Date: 2026-01-28
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Default executable
EXECUTABLE="${1:-../xl_large}"

# Check if executable exists
if [ ! -x "$EXECUTABLE" ]; then
    echo "ERROR: Executable not found: $EXECUTABLE"
    echo ""
    echo "Please build first:"
    echo "  cd .. && make"
    exit 1
fi

# Check if expect is available
if ! command -v expect &> /dev/null; then
    echo "ERROR: expect is not installed"
    exit 1
fi

# Make the expect script executable
chmod +x integration_demo.exp

echo "============================================================"
echo "XL Spreadsheet Integration Test"
echo "============================================================"
echo ""
echo "Executable: $EXECUTABLE"
echo ""

# Run the expect script
./integration_demo.exp "$EXECUTABLE"

# Check if demo.xl was created
if [ -f "demo.xl" ]; then
    echo ""
    echo "File demo.xl was created:"
    ls -la demo.xl
    echo ""
    echo "Contents:"
    cat demo.xl | head -50
elif [ -f "../demo.xl" ]; then
    echo ""
    echo "File demo.xl was created in parent directory:"
    ls -la ../demo.xl
fi

echo ""
echo "============================================================"
echo "Integration test completed"
echo "============================================================"
