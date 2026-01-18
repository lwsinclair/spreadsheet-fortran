#!/bin/bash
#
# configure.sh - Configure XL Spreadsheet for different platforms
#
# Usage: ./configure.sh [full|cpm|minimal]
#
# This script updates PARAMETER statements in all source files
# to match the selected configuration.
#
# Author: Claude Code
# Date: 2026-01-19
#

set -e

CONFIG="${1:-full}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration values
case "$CONFIG" in
    full)
        echo -e "${GREEN}Configuring for: FULL (CP-V, large systems)${NC}"
        MAXCEL=2000
        HASHSZ=1024
        MAXSTR=10000
        MAXDEP=1000
        DEPHSZ=256
        MAXQUE=500
        MAXTOK=100
        MAXSTK=50
        MAXDPS=100
        TARGET="CP-V (512KB+)"
        ;;
    cpm)
        echo -e "${GREEN}Configuring for: COMPACT (CP/M)${NC}"
        MAXCEL=300
        HASHSZ=256
        MAXSTR=2000
        MAXDEP=150
        DEPHSZ=64
        MAXQUE=75
        MAXTOK=50
        MAXSTK=25
        MAXDPS=50
        TARGET="CP/M (48KB)"
        ;;
    minimal)
        echo -e "${GREEN}Configuring for: MINIMAL (Tiny systems)${NC}"
        MAXCEL=100
        HASHSZ=64
        MAXSTR=500
        MAXDEP=50
        DEPHSZ=32
        MAXQUE=25
        MAXTOK=25
        MAXSTK=15
        MAXDPS=25
        TARGET="Educational/Embedded"
        ;;
    *)
        echo -e "${RED}Error: Unknown configuration '$CONFIG'${NC}"
        echo "Usage: $0 [full|cpm|minimal]"
        exit 1
        ;;
esac

echo ""
echo "Target Platform: $TARGET"
echo "Configuration Parameters:"
echo "  MAXCEL=$MAXCEL  HASHSZ=$HASHSZ  MAXSTR=$MAXSTR"
echo "  MAXDEP=$MAXDEP  DEPHSZ=$DEPHSZ  MAXQUE=$MAXQUE"
echo "  MAXTOK=$MAXTOK  MAXSTK=$MAXSTK  MAXDPS=$MAXDPS"
echo ""

# Function to update a single PARAMETER statement
update_param() {
    local file="$1"
    local param_pattern="$2"
    local new_values="$3"

    if [ ! -f "$file" ]; then
        echo -e "${RED}Error: File not found: $file${NC}"
        return 1
    fi

    # Use sed to replace the PARAMETER line
    # This matches: PARAMETER (PARAM1=value1, PARAM2=value2, ...)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS sed requires -i ''
        sed -i '' "s/PARAMETER ($param_pattern)/PARAMETER ($new_values)/" "$file"
    else
        # Linux sed
        sed -i "s/PARAMETER ($param_pattern)/PARAMETER ($new_values)/" "$file"
    fi

    echo "  ✓ Updated $file"
}

echo "Updating source files..."

# Update CELLS.FOR
update_param "src/layer1/CELLS.FOR" \
    "MAXCEL=[0-9]*, *HASHSZ=[0-9]*, *MAXSTR=[0-9]*" \
    "MAXCEL=$MAXCEL, HASHSZ=$HASHSZ, MAXSTR=$MAXSTR"

# Update DEPS.FOR - Main parameters
update_param "src/layer1/DEPS.FOR" \
    "MAXDEP=[0-9]*, *DEPHSZ=[0-9]*, *MAXQUE=[0-9]*" \
    "MAXDEP=$MAXDEP, DEPHSZ=$DEPHSZ, MAXQUE=$MAXQUE"

# Update DEPS.FOR - MAXDPS in DEPSGET
update_param "src/layer1/DEPS.FOR" \
    "MAXDPS=[0-9]*" \
    "MAXDPS=$MAXDPS"

# Update DEPS.FOR - Two-parameter version
update_param "src/layer1/DEPS.FOR" \
    "MAXDEP=[0-9]*, *DEPHSZ=[0-9]*" \
    "MAXDEP=$MAXDEP, DEPHSZ=$DEPHSZ"

# Update DEPS.FOR - DEPHSZ only (in DEPSHSH)
update_param "src/layer1/DEPS.FOR" \
    "DEPHSZ=[0-9]*" \
    "DEPHSZ=$DEPHSZ"

# Update PARSE.FOR
update_param "src/layer1/PARSE.FOR" \
    "MAXTOK=[0-9]*, *MAXSTK=[0-9]*" \
    "MAXTOK=$MAXTOK, MAXSTK=$MAXSTK"

# Update EVAL.FOR
update_param "src/layer1/EVAL.FOR" \
    "MAXTOK=[0-9]*, *MAXSTK=[0-9]*" \
    "MAXTOK=$MAXTOK, MAXSTK=$MAXSTK"

# Update RECALC.FOR
update_param "src/layer1/RECALC.FOR" \
    "MAXQUE=[0-9]*, *MAXTOK=[0-9]*, *MAXDPS=[0-9]*" \
    "MAXQUE=$MAXQUE, MAXTOK=$MAXTOK, MAXDPS=$MAXDPS"

echo ""
echo -e "${GREEN}Configuration complete!${NC}"
echo ""
echo "Next steps:"
echo "  1. Review changes: git diff src/layer1/"
echo "  2. Build: make clean && make"
echo "  3. Test: python -m pytest test/unit/ -v"
echo ""
echo -e "${YELLOW}Note: This script uses sed pattern matching which may miss some occurrences.${NC}"
echo -e "${YELLOW}Please verify all PARAMETER statements were updated correctly.${NC}"
