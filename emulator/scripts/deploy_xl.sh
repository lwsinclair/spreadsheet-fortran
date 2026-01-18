#!/bin/bash
#
# deploy_xl.sh - Create tape image with all XL source files for CP-V
#
# Usage: ./scripts/deploy_xl.sh
#
# Creates: work/xl_transfer.tap (magnetic tape image)
#
# This tape can be mounted in the CP-V emulator and files copied to disk
#

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Creating XL Spreadsheet deployment tape for CP-V${NC}"
echo ""

# Source files to include
SRC_DIR="../src"
WORK_DIR="work"
TAPE_FILE="$WORK_DIR/xl_transfer.tap"

# Create work directory if needed
mkdir -p "$WORK_DIR"

# Remove old tape
rm -f "$TAPE_FILE"

echo "Including source files:"

# Layer 0
echo "  Layer 0: STRUTIL.FOR"
FILES="$SRC_DIR/layer0/STRUTIL.FOR"

# Layer 1
echo "  Layer 1: CELLS.FOR DEPS.FOR PARSE.FOR EVAL.FOR RECALC.FOR"
FILES="$FILES $SRC_DIR/layer1/CELLS.FOR"
FILES="$FILES $SRC_DIR/layer1/DEPS.FOR"
FILES="$FILES $SRC_DIR/layer1/PARSE.FOR"
FILES="$FILES $SRC_DIR/layer1/EVAL.FOR"
FILES="$FILES $SRC_DIR/layer1/RECALC.FOR"

# Layer 2
echo "  Layer 2: UI.FOR DISPLAY.FOR MSG.FOR"
FILES="$FILES $SRC_DIR/layer2/UI.FOR"
FILES="$FILES $SRC_DIR/layer2/DISPLAY.FOR"
FILES="$FILES $SRC_DIR/layer2/MSG.FOR"

# Layer 3
echo "  Layer 3: TERMCPV.FOR"
FILES="$FILES $SRC_DIR/layer3/TERMCPV.FOR"

# Main program
echo "  Main: XLMAIN.FOR"
FILES="$FILES $SRC_DIR/XLMAIN.FOR"

echo ""
echo "Creating tape image..."

# Use the existing make_tape.sh script (if it handles multiple files)
# Or create tape manually using simh tape format

# For now, copy files to work directory with CP-V friendly names
# (CP-V may have file name length limitations)

cp "$SRC_DIR/layer0/STRUTIL.FOR" "$WORK_DIR/STRUTIL.FOR"
cp "$SRC_DIR/layer1/CELLS.FOR" "$WORK_DIR/CELLS.FOR"
cp "$SRC_DIR/layer1/DEPS.FOR" "$WORK_DIR/DEPS.FOR"
cp "$SRC_DIR/layer1/PARSE.FOR" "$WORK_DIR/PARSE.FOR"
cp "$SRC_DIR/layer1/EVAL.FOR" "$WORK_DIR/EVAL.FOR"
cp "$SRC_DIR/layer1/RECALC.FOR" "$WORK_DIR/RECALC.FOR"
cp "$SRC_DIR/layer2/UI.FOR" "$WORK_DIR/UI.FOR"
cp "$SRC_DIR/layer2/DISPLAY.FOR" "$WORK_DIR/DISPLAY.FOR"
cp "$SRC_DIR/layer2/MSG.FOR" "$WORK_DIR/MSG.FOR"
cp "$SRC_DIR/layer3/TERMCPV.FOR" "$WORK_DIR/TERMCPV.FOR"
cp "$SRC_DIR/XLMAIN.FOR" "$WORK_DIR/XLMAIN.FOR"

echo -e "${GREEN}✓ Files copied to work/ directory${NC}"
echo ""
echo "Files ready for transfer:"
ls -lh "$WORK_DIR"/*.FOR
echo ""

# XLBUILD.JOB is already created directly in work/
if [ -f "$WORK_DIR/XLBUILD.JOB" ]; then
    echo "Batch job file:"
    ls -lh "$WORK_DIR/XLBUILD.JOB"
    echo ""
fi

echo -e "${YELLOW}Next steps:${NC}"
echo "1. Start CP-V emulator: sigma boot_cpv.ini"
echo "2. Connect via telnet: telnet localhost 5001"
echo "3. Log in: :SYS,LBE"
echo "4. Transfer files (11 .FOR + 1 .JOB) using one of:"
echo ""
echo "   Option A: Batch job deployment (RECOMMENDED for 1978 authenticity)"
echo "   - Transfer all 12 files to CP-V"
echo "   - Submit batch job: SUBMIT XLBUILD.JOB"
echo "   - See BATCH_DEPLOYMENT.md for details"
echo ""
echo "   Option B: Interactive compilation"
echo "   - Transfer .FOR files only"
echo "   - Compile manually at CP-V prompt"
echo "   - See CPV_DEPLOYMENT.md for details"
echo ""
echo "See emulator/BATCH_DEPLOYMENT.md for batch job instructions"
echo ""
echo -e "${GREEN}Deployment package ready!${NC}"
