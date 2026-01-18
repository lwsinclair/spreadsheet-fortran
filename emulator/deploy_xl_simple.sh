#!/bin/bash
#
# Simple XL Spreadsheet Deployment to CP-V
#
# Prerequisites:
# - CP-V system files extracted (f00rad-system.zip)
# - expect installed (brew install expect)
# - screen installed (built-in on macOS)
#

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

echo "=========================================="
echo "XL Spreadsheet - CP-V Deployment"
echo "=========================================="
echo ""

# Step 1: Check if CP-V is already running
echo "Step 1: Checking if CP-V is running..."
if lsof -i :5001 >/dev/null 2>&1; then
    echo "✓ CP-V is already running on port 5001"
else
    echo "CP-V is not running, starting it now..."
    ./scripts/boot_cpv_screen.sh
    if [ $? -ne 0 ]; then
        echo "ERROR: Failed to boot CP-V"
        exit 1
    fi
fi

echo ""

# Step 2: Verify we can connect
echo "Step 2: Verifying CP-V is ready..."
sleep 2
if ! lsof -i :5001 >/dev/null 2>&1; then
    echo "ERROR: CP-V is not responding on port 5001"
    exit 1
fi
echo "✓ CP-V is ready"
echo ""

# Step 3: Transfer files and submit batch job
echo "Step 3: Transferring files and submitting batch job..."
echo "This will take 3-4 minutes..."
echo ""

# TODO: Create deploy_xl.exp script for file transfer
# For now, give manual instructions

echo "=========================================="
echo "CP-V is running and ready!"
echo "=========================================="
echo ""
echo "To deploy XL Spreadsheet manually:"
echo "1. Connect: nc localhost 5001"
echo "2. Login: :SYS,LBE"
echo "3. For each file in work/:"
echo "   - EDIT filename"
echo "   - Paste file contents"
echo "   - :FILE"
echo "   - :QUIT"
echo "4. After all 12 files: SUBMIT XLBUILD.JOB"
echo ""
echo "Automated file transfer coming soon!"
echo ""
echo "To shutdown: screen -S cpv -X quit"
echo "=========================================="
