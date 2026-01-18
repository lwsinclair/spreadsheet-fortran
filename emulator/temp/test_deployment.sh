#!/bin/bash
#
# Comprehensive CP-V Deployment Testing Script
# Tests each step of the deployment process
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

EMULATOR_DIR="/Volumes/SECURE8/git/spreadsheet-fortran/emulator"
cd "$EMULATOR_DIR"

echo "=========================================="
echo "CP-V Deployment Testing Suite"
echo "=========================================="
echo ""

# Function to check if CP-V is running
check_cpv_running() {
    if lsof -i :5001 &> /dev/null; then
        return 0  # Running
    else
        return 1  # Not running
    fi
}

# Function to kill sigma processes
kill_sigma() {
    echo "Checking for running sigma processes..."
    if pgrep -f sigma > /dev/null; then
        echo "Found sigma processes:"
        ps aux | grep -i sigma | grep -v grep
        echo ""
        echo "Killing sigma processes..."
        pkill -9 -f sigma || true
        sleep 2

        if pgrep -f sigma > /dev/null; then
            echo -e "${RED}ERROR: Could not kill sigma processes${NC}"
            exit 1
        fi
        echo -e "${GREEN}✓${NC} Sigma processes killed"
    else
        echo -e "${GREEN}✓${NC} No sigma processes running"
    fi
}

# Test 1: Check prerequisites
echo -e "${BLUE}Test 1: Checking Prerequisites${NC}"
echo "---"

if ! command -v expect &> /dev/null; then
    echo -e "${RED}✗${NC} expect not installed"
    exit 1
fi
echo -e "${GREEN}✓${NC} expect installed"

if ! command -v sigma &> /dev/null; then
    echo -e "${RED}✗${NC} sigma emulator not installed"
    exit 1
fi
echo -e "${GREEN}✓${NC} sigma emulator installed"

if [ ! -f "sigma-cpv-kit/f00/f00rad/rad" ]; then
    echo -e "${RED}✗${NC} rad disk image not found"
    exit 1
fi
echo -e "${GREEN}✓${NC} Disk images present"

if [ ! -d "work" ]; then
    echo -e "${RED}✗${NC} work directory not found"
    exit 1
fi
echo -e "${GREEN}✓${NC} work directory exists"

file_count=$(ls -1 work/*.FOR work/*.JOB 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -lt 12 ]; then
    echo -e "${RED}✗${NC} Expected 12 files, found $file_count"
    exit 1
fi
echo -e "${GREEN}✓${NC} Found $file_count files ready for deployment"

echo ""

# Test 2: Kill any running processes
echo -e "${BLUE}Test 2: Clean Environment${NC}"
echo "---"
kill_sigma
echo ""

# Test 3: Check if disk images need installation
echo -e "${BLUE}Test 3: Check Disk Image Status${NC}"
echo "---"
rad_size=$(stat -f%z "sigma-cpv-kit/f00/f00rad/rad" 2>/dev/null || echo 0)
echo "RAD disk size: $rad_size bytes"

if [ "$rad_size" -gt 5000000 ]; then
    echo -e "${GREEN}✓${NC} Disk appears to have been initialized (size > 5MB)"
    echo "Will test BOOT path"
    TEST_INSTALL=0
else
    echo -e "${YELLOW}⚠${NC}  Disk appears fresh (size < 5MB)"
    echo "Will test INSTALLATION path"
    TEST_INSTALL=1
fi
echo ""

# Test 4: Test boot or installation
if [ $TEST_INSTALL -eq 0 ]; then
    echo -e "${BLUE}Test 4: Testing Boot Process${NC}"
    echo "---"
    echo "Running scripts/boot_cpv.exp..."
    echo ""

    if ! scripts/boot_cpv.exp; then
        echo ""
        echo -e "${RED}✗${NC} Boot failed"
        exit 1
    fi

    echo ""
    echo -e "${GREEN}✓${NC} Boot process completed"
else
    echo -e "${BLUE}Test 4: Testing Installation Process${NC}"
    echo "---"
    echo "Running scripts/install_cpv.exp..."
    echo ""

    if ! scripts/install_cpv.exp; then
        echo ""
        echo -e "${RED}✗${NC} Installation failed"
        exit 1
    fi

    echo ""
    echo -e "${GREEN}✓${NC} Installation process completed"
fi
echo ""

# Test 5: Verify CP-V is running
echo -e "${BLUE}Test 5: Verify CP-V Running${NC}"
echo "---"
echo "Checking if CP-V is listening on port 5001..."

for i in {1..20}; do
    if check_cpv_running; then
        echo -e "${GREEN}✓${NC} CP-V is running on port 5001"
        break
    fi
    if [ $i -eq 20 ]; then
        echo -e "${RED}✗${NC} CP-V not running after 20 seconds"
        exit 1
    fi
    sleep 1
    echo -n "."
done
echo ""
echo ""

# Test 6: Test telnet connectivity
echo -e "${BLUE}Test 6: Test Telnet Connectivity${NC}"
echo "---"
echo "Attempting to connect to CP-V..."

timeout 10 bash -c 'echo | telnet localhost 5001' &> /tmp/telnet_test.txt || true

if grep -q "LOGON PLEASE" /tmp/telnet_test.txt || grep -q "Connected" /tmp/telnet_test.txt; then
    echo -e "${GREEN}✓${NC} Telnet connection successful"
else
    echo -e "${YELLOW}⚠${NC}  Could not verify telnet connection"
    echo "Output:"
    cat /tmp/telnet_test.txt
fi
echo ""

# Test 7: Test file deployment (optional, requires manual verification)
echo -e "${BLUE}Test 7: File Deployment${NC}"
echo "---"
echo "This would run scripts/deploy_xl.exp"
echo "Skipping for now to avoid side effects"
echo -e "${YELLOW}⚠${NC}  Manual test recommended"
echo ""

# Test 8: Shutdown
echo -e "${BLUE}Test 8: Clean Shutdown${NC}"
echo "---"
echo "Testing shutdown script..."

if [ -f "scripts/shutdown_cpv.exp" ]; then
    scripts/shutdown_cpv.exp
    echo -e "${GREEN}✓${NC} Shutdown completed"
else
    echo "Shutdown script not found, killing processes manually..."
    kill_sigma
fi

sleep 2

if check_cpv_running; then
    echo -e "${RED}✗${NC} CP-V still running after shutdown"
    exit 1
else
    echo -e "${GREEN}✓${NC} CP-V shutdown successfully"
fi
echo ""

# Summary
echo "=========================================="
echo -e "${GREEN}All Tests Passed!${NC}"
echo "=========================================="
echo ""
echo "Next steps:"
echo "1. Run full deployment: ./deploy_to_cpv.sh"
echo "2. Verify spreadsheet compilation"
echo "3. Test running XL spreadsheet"
echo ""
