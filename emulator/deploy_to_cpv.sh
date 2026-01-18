#!/bin/bash
#
# XL Spreadsheet - One-Command Deployment to CP-V
#
# This script:
# 1. Checks if CP-V is properly installed
# 2. Installs CP-V from tape if needed
# 3. Boots CP-V if not running
# 4. Transfers all XL Spreadsheet files
# 5. Submits the batch build job
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

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}XL Spreadsheet - CP-V Deployment${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check if expect is installed
if ! command -v expect &> /dev/null; then
    echo -e "${RED}ERROR: expect is not installed${NC}"
    echo "Install with: brew install expect"
    exit 1
fi

# Check if sigma emulator is installed
if ! command -v sigma &> /dev/null; then
    echo -e "${RED}ERROR: sigma emulator (simh) is not installed${NC}"
    echo "Please install simh Sigma emulator first"
    exit 1
fi

# Check if CP-V system files exist
if [ ! -f "sigma-cpv-kit/f00/f00rad/rad" ] || \
   [ ! -f "sigma-cpv-kit/f00/f00rad/sys1" ] || \
   [ ! -f "sigma-cpv-kit/f00/f00rad/sys2" ]; then
    echo -e "${RED}ERROR: CP-V system files not found${NC}"
    echo "Please unzip sigma-cpv-kit/f00/f00rad/f00rad-system.zip first"
    exit 1
fi

# Check if work files exist
if [ ! -d "work" ]; then
    echo -e "${RED}ERROR: work directory not found${NC}"
    exit 1
fi

# Count source files
file_count=$(ls -1 work/*.FOR work/*.JOB 2>/dev/null | wc -l | tr -d ' ')
if [ "$file_count" -lt 12 ]; then
    echo -e "${RED}ERROR: Expected 12 files (11 .FOR + 1 .JOB), found $file_count${NC}"
    echo "Files in work directory:"
    ls -1 work/
    exit 1
fi

echo -e "${GREEN}✓${NC} Prerequisites checked"
echo -e "${GREEN}✓${NC} Found $file_count files ready for deployment"
echo ""

# Check for running sigma processes and offer to clean them up
if pgrep -f "sigma.*cpv" > /dev/null || pgrep -f "sigma.*install" > /dev/null; then
    echo -e "${YELLOW}⚠${NC}  Found running sigma processes"
    ps aux | grep -i sigma | grep -v grep | head -5
    echo ""
    echo "These processes should be stopped before deployment."
    echo ""
    read -p "Kill these processes and continue? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "Stopping sigma processes..."
        pkill -9 -f "sigma"
        sleep 2
        echo -e "${GREEN}✓${NC} Processes stopped"
        SKIP_BOOT=0
    else
        echo "Deployment cancelled"
        exit 0
    fi
elif lsof -i :5001 &> /dev/null; then
    echo -e "${YELLOW}⚠${NC}  CP-V appears to be already running on port 5001"
    echo ""
    read -p "Do you want to proceed with deployment? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Deployment cancelled"
        exit 0
    fi
    SKIP_BOOT=1
else
    SKIP_BOOT=0
fi
echo ""

# Function to check if system is properly installed
check_cpv_installation() {
    # Check if rad file has been written to (size > 5MB suggests initialized)
    local rad_size=$(stat -f%z "sigma-cpv-kit/f00/f00rad/rad" 2>/dev/null || echo 0)
    if [ "$rad_size" -gt 5000000 ]; then
        # Check modification time - if recently modified, likely initialized
        local rad_mtime=$(stat -f%m "sigma-cpv-kit/f00/f00rad/rad" 2>/dev/null || echo 0)
        local sys1_mtime=$(stat -f%m "sigma-cpv-kit/f00/f00rad/sys1" 2>/dev/null || echo 0)

        # If both rad and sys1 were modified, system is likely installed
        if [ "$rad_mtime" -gt 0 ] && [ "$sys1_mtime" -gt 0 ]; then
            return 0  # System appears installed
        fi
    fi
    return 1  # System needs installation
}

# Decide if we need to install or just boot
if [ $SKIP_BOOT -eq 0 ]; then
    if check_cpv_installation; then
        echo -e "${BLUE}>>> Step 1: Booting CP-V from RAD${NC}"
        echo ""
        echo "Starting CP-V emulator..."
        echo "Boot process is automated and takes about 1 minute"
        echo ""

        # Boot from existing installation using expect script
        if [ -f "scripts/boot_cpv.exp" ]; then
            scripts/boot_cpv.exp
            BOOT_EXIT=$?

            if [ $BOOT_EXIT -ne 0 ]; then
                echo ""
                echo -e "${RED}ERROR: Boot failed (exit code: $BOOT_EXIT)${NC}"
                echo "Check the error messages above for details"
                exit 1
            fi

            echo ""
            echo -e "${GREEN}✓${NC} CP-V booted and configured successfully"
        else
            # Fallback to manual boot if script not found
            echo -e "${YELLOW}⚠${NC}  Boot script not found, using manual boot"
            echo "Starting in background..."
            sigma boot_cpv.ini &
            SIGMA_PID=$!

            # Wait for system to boot
            echo "Waiting for CP-V to start..."
            for i in {1..30}; do
                if lsof -i :5001 &> /dev/null; then
                    echo -e "${GREEN}✓${NC} CP-V is ready on port 5001"
                    break
                fi
                sleep 1
                echo -n "."
            done
            echo ""

            if ! lsof -i :5001 &> /dev/null; then
                echo -e "${RED}ERROR: CP-V did not start within 30 seconds${NC}"
                echo "Check the emulator console for errors"
                exit 1
            fi
        fi

    else
        echo -e "${BLUE}>>> Step 1: Installing CP-V from PO Tape${NC}"
        echo ""
        echo -e "${YELLOW}⚠${NC}  This is the first time setup"
        echo "This will install CP-V F00 from the installation tape"
        echo "Installation is fully automated and takes 2-3 minutes"
        echo ""

        # Run installation script
        if [ -f "scripts/install_cpv.exp" ]; then
            echo "Starting automated installation..."
            echo "(This may take several minutes - please be patient)"
            echo ""

            scripts/install_cpv.exp
            INSTALL_EXIT=$?

            if [ $INSTALL_EXIT -ne 0 ]; then
                echo ""
                echo -e "${RED}ERROR: Installation failed (exit code: $INSTALL_EXIT)${NC}"
                echo "Check the error messages above for details"
                echo ""
                echo "Common issues:"
                echo "  - Timeout: Installation took longer than expected"
                echo "  - Prompt not found: Installation tape may be corrupt"
                echo "  - System already running: Kill existing sigma processes first"
                exit 1
            fi

            echo ""
            echo -e "${GREEN}✓${NC} CP-V installed and configured successfully"
        else
            echo -e "${RED}ERROR: Installation script not found${NC}"
            echo "Expected: scripts/install_cpv.exp"
            exit 1
        fi
    fi

    # Give the system a moment to stabilize
    echo ""
    echo "Waiting for system to stabilize..."
    sleep 3

    # Verify CP-V is actually running on port 5001
    echo "Verifying CP-V is ready..."
    for i in {1..10}; do
        if lsof -i :5001 &> /dev/null; then
            echo -e "${GREEN}✓${NC} CP-V is running on port 5001"
            break
        fi
        if [ $i -eq 10 ]; then
            echo -e "${RED}ERROR: CP-V did not start properly${NC}"
            echo "The emulator may have crashed. Check for errors above."
            exit 1
        fi
        sleep 1
        echo -n "."
    done
fi

echo ""
echo -e "${BLUE}>>> Step 2: Deploying XL Spreadsheet${NC}"
echo ""
echo "Transferring 12 files to CP-V..."
echo "This will take several minutes..."
echo ""

# Run deployment script
if [ -f "scripts/deploy_xl.exp" ]; then
    scripts/deploy_xl.exp

    if [ $? -ne 0 ]; then
        echo ""
        echo -e "${RED}ERROR: Deployment failed${NC}"
        echo "Check the error messages above"
        exit 1
    fi
else
    echo -e "${RED}ERROR: Deployment script not found${NC}"
    exit 1
fi

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}Deployment Complete!${NC}"
echo -e "${GREEN}========================================${NC}"
echo ""
echo "Next steps:"
echo "1. The telnet session is still open for you"
echo "2. Check batch job status: STATUS"
echo "3. View compilation output in work/printer.txt"
echo "4. Run XL Spreadsheet: RUN XL"
echo ""
echo "To shutdown CP-V:"
echo "1. Press Ctrl-E in the emulator console"
echo "2. Type: ZAP"
echo "3. Wait for 'THAT'S ALL, FOLKS!!'"
echo "4. Type: quit"
echo ""
