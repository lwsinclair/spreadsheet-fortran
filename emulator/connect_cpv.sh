#!/bin/bash
#
# connect_cpv.sh - Connect to CP-V emulator
#
# Uses nc (netcat) instead of telnet
# Sets proper terminal emulation for VT-52
#

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║           Connecting to CP-V Emulator (VT-52 Mode)          ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if emulator is running
if ! lsof -i :5001 >/dev/null 2>&1; then
    echo -e "${YELLOW}Warning: CP-V emulator may not be running on port 5001${NC}"
    echo ""
    echo "To start emulator:"
    echo "  cd emulator"
    echo "  sigma boot_cpv.ini &"
    echo ""
    echo -e "Continue anyway? ${CYAN}Press RETURN to continue, Ctrl-C to abort${NC}"
    read
fi

# Set terminal type for VT-52/VT-100 compatibility
export TERM=vt100

echo -e "${CYAN}Terminal settings:${NC}"
echo "  Type: vt100 (VT-52 compatible)"
echo "  Lines: 24"
echo "  Columns: 80"
echo ""

echo -e "${CYAN}CP-V Login:${NC}"
echo "  At 'LOGON PLEASE:' type:  ${GREEN}:SYS,LBE${NC}"
echo "  No password required"
echo ""

echo -e "${CYAN}To exit:${NC}"
echo "  From CP-V:  ${GREEN}LOGOFF${NC}"
echo "  Force quit: ${GREEN}Ctrl-C${NC}"
echo ""

echo -e "${CYAN}Connecting to localhost:5001...${NC}"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

# Connect using netcat
nc localhost 5001

# After disconnect
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${GREEN}Disconnected from CP-V${NC}"
echo ""
