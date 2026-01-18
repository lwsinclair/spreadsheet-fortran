#!/bin/bash
#
# Boot CP-V cpcp system in detached screen session
# This system has working FORTRAN compiler (not ANSFORT)
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

echo "Starting CP-V cpcp system in detached screen session 'cpv'..."

# Kill any existing screen session
screen -S cpv -X quit 2>/dev/null

# Start in detached screen session
screen -dmS cpv "$SCRIPT_DIR/boot_cpcp.exp"

# Wait for port 5001 to be listening
echo "Waiting for CP-V to boot (this takes about 30-40 seconds)..."
for i in {1..60}; do
    if lsof -i :5001 >/dev/null 2>&1; then
        echo "✓ Port 5001 is listening"
        echo ""
        echo "=========================================="
        echo "CP-V cpcp system is ready!"
        echo "=========================================="
        echo "Connection: nc localhost 5001"
        echo "Login: :SYS,LBE (no password)"
        echo ""
        echo "Screen session: screen -r cpv"
        echo "Shutdown: screen -S cpv -X quit"
        echo "Or: pkill sigma"
        echo "=========================================="
        echo ""
        exit 0
    fi
    sleep 1
done

echo "ERROR: Port 5001 not listening after 60 seconds"
exit 1
