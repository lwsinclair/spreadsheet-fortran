#!/bin/bash
#
# Boot CP-V in a detached screen session
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Starting CP-V in detached screen session 'cpv'..."

# Start screen session running the boot script
screen -dmS cpv "$SCRIPT_DIR/boot_for_screen.exp"

# Wait a moment for it to start
sleep 2

echo "Waiting for CP-V to boot (this takes about 30-40 seconds)..."

# Wait for port 5001 to be listening
for i in {1..60}; do
    if lsof -i :5001 >/dev/null 2>&1; then
        echo "✓ Port 5001 is listening"
        sleep 5  # Extra time for system to stabilize
        echo ""
        echo "=========================================="
        echo "CP-V is ready!"
        echo "=========================================="
        echo "Connection: nc localhost 5001"
        echo "Login: :SYS,LBE"
        echo ""
        echo "Screen session: screen -r cpv"
        echo "Shutdown: screen -S cpv -X quit"
        echo "Or: pkill sigma"
        echo "==========================================\n"
        exit 0
    fi
    sleep 1
    [ $((i % 10)) -eq 0 ] && echo -n "."
done

echo ""
echo "ERROR: CP-V did not start within 60 seconds"
echo "Check screen session: screen -r cpv"
exit 1
