#!/bin/bash
#
# Boot CP-V in background and wait for it to be ready
#

echo "Starting CP-V emulator in background..."

# Start sigma in background
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
sigma boot_cpv.ini > /tmp/sigma_boot.log 2>&1 &
SIGMA_PID=$!

echo "Sigma started (PID: $SIGMA_PID)"
echo "Waiting for CP-V to boot..."

# Wait for port 5001 to be listening (up to 60 seconds)
for i in {1..60}; do
    if lsof -i :5001 >/dev/null 2>&1; then
        echo "✓ Port 5001 is listening"
        sleep 5  # Give it a bit more time to fully initialize
        echo "✓ CP-V is ready!"
        echo ""
        echo "Connection: nc localhost 5001"
        echo "Login: :SYS,LBE"
        echo ""
        echo "To shutdown: pkill sigma"
        exit 0
    fi
    sleep 1
    echo -n "."
done

echo ""
echo "ERROR: Port 5001 never started listening"
echo "Check /tmp/sigma_boot.log for errors"
exit 1
