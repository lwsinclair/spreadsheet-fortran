#!/bin/bash
# Boot RSX-11M on SIMH PDP-11 emulator
#
# This script starts the PDP-11 emulator with RSX-11M V3.2

WORKDIR="/Volumes/SECURE8/git/spreadsheet-fortran/emulator/pdp11-rsx"
cd "$WORKDIR"

echo "================================================================"
echo "Starting PDP-11/RSX-11M Emulator"
echo "================================================================"
echo ""
echo "Console will be available on: telnet localhost 10070"
echo "DZ11 terminals available on:  telnet localhost 10071-10078"
echo ""
echo "To exit emulator: Press Ctrl-E, then type 'quit'"
echo ""
echo "================================================================"
echo ""

# Start SIMH
pdp11 pdp11.ini
