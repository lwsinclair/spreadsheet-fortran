#!/bin/bash
# Setup script for PDP-11/RSX-11M emulator for XL Spreadsheet testing
#
# Downloads RSX-11M V3.2 disk images from bitsavers.org
# Sets up SIMH PDP-11 emulator environment

set -e

WORKDIR="/Volumes/SECURE8/git/spreadsheet-fortran/emulator/pdp11-rsx"
cd "$WORKDIR"

echo "================================================================"
echo "PDP-11/RSX-11M Emulator Setup for XL Spreadsheet"
echo "================================================================"
echo ""

# Create media directory
mkdir -p media
cd media

BASE_URL="http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2"

echo "Downloading RSX-11M V3.2 disk images from bitsavers.org..."
echo ""

# Essential files for XL Spreadsheet testing
FILES=(
    "RSX-11M_V3.2_RSX11MBL26_3.2.DSK.gz"  # Base system (required)
    "F4_IAS_RSX_2.1.DSK.gz"                # FORTRAN IV compiler (required)
    "RSX-11M_V3.2_RMSKIT.DSK.gz"           # RMS utilities (optional)
)

for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  [SKIP] $file already downloaded"
    else
        echo "  [DOWNLOAD] $file"
        curl -L -o "$file" "$BASE_URL/$file"
    fi
done

echo ""
echo "Decompressing disk images..."
echo ""

for file in *.DSK.gz; do
    if [ -f "$file" ]; then
        outfile="${file%.DSK.gz}.rl01"
        if [ -f "$outfile" ]; then
            echo "  [SKIP] $outfile already exists"
        else
            echo "  [DECOMPRESS] $file -> $outfile"
            gunzip -c "$file" > "$outfile"
        fi
    fi
done

echo ""
echo "Creating work disk for XL Spreadsheet..."
echo ""

# Create a blank RL02 disk (10MB) for user work
if [ ! -f "xl_work.rl02" ]; then
    # SIMH will create and initialize this when attached
    touch xl_work.rl02
    echo "  [CREATE] xl_work.rl02 (will be initialized by SIMH)"
else
    echo "  [SKIP] xl_work.rl02 already exists"
fi

cd ..

echo ""
echo "================================================================"
echo "Setup complete!"
echo "================================================================"
echo ""
echo "Next steps:"
echo "  1. Run: ./boot_rsx.sh"
echo "  2. Connect to console: telnet localhost 10070"
echo "  3. Login to RSX-11M"
echo "  4. Transfer XL source files"
echo "  5. Build and test XL"
echo ""
