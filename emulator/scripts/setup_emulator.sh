#!/bin/bash
#
# Setup Xerox Sigma CP-V Emulator
#
# This script installs and configures the simh Sigma emulator
# for running XL spreadsheet on CP-V F00 (1978).
#

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMULATOR_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$EMULATOR_DIR")"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  XL Spreadsheet - CP-V Emulator Setup"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo

# Step 1: Install simh
echo "Step 1: Installing simh Sigma emulator..."
echo

if command -v sigma &> /dev/null; then
    echo "✓ simh already installed"
else
    echo "Installing open-simh via Homebrew..."
    brew install open-simh
    echo "✓ simh installed"
fi

# Step 2: Download CP-V kit
echo
echo "Step 2: Downloading CP-V kit..."
echo

CPV_KIT_DIR="$EMULATOR_DIR/sigma-cpv-kit"

if [ -d "$CPV_KIT_DIR" ]; then
    echo "✓ CP-V kit already downloaded"
else
    echo "Cloning sigma-cpv-kit repository..."
    cd "$EMULATOR_DIR"
    git clone https://github.com/kenrector/sigma-cpv-kit.git
    echo "✓ CP-V kit downloaded"
fi

# Step 3: Create working directory
echo
echo "Step 3: Setting up working directory..."
echo

WORK_DIR="$EMULATOR_DIR/work"
mkdir -p "$WORK_DIR"

echo "✓ Working directory created: $WORK_DIR"

# Step 4: Create boot script
echo
echo "Step 4: Creating boot script..."
echo

cat > "$EMULATOR_DIR/boot_cpv.ini" << 'EOF'
; Xerox Sigma 9 CP-V F00 Boot Configuration
; For XL Spreadsheet development

set cpu sigma9
set cpu 512k

; Disk configuration
attach dp0 sigma-cpv-kit/cpsixf00.dsk

; Tape for file transfer
attach mt0 work/transfer.tap

; Console terminal
set tt0 8b
attach tt0 5000

; Boot CP-V
boot dp0
EOF

echo "✓ Boot script created: boot_cpv.ini"

# Step 5: Create transfer tape helper
echo
echo "Step 5: Creating file transfer helper..."
echo

cat > "$EMULATOR_DIR/scripts/make_tape.sh" << 'EOF'
#!/bin/bash
# Create tape image for file transfer to CP-V

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <file1.FOR> [file2.FOR ...]"
    exit 1
fi

WORK_DIR="$(dirname "$0")/../work"
TAPE_IMAGE="$WORK_DIR/transfer.tap"

echo "Creating tape image: $TAPE_IMAGE"

# simh tap tool
echo "Files:" "$@"

# Simple text file concatenation (CP-V can read ASCII tape)
: > "$TAPE_IMAGE"

for file in "$@"; do
    echo "Adding: $file"
    cat "$file" >> "$TAPE_IMAGE"
done

echo "✓ Tape created: $TAPE_IMAGE"
EOF

chmod +x "$EMULATOR_DIR/scripts/make_tape.sh"
echo "✓ Transfer helper created"

# Step 6: Create quick start guide
echo
echo "Step 6: Creating quick start guide..."
echo

cat > "$EMULATOR_DIR/QUICKSTART.md" << 'EOF'
# CP-V Emulator Quick Start

## Starting CP-V

```bash
cd emulator
simh-sigma boot_cpv.ini
```

This will:
1. Start Sigma 9 emulator (512KB RAM)
2. Boot CP-V F00 from disk
3. Start console on port 5000

## Connecting to Console

In another terminal:
```bash
telnet localhost 5000
```

## Logging In

```
Username: SYSTEM
Password: SYSTEM
```

## Compiling FORTRAN on CP-V

```
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*GO
```

## Transferring Files to CP-V

### 1. Create tape image
```bash
./scripts/make_tape.sh ../src/layer0/STRUTIL.FOR
```

### 2. In CP-V console
```
$ COPY MT0: STRUTIL.FOR
```

## Running XL Spreadsheet

```
$ RUN XL
```

## Exiting

```
$ LOGOUT
```

In simh:
```
sim> quit
```

## Troubleshooting

### Emulator won't start
- Check CP-V kit is downloaded: `emulator/sigma-cpv-kit/`
- Verify disk image exists: `sigma-cpv-kit/cpsixf00.dsk`

### Can't connect to console
- Check telnet port 5000 is available
- Verify firewall allows localhost connections

### FORTRAN compiler not found
- CP-V F00 includes FORTRAN IV compiler
- Run `$ CATALOG` to list available programs

## References

- CP-V Documentation: https://www.andrews.edu/~calkins/sigma/
- simh Guide: http://simh.trailing-edge.com/
- sigma-cpv-kit: https://github.com/kenrector/sigma-cpv-kit
EOF

echo "✓ Quick start guide created: QUICKSTART.md"

# Summary
echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  Setup Complete!"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "CP-V emulator is ready to use."
echo
echo "To start CP-V:"
echo "  cd $EMULATOR_DIR"
echo "  simh-sigma boot_cpv.ini"
echo
echo "Then connect in another terminal:"
echo "  telnet localhost 5000"
echo
echo "See QUICKSTART.md for detailed instructions."
echo
