# XL Spreadsheet - RSX-11M Installation Guide

**Version:** 1.0
**Date:** 2026-01-19
**Platform:** PDP-11/RSX-11M
**Author:** Claude Code

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Quick Start](#quick-start)
3. [Manual Installation](#manual-installation)
4. [Verification](#verification)
5. [Troubleshooting](#troubleshooting)
6. [System Requirements](#system-requirements)
7. [File Manifest](#file-manifest)

---

## Prerequisites

### Hardware Requirements

- **PDP-11/70** (or compatible model with FPP)
- **256KB minimum memory** (512KB recommended)
- **VT-100 or VT-52 compatible terminal**
- **DZ11 terminal controller** (standard on most RSX-11M systems)

### Software Requirements

- **RSX-11M v3.2 or later**
- **PDP-11 FORTRAN IV compiler (F4 v2.1+)**
- **TKB (Task Builder)** - included with RSX-11M
- **Sufficient disk space** (~500KB for source + objects + executable)

### Terminal Setup

Your terminal must support VT-52 escape sequences. VT-100 terminals automatically support VT-52 compatibility mode.

**Telnet users:** Connect to RSX-11M on port 10001 with VT-100 emulation enabled.

---

## Quick Start

### Automated Installation

The fastest way to install XL Spreadsheet is using the automated installation script:

```
$ SET DEFAULT [USER.XL.PDP11]
$ @INSTALL
```

This will:
1. Verify prerequisites
2. Compile all source modules
3. Link the executable
4. Build and link the terminal test program
5. Display success message

**Expected output:**
```
XL Spreadsheet - RSX-11M Installation
======================================

Step 1: Verifying prerequisites...
  [OK] FORTRAN IV compiler found
  [OK] Task Builder (TKB) found
  [OK] Source files found

Step 2: Compiling source modules...
Building Layer 0: Utilities...
Building Layer 1: Calculation Engine...
Building Layer 2: Application Logic...
Building Layer 3: Terminal Driver...
Building Main Program...
Build complete!

Step 3: Linking executable...
Link complete!

Step 4: Testing terminal driver...
Terminal test ready.

=========================================
Installation Complete!
=========================================

Executables created:
  XL.TSK      - Main spreadsheet program
  TESTTERM.TSK - Terminal driver test

To run XL Spreadsheet:
  $ RUN XL
```

---

## Manual Installation

### Step 1: Verify Environment

Check that FORTRAN IV is installed:
```
$ SHOW SYSTEM/TASKS
```
Look for `FOR` in the task list.

Check for TKB:
```
$ SHOW SYSTEM/TASKS
```
Look for `TKB` in the task list.

### Step 2: Compile Source Files

Navigate to the pdp11 directory and run the build script:
```
$ SET DEFAULT [USER.XL.PDP11]
$ @BUILD
```

This compiles all 11 FORTRAN modules:
- Layer 0: STRUTIL.FOR
- Layer 1: CELLS.FOR, DEPS.FOR, PARSE.FOR, EVAL.FOR, RECALC.FOR
- Layer 2: UI.FOR, DISPLAY.FOR, MSG.FOR
- Layer 3: TERMRSX.FOR
- Main: XLMAIN.FOR

**Expected output:**
```
XL Spreadsheet - RSX-11M Build
==============================

Building Layer 0: Utilities...
Building Layer 1: Calculation Engine...
Building Layer 2: Application Logic...
Building Layer 3: Terminal Driver...
Building Main Program...

Build complete!
Object files ready for linking.

Next step: @XLBUILD to link executable
```

If any compilation fails, see [Troubleshooting](#troubleshooting).

### Step 3: Link Executable

After successful compilation, link the executable:
```
$ @XLBUILD
```

This uses TKB to link all object files with the FORTRAN runtime library.

**Expected output:**
```
XL Spreadsheet - RSX-11M Linker
================================

Linking XL.TSK...
Link complete!
Executable: XL.TSK

To run: RUN XL
```

### Step 4: Test Terminal Driver

Before running XL, verify the terminal driver works:
```
$ FOR TESTTERM=TESTTERM.FOR
$ TKB @TESTTERM.CMD
$ RUN TESTTERM
```

The test program will:
- Clear the screen
- Display test patterns
- Prompt for arrow key input
- Exit on ESC

If tests fail, see [Troubleshooting](#troubleshooting-terminal-issues).

---

## Verification

### Run XL Spreadsheet

Start the spreadsheet:
```
$ RUN XL
```

You should see:
```
┌──────────────────────────────────────────────────────────────────────┐
│                                                                      │
│                    XL Spreadsheet - RSX-11M Port                     │
│                                                                      │
│                          Version 1.0                                 │
│                                                                      │
│                     Press any key to begin...                        │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

### Basic Functionality Test

1. Press any key to start
2. The grid should display with column headers (A-Z) and row numbers (1-254)
3. Use arrow keys to move cursor
4. Enter a value: `10` then RETURN
5. Move to another cell and enter: `=A1*2` then RETURN
6. Verify formula evaluates correctly
7. Type `/QUIT` to exit

If this works, installation is successful!

---

## Troubleshooting

### Compilation Errors

#### "FOR.TSK not found"
**Problem:** FORTRAN IV compiler not installed
**Solution:** Install F4 from RSX-11M distribution tapes

#### "PARAMETER not supported"
**Problem:** Older FORTRAN IV version
**Solution:** Upgrade to F4 v2.1 or later, or modify source to use fixed dimensions

#### "Syntax error in FORMAT"
**Problem:** `$` format not supported
**Solution:** Replace `$` with space in FORMAT statements (disables non-advancing I/O)

### Linking Errors

#### "F77FCS not found"
**Problem:** FORTRAN runtime library missing
**Solution:**
```
$ COPY [1,1]F77FCS.OLB [USER.XL]
$ EDIT XLBUILD.CMD
  (change /LB:[1,1]F77FCS to /LB:F77FCS)
```

#### "Insufficient memory"
**Problem:** Not enough memory for linking
**Solution:** Reduce memory configuration or use FULL config instead of CPM

### Terminal Issues

#### Screen doesn't clear
**Problem:** Terminal not in VT-52 mode
**Solution:**
- VT-100: Press SET-UP, select "VT52 Mode"
- Telnet: Enable VT-52 or VT-100 emulation

#### Garbage characters appear
**Problem:** Escape sequences not interpreted
**Solution:** Check terminal settings, ensure 8-bit clean connection

#### Arrow keys don't work
**Problem:** Terminal sending wrong codes
**Solution:** Check terminal key mode (application vs. cursor mode)

### Runtime Errors

#### "Task size too large"
**Problem:** Insufficient task memory
**Solution:** Increase task size limit or reduce MAXCEL parameter

#### "File not found: TI:"
**Problem:** Terminal input not available
**Solution:** Verify terminal controller (DZ11) is online

---

## System Requirements

### Minimum Configuration

- **CPU:** PDP-11/70 or equivalent
- **Memory:** 256KB
- **Disk:** 500KB free space
- **OS:** RSX-11M v3.2
- **Compiler:** F4 v2.0
- **Terminal:** VT-52 compatible

### Recommended Configuration

- **CPU:** PDP-11/70 with FPP
- **Memory:** 512KB or more
- **Disk:** 1MB free space
- **OS:** RSX-11M v4.0+
- **Compiler:** F4 v2.1+
- **Terminal:** VT-100 with VT-52 mode

### Memory Configuration

XL Spreadsheet supports three memory configurations:

1. **MINIMAL** (64KB): 100 cells, basic features
2. **CPM** (128KB): 500 cells, suitable for CP/M-86
3. **FULL** (256KB+): 2000 cells, recommended for RSX-11M

The default configuration is **FULL** for RSX-11M.

To change configuration, edit `src/layer1/CELLS.FOR` and rebuild.

---

## File Manifest

### Source Files (pdp11/src/)

**Layer 0: Utilities**
- `layer0/STRUTIL.FOR` - String operations (484 lines)

**Layer 1: Calculation Engine**
- `layer1/CELLS.FOR` - Cell storage (466 lines)
- `layer1/DEPS.FOR` - Dependency tracking (287 lines)
- `layer1/PARSE.FOR` - Formula parser (318 lines)
- `layer1/EVAL.FOR` - Expression evaluator (143 lines)
- `layer1/RECALC.FOR` - Recalculation engine (130 lines)

**Layer 2: Application Logic**
- `layer2/UI.FOR` - User interface (244 lines)
- `layer2/DISPLAY.FOR` - Screen rendering (473 lines)
- `layer2/MSG.FOR` - Message strings (298 lines)

**Layer 3: Platform-Specific**
- `layer3/TERMRSX.FOR` - RSX-11M terminal driver (505 lines)

**Main Program**
- `XLMAIN.FOR` - Event loop and main program (340 lines)

**Total:** 11 files, ~3,883 lines of FORTRAN IV

### Build Scripts

- `BUILD.CMD` - Compile all modules
- `XLBUILD.CMD` - Link executable
- `INSTALL.CMD` - Automated installation

### Test Programs

- `TESTTERM.FOR` - Terminal driver test

### Documentation

- `INSTALL.md` - This file
- `README.md` - User manual
- `COMPARISON.md` - CP-V vs RSX-11M comparison
- `BUILD_NOTES.md` - Technical build notes

### Output Files (after build)

- `*.OBJ` - 11 object files
- `XL.TSK` - Main executable (~100KB)
- `TESTTERM.TSK` - Test executable (~20KB)

---

## Optional: System-Wide Installation

To make XL available to all users:

```
$ SET DEFAULT [USER.XL.PDP11]
$ COPY XL.TSK [1,54]XL.TSK
$ SET PROTECTION [1,54]XL.TSK/WORLD:RE
```

Users can then run:
```
$ RUN [1,54]XL
```

Or create a global command:
```
$ MCR INS [1,54]XL/GBLDEF=XL
```

Then users can simply type:
```
$ XL
```

---

## Support

For issues, see:
- `BUILD_NOTES.md` - Technical implementation details
- `COMPARISON.md` - Differences from CP-V version
- `README.md` - User manual and feature guide

For bugs or questions, contact the maintainer or file an issue on GitHub.

---

## License

See main repository LICENSE file.

---

## Acknowledgments

This port demonstrates the portability of well-designed FORTRAN IV code across different 1970s-era platforms. Special thanks to the RSX-11M and PDP-11 preservation communities.
