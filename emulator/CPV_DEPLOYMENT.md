# Deploying XL Spreadsheet to CP-V

This guide explains how to build and run the XL Spreadsheet on a Xerox Sigma 7 running CP-V F00 with a VT-52 terminal.

## Quick Start

```bash
# 1. Start the CP-V emulator
cd emulator
sigma boot_cpv.ini

# 2. In another terminal, connect via telnet
telnet localhost 5000

# 3. Log in
LOGON PLEASE: :SYS,LBE

# 4. Transfer files (from host system in emulator/ directory)
./scripts/deploy_xl.sh

# 5. In CP-V, compile and link
$ RUN FORTRAN
*SOURCE=XLMAIN.FOR
*OBJECT=XLMAIN.OBJ
*LINK
*GO
```

## Detailed Instructions

### Step 1: Prepare Source Files

The following files need to be transferred to CP-V:

**Layer 0 (String Utilities):**
- `STRUTIL.FOR`

**Layer 1 (Calculation Engine):**
- `CELLS.FOR`
- `DEPS.FOR`
- `PARSE.FOR`
- `EVAL.FOR`
- `RECALC.FOR`

**Layer 2 (Application):**
- `UI.FOR`
- `DISPLAY.FOR`
- `MSG.FOR`
- `COMMANDS.FOR` (future)
- `FILES.FOR` (future)

**Layer 3 (Terminal I/O for CP-V/VT-52):**
- `TERMCPV.FOR`

**Main Program:**
- `XLMAIN.FOR`

### Step 2: Transfer to CP-V

#### Option A: Using Tape Image (Recommended)

From the `emulator/` directory:

```bash
# Create tape with all source files
./scripts/make_xl_tape.sh

# This creates work/xl_transfer.tap
```

In CP-V console:

```
$ COPY MT0: STRUTIL.FOR
$ COPY MT0: CELLS.FOR
$ COPY MT0: DEPS.FOR
$ COPY MT0: PARSE.FOR
$ COPY MT0: EVAL.FOR
$ COPY MT0: RECALC.FOR
$ COPY MT0: UI.FOR
$ COPY MT0: DISPLAY.FOR
$ COPY MT0: MSG.FOR
$ COPY MT0: TERMCPV.FOR
$ COPY MT0: XLMAIN.FOR
```

#### Option B: Manual File Creation

Use the CP-V text editor to create each file:

```
$ EDIT STRUTIL.FOR
[Copy and paste contents]
:FILE
:QUIT

$ EDIT CELLS.FOR
[Copy and paste contents]
:FILE
:QUIT

... (repeat for each file)
```

### Step 3: Compile on CP-V

CP-V F00 includes the FORTRAN IV compiler. Compile each module:

```fortran
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=CELLS.FOR
*OBJECT=CELLS.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=DEPS.FOR
*OBJECT=DEPS.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=PARSE.FOR
*OBJECT=PARSE.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=EVAL.FOR
*OBJECT=EVAL.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=RECALC.FOR
*OBJECT=RECALC.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=UI.FOR
*OBJECT=UI.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=DISPLAY.FOR
*OBJECT=DISPLAY.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=MSG.FOR
*OBJECT=MSG.OBJ
*LINK
$ RUN FORTRAN
*SOURCE=TERMCPV.FOR
*OBJECT=TERMCPV.OBJ
*LINK
```

### Step 4: Link Main Program

Link all object files together:

```fortran
$ RUN FORTRAN
*SOURCE=XLMAIN.FOR
*OBJECT=XLMAIN.OBJ
*LIBRARY=STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMCPV
*LINK
*MAP
*GO
```

**Note:** CP-V FORTRAN linker syntax may vary. Consult CP-V FORTRAN manual for exact LIBRARY syntax.

### Step 5: Create Executable

After successful link, create executable:

```
$ SAVE XL
```

This creates the `XL` executable file.

### Step 6: Run XL Spreadsheet

```
$ RUN XL
```

You should see:

```
A1  NAV
    A         B         C         D         E         F         G         H
────────────────────────────────────────────────────────────────────────────
 1
 2
 3
 ...
```

## Using XL Spreadsheet

### Navigation

- **Arrow keys**: Move cursor (Up/Down/Left/Right)
- **Type number**: Enter value (e.g., `100` then RETURN)
- **Type formula**: Enter formula (e.g., `=A1+B1` then RETURN)
- **ESC**: Cancel entry

### Commands

- **`/QUIT`**: Exit spreadsheet

### Example Session

```
[Navigate to A1]
100 [RETURN]

[Navigate to A2]
200 [RETURN]

[Navigate to A3]
=A1+A2 [RETURN]

[A3 shows: 300.00]

/QUIT [RETURN]
[Exit to CP-V]
```

## Terminal Requirements

XL requires a VT-52 compatible terminal or emulator. The CP-V emulator's MUX controller (port 5000) emulates a VT-52 terminal when connected via telnet.

### VT-52 Features Used

- `ESC H` - Home cursor
- `ESC J` - Clear to end of screen
- `ESC K` - Clear to end of line
- `ESC Y row col` - Direct cursor addressing
- `ESC A/B/C/D` - Cursor movement (arrow keys)

Most modern terminal emulators support VT-52 mode or are backward-compatible.

## Troubleshooting

### Compile Errors

**"PARAMETER not recognized":**
- CP-V FORTRAN IV may not support PARAMETER
- Edit source files to replace PARAMETER with hardcoded values
- Example: Change `PARAMETER (MAXCEL=2000)` to just use `2000` in arrays

**"Syntax error in DATA statement":**
- Check that DATA statements use correct Hollerith syntax
- CP-V accepts both `/data/` and `'data'` styles

**"Block IF not supported":**
- This shouldn't occur - all code uses arithmetic IF
- If you see this, report as bug

### Link Errors

**"Undefined reference to CELINI":**
- Make sure all object files are included in LIBRARY statement
- Check spelling of library names

**"Duplicate symbol":**
- Don't compile/link the same module twice
- Clear old object files: `$ DELETE *.OBJ`

### Runtime Errors

**"Array bounds exceeded":**
- You may be using Compact or Minimal configuration
- Check PARAMETER values in source files
- Full config uses MAXCEL=2000, Compact uses MAXCEL=300

**"Format error in WRITE":**
- CP-V FORTRAN I/O may differ slightly
- Check TERMCPV.FOR format statements
- The `$` non-advancing I/O may need adjustment

**Terminal shows garbage:**
- Your terminal may not be VT-52 compatible
- Try: `SET TERMINAL/VT52` (if available)
- Or use a different terminal emulator

### Screen Not Clearing

If screen doesn't clear properly:

1. Check terminal type is VT-52
2. Verify TERMCPV.FOR escape sequences are correct
3. CP-V F00 should support VT-52 natively

## Performance Notes

### Memory Usage

**Full Configuration:**
- Data: ~91 KB
- Code: ~20 KB
- Total: ~111 KB
- **Fits in CP-V (512 KB available)**

### Speed

On a Sigma 7 (1978 hardware):
- Screen refresh: < 1 second
- Cell calculation: Instantaneous
- Large formula (100+ cells): < 1 second

### Capacity

**Full Configuration:**
- 2000 cells maximum
- 10000 character formula pool
- Supports spreadsheets up to ~50 rows × 40 columns

## CP-V Specific Notes

### File System

CP-V uses a hierarchical file system:
- Files are stored in your account directory
- Use `CATALOG` to list files
- Use `DELETE filename` to remove files

### Terminal Modes

CP-V supports multiple terminal types. Ensure yours is set to VT-52:

```
$ SET TERMINAL/VT52
```

### Batch Mode

To compile in batch mode, create a batch job file:

```
$ EDIT XLBUILD.JOB
[Enter compile commands]
:FILE
:QUIT

$ SUBMIT XLBUILD.JOB
```

## Advanced: Source Code Modifications

### Changing Configuration

To build for limited memory (e.g., testing):

1. Edit all source files
2. Find `PARAMETER` statements
3. Change values:
   - `MAXCEL=2000` → `MAXCEL=300` (CP/M config)
   - `HASHSZ=1024` → `HASHSZ=256`
   - `MAXSTR=10000` → `MAXSTR=2000`
4. Recompile all affected modules

### Adding Features

CP-V's FORTRAN IV compiler supports:
- All standard FORTRAN IV features
- Some FORTRAN 66 extensions
- No FORTRAN 77 features

Stay within FORTRAN IV for maximum compatibility.

## Historical Note

This spreadsheet is designed to run as it would have in 1978 on a university computer system:

- VT-52 CRT terminal connected to Sigma 7 mainframe
- Time-share access via 300-1200 baud modem
- Character-cell display (no graphics)
- Real-time calculation and display

The experience should be authentic to the era!

## References

- **CP-V Documentation**: https://www.andrews.edu/~calkins/sigma/
- **CP-V FORTRAN Manual**: http://bitsavers.org/pdf/sds/sigma/cp-v/
- **VT-52 Reference**: DEC VT52 User's Manual
- **simh Sigma Emulator**: http://simh.trailing-edge.com/

## Support

For issues specific to:
- **XL Spreadsheet code**: Check source comments
- **CP-V system**: See CP-V documentation
- **simh emulator**: See emulator/QUICKSTART.md

---

**Ready to experience 1978 computing!** 🖥️
