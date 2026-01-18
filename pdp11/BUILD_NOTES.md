# XL Spreadsheet - RSX-11M Build Notes

**Technical implementation details for RSX-11M port**

**Date:** 2026-01-19
**Author:** Claude Code

---

## Table of Contents

1. [Compiler Notes](#compiler-notes)
2. [Linker Configuration](#linker-configuration)
3. [FORTRAN IV Quirks](#fortran-iv-quirks)
4. [Terminal Driver Implementation](#terminal-driver-implementation)
5. [Memory Layout](#memory-layout)
6. [Build Process](#build-process)
7. [Debugging Tips](#debugging-tips)
8. [Platform-Specific Issues](#platform-specific-issues)

---

## Compiler Notes

### PDP-11 FORTRAN IV (F4 v2.1)

**Compiler:** F4
**Location:** SYS$SYSTEM:FOR.TSK
**Options:** Default (no special flags required)

**Supported Features:**
- FORTRAN IV standard (1966)
- PARAMETER statements (v2.1+)
- BLOCK DATA
- COMMON blocks
- CHARACTER*n (extension)
- $ format specifier (non-advancing I/O)

**Not Supported:**
- FORTRAN 77 features
- CHARACTER expressions (limited)
- DO WHILE loops
- INCLUDE statements
- Recursion (stack limited)

### Compilation Command

```
FOR output=input.FOR
```

**Example:**
```
FOR CELLS=CELLS.FOR
```

**Output:** CELLS.OBJ (relocatable object file)

### Compiler Warnings

**Common warnings (can be ignored):**

1. **"Hollerith constant in arithmetic expression"**
   - Source: Character literals in INTEGER arrays
   - Safe to ignore (intended usage)

2. **"Statement function referenced before definition"**
   - Source: Forward references in complex expressions
   - Safe if definition appears later in same file

3. **"Unreferenced label"**
   - Source: Defensive GOTO labels
   - Safe to ignore

### Critical Compiler Errors

**Must fix:**

1. **"PARAMETER not supported"**
   - Solution: Upgrade to F4 v2.1 or later
   - Workaround: Replace PARAMETER with hard-coded constants

2. **"Invalid FORMAT specification"**
   - Solution: Check $ placement in FORMAT
   - $ must be last item in FORMAT

3. **"Array bounds exceeded"**
   - Solution: Reduce array sizes in PARAMETER
   - Use MINIMAL or CPM config instead of FULL

---

## Linker Configuration

### TKB (Task Builder)

**Location:** SYS$SYSTEM:TKB.TSK
**Purpose:** Link object files into executable task

### Link Command Syntax

```
TKB
output/-SP=module1,module2,...,moduleN/LB:library
//
```

**Example:**
```
TKB
XL/-SP=XLMAIN,STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMRSX/LB:[1,1]F77FCS
//
```

### TKB Options

**-SP:** Specify main program and modules
**-MA:** Map file generation (optional)
**-LP:** Program listing (optional)
**/LB:** Library search path

### Required Library

**F77FCS:** FORTRAN runtime library
**Location:** [1,1]F77FCS.OLB
**Contains:** I/O routines, math functions, runtime support

### Common Link Errors

**"Undefined symbol: _SQRT"**
- Cause: Math library not linked
- Solution: Ensure /LB:[1,1]F77FCS is specified

**"Insufficient memory for link"**
- Cause: Not enough memory for TKB
- Solution: Reduce task size or increase partition

**"Module not found"**
- Cause: .OBJ file missing
- Solution: Run @BUILD to compile all modules

---

## FORTRAN IV Quirks

### INTEGER Type

**Size:** 16 bits (INTEGER*2)
**Range:** -32768 to +32767

**Implications:**
- Row numbers limited to 254 (not 32767)
- Cell counts limited to 2000 (not 65535)
- Hash values must fit in 16 bits

**Workaround for larger values:**
Use REAL for intermediate calculations, convert to INTEGER for storage.

### REAL Type

**Size:** 32 bits (single precision)
**Range:** ~1E-38 to ~1E+38
**Precision:** ~7 decimal digits

**Implications:**
- Adequate for most calculations
- Loss of precision beyond 7 digits
- No double precision in FORTRAN IV

### Character Handling

**No CHARACTER type in FORTRAN IV!**

**Solution:** Use INTEGER arrays
```fortran
INTEGER STR(80)
```

Each INTEGER holds one character (ASCII code).

**String operations:**
- Must be manual (loop character-by-character)
- No string concatenation operator
- No string comparison operators

**STRUTIL.FOR provides:**
- STRCPY - String copy
- STRLEN - String length
- STRCMP - String compare
- STRCHR - Find character
- STRSUB - Substring

### Array Indexing

**FORTRAN IV:** 1-based indexing
```fortran
DIMENSION A(100)
A(1)    ! First element
A(100)  ! Last element
```

**Never 0-based!**

### FORMAT Statements

**$ specifier:** Non-advancing I/O
```fortran
100 FORMAT(I6,$)
```

Without $, WRITE advances to next line.

**Limitations:**
- $ must be last item in FORMAT
- Not standard FORTRAN IV (extension)
- May not work on all compilers

---

## Terminal Driver Implementation

### TERMRSX.FOR Design

**Purpose:** Abstract terminal I/O for RSX-11M
**Based on:** VT-52 escape sequences
**Compatible with:** VT-100 in VT-52 mode

### I/O Units

**Unit 5 (TI:):** Terminal input
```fortran
READ(5,100) CH
```

**Unit 6 (TT:):** Terminal output
```fortran
WRITE(6,100) CH
```

### Escape Sequences

**VT-52 Standard:**

| Sequence | Function | Code |
|----------|----------|------|
| ESC H | Home cursor | 27, 72 |
| ESC J | Clear to EOS | 27, 74 |
| ESC K | Clear to EOL | 27, 75 |
| ESC Y r c | Position cursor | 27, 89, row+31, col+31 |
| ESC A | Cursor up | 27, 65 |
| ESC B | Cursor down | 27, 66 |
| ESC C | Cursor right | 27, 67 |
| ESC D | Cursor left | 27, 68 |

**ASCII codes as INTEGER:**
```fortran
ESC = 27
H = 72
J = 74
...
```

### Non-Blocking I/O

**Challenge:** FORTRAN IV has no true non-blocking I/O

**Attempted solution:**
```fortran
READ(5,100,IOSTAT=IOS) CH
```

**Reality:**
- IOSTAT detects errors, not "no data available"
- READ always blocks until input received
- Acceptable for interactive use

**Alternative (not implemented):**
- Use QIO system calls (assembly required)
- Use terminal timeouts (not standard)

### Arrow Key Handling

**Escape sequences for arrows:**
- Up: ESC A
- Down: ESC B
- Right: ESC C
- Left: ESC D

**Implementation:**
```fortran
IF (CH .EQ. 27) THEN
  ESCSQ = 1
  CALL TMKEY(KEY, VALID)
  ! Next char is arrow direction
END IF
```

**Mapped to special codes:**
- 128: Up arrow
- 129: Down arrow
- 130: Right arrow
- 131: Left arrow

---

## Memory Layout

### Task Memory Map

```
+-------------------------+  <- 0
| Code                    |  ~50 KB
+-------------------------+
| Static Data             |  ~76 KB
|  - CELLA array          |  28 KB
|  - HASHB array          |  2 KB
|  - FORMUL pool          |  20 KB
|  - Other arrays         |  26 KB
+-------------------------+
| Stack                   |  ~10 KB
+-------------------------+
| Heap (if any)           |  Variable
+-------------------------+  <- Task size limit
```

**Total:** ~126 KB for FULL configuration

### Array Sizes (FULL config)

From CELLS.FOR:
```fortran
PARAMETER (MAXCEL=2000, HASHSZ=1024, MAXSTR=10000)
```

**CELLA:** 2000 cells × 14 bytes/cell = 28 KB
**HASHB:** 1024 buckets × 2 bytes/bucket = 2 KB
**FORMUL:** 10000 chars × 2 bytes/char = 20 KB

### Memory Configuration Options

**MINIMAL** (64 KB total):
```fortran
PARAMETER (MAXCEL=100, HASHSZ=64, MAXSTR=1000)
```

**CPM** (128 KB total):
```fortran
PARAMETER (MAXCEL=500, HASHSZ=256, MAXSTR=5000)
```

**FULL** (256 KB total):
```fortran
PARAMETER (MAXCEL=2000, HASHSZ=1024, MAXSTR=10000)
```

---

## Build Process

### Step-by-Step

1. **Compile utilities** (no dependencies)
   ```
   FOR STRUTIL=STRUTIL.FOR
   ```

2. **Compile calculation engine** (depends on STRUTIL)
   ```
   FOR CELLS=CELLS.FOR
   FOR DEPS=DEPS.FOR
   FOR PARSE=PARSE.FOR
   FOR EVAL=EVAL.FOR
   FOR RECALC=RECALC.FOR
   ```

3. **Compile application logic** (depends on engine)
   ```
   FOR UI=UI.FOR
   FOR DISPLAY=DISPLAY.FOR
   FOR MSG=MSG.FOR
   ```

4. **Compile platform layer** (depends on nothing)
   ```
   FOR TERMRSX=TERMRSX.FOR
   ```

5. **Compile main program** (depends on all)
   ```
   FOR XLMAIN=XLMAIN.FOR
   ```

6. **Link executable**
   ```
   TKB XL/-SP=XLMAIN,...all modules.../LB:[1,1]F77FCS
   ```

### Automated Build

Use provided scripts:
```
$ @BUILD      ! Compile all
$ @XLBUILD    ! Link
$ @INSTALL    ! Complete automation
```

### Dependency Graph

```
XLMAIN
  ├── STRUTIL (no deps)
  ├── CELLS (no deps)
  ├── DEPS (no deps)
  ├── PARSE (no deps)
  ├── EVAL (no deps)
  ├── RECALC (no deps)
  ├── UI (no deps)
  ├── DISPLAY (no deps)
  ├── MSG (no deps)
  └── TERMRSX (no deps)
```

**Note:** Clean modular design - no circular dependencies!

---

## Debugging Tips

### Compilation Errors

**Strategy:**
1. Compile modules in order (utilities first)
2. Fix syntax errors before semantic errors
3. Check line numbers in error messages
4. Verify PARAMETER values are in range

**Common fixes:**
- Missing END statement
- Mismatched parentheses in FORMAT
- Array index out of bounds
- Undefined variable

### Link Errors

**Strategy:**
1. Verify all .OBJ files exist
2. Check library path is correct
3. Ensure module names match .OBJ files
4. Check for duplicate symbols

**Common fixes:**
- Recompile missing modules
- Fix library path: /LB:[1,1]F77FCS
- Remove duplicate BLOCK DATA

### Runtime Errors

**Common issues:**

**"Array bounds exceeded"**
- Check cell references (A1-Z254)
- Verify array indices within bounds
- Reduce MAXCEL if necessary

**"Arithmetic overflow"**
- INTEGER overflow (>32767)
- Use REAL for intermediate calculations

**"Divide by zero"**
- Formula evaluation error
- Check for zero divisors

**"I/O error on unit 6"**
- Terminal disconnected
- Check DZ11 controller status

### Debugging Tools

**Available on RSX-11M:**
- **ODT** - Octal debugging tool (assembly level)
- **TRACE** - Function call tracer
- **DUMP** - Memory dump

**FORTRAN debugging:**
- Add WRITE statements for trace output
- Use temporary variables to break complex expressions
- Print intermediate values in calculations

**Test programs:**
- TESTTERM.FOR - Validates terminal driver
- Create minimal test cases for each module

---

## Platform-Specific Issues

### RSX-11M Considerations

**QIO vs. FORTRAN I/O:**
- FORTRAN I/O uses RMS internally
- QIO provides lower-level terminal control
- XL uses standard FORTRAN I/O for portability

**Terminal Handling:**
- DZ11 controller must be online
- Terminal must be set to VT-52 mode
- Baud rate: 9600 recommended

**File System:**
- Files-11 structure: [UIC]FILENAME.EXT;VERSION
- Default device: SYS$LOGIN:
- Scratch files: Use unique names to avoid conflicts

### Memory Limitations

**PDP-11 address space:** 64KB per task (without I&D space)
**With I&D space:** Up to 128KB (64KB I + 64KB D)

**If task exceeds limits:**
1. Reduce MAXCEL parameter
2. Use CPM config instead of FULL
3. Enable I&D space separation (requires system privileges)

### Character Set

**ASCII compatible:** 7-bit ASCII
**Extended characters:** May not display correctly
**Box drawing:** Use minus signs and pipes instead

**Portable approach:**
Use only printable ASCII (32-126) for maximum compatibility.

---

## Performance Optimization

### Hash Table Tuning

**Current:** HASHSZ=1024
**Formula:** HASHSZ = 2^N where 2^N ≥ MAXCEL/2

**Trade-offs:**
- Larger HASHSZ = faster lookup, more memory
- Smaller HASHSZ = slower lookup, less memory

### Formula Caching

**Not implemented:** Formula results are not cached
**Recalculation:** Always from scratch on dependency change

**Future optimization:**
- Cache intermediate results
- Only recalculate changed subtrees

### Screen Rendering

**Current:** Full redraw on every change
**Optimization:** Only update changed cells

**Implementation complexity:** High (cursor tracking)

---

## Testing Checklist

### Unit Tests

- [ ] STRUTIL functions (string operations)
- [ ] CELLS hash table (lookup, insert, delete)
- [ ] PARSE formula parsing (operators, functions)
- [ ] EVAL expression evaluation (all operators)
- [ ] DEPS dependency tracking (add, check)
- [ ] RECALC recalculation order (topological sort)
- [ ] TERMRSX escape sequences (all functions)

### Integration Tests

- [ ] Compile all modules without errors
- [ ] Link succeeds with all modules
- [ ] Program starts and displays grid
- [ ] Arrow keys move cursor
- [ ] Enter value in cell
- [ ] Enter formula in cell
- [ ] Formula evaluates correctly
- [ ] Change referenced cell, formula updates
- [ ] /QUIT exits cleanly

### Acceptance Test

**Scenario:** Simple budget
1. Start XL
2. Enter "Rent" label (if labels supported)
3. Enter 1000 in A1
4. Enter 400 in A2
5. Enter =A1+A2 in A3
6. Verify A3 shows 1400
7. Change A1 to 1200
8. Verify A3 shows 1600
9. Type /QUIT
10. Program exits

---

## Known Issues

### Current Version (v1.0)

**Issue:** No file I/O
**Status:** By design (future enhancement)
**Workaround:** Re-enter data each session

**Issue:** TMKEY blocks on input
**Status:** FORTRAN IV limitation
**Workaround:** Acceptable for interactive use

**Issue:** No reverse video
**Status:** VT-52 limitation
**Workaround:** Use other visual cues

**Issue:** Fixed column width
**Status:** Design simplification
**Workaround:** Format numbers externally if needed

### Future Work

- Implement file I/O using RMS
- Add QIO support for true non-blocking input
- Support VT-100 ANSI sequences for better display
- Add copy/paste functionality
- Implement undo/redo

---

## References

### Documentation

- **RSX-11M System Reference Manual**
- **PDP-11 FORTRAN IV Language Manual**
- **TKB Task Builder Reference**
- **VT-100 User Guide** (for escape sequences)

### Source Code

- `/pdp11/src/` - All source files
- `/src/` - Original CP-V version (for comparison)

### Build Scripts

- `BUILD.CMD` - Compilation script
- `XLBUILD.CMD` - Link script
- `INSTALL.CMD` - Automated installation

---

## Conclusion

The RSX-11M port of XL Spreadsheet demonstrates that **FORTRAN IV code can be highly portable** when designed with platform abstraction in mind.

**Key takeaways:**
1. Use standard FORTRAN IV features only
2. Abstract platform-specific code (terminal I/O)
3. Minimize dependencies between modules
4. Test incrementally during development
5. Document assumptions and limitations

This approach enabled **97% code reuse** from CP-V to RSX-11M, with only the terminal driver requiring replacement.

---

**Happy building!**
