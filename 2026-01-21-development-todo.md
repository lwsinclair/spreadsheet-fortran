# XL Spreadsheet Development TODO
## 2026-01-22 (Updated)

Engineering tasks and implementation plans for agentic development.

---

## Implementation Priority

| Priority | Phase | Feature | Complexity | Status |
|----------|-------|---------|------------|--------|
| 1 | 1 | Functions (SUM, MIN, MAX, COUNT) | Medium | ✅ Done |
| 2 | 1 | Range syntax (A1:A10) | Medium | ✅ Done |
| 3 | 2 | Reference types ($A$1) | Medium | ✅ Done |
| 4 | 2 | Copy command | High | ✅ Done |
| 5 | 5 | Save/Open files | High | ✅ Done |
| 6 | - | Grid: 255 cols × 255 rows (A-IU) | Medium | ✅ Done |
| 7 | 3 | Recalc modes | Low | ✅ Done |
| 8 | 7 | Column widths | Medium | ✅ Done |
| 9 | 4 | Row/Column insert/delete | High | ✅ Done |
| 10 | - | TDD Test Suite | Medium | ✅ Done |

---

## Phase 3: Recalculation Modes ✅ COMPLETED

### 3.1 Modes
| Mode | Description |
|------|-------------|
| AUTO | Recalculate immediately on any change (default) |
| MANUAL | Only recalculate on explicit command |

### 3.2 Commands
- `/RECALC AUTO` - Enable automatic recalculation
- `/RECALC MANUAL` - Disable automatic recalculation
- `/RECALC` - Force recalculation of all cells

### 3.3 Implementation (Completed)
1. **Global state**: Added RCMODE to UIDAT COMMON block (1=auto, 2=manual)
2. **ENTKEY changes**: Checks RCMODE before calling RECALC
3. **Command handler**: Added /RECALC command parsing in CMDKEY
4. **Full recalc routine**: RECALL subroutine iterates all formula cells
5. **Status messages**: Added messages 7-9 for recalc feedback

---

## Phase 4: Row and Column Operations ✅ COMPLETED

### 4.1 Insert Operations
- `/IR` or `/IR n` - Insert row at cursor or row n
- `/IC` or `/IC A` - Insert column at cursor or column A

### 4.2 Delete Operations
- `/DR` or `/DR n` - Delete row at cursor or row n
- `/DC` or `/DC A` - Delete column at cursor or column A

### 4.3 Implementation (Completed)
1. **Cell storage (CELLS.FOR)**:
   - `ROWINS(row)` - Shifts cells down, rehashes
   - `ROWDEL(row)` - Deletes row, shifts cells up
   - `COLINS(col)` - Shifts cells right, rehashes
   - `COLDEL(col)` - Deletes column, shifts cells left
   - `CELRHSH(idx, col, row)` - Rehashes cell after position change

2. **Formula adjustment (CELLS.FOR)**:
   - `ROWADF(row, delta)` - Adjusts row refs in all formulas
   - `COLADF(col, delta)` - Adjusts column refs in all formulas
   - `ADJROW(fstr, flen, nfstr, nflen, atrow, delta)` - Single formula row adjust
   - `ADJCOL(fstr, flen, nfstr, nflen, atcol, delta)` - Single formula col adjust
   - Absolute references ($A$1) are preserved unchanged

3. **Commands (XLMAIN.FOR)**:
   - Added IR, DR, IC, DC command parsing
   - Added PRSNUM, PRSCOL helper parsers

4. **Status messages (DISPLAY.FOR)**:
   - Messages 12-17 for row/col operation feedback

---

## Phase 6: Print Utility (Separate Program)

### 6.1 Design Philosophy
- Keep main XL app minimal - NO print code in XL itself
- The .xl file format already includes calculated values for formulas
- Separate `xlprint` utility reads .xl files directly
- Reduces code bloat on small machines (Apple II, CP/M, timesharing)

### 6.2 File Format Support
The .xl format already contains everything needed for printing:
- Formula cells save both `"formula"` and `"value"` fields
- Print utility ignores `"formula"`, uses only `"value"`
- Text cells include alignment information
- Column widths (when implemented) will be in .xl file

### 6.3 xlprint Utility (Separate Program)
- **NOT part of XL** - completely separate executable
- Reads .xl files, discards formulas
- Formats output for printer or screen
- Could also support graphing/charting
- Minimal memory footprint (no calculation engine)

---

## Phase 7: Column Widths ✅ COMPLETED

### 7.1 Overview
Each column can have a different width (default: 8 characters).

### 7.2 Storage
- Array COLWID(255) in CWDAT COMMON block
- Default width: 8 characters
- Valid range: 3-40 characters

### 7.3 Commands
- `/WIDTH col width` - Set column width (e.g., `/WIDTH A 12`)

### 7.4 Implementation (Completed)
1. **Column width storage** (UI.FOR):
   - Added CWDAT COMMON block with COLWID(255) array
   - `CWINI` - Initialize all widths to default (8)
   - `CWGET(col)` - Get column width
   - `CWSET(col, width)` - Set column width

2. **Display changes** (DISPLAY.FOR):
   - DSPHDR: Variable width column headers
   - DSPGRD/DSPCLL: Variable width cell display
   - DSPSCR: Variable width scroll calculation

3. **Command handler** (XLMAIN.FOR):
   - Added /WIDTH command parsing with WIDPRS helper

4. **File I/O**:
   - FILESAV.FOR: FLWCWD saves column widths
   - FILELOAD.FOR: FLPCWD loads column widths

5. **Terminal support** (IOUNIX.FOR, termio.c):
   - Added IOPUTRW for repeated character output

---

## Phase 8: Future Enhancements

### 8.1 Additional Functions
- `AVG(range)` - Average (same as SUM/COUNT) ✅ Done
- `ABS(value)` - Absolute value
- `INT(value)` - Integer portion
- `ROUND(value, decimals)` - Round to decimals
- `IF(condition, true_val, false_val)` - Conditional

### 8.2 Multiple Sheets
- Tab between sheets
- Cross-sheet references: `Sheet2!A1`
- Sheet management commands

### 8.3 Cell Formatting
- Number formats (decimal places, thousands separator)
- Date formats

### 8.4 Additional Commands
- `/GOTO cellref` - Jump to cell
- `/FIND text` - Search for text
- `/REPLACE old new` - Search and replace

---

## Test Suite ✅ COMPLETED

### Overview
TDD test framework with 78 tests covering critical code paths.

### Test Files

| File | Tests | Purpose |
|------|------:|---------|
| TESTLIB.FOR | - | Test framework (TSTINI, TSTEQ, TSTREQ, TSTEND) |
| TCORE.FOR | 36 | Core canary tests (parser, evaluator, hash, deps) |
| TROWCOL.FOR | 42 | Row/column insert/delete operations |
| TESTMAIN.FOR | - | Test runner main program |

### Canary Tests (TCORE.FOR)
Tests designed to catch silent data corruption:

1. **Operator Precedence** - `2+3*4=14`, `(2+3)*4=20`, `10-2-3=5`, `8/4/2=1`
2. **Multi-Column Refs** - AA1=col27, AZ1=col52 decode correctly
3. **Hash Chain Integrity** - Delete middle of chain, verify rest accessible
4. **Dependency Propagation** - A1=10, B1=A1*2 -> change A1, B1 updates
5. **Formula Adjustment** - B2->B3 on insert, $B$2 stays unchanged

### Running Tests

```bash
cd native/tests
make          # Build and run
make build    # Build only
make run      # Run only
make clean    # Clean artifacts
```

---

## File Structure

```
native/
  XLMAIN.FOR      - Main program, input handling, commands
  BRIDGE.FOR      - External function wrappers
  termio.c        - C helper for Unix terminal I/O

  layer0/
    STRUTIL.FOR   - String utilities

  layer1/
    CELLS.FOR     - Cell storage, row/col operations
    DEPS.FOR      - Dependency tracking
    PARSE.FOR     - Formula parser
    EVAL.FOR      - Expression evaluator
    RECALC.FOR    - Recalculation engine

  layer2/
    DISPLAY.FOR   - Screen rendering, variable widths
    UI.FOR        - UI state, recalc mode, column widths
    MSG.FOR       - Status messages
    FILES.FOR     - File utilities
    FILESAV.FOR   - JSON file writer (includes widths)
    FILELOAD.FOR  - JSON file parser (includes widths)
    COMMANDS.FOR  - [EXCLUDED] Dead code, not compiled

  layer3/
    TERMINAL.FOR  - Terminal driver
    PROTVT100.FOR - VT-100/ANSI protocol
    PROTVT52.FOR  - VT-52 protocol
    IOUNIX.FOR    - Unix I/O (includes IOPUTRW)
    FIOUNIX.FOR   - Unix file I/O
    IORSX.FOR     - RSX-11M I/O

  tests/
    Makefile      - Test build system
    TESTLIB.FOR   - Test framework library
    TCORE.FOR     - Core canary tests
    TROWCOL.FOR   - Row/column operation tests
    TESTMAIN.FOR  - Test runner main
```

---

## Development Constraints

- All code must maintain FORTRAN IV/66 compatibility
- No recursion (use explicit stacks)
- Fixed-format source (columns 1-72)
- Minimal memory footprint for vintage systems
- Test on both native (macOS/Linux) and emulated (PDP-11, Sigma 7) systems

---

## Build Commands

```bash
# Build all variants
cd native && make

# Build specific variant
make small    # 100 cells
make medium   # 300 cells
make large    # 2000 cells

# Clean build
make clean && make
```

---

## Test Commands

```bash
# Run application
./xl_large

# Load sample file
/open sample.xl

# Test commands
/save test.xl
/copy A1:B5 C1
/width A 15
/recalc manual
/recalc
/ir 5
/dr 3
/ic B
/dc C
/quit
```

---

## Remaining Future Work

### Not Yet Implemented
- Print utility (separate xlprint program)
- Additional functions (ABS, INT, ROUND, IF)
- Multiple sheets
- Cell formatting (number formats, dates)
- Search/replace commands
