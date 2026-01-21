# XL Spreadsheet Development TODO
## 2026-01-21

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
| 6 | 6 | Print export | Medium | ⬚ TODO |
| 7 | 3 | Recalc modes | Low | ⬚ TODO |
| 8 | 4 | Row/Column insert/delete | High | ⬚ TODO |

---

## Phase 3: Recalculation Modes

### 3.1 Modes
| Mode | Description |
|------|-------------|
| AUTO | Recalculate immediately on any change (current behavior) |
| MANUAL | Only recalculate on explicit command |

### 3.2 Commands
- `/RECALC AUTO` - Enable automatic recalculation
- `/RECALC MANUAL` - Disable automatic recalculation
- `/RECALC` or `F9` equivalent - Force recalculation (manual mode)

### 3.3 Implementation Plan
1. **Global state**: Add RCMODE variable (1=auto, 2=manual)
2. **ENTKEY changes**: Check RCMODE before calling RECALC
3. **Command handler**: Add /RECALC command parsing
4. **Full recalc routine**: Recalculate all formula cells

---

## Phase 4: Row and Column Operations

### 4.1 Insert Operations
- `/IR` or `/INSERT ROW` - Insert row at cursor
- `/IC` or `/INSERT COL` - Insert column at cursor

### 4.2 Delete Operations
- `/DR` or `/DELETE ROW` - Delete row at cursor
- `/DC` or `/DELETE COL` - Delete column at cursor

### 4.3 Implementation Plan
1. **Cell storage changes**:
   - `ROWINS(row)` - Increment row numbers >= row
   - `ROWDEL(row)` - Decrement row numbers > row, delete row
   - `COLINS(col)` - Increment col numbers >= col
   - `COLDEL(col)` - Decrement col numbers > col, delete col

2. **Formula adjustment**:
   - Update cell references in all formulas
   - Handle references that become invalid (deleted row/col)
   - Set error state for invalid references

3. **Dependency update**:
   - Rebuild dependency graph after row/col changes

---

## Phase 6: Print Export

### 6.1 Design Philosophy
- Keep main XL app minimal
- Export to .xlp format for external print utility
- Separate `xlprint` application handles actual printing

### 6.2 Print Format (.xlp)
JSON-based format containing display values only:

```json
{
  "version": "1.0",
  "export_date": "2026-01-21",
  "source_file": "budget.xl",
  "range": {
    "start_col": 1,
    "start_row": 1,
    "end_col": 8,
    "end_row": 20
  },
  "columns": [
    {"col": 1, "width": 8, "header": "A"},
    {"col": 2, "width": 8, "header": "B"}
  ],
  "rows": [
    {
      "row": 1,
      "cells": [
        {"col": 1, "display": "Item", "align": "left"},
        {"col": 2, "display": "123.45", "align": "right"},
        {"col": 3, "display": "", "align": "left"}
      ]
    }
  ]
}
```

### 6.3 Commands
- `/PRINT filename` - Export all to filename.xlp
- `/PRINT filename A1:H20` - Export range to filename.xlp
- `/PRINT` - Export all to default name (current file + .xlp)

### 6.4 Implementation Plan
1. **Print export** (PRINT.FOR):
   - Iterate cells in range (or all non-empty)
   - Get display value (not formula)
   - Write .xlp JSON format

2. **External xlprint utility** (separate program):
   - Read .xlp file
   - Format for output device
   - Support various print options

---

## Phase 7: Future Enhancements

### 7.1 Additional Functions
- `AVG(range)` - Average (same as SUM/COUNT) ✅ Done
- `ABS(value)` - Absolute value
- `INT(value)` - Integer portion
- `ROUND(value, decimals)` - Round to decimals
- `IF(condition, true_val, false_val)` - Conditional

### 7.2 Multiple Sheets
- Tab between sheets
- Cross-sheet references: `Sheet2!A1`
- Sheet management commands

### 7.3 Cell Formatting
- Number formats (decimal places, thousands separator)
- Date formats
- Column width adjustment

### 7.4 Additional Commands
- `/GOTO cellref` - Jump to cell
- `/FIND text` - Search for text
- `/REPLACE old new` - Search and replace

---

## File Structure

```
native/
  XLMAIN.FOR      - Main program, input handling
  BRIDGE.FOR      - External function declarations
  termio.c        - C helper for Unix terminal I/O

  layer0/
    STRUTIL.FOR   - String utilities

  layer1/
    CELLS.FOR     - Cell storage
    DEPS.FOR      - Dependency tracking
    PARSE.FOR     - Formula parser
    EVAL.FOR      - Expression evaluator
    RECALC.FOR    - Recalculation engine

  layer2/
    DISPLAY.FOR   - Screen rendering
    UI.FOR        - UI state management
    MSG.FOR       - Status messages
    FILES.FOR     - File utilities
    FILESAV.FOR   - JSON file writer
    FILELOAD.FOR  - JSON file parser
    COMMANDS.FOR  - [EXCLUDED] Dead code, not compiled
    PRINT.FOR     - [TODO] Print export

  layer3/
    TERMINAL.FOR  - Terminal driver
    PROTVT100.FOR - VT-100/ANSI protocol
    PROTVT52.FOR  - VT-52 protocol
    IOUNIX.FOR    - Unix I/O
    FIOUNIX.FOR   - Unix file I/O
    IORSX.FOR     - RSX-11M I/O
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
/quit
```
