# XL Spreadsheet Development Plan
## 2026-01-21

---

## Part 1: Implemented Features

### Core Architecture
- **Multi-layered design** for portability:
  - Layer 0: Utilities (string handling, numeric conversion)
  - Layer 1: Engine (cell storage, dependency tracking, recalculation)
  - Layer 2: UI/UX (display, input handling, commands)
  - Layer 3a: Protocol (VT-52, VT-100/ANSI escape sequences)
  - Layer 3b: Platform I/O (Unix, RSX-11M, CP-V)
  - Layer 3c: Terminal driver (combines protocol + I/O)

- **Build configurations**:
  - Small: 100 cells, 64 hash buckets, 500 char string pool
  - Medium: 300 cells, 256 hash buckets, 2000 char string pool
  - Large: 2000 cells, 1024 hash buckets, 10000 char string pool

### Cell Storage (Layer 1 - CELLS.FOR)
- Hash table with open chaining for sparse storage
- Cell types:
  - TYPE=0: Empty
  - TYPE=1: Numeric value
  - TYPE=2: Formula (with calculated result)
  - TYPE=3: Text/Label (with alignment)
- Formula token storage in string pool
- Text storage with alignment (left/right/center)

### Dependency Tracking (Layer 1 - DEPS.FOR)
- Tracks which cells depend on which other cells
- Supports recalculation propagation

### Formula Parser (Layer 1 - PARSE.FOR)
- Tokenizes input into:
  - Numbers (integers and decimals)
  - Cell references (A1, B2, etc.)
  - Operators (+, -, *, /)
  - Parentheses
- Outputs token array for evaluation

### Formula Evaluator (Layer 1 - EVAL.FOR)
- Stack-based expression evaluation
- Supports basic arithmetic operations
- Cell reference resolution

### Recalculation (Layer 1 - RECALC.FOR)
- Automatic recalculation when cell values change
- Propagates changes to dependent cells

### Display System (Layer 2 - DISPLAY.FOR)
- 24-line VT-52/VT-100 terminal layout:
  - Row 1: Status line (cell reference, mode)
  - Row 2: Column headers (A-H visible)
  - Row 3: Separator line
  - Rows 4-23: Grid (20 rows x 8 columns visible)
  - Row 24: Edit line
- Viewport scrolling (follows cursor)
- Reverse video highlighting for current cell
- 9-character cell width (1 border + 8 content)
- Right-aligned numbers, left/right/center text

### User Interface (Layer 2 - UI.FOR, XLMAIN.FOR)
- **Modes**:
  - NAV: Navigation mode (arrow keys move cursor)
  - ENTRY: Data entry mode (typing values/formulas)
  - COMMAND: Slash command mode (/QUIT)

- **Navigation**:
  - Arrow keys: Move one cell
  - Ctrl+Arrow: Jump to grid edge
  - Enter after entry: Move down
  - Tab after entry: Move right

- **Data Entry**:
  - Numbers: Direct entry (e.g., `123.45`)
  - Formulas: Expression entry (e.g., `A1+B1*2`)
  - Text labels with Lotus 1-2-3 prefixes:
    - `'text` = left-aligned
    - `"text` = right-aligned
    - `^text` = centered
  - Plain text defaults to left-aligned

- **Cell Operations**:
  - Delete/Backspace in NAV mode: Clear cell
  - Blank entry (spaces only): Clear cell
  - ESC: Cancel entry

### Terminal I/O (Layer 3)
- VT-100/ANSI escape sequence support
- Arrow key detection (ESC [ A/B/C/D)
- Ctrl+Arrow key detection (ESC [ 1 ; 5 A/B/C/D)
- Escape sequence state machine with timeout recovery
- Raw terminal mode (Unix) via C helper (termio.c)

### Commands
- `/QUIT` - Exit application

---

## Part 2: Planned Features

### Phase 1: Functions and Ranges

#### 1.1 Range Syntax
- **Colon operator**: `A1:A10` specifies a range
- Range can be:
  - Single column: `A1:A10`
  - Single row: `A1:H1`
  - Rectangle: `A1:C5`

#### 1.2 Built-in Functions
| Function | Description | Example |
|----------|-------------|---------|
| `SUM(range)` | Sum of values in range | `=SUM(A1:A10)` |
| `MIN(range)` | Minimum value in range | `=MIN(B1:B5)` |
| `MAX(range)` | Maximum value in range | `=MAX(B1:B5)` |
| `COUNT(range)` | Count of non-empty cells | `=COUNT(A1:A10)` |

#### 1.3 Function Combinations
- Functions return numeric values usable in expressions
- Example: `=SUM(A1:A10)/COUNT(A1:A10)` (manual average)
- Example: `=MAX(B1:B5)-MIN(B1:B5)` (range spread)

#### 1.4 Implementation Plan
1. **Parser changes** (PARSE.FOR):
   - Add function name token type (TYPE=5)
   - Add colon operator for ranges (TYPE=6)
   - Recognize SUM, MIN, MAX, COUNT keywords

2. **Evaluator changes** (EVAL.FOR):
   - Add function evaluation
   - Add range expansion (iterate cells in range)
   - Implement each function's logic

3. **Dependency tracking** (DEPS.FOR):
   - Register all cells in range as dependencies

---

### Phase 2: Cell References and Copying

#### 2.1 Reference Types
| Type | Syntax | Behavior on Copy |
|------|--------|------------------|
| Relative | `A1` | Adjusts row and column |
| Absolute | `$A$1` | Never changes |
| Mixed (col) | `$A1` | Column fixed, row adjusts |
| Mixed (row) | `A$1` | Row fixed, column adjusts |

#### 2.2 Copy Operations (Lotus 1-2-3 Syntax)
- `/C` or `/COPY` command
- Prompts:
  1. Source range (single cell or range)
  2. Destination (single cell or range)
- Reference adjustment during copy:
  - Relative refs adjust by offset
  - Absolute refs ($) remain unchanged

#### 2.3 Implementation Plan
1. **Parser changes**:
   - Detect `$` prefix on column and/or row
   - Store reference type in token (bits for col-absolute, row-absolute)

2. **Copy command** (XLMAIN.FOR):
   - `/COPY` enters copy mode
   - Prompt for source, destination
   - For each destination cell:
     - Copy cell type
     - Adjust formula references based on offset and $ markers
     - Re-tokenize and store adjusted formula

3. **Reference adjustment routine**:
   - Input: token array, row offset, col offset
   - Output: adjusted token array
   - Skip adjustment for absolute references

---

### Phase 3: Recalculation Modes

#### 3.1 Modes
| Mode | Description |
|------|-------------|
| AUTO | Recalculate immediately on any change (current behavior) |
| MANUAL | Only recalculate on explicit command |

#### 3.2 Commands
- `/RECALC AUTO` - Enable automatic recalculation
- `/RECALC MANUAL` - Disable automatic recalculation
- `/RECALC` or `F9` equivalent - Force recalculation (manual mode)

#### 3.3 Implementation Plan
1. **Global state**: Add RCMODE variable (1=auto, 2=manual)
2. **ENTKEY changes**: Check RCMODE before calling RECALC
3. **Command handler**: Add /RECALC command parsing
4. **Full recalc routine**: Recalculate all formula cells

---

### Phase 4: Row and Column Operations

#### 4.1 Insert Operations
- `/IR` or `/INSERT ROW` - Insert row at cursor
- `/IC` or `/INSERT COL` - Insert column at cursor

#### 4.2 Delete Operations
- `/DR` or `/DELETE ROW` - Delete row at cursor
- `/DC` or `/DELETE COL` - Delete column at cursor

#### 4.3 Implementation Plan
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

### Phase 5: File Operations

#### 5.1 File Format (.xl)
JSON-based format for full spreadsheet state:

```json
{
  "version": "1.0",
  "settings": {
    "recalc_mode": "auto",
    "cursor_col": 1,
    "cursor_row": 1
  },
  "cells": [
    {
      "ref": "A1",
      "type": "number",
      "value": 123.45,
      "format": "default"
    },
    {
      "ref": "B1",
      "type": "formula",
      "formula": "=A1*2",
      "value": 246.90,
      "format": "default"
    },
    {
      "ref": "C1",
      "type": "text",
      "value": "Hello",
      "align": "left",
      "format": "default"
    }
  ],
  "sheets": [
    {
      "name": "Sheet1",
      "cells": "..."
    }
  ]
}
```

#### 5.2 Commands
- `/SAVE filename` - Save to filename.xl
- `/SAVE` - Save to current filename (if set)
- `/OPEN filename` - Open filename.xl
- `/NEW` - Clear spreadsheet, start fresh

#### 5.3 Implementation Plan
1. **JSON writer** (FILESAV.FOR):
   - Iterate all non-empty cells
   - Write cell data in JSON format
   - Handle string escaping

2. **JSON reader** (FILELOAD.FOR):
   - Simple JSON parser (cells array only)
   - Populate cell storage
   - Rebuild dependency graph

3. **Command handler**:
   - Parse /SAVE and /OPEN commands
   - Filename handling (default extension .xl)

---

### Phase 6: Print Export

#### 6.1 Design Philosophy
- Keep main XL app minimal
- Export to .xlp format for external print utility
- Separate `xlprint` application handles actual printing

#### 6.2 Print Format (.xlp)
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

#### 6.3 Commands
- `/PRINT filename` - Export all to filename.xlp
- `/PRINT filename A1:H20` - Export range to filename.xlp
- `/PRINT` - Export all to default name (current file + .xlp)

#### 6.4 Implementation Plan
1. **Print export** (PRINT.FOR):
   - Iterate cells in range (or all non-empty)
   - Get display value (not formula)
   - Write .xlp JSON format

2. **External xlprint utility** (separate program):
   - Read .xlp file
   - Format for output device
   - Support various print options

---

### Phase 7: Future Enhancements

#### 7.1 Additional Functions
- `AVG(range)` - Average (same as SUM/COUNT)
- `ABS(value)` - Absolute value
- `INT(value)` - Integer portion
- `ROUND(value, decimals)` - Round to decimals
- `IF(condition, true_val, false_val)` - Conditional

#### 7.2 Multiple Sheets
- Tab between sheets
- Cross-sheet references: `Sheet2!A1`
- Sheet management commands

#### 7.3 Cell Formatting
- Number formats (decimal places, thousands separator)
- Date formats
- Column width adjustment

#### 7.4 Additional Commands
- `/GOTO cellref` - Jump to cell
- `/FIND text` - Search for text
- `/REPLACE old new` - Search and replace

---

## Implementation Priority

| Priority | Phase | Feature | Complexity |
|----------|-------|---------|------------|
| 1 | 1 | Functions (SUM, MIN, MAX, COUNT) | Medium |
| 2 | 1 | Range syntax (A1:A10) | Medium |
| 3 | 2 | Reference types ($A$1) | Medium |
| 4 | 2 | Copy command | High |
| 5 | 5 | Save/Open files | High |
| 6 | 6 | Print export | Medium |
| 7 | 3 | Recalc modes | Low |
| 8 | 4 | Row/Column insert/delete | High |

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
    FUNCS.FOR     - [NEW] Function implementations
    FILEIO.FOR    - [NEW] File save/load

  layer2/
    DISPLAY.FOR   - Screen rendering
    UI.FOR        - UI state management
    COMMANDS.FOR  - [NEW] Command processing
    COPY.FOR      - [NEW] Copy operations
    PRINT.FOR     - [NEW] Print export

  layer3/
    TERMINAL.FOR  - Terminal driver
    PROTVT100.FOR - VT-100/ANSI protocol
    PROTVT52.FOR  - VT-52 protocol
    IOUNIX.FOR    - Unix I/O
    IORSX.FOR     - RSX-11M I/O
```

---

## Notes

- All new features must maintain FORTRAN IV/66 compatibility
- No recursion (use explicit stacks)
- Fixed-format source (columns 1-72)
- Minimal memory footprint for vintage systems
- Test on both native (macOS/Linux) and emulated (PDP-11, Sigma 7) systems
