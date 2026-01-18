# XL — Developer Specification

**Product Name:** XL  
**Version:** 1.0  
**Primary Target:** Xerox Sigma CP-V (FORTRAN IV / ANSI X3.9-1966)  
**Secondary Targets:** CP/M (Microsoft FORTRAN-80), future microcomputer ports  
**Date:** February 1978  

---

## Part I: Product Vision and Business Case

### 1.1 The Problem

Financial planning, budgeting, and modeling today require either:

- **Manual recalculation** on paper or adding machines (slow, error-prone, tedious)
- **Batch programs** written by professional programmers (expensive, inflexible, long turnaround)
- **Timesharing services** like LANPAR or AutoPlan ($1000+/month, requires training, no visual feedback)

None of these let a businessperson, accountant, or analyst *directly manipulate* a financial model and *immediately see* the consequences of changes.

### 1.2 The Opportunity

Interactive terminals are becoming common. The Sigma CP-V supports multiple simultaneous users on video display terminals. Microcomputers (Apple II, CP/M machines) are emerging. There is no interactive, visual calculation tool for these platforms.

### 1.3 The Product

XL is an interactive visual calculator organized as a grid of cells. The user points at a cell, types a number or formula, and sees results instantly. Formulas reference other cells by pointing. When any cell changes, dependent cells recalculate automatically. Historically, and counterfactually speaking, can think of it as a small like Visicalc, more complete like Lotus 1-2-3, but developed for an even earlier market in early 1978.

This is "programming by example" — the user builds a calculation model by demonstrating relationships, not by writing a program.

### 1.4 Target Users

- Financial analysts building budget projections
- Accountants doing what-if analysis
- Engineers calculating bills of materials
- Managers tracking metrics
- Anyone who currently uses columnar paper and a calculator

### 1.5 Competitive Differentiation

| Feature | LANPAR (1969) | AutoPlan/AutoTab | XL |
|---------|---------------|------------------|-----------|
| Interactive editing | No | No | **Yes** |
| Point-to-reference | No | No | **Yes** |
| Instant recalculation | No | No | **Yes** |
| Visible grid | No | No | **Yes** |
| Requires training | Extensive | Extensive | **Minimal** |
| Platform | Mainframe | Timesharing | **Mini/Micro** |
| Cost | $1000+/month | $1000+/month | **One-time purchase** |

XL's differentiator is not being the first row-column calculator — it is being the first *interactive, visual, point-to-reference* calculator that runs on accessible hardware.

---

## Part II: Design Philosophy and UX Principles

### 2.1 Core Principle

**The grid is the program.** The user builds a model by entering values and relationships, not by writing a conventional program.

The fundamental interaction loop:

1. Move to a cell
2. Type a number, label, or formula
3. Confirm (RETURN/TAB)
4. See results propagate immediately
5. Continue without leaving the grid context

### 2.2 Non-Negotiables

These UX invariants must not change across ports or be "simplified away" for implementation convenience:

1. **Point-to-reference is first-class.** Creating formulas by pointing at cells (not just typing addresses) is the defining innovation. It is not optional.

2. **Immediate feedback.** After accepting an entry, the screen reflects updated results immediately (subject to manual recalculation mode). The user must never wonder "did it work?"

3. **Visible state, minimal surprise.** The status line always shows where you are, what's in the cell, and what mode you're in. The user should never be confused about the current state.

4. **Low training burden.** A non-programmer should succeed via discoverable commands and consistent behavior. If it requires reading a manual to do basic operations, we have failed.

5. **The grid stays alive.** Navigation and reading results is the default state; command entry is a brief interruption, not a mode change that hides the grid.

### 2.3 Programming by Example

Developers should conceptualize each cell entry as either:

- A **fact** (number or label) — static data
- A **relationship** (formula) — computed from other cells

The sheet is a dependency network. Recalculation is the runtime. The user is programming without knowing they're programming.

### 2.4 Application Modes

The program has three modes, though we avoid using the word "mode" with users:

| Mode | Description | Status Display |
|------|-------------|----------------|
| NAV | Navigation — arrow keys move cursor, no entry in progress | `NAV` |
| ENTRY | Building a cell's contents on the command line | `ENT` |
| POINT | While composing a formula, cursor movement selects referenced cells | `PNT` |

**Requirement:** The status line must always show the current mode.

### 2.5 Responsiveness Requirements

On the Sigma CP-V with typical terminals:

- Cell entry acceptance and redraw: < 0.5 seconds
- Navigation between cells: < 0.1 seconds (feels instant)
- Full recalculation of 100-cell model: < 2 seconds

If these targets cannot be met, the UX fails regardless of correctness.

---

## Part III: Target System and Constraints

### 3.1 Primary Platform: Xerox Sigma CP-V

**Hardware:**
- Xerox Sigma 5, 7, or 9 mainframe
- 128K words (512KB) or more memory
- Interactive terminals: Xerox 7015 display, DEC VT52, Teletype Model 40

**Software:**
- CP-V operating system
- FORTRAN IV compiler (ANS FORTRAN-66 compatible)
- Terminal support via M:TY and SET commands

**Constraints:**
- FORTRAN IV has no CHARACTER data type (strings are integer arrays)
- No dynamic memory allocation
- No recursion
- Limited control structures (no block IF/ELSE)
- 6-character identifier significance in many compilers

### 3.2 Secondary Platform: CP/M

**Hardware:**
- Intel 8080 or Zilog Z80 processor
- 64KB total RAM (~40KB usable for application)
- ADM-3A, VT52, Hazeltine terminals

**Software:**
- CP/M 1.4 or 2.0
- Microsoft FORTRAN-80

**Constraints:**
- 16-bit integers only (max array index 32767)
- Single precision float only (~7 significant digits)
- ~40KB total program + data space
- No complex numbers
- Limited string handling

### 3.3 Memory Architecture by Platform

| Platform | Available RAM | Target Grid | Max Cells | String Pool |
|----------|---------------|-------------|-----------|-------------|
| CP-V | 512KB | 64 cols × 256 rows | 2000 | 16000 chars |
| CP/M | ~40KB | 26 cols × 64 rows | 500 | 4000 chars |
| Apple II | ~32KB | 26 cols × 50 rows | 400 | 3000 chars |

The CP-V version ships with full capacity. Other ports reduce limits at compile time. Core logic remains unchanged.

---

## Part IV: Software Architecture

### 4.1 Layered Design

The application separates into four layers to maximize portability:

```
┌─────────────────────────────────────────────────────┐
│  Layer 3: Terminal Driver (platform-specific)       │
│    Raw key input, cursor control, screen clear      │
├─────────────────────────────────────────────────────┤
│  Layer 2: UI/Application (portable FORTRAN IV)      │
│    Navigation, command line editor, mode machine    │
│    Screen layout, incremental redraw scheduling     │
├─────────────────────────────────────────────────────┤
│  Layer 1: Calculation Engine (portable FORTRAN IV)  │
│    Cell storage, formula parse/compile/evaluate     │
│    Dependency tracking, recalculation scheduling    │
├─────────────────────────────────────────────────────┤
│  Layer 0: String Utilities (portable FORTRAN IV)    │
│    All text as integer arrays, conversions          │
└─────────────────────────────────────────────────────┘
```

### 4.2 Layer Responsibilities

**Layer 0 — String Utilities (STRUTIL)**

All string manipulation in the application goes through this layer. No other module performs "raw string work" with integer arrays directly.

This exists because FORTRAN IV has no CHARACTER type. All text is represented as integer arrays (one character code per element).

**Layer 1 — Calculation Engine**

Pure computational FORTRAN. No I/O statements. Operates on in-memory data structures.

This code:
- Ports unchanged to any FORTRAN environment
- Can be transliterated to assembly by reading the source
- Contains zero platform dependencies

**Layer 2 — Application Layer**

Grid navigation, command parsing, display formatting, mode state machine.

FORTRAN with calls to Layer 1 and Layer 3. Portable with minor modifications (primarily compile-time size constants).

**Layer 3 — Terminal I/O Layer**

All terminal interaction: raw keystroke input, cursor positioning, screen clearing, highlighting.

Completely rewritten for each target platform. The interface contract (what routines exist and what they do) is fixed; the implementation varies.

### 4.3 Porting Strategy

To port XL to a new platform:

1. **Rewrite Layer 3** (terminal driver) implementing the interface contract
2. **Adjust size constants** in Layer 1 and 2 for memory limits
3. **Modify file I/O** in Layer 2 for platform file system conventions
4. **Test and tune** performance

Layers 0 and 1 should require no changes. Layer 2 changes are limited to constants and possibly file path handling.

### 4.4 Why This Matters

This architecture ensures:
- Bug fixes in calculation engine benefit all platforms
- UX behavior is consistent (same mode machine, same command line editor)
- Platform porters have a clear, bounded task
- Future microcomputer versions are feasible

---

## Part V: Data Structures

### 5.1 Design Principles

**Sparse storage:** Empty cells cost nothing. Only cells with content consume storage. This is critical for both memory efficiency and performance (don't scan 64×256 cells).

**Hash table access:** Cell lookup by (column, row) must be O(1). Linear search through all cells would destroy interactive responsiveness.

**Static allocation:** All storage is fixed-size arrays in COMMON blocks. No dynamic allocation exists in FORTRAN IV.

### 5.2 Cell Storage

```fortran
C     CELL TYPE CODES
C     CEMPTY=0, CLABEL=1, CVALUE=2, CFORM=3

C     MAXIMUM SIZES (ADJUST PER PLATFORM)
C     MAXCOL=64, MAXROW=256, MAXCEL=2000

C     CELL TABLE (ACTIVE CELLS ONLY)
      INTEGER NCELL                    ! Count of active cells
      INTEGER CHASH(1024)              ! Hash table heads
      INTEGER CNEXT(MAXCEL)            ! Hash chain links
      INTEGER CCOL(MAXCEL)             ! Column (1-MAXCOL)
      INTEGER CROW(MAXCEL)             ! Row (1-MAXROW)
      INTEGER CTYPE(MAXCEL)            ! Cell type code
      REAL    CVAL(MAXCEL)             ! Numeric value or result
      INTEGER CTXTP(MAXCEL)            ! Pointer into string pool
      INTEGER CTXTL(MAXCEL)            ! Length of text

      COMMON /CELLT/ NCELL, CHASH, CNEXT, CCOL, CROW, CTYPE,
     +               CVAL, CTXTP, CTXTL
```

**Hash function:** `(COL * 257 + ROW) MOD 1024` provides reasonable distribution.

**Cell lookup algorithm:**
1. Compute hash of (col, row)
2. Follow chain from CHASH(hash) through CNEXT links
3. Compare CCOL and CROW at each node
4. Return index if found, 0 if not

### 5.3 String Pool

```fortran
C     STRING STORAGE (INTEGER ARRAY, ONE CHAR PER ELEMENT)
      INTEGER NXTSTR                   ! Next free position
      INTEGER STRPOL(MAXSTR)           ! Character storage

      COMMON /STRP/ NXTSTR, STRPOL
```

Labels and formula source text are stored here. CTXTP points to starting position; CTXTL gives length.

**Allocation:** Strings are appended at NXTSTR. When a cell is edited, its old string space is abandoned (simple and fast). Compaction can be added later if fragmentation becomes a problem.

### 5.4 Dependency Graph

When cell X contains a formula referencing cell Y, we need to find X when Y changes.

**Recommended structure:** Adjacency lists for fast reverse lookup.

```fortran
C     DEPENDENCY TRACKING
C     For each referenced cell Y, maintain list of dependents X

      INTEGER DHED(MAXCEL)             ! Head of dependent list for cell
      INTEGER DNXT(MAXDEP)             ! Next node in list
      INTEGER DFRM(MAXDEP)             ! Dependent cell index
      INTEGER DUSE                     ! Next free dependency node

      COMMON /DEPT/ DHED, DNXT, DFRM, DUSE
```

**On cell Y change:**
1. Start at DHED(Y)
2. Follow DNXT chain
3. Mark each DFRM cell as dirty

This is O(number of dependents), not O(total cells).

### 5.5 Compiled Formula Storage

Formulas compile to postfix operation sequences for fast evaluation.

```fortran
C     COMPILED FORMULAS
      INTEGER NOPS                     ! Operations used
      INTEGER OPCOD(MAXOPS)            ! Operation code
      INTEGER OP1(MAXOPS)              ! First argument
      INTEGER OP2(MAXOPS)              ! Second argument
      INTEGER FBEG(MAXCEL)             ! First op for formula cell
      INTEGER FEND(MAXCEL)             ! Last op for formula cell

      COMMON /FORMT/ NOPS, OPCOD, OP1, OP2, FBEG, FEND
```

Source text is retained (in string pool) for editing and file save. Compiled form is used for evaluation.

### 5.6 View State

```fortran
C     CURRENT VIEW STATE
      INTEGER VCOL1, VROW1             ! Top-left visible cell
      INTEGER VCOLS, VROWS             ! Visible columns and rows
      INTEGER CURCL, CURRW             ! Cursor position (current cell)
      INTEGER COLWID(MAXCOL)           ! Column widths

      COMMON /VIEWT/ VCOL1, VROW1, VCOLS, VROWS, CURCL, CURRW, COLWID
```

### 5.7 Recalculation State

```fortran
C     RECALCULATION STATE
      INTEGER RCMOD                    ! 0=auto, 1=manual
      INTEGER DIRTY(MAXCEL)            ! Needs recalc flag
      INTEGER RCORD(MAXCEL)            ! Evaluation order
      INTEGER NRCOR                    ! Count to evaluate

      COMMON /RECST/ RCMOD, DIRTY, RCORD, NRCOR
```

---

## Part VI: String Utility Library (Layer 0)

### 6.1 Mandatory Rule

**No module outside STRUTIL is allowed to do raw string work.**

All text manipulation uses integer arrays and STRUTIL calls. This isolates the FORTRAN IV string representation problem to one module.

### 6.2 Character Representation

Characters are stored as their ASCII/terminal codes in INTEGER arrays. One character per array element.

Common codes:
- Space: 32
- Digits 0-9: 48-57
- Uppercase A-Z: 65-90
- Lowercase a-z: 97-122

### 6.3 Required Routines

**String operations:**

| Routine | Purpose |
|---------|---------|
| STREQ(A, NA, B, NB) | Compare strings, return 1 if equal |
| STRCPY(SRC, NS, DST, ND) | Copy string |
| STRFND(STR, N, CH) | Find character, return position or 0 |
| STRTRM(STR, N, NOUT) | Trim trailing blanks |
| STRPAD(STR, N, W, J, OUT) | Pad/justify to width W (J=left/right/center) |
| STRINS(STR, N, P, CH) | Insert character at position |
| STRDEL(STR, N, P) | Delete character at position |
| IUPPER(ICH) | Convert char code to uppercase |

**Numeric conversions:**

| Routine | Purpose |
|---------|---------|
| ITOA(IVAL, STR, N) | Integer to string |
| ATOI(STR, N, IVAL, IERR) | String to integer |
| RTOA(RVAL, STR, N, FMT) | Real to string with format |
| ATOR(STR, N, RVAL, IERR) | String to real |

**Cell address conversions:**

| Routine | Purpose |
|---------|---------|
| COLTOA(ICOL, STR, N) | Column number to letter(s): 1→A, 27→AA |
| ATOCOL(STR, N, ICOL, IERR) | Letter(s) to column number |
| FMTCEL(COL, ROW, STR, N) | Format cell address: (2,5)→B5 |
| PARCEL(STR, N, COL, ROW, IERR) | Parse cell address |

**Display formatting:**

| Routine | Purpose |
|---------|---------|
| FMTNUM(RVAL, FMT, W, STR, N) | Format number per cell format |
| FMTERR(IERR, STR, N) | Render error code as string |

### 6.4 Numeric Format Codes

| Code | Meaning | Example |
|------|---------|---------|
| G | General | 1234.5 |
| F2 | Fixed 2 decimal | 1234.50 |
| $ | Currency | $1,234.50 |
| % | Percent | 45.00% |
| I | Integer | 1235 |

---

## Part VII: Terminal Abstraction (Layer 3)

### 7.1 Interface Contract

All terminal operations go through these routines. Platform porters implement only this module.

```fortran
C     TERMINAL INITIALIZATION
      SUBROUTINE TINIT
C     Detect terminal type, set dimensions, enable raw mode

C     TERMINAL SHUTDOWN
      SUBROUTINE TTERM
C     Restore terminal state

C     CLEAR SCREEN
      SUBROUTINE TCLR

C     POSITION CURSOR
      SUBROUTINE TCURS(ROW, COL)
      INTEGER ROW, COL
C     Row 1-24, Col 1-80 (typical)

C     WRITE N CHARACTERS FROM INTEGER ARRAY AT CURRENT POSITION
      SUBROUTINE TWRIT(ISTR, N)
      INTEGER ISTR(1), N

C     WRITE AT POSITION (combines TCURS + TWRIT)
      SUBROUTINE TWRAT(ROW, COL, ISTR, N)
      INTEGER ROW, COL, ISTR(1), N

C     READ SINGLE KEYSTROKE (NO ECHO, NO WAIT FOR CR)
      SUBROUTINE TGETC(ICH)
      INTEGER ICH
C     Returns ASCII/terminal code in ICH

C     HIGHLIGHT ON/OFF
      SUBROUTINE THLON
      SUBROUTINE THLOF

C     QUERY SCREEN DIMENSIONS
      SUBROUTINE TSIZE(ROWS, COLS)
      INTEGER ROWS, COLS
```

### 7.2 Critical Requirement: Raw Key Input

TGETC must return immediately when a key is pressed, without:
- Waiting for RETURN/ENTER
- Echoing the character
- Line buffering

This is essential for navigation (arrow keys) and point-to-reference.

On CP-V, this requires special DCB (Device Control Block) setup. On CP/M, use BDOS function 6 (direct console I/O).

### 7.3 Where Line Editing Lives

**Important architectural decision:** The terminal layer provides raw keystrokes. The application layer (Layer 2) implements command line editing.

This ensures:
- Consistent editing behavior across platforms
- ENTRY and POINT modes work identically everywhere
- Platform porters don't accidentally break UX

The terminal layer is deliberately minimal.

### 7.4 Screen Redraw Strategy

To achieve responsiveness on slow terminals:

1. **Maintain screen image buffer** in application layer (integer arrays per row)
2. **Track dirty regions** (which rows/cells changed)
3. **Redraw only what changed**

Full-screen redraw on every keystroke will feel laggy on 9600 baud terminals. Incremental update is required for acceptable UX.

### 7.5 CP-V Implementation Notes

- Terminals declared via M:TY command
- Terminal type set via SET command
- Support Xerox 7015 display, DEC VT52, Teletype Model 40
- Use ENCODE/DECODE for internal string formatting
- Cursor control via terminal-specific escape sequences
- Single-character input requires special DCB configuration

### 7.6 CP/M Implementation Notes

BDOS calls via assembly language stubs linked with FORTRAN:

- Function 1: Console input (with echo, wait for CR) — not useful
- Function 2: Console output
- Function 6: Direct console I/O (raw mode) — use this

Terminal escape sequences vary:
- ADM-3A: ESC = row col for cursor positioning
- VT52: ESC Y row col
- Hazeltine: ~ DC1 col row

---

## Part VIII: Screen Layout and Navigation

### 8.1 Screen Layout

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ A1: +B1+C1                                              NAV  AUTO  Mem: 847  │  ← Status line
├──────────────────────────────────────────────────────────────────────────────┤
│       A          B          C          D          E          F          G    │  ← Column headers
├──────────────────────────────────────────────────────────────────────────────┤
│  1  Revenue    50000      32000      18000                                   │
│  2  Expenses   32000      28000      31000                                   │
│  3  ─────────────────────────────────────────                                │
│  4  Profit    [18000]     4000      -13000                                   │  ← Current cell
│  5                                                                           │
│     ...                                                                      │
│ 20                                                                           │
├──────────────────────────────────────────────────────────────────────────────┤
│ CMD:                                                                         │  ← Command line
└──────────────────────────────────────────────────────────────────────────────┘
```

### 8.2 Status Line Contents

| Element | Description |
|---------|-------------|
| Cell address | Current cell (e.g., "A1", "B4") |
| Cell contents | For formulas, shows source ("+B1+C1"); for values, shows value |
| Mode | NAV, ENT, or PNT |
| Recalc mode | AUTO or MANUAL (with * if dirty cells exist) |
| Memory | Remaining cell capacity |

### 8.3 Grid Area

Variable based on terminal size:
- 80-column terminal: 7-8 columns × 18-20 rows visible
- 40-column terminal: 3-4 columns × 18-20 rows visible

Column widths are configurable per-column (default: 9 characters).

### 8.4 Current Cell Indication

The current cell is visually distinguished:
- Bracketed display: [18000]
- Or reverse video if terminal supports highlighting

### 8.5 Navigation Commands

| Key | Action |
|-----|--------|
| ↑ ↓ ← → | Move one cell |
| H J K L | Move one cell (for terminals without arrow keys) |
| CTRL-F | Page forward (down one screen) |
| CTRL-B | Page back (up one screen) |
| TAB | Move right one column |
| CTRL-A | Go to cell A1 |
| > | Goto prompt — enter cell address directly |

### 8.6 Viewport Management

The viewport scrolls to keep the cursor visible:
- Cursor moves off bottom: scroll down
- Cursor moves off right: scroll right to show current column
- Horizontal scroll snaps to column boundaries

---

## Part IX: Data Entry

### 9.1 Entry Mode Detection

When the user begins typing at a cell, the first character determines cell type:

| First Character | Cell Type | Example |
|-----------------|-----------|---------|
| 0-9, period, minus | Number | 123, -45.67, .5 |
| Single quote (') | Label, left-aligned | 'Revenue |
| Double quote (") | Label, right-aligned | "Total |
| Plus (+) | Formula | +B1+C1 |
| At sign (@) | Formula (function) | @SUM(A1:A10) |
| Letter | Label, left-aligned | Revenue |

### 9.2 Number Entry

**Valid:** digits, single decimal point, leading minus
```
123   -45.67   .5   0.123
```

**Invalid:** multiple decimals, multiple minus signs, commas
```
1.2.3   --5   1,234
```

Store as REAL in CVAL. Display formatted per cell/column format setting.

### 9.3 Label Entry

Store text in string pool. Maximum length: 40 characters (one screen width minus overhead).

### 9.4 Formula Entry

Formulas begin with + or @. Cell references can be created by:

1. **Typing** the cell address directly (A1, B23, AA5)
2. **Pointing** — press period (.) to enter POINT mode, use arrow keys to select, press period again to anchor

### 9.5 Point-to-Reference (Critical Feature)

This is the key innovation. Example session:

```
User at D4, wants formula +C2*2

Types: +
Display enters ENTRY mode, command line shows: +

Types: .
Display enters POINT mode, highlight moves to A1
Status shows: PNT

Presses → →
Highlight moves to C1

Presses ↓
Highlight moves to C2
Command line shows: +[C2]

Types: .
Point mode ends, reference anchored
Command line shows: +C2

Types: *2
Command line shows: +C2*2

Presses RETURN
Formula accepted, cell shows result
```

**Implementation requirements:**
- Formula edit buffer must be mutable (insert cell address when user anchors)
- POINT mode updates highlighted cell display without committing
- Return to ENTRY mode on anchor, POINT mode on period

### 9.6 Entry Confirmation

| Key | Action |
|-----|--------|
| RETURN | Accept entry, move cursor down |
| TAB | Accept entry, move cursor right |
| ESC | Cancel entry, restore original cell contents |
| CTRL-G | Cancel (fallback for terminals without ESC) |

---

## Part X: Formula Language

### 10.1 Operators

| Operator | Meaning | Precedence |
|----------|---------|------------|
| ^ | Exponentiation | 1 (highest) |
| * / | Multiply, divide | 2 |
| + - | Add, subtract | 3 |
| = <> < > <= >= | Comparison (returns 1 or 0) | 4 (lowest) |

Parentheses override precedence. Evaluation left-to-right within precedence level.

### 10.2 Cell References

| Syntax | Type | Behavior on Copy |
|--------|------|------------------|
| B5 | Relative | Adjusts by offset |
| $B5 | Column absolute | Column fixed, row adjusts |
| B$5 | Row absolute | Row fixed, column adjusts |
| $B$5 | Fully absolute | No adjustment |

### 10.3 Functions (Version 1.0)

| Function | Arguments | Description |
|----------|-----------|-------------|
| @SUM(range) | Range | Sum of values in range |
| @AVG(range) | Range | Average (mean) |
| @MIN(range) | Range | Minimum value |
| @MAX(range) | Range | Maximum value |
| @COUNT(range) | Range | Count of non-empty cells |
| @ABS(value) | Single | Absolute value |
| @INT(value) | Single | Integer part (truncate toward zero) |
| @SQRT(value) | Single | Square root |
| @IF(cond,true,false) | Three | Conditional: if cond≠0, return true, else false |
| @NA | None | Returns "not available" marker |
| @ERR | None | Returns error marker |
| @ROUND(value,places) | Two | Round to decimal places |

**Range syntax:** A1:C10 means rectangular block from A1 to C10 inclusive.

### 10.4 Formula Compilation

Formulas compile to postfix operation sequences:

```
Formula: +A1+B1*2

Compiles to:
  OPCELL   A1        ; push value of A1
  OPCELL   B1        ; push value of B1
  OPCONST  2.0       ; push constant 2.0
  OPMUL              ; multiply top two: B1*2
  OPADD              ; add: A1 + (B1*2)
```

Operation codes:
```
OPCONST = 1    Push constant (arg1 = constant index)
OPCELL  = 2    Push cell value (arg1 = col, arg2 = row)
OPADD   = 3    Add
OPSUB   = 4    Subtract
OPMUL   = 5    Multiply
OPDIV   = 6    Divide
OPPOW   = 7    Exponentiate
OPNEG   = 8    Negate (unary minus)
OPFUNC  = 9    Call function (arg1 = function code)
OPRANG  = 10   Push range descriptor
```

### 10.5 Parser Architecture

Single-pass tokenizer + shunting-yard algorithm:

1. Tokenize input into numbers, cell references, operators, functions
2. Use operator stack to handle precedence
3. Output postfix operations to compiled form
4. Build dependency list as cell references are encountered

**FORTRAN IV constraint:** No recursion. Use explicit stacks (operator stack, output queue) as arrays.

### 10.6 Evaluation

Stack-based postfix evaluation:

```fortran
      REAL FUNCTION EVALFM(CIDX)
      INTEGER CIDX
C
C     Evaluate formula for cell CIDX
C     Returns result, sets error flag in common if error
C
      REAL STACK(50)
      INTEGER SP
      ...
```

Stack depth of 50 should be sufficient. Overflow indicates formula too complex (report error).

---

## Part XI: Recalculation

### 11.1 Dependency Tracking

When a formula is entered or modified:

1. Parse formula, identify all cell references
2. Remove old dependencies for this cell (if editing existing formula)
3. Add new dependency entries: (this cell depends on referenced cell)

### 11.2 Change Propagation

When a cell value changes:

1. Mark cell as dirty
2. Find all cells that depend on this cell (via dependency graph)
3. Mark those as dirty
4. Recursively mark dependents of dependents
5. Build evaluation order (topological sort)
6. Evaluate all dirty cells in dependency order

### 11.3 Circular Reference Detection

During topological sort, if a cell appears in its own dependency chain, it's a circular reference:

1. Mark cell with CIRC error
2. Display "CIRC" instead of value
3. Continue with other cells (don't abort entire recalc)

### 11.4 Recalculation Modes

| Mode | Behavior | Use Case |
|------|----------|----------|
| Automatic | Recalculate after every cell change | Normal use |
| Manual | Recalculate only on ! command | Bulk data entry |

Status line shows mode. In manual mode, show indicator when dirty cells exist (e.g., "MANUAL*").

---

## Part XII: Commands

### 12.1 Command Entry

All commands begin with / (slash). The slash is typed in NAV mode and immediately shows on the command line. A single letter follows.

### 12.2 Command Summary

| Command | Action |
|---------|--------|
| /B | Blank current cell |
| /BR | Blank range (prompts for range) |
| /C | Copy (prompts for source, destination) |
| /E | Edit current cell (load contents to command line) |
| /F | Format current cell or range |
| /FS | File Save |
| /FL | File Load |
| /FD | File Directory listing |
| /FN | File New (clear worksheet) |
| /FP | File Print export |
| /H | Help |
| /M | Move (prompts for source, destination) |
| /R | Toggle recalculation mode (auto/manual) |
| /W | Set column width |
| /! | Force recalculation (in manual mode) |

### 12.3 Command Interaction Pattern

Commands use a consistent prompt pattern:

1. User types / — command line shows "/"
2. User types command letter — command line shows "/C" for copy
3. System prompts "From: " on command line
4. User enters cell/range or points
5. System prompts "To: "
6. User enters destination
7. Command executes

### 12.4 Copy Command Details

**Reference adjustment on copy:**

- Relative references adjust by (dest_col - source_col, dest_row - source_row)
- Absolute references (marked with $) remain unchanged
- Mixed references adjust only the relative portion

**Example:**
```
Cell A1 contains: +B1+$C$1
Copy to A2
Result in A2: +B2+$C$1

B1 became B2 (relative, adjusted down 1 row)
$C$1 stayed $C$1 (absolute, unchanged)
```

### 12.5 Format Command Options

| Option | Meaning |
|--------|---------|
| G | General format |
| F n | Fixed n decimal places |
| $ | Currency format |
| % | Percentage format |
| I | Integer format |

Format is stored per-cell, with column default and global default as fallbacks.

---

## Part XIII: File Operations

### 13.1 File Format (.CAL)

Line-oriented text format. Human-readable. Easy to debug.

```
*XL 1.0
*CREATED 1978-02-15
*GLOBALS
FMT:G
PREC:2
RECALC:A
*COLUMNS
A:12
B:9
C:15
*CELLS
A1:L:Revenue
B1:V:50000
B2:V:32000
A4:L:Profit
B4:F:+B1-B2:18000
C1:V:32000
C2:V:28000
C4:F:+C1-C2:4000
*END
```

### 13.2 Section Markers

Lines beginning with * are section headers:
- `*XL 1.0` — file identifier and version
- `*GLOBALS` — global settings
- `*COLUMNS` — column width overrides
- `*CELLS` — cell data
- `*END` — end of file

### 13.3 Cell Line Format

`address:type:content[:cached_value]`

| Type | Meaning |
|------|---------|
| L | Label |
| V | Value (number) |
| F | Formula (cached result follows second colon) |

### 13.4 Print Export (.PRN)

Exports specified range as formatted text suitable for printing:

```
Revenue     50000      32000
Expenses    32000      28000
─────────────────────────────
Profit      18000       4000
```

No formulas or cell addresses — just visible values.

---

## Part XIV: Error Handling

### 14.1 Error Display Values

| Display | Meaning |
|---------|---------|
| ERR | General error (divide by zero, invalid operation) |
| NA | Explicitly marked not available (@NA function) |
| CIRC | Circular reference detected |
| OVF | Numeric overflow |
| REF | Invalid cell reference |

Errors display in place of cell values. They propagate: a formula referencing an error cell produces an error.

### 14.2 Memory Exhaustion

When cell storage is full:

1. Reject new cell entry
2. Display "MEMORY FULL" on status line
3. Allow user to delete cells or save/reload smaller range

Status line shows remaining capacity: "Mem: 847" means 847 more cells can be stored.

### 14.3 Input Validation

| Situation | Response |
|-----------|----------|
| Invalid number | Reject entry, display "INVALID NUMBER" |
| Invalid cell address | Reject entry, display "INVALID ADDRESS" |
| Formula syntax error | Reject entry, display "SYNTAX ERROR" |
| File not found | Display "FILE NOT FOUND", remain in current state |
| Disk full on save | Display "DISK FULL", file not saved |

---

## Part XV: Module Organization

### 15.1 Source Files

```
CALCSH.FOR    Main program, initialization, command loop
STRUTIL.FOR   Layer 0: String utility library
CELLS.FOR     Layer 1: Cell storage, hash table, lookup
DEPS.FOR      Layer 1: Dependency graph management
PARSE.FOR     Layer 1: Formula parser
EVAL.FOR      Layer 1: Formula evaluator
RECALC.FOR    Layer 1: Recalculation engine
MSG.FOR       Layer 2: Message table, prompts, help text
UI.FOR        Layer 2: Mode machine, command line editor
DISPLAY.FOR   Layer 2: Screen layout, grid rendering, redraw
COMMANDS.FOR  Layer 2: Command handlers (/C, /M, /B, etc.)
FILES.FOR     Layer 2: File save/load
TERMCPV.FOR   Layer 3: Terminal driver for CP-V
```

### 15.2 Build Order

Layer 0 → Layer 1 → Layer 2 → Layer 3 → Main

Dependencies flow upward only: Layer 1 calls Layer 0, Layer 2 calls Layers 0 and 1, etc.

### 15.3 Naming Conventions

- COMMON block names: 5-6 characters, descriptive (/CELLT/, /STRP/, /VIEWT/)
- Public subroutines: 6 characters max for portability (TINIT, TGETC, EVALFM)
- Internal routines can use longer names if compiler allows

---

## Part XVI: FORTRAN IV Coding Standards

### 16.1 Language Restrictions

These restrictions ensure maximum portability across FORTRAN IV compilers:

1. **No CHARACTER type.** All strings are integer arrays processed through STRUTIL.

2. **No PARAMETER statement** in portable code. Use COMMON blocks with initialized values, or accept magic numbers with clear comments.

3. **No block IF/ELSE.** Use IF (...) GO TO ..., arithmetic IF, or subroutine calls for branching.

4. **No recursion.** FORTRAN IV does not support recursive calls. Use explicit stacks.

5. **No assumed-length arrays.** Array dimensions must be explicit.

6. **Array indices start at 1.** Some compilers don't support 0-based arrays.

7. **Six-character significance.** Assume identifiers beyond 6 characters may be truncated.

8. **REAL only.** Avoid DOUBLE PRECISION for CP/M compatibility.

### 16.2 Coding Conventions

1. **Declare all variables explicitly.** No reliance on implicit typing.

2. **One statement per line.** No multiple statements on one line.

3. **Label all CONTINUE statements** that are targets.

4. **Comment every subroutine** with purpose, inputs, outputs.

5. **Use consistent indentation** (2 or 3 spaces) even though FORTRAN doesn't require it.

### 16.3 Error Handling Pattern

```fortran
      SUBROUTINE EXAMPL(ARG1, ARG2, IERR)
      INTEGER ARG1, ARG2, IERR
C
C     PURPOSE: Example subroutine
C     INPUTS:  ARG1 - first argument
C              ARG2 - second argument
C     OUTPUTS: IERR - 0 if success, nonzero error code
C
      IERR = 0
C     ... do work ...
      IF (error condition) IERR = 1
      IF (IERR .NE. 0) RETURN
C     ... continue ...
      RETURN
      END
```

---

## Part XVII: UI State Machine

### 17.1 States

| State | Description |
|-------|-------------|
| NAV | Navigation mode, no entry in progress |
| ENTRY | Building cell contents, formula editing |
| POINT | Selecting cell reference while in formula |
| CMD | Processing slash command |
| PROMPT | Awaiting input for command parameter |

### 17.2 Transitions

```
NAV:
  Arrow keys → move cursor, stay NAV
  0-9, +, @, ', ", letter → begin entry, go ENTRY
  / → go CMD
  
ENTRY:
  Characters → append to buffer
  Arrow keys → if formula with incomplete ref, go POINT; else reject
  . (period) → if in formula, go POINT
  RETURN → accept entry, recalc, go NAV
  TAB → accept entry, recalc, move right, go NAV
  ESC → cancel, go NAV
  
POINT:
  Arrow keys → move highlight (selection)
  . (period) → anchor reference, go ENTRY
  ESC → cancel point, go ENTRY
  
CMD:
  Letter → execute command or go PROMPT
  ESC → cancel, go NAV
  
PROMPT:
  Characters → build response
  RETURN → process response, continue command or go NAV
  ESC → cancel command, go NAV
```

### 17.3 State Variables

```fortran
      INTEGER UISTAT              ! Current state (NAV, ENTRY, etc.)
      INTEGER EDBUF(80)           ! Edit buffer (integer array)
      INTEGER EDLEN               ! Edit buffer length
      INTEGER EDPOS               ! Cursor position in buffer
      INTEGER PNTCOL, PNTROW      ! Highlighted cell in POINT mode
      INTEGER CMDCH               ! Current command character
      
      COMMON /UIST/ UISTAT, EDBUF, EDLEN, EDPOS, PNTCOL, PNTROW, CMDCH
```

---

## Part XVIII: Testing Strategy

### 18.1 Unit Tests

Each module includes a test driver program:

```fortran
      PROGRAM TSTPAR
C     Test PARSE module
      ...
      CALL PARSE(formula, OPS, NOPS, IERR)
      IF (IERR .NE. 0) PRINT *, 'FAIL: basic formula'
      ...
```

### 18.2 Module Test Coverage

| Module | Test Cases |
|--------|------------|
| STRUTIL | All conversions, edge cases (empty, max length, overflow) |
| CELLS | Hash collision, full table, lookup miss |
| PARSE | All operators, all functions, nested parens, errors |
| EVAL | Arithmetic accuracy, error propagation, stack overflow |
| DEPS | Add/remove, circular detection |
| RECALC | Propagation order, manual mode, large chains |

### 18.3 Integration Tests

Standard test worksheets:

1. **12-month budget** — realistic use case, ~50 formulas
2. **Deep chain** — A1→B1→C1→...→Z1, tests propagation
3. **Wide fan** — one cell referenced by 50 others
4. **Circular detection** — A1→B1→A1, must show CIRC
5. **Max capacity** — fill to MAXCEL cells, verify behavior
6. **Copy with references** — test absolute/relative adjustment

### 18.4 UX Acceptance Criteria

1. **Point-to-reference works:** User can build +C2*2 by pointing, not typing C2
2. **Mode visibility:** Status line always shows cell + contents + mode + recalc
3. **Latency:** Cell entry response < 0.5 second on Sigma 9
4. **Edit safety:** ESC restores original, no partial corruption
5. **Discoverability:** /H provides enough guidance for first-time success
6. **Round-trip:** Save and load produces identical worksheet

---

## Part XIX: Development Schedule

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 1. STRUTIL + data structures | 2 weeks | Layer 0 complete, cell/string storage |
| 2. Formula parser/evaluator | 3 weeks | PARSE, EVAL with comprehensive tests |
| 3. Dependency and recalc | 2 weeks | DEPS, RECALC working correctly |
| 4. Terminal layer (CP-V) | 2 weeks | TERMCPV functional on target terminals |
| 5. Display and navigation | 2 weeks | Grid display, cursor movement, viewport |
| 6. Mode machine and entry | 2 weeks | NAV/ENTRY/POINT working, point-to-reference |
| 7. Commands | 2 weeks | All slash commands implemented |
| 8. File I/O | 1 week | Save/load functional |
| 9. Testing and polish | 2 weeks | Bug fixes, edge cases, performance |
| **Total** | **18 weeks** | Production CP-V release |

**CP/M port estimate:** 6-8 weeks after CP-V completion (terminal rewrite, memory tuning, testing).

---

## Part XX: Version 1.0 Limitations

These are explicit design choices, documented for users and future development:

- Single worksheet per file
- No graphics or charts
- No macros or keyboard recording
- No sorting
- No database functions (VLOOKUP, HLOOKUP)
- Column width affects all cells in column (no per-cell width)
- Maximum 40-character labels
- Maximum formula complexity: 50 stack operations
- No inter-worksheet references

Future versions may address these as memory and market permit.

---

## Appendix A: Key Constants

These values are the primary tuning points for platform ports:

| Constant | CP-V Value | CP/M Value | Purpose |
|----------|------------|------------|---------|
| MAXCOL | 64 | 26 | Maximum columns |
| MAXROW | 256 | 64 | Maximum rows |
| MAXCEL | 2000 | 500 | Maximum active cells |
| MAXSTR | 16000 | 4000 | String pool size |
| MAXDEP | 4000 | 1000 | Dependency nodes |
| MAXOPS | 8000 | 2000 | Compiled formula ops |

---

## Appendix B: ASCII Character Codes

For STRUTIL implementers:

| Character | Code | Character | Code |
|-----------|------|-----------|------|
| Space | 32 | 0-9 | 48-57 |
| + | 43 | A-Z | 65-90 |
| - | 45 | a-z | 97-122 |
| . | 46 | $ | 36 |
| / | 47 | : | 58 |
| * | 42 | @ | 64 |
| ^ | 94 | ( ) | 40, 41 |
| = | 61 | < > | 60, 62 |

---

## Appendix C: Terminal Escape Sequences

For Layer 3 implementers:

**VT52:**
- Cursor position: ESC Y row+32 col+32
- Clear screen: ESC H ESC J
- Cursor home: ESC H

**ADM-3A:**
- Cursor position: ESC = row+32 col+32
- Clear screen: CTRL-Z

**Teletype Model 40:**
- No cursor positioning (output only)
- Clear by sending formfeed or newlines

---

*End of Specification*