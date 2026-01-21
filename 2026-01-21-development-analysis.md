# XL Spreadsheet Development Analysis

## 2026-01-22 (Updated)

Product development metrics, implemented features, and release documentation.

---

## Implemented Features

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

- Shunting-yard algorithm for infix to postfix conversion
- Tokenizes input into:
  - Numbers (integers and decimals)
  - Cell references (A1, B2, $A$1, $A1, A$1)
  - Operators (+, -, *, /, ^)
  - Parentheses for grouping
  - Functions (@SUM, @AVG, @MIN, @MAX, @N)
- Case-insensitive parsing
- Outputs token array for evaluation

### Formula Evaluator (Layer 1 - EVAL.FOR)

- Stack-based postfix expression evaluation
- Supports all arithmetic operations
- Cell reference resolution
- Function evaluation over ranges

### Recalculation (Layer 1 - RECALC.FOR)

- Automatic or manual recalculation modes
- Propagates changes to dependent cells
- RECALL routine for full spreadsheet recalculation

### Row/Column Operations (Layer 1 - CELLS.FOR)

- Insert row: `/IR` or `/IR n` - shifts cells down
- Delete row: `/DR` or `/DR n` - shifts cells up
- Insert column: `/IC` or `/IC A` - shifts cells right
- Delete column: `/DC` or `/DC A` - shifts cells left
- Formula references automatically adjusted
- Absolute references ($A$1) preserved unchanged
- Hash table properly rehashed after cell moves

### Display System (Layer 2 - DISPLAY.FOR)

- 24-line VT-52/VT-100 terminal layout:
  - Row 1: Status line (cell reference, mode)
  - Row 2: Column headers (variable width)
  - Row 3: Separator line
  - Rows 4-23: Grid (20 rows, variable columns visible)
  - Row 24: Edit line / Status messages
- Viewport scrolling (follows cursor)
- Reverse video highlighting for current cell
- Variable column widths (3-40 characters, default 8)
- Right-aligned numbers, left/right/center text

### Column Widths (Layer 2 - UI.FOR)

- Per-column width storage (COLWID array)
- `/WIDTH col width` command to set width
- Widths saved/loaded in .xl files
- Display adapts dynamically to widths

### User Interface (Layer 2 - UI.FOR, XLMAIN.FOR)

- **Modes**:
  
  - NAV: Navigation mode (arrow keys move cursor)
  - ENTRY: Data entry mode (typing values/formulas)
  - COMMAND: Slash command mode

- **Navigation**:
  
  - Arrow keys: Move one cell
  - Ctrl+Arrow: Jump to grid edge
  - Enter after entry: Move down
  - Tab after entry: Move right

- **Data Entry**:
  
  - Numbers: Direct entry (e.g., `123.45`)
  - Formulas: Expression entry (e.g., `=A1+B1*2`)
  - Functions: @SUM, @AVG, @MIN, @MAX, @N with ranges
  - Text labels with Lotus 1-2-3 prefixes:
    - `'text` = left-aligned
    - `"text` = right-aligned
    - `^text` = centered
  - Plain text defaults to left-aligned

- **Cell Operations**:
  
  - Delete/Backspace in NAV mode: Clear cell
  - Blank entry (spaces only): Clear cell
  - ESC: Cancel entry

### Commands (Case-Insensitive)

- `/QUIT` - Exit application
- `/SAVE filename` - Save spreadsheet to .xl file
- `/OPEN filename` - Load spreadsheet from .xl file
- `/COPY A1:B5 C1` - Copy range to destination (adjusts relative refs)
- `/WIDTH col width` - Set column width (e.g., `/WIDTH A 12`)
- `/RECALC` - Force recalculation of all cells
- `/RECALC AUTO` - Enable automatic recalculation
- `/RECALC MANUAL` - Disable automatic recalculation
- `/IR` or `/IR n` - Insert row at cursor or row n
- `/DR` or `/DR n` - Delete row at cursor or row n
- `/IC` or `/IC A` - Insert column at cursor or column A
- `/DC` or `/DC A` - Delete column at cursor or column A

### File Format (.xl)

JSON-based format for full spreadsheet state:

```json
{
  "version": "1.0",
  "settings": {
    "cursor_col": 1, "cursor_row": 1
  },
  "cells": [
    {
      "ref": "A1",
      "type": "formula",
      "formula": "=@SUM(B1:B5)",
      "value": 100.00
    }
  ]
}
```

### Terminal I/O (Layer 3)

- VT-100/ANSI escape sequence support
- Arrow key detection (ESC [ A/B/C/D)
- Ctrl+Arrow key detection (ESC [ 1 ; 5 A/B/C/D)
- Escape sequence state machine with timeout recovery
- Raw terminal mode (Unix) via C helper (termio.c)

---

## Test Suite

### Overview

TDD test framework with 78 tests covering critical code paths.
Tests designed to be compact (~820 lines total) for teletype entry.

### Test Files

| File         | Lines     | Tests  | Purpose                                   |
| ------------ | ---------:| ------:| ----------------------------------------- |
| TESTLIB.FOR  | 169       | -      | Framework (TSTINI, TSTEQ, TSTREQ, TSTEND) |
| TCORE.FOR    | 320       | 36     | Core canary tests                         |
| TROWCOL.FOR  | 503       | 42     | Row/column operations                     |
| TESTMAIN.FOR | 25        | -      | Test runner main                          |
| Makefile     | 51        | -      | Build system                              |
| **Total**    | **1,068** | **78** |                                           |

### Canary Tests (TCORE.FOR)

Tests designed to catch silent data corruption:

| Test Group             | Tests | What It Catches                           |
| ---------------------- | -----:| ----------------------------------------- |
| Operator Precedence    | 6     | Parser precedence bugs                    |
| Cell Reference Parsing | 7     | Multi-column decode bugs (AA1, AZ1)       |
| Hash Chain Integrity   | 8     | Cell loss after delete in collision chain |
| Dependency Propagation | 3     | Formula recalculation failures            |
| Formula Ref Adjustment | 12    | Absolute ref preservation, row/col shifts |

### Row/Column Tests (TROWCOL.FOR)

| Test Group          | Tests | What It Catches            |
| ------------------- | -----:| -------------------------- |
| Row Insert          | 10    | Cell shift, formula adjust |
| Row Delete          | 8     | Cell shift, formula adjust |
| Column Insert       | 8     | Cell shift, formula adjust |
| Column Delete       | 8     | Cell shift, formula adjust |
| Absolute References | 8     | $A$1 preservation          |

---

## Memory Analysis & Platform Requirements

### Configuration Parameters

| Parameter               | Small | Medium | Large  |
| ----------------------- | ----- | ------ | ------ |
| MAXCEL (cells)          | 100   | 300    | 2,000  |
| HASHSZ (hash buckets)   | 64    | 256    | 1,024  |
| MAXSTR (string pool)    | 500   | 2,000  | 10,000 |
| MAXDEP (dependencies)   | 50    | 150    | 1,000  |
| MAXTOK (tokens/formula) | 25    | 50     | 100    |
| MAXQUE (recalc queue)   | 25    | 75     | 500    |

---

### Data Memory by Module (16-bit, 2-byte INTEGER, 4-byte REAL)

| Module                  | Small     | Medium     | Large      |
| ----------------------- | --------- | ---------- | ---------- |
| **Layer 0: STRUTIL**    | 0.2 KB    | 0.2 KB     | 0.2 KB     |
| **Layer 1: CELLS**      | 3.4 KB    | 11.4 KB    | 68.4 KB    |
| **Layer 1: DEPS**       | 0.5 KB    | 1.3 KB     | 8.3 KB     |
| **Layer 1: PARSE**      | 0.2 KB    | 0.4 KB     | 0.9 KB     |
| **Layer 1: EVAL**       | 0.1 KB    | 0.1 KB     | 0.2 KB     |
| **Layer 1: RECALC**     | 0.2 KB    | 0.4 KB     | 2.0 KB     |
| **Layer 2: UI/Display** | 0.5 KB    | 0.5 KB     | 0.5 KB     |
| **Layer 2: Files**      | 0.5 KB    | 0.5 KB     | 0.5 KB     |
| **Layer 3: Terminal**   | 0.3 KB    | 0.3 KB     | 0.3 KB     |
| **Total Data**          | **~6 KB** | **~15 KB** | **~82 KB** |

---

### Estimated Code Size by Layer (Small Build with Binary File I/O)

| Layer               | Total Lines | Active Lines | 8-bit (Z80/6502) | 16-bit (PDP-11) |
| ------------------- | -----------:| ------------:| ----------------:| ---------------:|
| Layer 0: Utilities  | 583         | 276          | ~1.7 KB          | ~2.2 KB         |
| Layer 1: Engine     | 2,766       | 1,557        | ~9.3 KB          | ~12.5 KB        |
| Layer 2: UI & Files | 2,200       | 1,237        | ~7.4 KB          | ~9.9 KB         |
| Layer 3: Terminal*  | 1,208       | 581          | ~3.5 KB          | ~4.6 KB         |
| Main                | 1,511       | 915          | ~5.5 KB          | ~7.3 KB         |
| **Total Code**      | **8,268**   | **4,566**    | **~27 KB**       | **~37 KB**      |

*One terminal variant per build. Estimates at 6 bytes/active line (8-bit) or 8 bytes/active line (16-bit).*

---

### Total Memory Requirements

| Variant | Data  | Code (8-bit) | Code (16-bit) | **Total 8-bit** | **Total 16-bit** |
| ------- | ----- | ------------ | ------------- | --------------- | ---------------- |
| Small   | 6 KB  | 27 KB        | 37 KB         | **33 KB**       | **43 KB**        |
| Medium  | 15 KB | 28 KB        | 38 KB         | **43 KB**       | **53 KB**        |
| Large   | 82 KB | 33 KB        | 45 KB         | **115 KB**      | **127 KB**       |

*Code estimates at 6 bytes/active line (8-bit) or 8 bytes/active line (16-bit). Add ~2-4 KB for FORTRAN runtime library and stack.*

---

### Platform Compatibility

| Platform               | RAM     | Usable  | Small     | Medium   | Large |
| ---------------------- | ------- | ------- | --------- | -------- | ----- |
| **Apple II+ (6502)**   | 48 KB   | ~40 KB  | ⚠️ Tight* | ❌ No     | ❌ No  |
| **Apple IIe (6502)**   | 64 KB   | ~52 KB  | ✅ Yes     | ⚠️ Tight | ❌ No  |
| **Apple IIe + Aux**    | 128 KB  | ~110 KB | ✅ Yes     | ✅ Yes    | ❌ No  |
| **CP/M (Z80) 48KB**    | 48 KB   | ~44 KB  | ⚠️ Tight* | ❌ No     | ❌ No  |
| **CP/M (Z80) 64KB**    | 64 KB   | ~58 KB  | ✅ Yes     | ✅ Yes    | ❌ No  |
| **Kaypro, Osborne**    | 64 KB   | ~58 KB  | ✅ Yes     | ✅ Yes    | ❌ No  |
| **PDP-11/23 (64KB)**   | 64 KB   | ~56 KB  | ✅ Yes     | ⚠️ Tight | ❌ No  |
| **PDP-11/23+ (256KB)** | 256 KB  | ~200 KB | ✅ Yes     | ✅ Yes    | ✅ Yes |
| **PDP-11/73 RSX-11M**  | 256+ KB | ~200 KB | ✅ Yes     | ✅ Yes    | ✅ Yes |
| **Xerox Sigma 7 CP-V** | 256+ KB | ~200 KB | ✅ Yes     | ✅ Yes    | ✅ Yes |
| **TRS-80 Model 4**     | 64 KB   | ~52 KB  | ✅ Yes     | ⚠️ Tight | ❌ No  |
| **IBM PC (DOS)**       | 640 KB  | ~500 KB | ✅ Yes     | ✅ Yes    | ✅ Yes |

**Legend:** ✅ Runs comfortably | ⚠️ Marginal, may need optimization | ❌ Insufficient memory

*\*48KB systems (Apple II+, CP/M 48KB): Small build at ~33-37KB fits with tight margins. Assembly optimization of STRUTIL + Terminal I/O (~4KB savings) provides comfortable headroom.*

---

### Platform Recommendations

| Target System      | Recommended Variant | Notes                                      |
| ------------------ | ------------------- | ------------------------------------------ |
| Apple II+ (48KB)   | Small               | Fits with binary I/O; assembly for comfort |
| Apple IIe (64KB)   | Small or Medium     | Small runs comfortably                     |
| CP/M 48KB          | Small               | Fits with binary I/O; assembly for comfort |
| CP/M 64KB          | Small or Medium     | Both run comfortably                       |
| PDP-11/23 (64KB)   | Small               | Fits comfortably                           |
| PDP-11 RSX-11M     | Medium or Large     | Depends on partition size                  |
| Xerox Sigma 7 CP-V | Large               | Plenty of memory                           |
| IBM PC DOS         | Large               | 640KB is overkill                          |

---

## 48KB Optimization Analysis

### Current Code Size (Small Build - Binary File I/O Only)

| Metric                                 | Value      |
| -------------------------------------- | ---------- |
| Total source lines                     | 8,268      |
| Active code lines (no comments/blanks) | 4,566      |
| Est. 8-bit code @ 6 bytes/line         | ~27 KB     |
| Est. 8-bit code @ 8 bytes/line         | ~37 KB     |
| Data memory (small config)             | ~6 KB      |
| **Est. total @ 6 bytes/line**          | **~33 KB** |
| **Est. total @ 8 bytes/line**          | **~43 KB** |

### Target Platform Memory

| Platform         | Total RAM | System/OS | Usable | Current Est. | Status                      |
| ---------------- | --------- | --------- | ------ | ------------ | --------------------------- |
| Apple II+ (48KB) | 48 KB     | ~8 KB     | ~40 KB | **~33 KB**   | ✅ Fits with ~7 KB headroom  |
| Apple IIe (64KB) | 64 KB     | ~8 KB     | ~56 KB | ~33 KB       | ✅ Fits with ~23 KB headroom |
| CP/M Z80 (64KB)  | 64 KB     | ~6 KB     | ~58 KB | ~33 KB       | ✅ Fits with ~25 KB headroom |

*Estimates at 6 bytes/active line. At 8 bytes/line (~43 KB), Apple II+ would need assembly optimization.*

### VisiCalc Comparison (27KB on Apple II)

| Feature        | VisiCalc                                                           | XL Small                      | Notes             |
| -------------- | ------------------------------------------------------------------ | ----------------------------- | ----------------- |
| Arithmetic     | +, -, *, /                                                         | +, -, *, /, ^                 | Same              |
| Cell refs      | Relative only                                                      | Relative + Absolute           | XL has more       |
| Functions      | ~20 (SUM, AVG, MIN, MAX, NPV, LOOKUP, ABS, INT, SQRT, LOG, EXP...) | 5 (SUM, AVG, MIN, MAX, COUNT) | VisiCalc has more |
| Recalc modes   | Yes                                                                | Yes                           | Same              |
| Formula copy   | Yes                                                                | Yes                           | Same              |
| Column widths  | Yes                                                                | Yes                           | Same              |
| Number format  | Yes                                                                | No                            | VisiCalc has more |
| Save/Load      | Yes                                                                | Yes (binary format)           | Same              |
| Print          | Yes                                                                | No                            | VisiCalc has more |
| Grid           | 250×63                                                             | 255×255                       | XL larger         |
| Row/Col insert | No                                                                 | Yes                           | XL has more       |

**Key insight:** VisiCalc achieved 27KB with hand-written 6502 assembly. Our FORTRAN-compiled code will be larger for equivalent features. To reach ~32KB, we need assembly for the most verbose modules.

### Binary File Format Implementation (Completed)

Small/medium builds now use binary-only file I/O, saving code vs JSON:

| Format                                   | Lines     | Est. 8-bit Size |
| ---------------------------------------- | ---------:| ---------------:|
| JSON (FILESAV + FILELOAD)                | 1,272     | ~6.5 KB         |
| Binary (FILEBIN + FILEBINS + primitives) | 611       | ~3.5 KB         |
| **Savings**                              | 661 lines | **~3 KB**       |

This optimization is already implemented in the current small build.

---

### Optimization Strategy 1: FORTRAN-Level (Save ~2-4KB)

**Reduce message strings:**

```fortran
C Current: Each message spelled out character-by-character
C "File saved." = 11 CALL TMPUTC statements = ~66 bytes code

C Better: Message table with indices
      INTEGER MSGS(100)
      DATA MSGS /70,105,108,101,32,115,97,118,101,100,46,0.../
```

Estimated savings: ~1KB

**Merge similar routines:**

- CELPUT/CELTXT share 80% of code
- ADJROW/ADJCOL are nearly identical
- Estimated savings: ~1-2KB

**Smaller buffers:**

```fortran
C Current
      INTEGER INBUF(80), FSTR(80), TOKENS(100,4)
C Smaller
      INTEGER INBUF(40), FSTR(40), TOKENS(50,4)
```

Estimated savings: ~0.5KB data

---

### Optimization Strategy 2: Assembly Language (Primary - Required for 32KB Target)

Assembly provides the biggest wins for tight, frequently-called routines. Based on actual small build file sizes:

| Module                 | Lines | Active | FORTRAN Est. | Assembly Est. | Savings     | Priority   |
| ---------------------- | -----:| ------:| ------------:| -------------:| -----------:| ---------- |
| STRUTIL.FOR            | 583   | 276    | ~1.7 KB      | ~0.3 KB       | **1.4 KB**  | 1 - Easy   |
| IOUNIX.FOR             | 174   | 82     | ~0.5 KB      | ~0.1 KB       | **0.4 KB**  | 2 - Easy   |
| PROTVT100.FOR          | 346   | 183    | ~1.1 KB      | ~0.2 KB       | **0.9 KB**  | 3 - Easy   |
| TERMINAL.FOR           | 345   | 134    | ~0.8 KB      | ~0.2 KB       | **0.6 KB**  | 4 - Easy   |
| MSG.FOR                | 254   | 149    | ~0.9 KB      | ~0.2 KB       | **0.7 KB**  | 5 - Easy   |
| Hash ops (CELLS)       | ~150  | ~80    | ~0.5 KB      | ~0.1 KB       | **0.4 KB**  | 6 - Medium |
| **Easy modules total** | 1,702 | 824    | ~5.0 KB      | ~1.0 KB       | **~4.0 KB** |            |
| Parser tokenizer       | 521   | 294    | ~1.8 KB      | ~0.6 KB       | **1.2 KB**  | 7 - Medium |
| Evaluator stack        | 272   | 137    | ~0.8 KB      | ~0.4 KB       | **0.4 KB**  | 8 - Hard   |

**Realistic assembly savings (easy + medium): ~5-6 KB**

This brings the estimated 8-bit code from 27-37KB down to **22-32KB**, achieving the 32KB target.

**Why These Are Good Candidates:**

*STRUTIL (Best ROI):*

```fortran
C FORTRAN: ~20 lines for string copy
      SUBROUTINE STRCPY(DST, SRC, LEN)
      INTEGER DST(*), SRC(*), LEN, I
      DO 10 I = 1, LEN
        DST(I) = SRC(I)
10    CONTINUE
      RETURN
      END
```

```z80
; Z80 Assembly: 6 bytes
STRCPY: LD BC,(LEN)
        LDIR
        RET
```

*Terminal I/O (Easy, high impact):*

```z80
; Z80: Output character - 8 bytes vs ~50 in FORTRAN
TMPUTC: LD A,L        ; char in HL
        RST 08H       ; or CALL BDOS
        RET
```

*Hash function:*

```z80
; Z80: CELHSH - ~20 bytes vs ~100 in FORTRAN
CELHSH: LD A,H        ; col
        LD D,0
        LD E,A
        ADD HL,HL     ; col*2
        ADD HL,HL     ; col*4
        ...           ; col*257 + row, MOD hashsz
```

---

### Optimization Strategy 3: Overlay System (Limited Value)

**Practical Reality:**

Overlays require disk access, which is unacceptable for frequent operations. On a floppy-based system:

- Disk seek: 100-300ms
- Track read: 200-500ms
- Total: 300-800ms per overlay load

Users will not tolerate floppy disk waits for common operations like:

- Insert/delete row or column (frequent during editing)
- Copy command (used constantly)
- Column width changes

**Only Viable Overlay Candidate: File I/O**

File operations already involve disk access, so users expect to wait. These are also infrequent (typically once per session).

| Module                     | Lines        | Est. Code Size   |
| -------------------------- | ------------:| ----------------:|
| FILESAV.FOR                | 599          | ~3 KB            |
| FILELOAD.FOR               | 673          | ~3.5 KB          |
| **Total if both resident** | 1,272        | ~6.5 KB          |
| **Overlay approach**       | max(599,673) | ~3.5 KB reserved |
| Overlay loader code        |              | ~0.5 KB          |
| **Net Savings**            |              | **~2.5 KB**      |

**Critical Constraint:** Must always reserve space for the largest overlay (FILELOAD at ~3.5KB). Otherwise a user could create a spreadsheet but not have room to load the code to save it.

**Verdict:** Overlays provide modest savings (~2.5KB) with added complexity. Assembly language rewrites provide better ROI and should be prioritized first

---

### Optimization Strategy 4: Feature Reduction (NOT AN OPTION)

All current features must be retained - no feature cuts allowed:

- ✓ Row/column insert/delete - keep
- ✓ Copy command with reference adjustment - keep
- ✓ Column widths - keep
- ✓ Functions (@SUM, @AVG, @MIN, @MAX, @COUNT) - keep
- ✓ Absolute cell references ($A$1) - keep
- ✓ 255×255 grid - keep
- ✓ Binary file I/O - keep

Assembly language optimization is the path to 32KB, not feature cuts.

---

### Realistic 32KB Configuration (Apple II+ / CP/M Target)

Based on actual small build (4,566 active lines @ 6 bytes/line) with assembly optimizations:

| Component                      | Active Lines | FORTRAN Est. | With Assembly | Savings   |
| ------------------------------ | ------------:| ------------:| -------------:| ---------:|
| Layer 0: STRUTIL               | 276          | 1.7 KB       | 0.3 KB        | 1.4 KB    |
| Layer 1: Engine                | 1,557        | 9.3 KB       | 8.3 KB        | 1.0 KB    |
| Layer 2: UI/Files (binary I/O) | 1,237        | 7.4 KB       | 6.4 KB        | 1.0 KB    |
| Layer 3: Terminal              | 581          | 3.5 KB       | 1.5 KB        | 2.0 KB    |
| Main                           | 915          | 5.5 KB       | 5.0 KB        | 0.5 KB    |
| **Code Subtotal**              | **4,566**    | **~27 KB**   | **~21 KB**    | **~6 KB** |
| Data (small config)            | -            | 6 KB         | 6 KB          | -         |
| **Grand Total**                |              | **~33 KB**   | **~27 KB**    |           |

**Status:** At 33 KB total, small build fits on 48KB systems with ~7 KB headroom (for stack/runtime).
With assembly optimizations (STRUTIL + Terminal = ~3.4 KB savings), total drops to ~30 KB.

### Optimization Summary

| Strategy             | Effort | Savings | Status          |
| -------------------- | ------ | ------- | --------------- |
| Binary file format   | Done   | ~3 KB   | ✓ Implemented   |
| Assembly STRUTIL     | Low    | 1.4 KB  | Recommended     |
| Assembly Terminal/IO | Low    | 2.0 KB  | Recommended     |
| Assembly MSG         | Low    | 0.7 KB  | Recommended     |
| Assembly hash ops    | Medium | 0.4 KB  | If needed       |
| Assembly parser      | Medium | 1.2 KB  | If needed       |
| Feature removal      | -      | -       | **Not allowed** |

**Priority:** STRUTIL + Terminal I/O + MSG in assembly saves ~4KB with low effort, bringing code to ~23KB and total to ~29KB.

---

### Recommended Implementation Phases

**Phase 1: Easy assembly wins (save ~4KB)**

1. STRUTIL.FOR → Z80/6502 assembly (string copy, compare, convert)
2. IOUNIX.FOR → Assembly (character I/O is trivial)
3. PROTVT100.FOR → Assembly (escape sequence output)
4. MSG.FOR → Assembly (message table lookup)

**Phase 2: If still needed (save ~1.6KB)**

1. Hash operations in CELLS.FOR → Assembly
2. Parser tokenizer → Assembly (character classification loops)

---

## Source Code Documentation

### Print Specifications

- **Paper:** 8.5" × 11" (US Letter)
- **Font:** 12 point Courier (monospace)
- **Spacing:** Double-spaced
- **Margins:** 1" all sides
- **Lines per page:** 27

---

### Layer 0: Utilities

| File              | Purpose                      | Lines   | Pages  |
| ----------------- | ---------------------------- | -------:| ------:|
| STRUTIL.FOR       | String utilities, conversion | 583     | 22     |
| **Layer 0 Total** |                              | **583** | **22** |

---

### Layer 1: Engine (Core Spreadsheet Logic)

| File              | Purpose                               | Lines     | Pages   |
| ----------------- | ------------------------------------- | ---------:| -------:|
| CELLS.FOR         | Cell storage, hash table, row/col ops | 1,449     | 54      |
| PARSE.FOR         | Formula parser, shunting-yard         | 506       | 19      |
| DEPS.FOR          | Dependency tracking                   | 335       | 12      |
| EVAL.FOR          | Expression evaluator                  | 272       | 10      |
| RECALC.FOR        | Recalculation engine                  | 189       | 7       |
| **Layer 1 Total** |                                       | **2,751** | **102** |

---

### Layer 2: User Interface & File I/O

| File              | Purpose                           | Lines     | Pages   |
| ----------------- | --------------------------------- | ---------:| -------:|
| DISPLAY.FOR       | Screen rendering, variable widths | 939       | 35      |
| FILELOAD.FOR      | JSON file parser, column widths   | 673       | 25      |
| FILESAV.FOR       | JSON file writer, column widths   | 599       | 22      |
| MSG.FOR           | Status messages                   | 254       | 9       |
| UI.FOR            | UI state, recalc mode, col widths | 380       | 14      |
| FILES.FOR         | File utilities                    | 179       | 7       |
| **Layer 2 Total** |                                   | **3,024** | **112** |

*Note: COMMANDS.FOR (203 lines) excluded as dead code*

---

### Layer 3: Terminal & Platform I/O

| File              | Purpose               | Lines     | Pages   |
| ----------------- | --------------------- | ---------:| -------:|
| TERMCPV.FOR       | Xerox CP-V terminal   | 514       | 19      |
| TERMANSI.FOR      | ANSI terminal         | 466       | 17      |
| TERMNAT.FOR       | Native macOS terminal | 382       | 14      |
| PROTVT100.FOR     | VT-100 protocol       | 346       | 13      |
| TERMINAL.FOR      | Terminal abstraction  | 345       | 13      |
| PROTVT52.FOR      | VT-52 protocol        | 225       | 8       |
| FIOUNIX.FOR       | Unix file I/O         | 178       | 7       |
| IORSX.FOR         | RSX-11M I/O           | 159       | 6       |
| IOUNIX.FOR        | Unix console I/O      | 149       | 6       |
| **Layer 3 Total** |                       | **2,764** | **103** |

*Note: Only one terminal variant is linked per build (~400-500 lines)*

---

### Main Program

| File           | Purpose                         | Lines     | Pages  |
| -------------- | ------------------------------- | ---------:| ------:|
| XLMAIN.FOR     | Main program, commands, parsers | 1,432     | 53     |
| BRIDGE.FOR     | External function wrappers      | 78        | 3      |
| **Main Total** |                                 | **1,510** | **56** |

---

### Summary by Layer (All Source)

| Layer     | Description  | Lines      | Pages   | % of Total |
| --------- | ------------ | ----------:| -------:| ----------:|
| Layer 0   | Utilities    | 583        | 22      | 5%         |
| Layer 1   | Engine       | 2,751      | 102     | 26%        |
| Layer 2   | UI & Files   | 3,024      | 112     | 29%        |
| Layer 3   | Terminal I/O | 2,764      | 103     | 26%        |
| Main      | Program      | 1,510      | 56      | 14%        |
| **TOTAL** |              | **10,632** | **395** | **100%**   |

*Note: Line counts increased due to recalc modes, column widths, and row/col operations*

---

### Physical Document Characteristics (All Source)

| Metric               | Value                  |
| -------------------- | ---------------------- |
| Total Lines          | 10,632                 |
| Total Pages          | 395                    |
| Paper Weight (20 lb) | ~1.8 lbs (0.8 kg)      |
| Stack Height         | ~1.5 inches (38 mm)    |
| Binder Size Needed   | 1.5" three-ring binder |

---

### Typical Build (One Terminal Variant)

For a single-platform build (excluding alternate terminal drivers):

| Component                        | Lines      | Pages    |
| -------------------------------- | ----------:| --------:|
| Layer 0: Utilities               | 583        | 22       |
| Layer 1: Engine                  | 2,751      | 102      |
| Layer 2: UI & Files              | 3,024      | 112      |
| Layer 3: One terminal (~450 avg) | ~450       | ~17      |
| Main Program                     | 1,510      | 56       |
| **Build Total**                  | **~8,318** | **~309** |

A complete single-platform build prints to approximately **310 pages** or about **1.2 inches of paper**.

---

### Comparison to Historical Software

| Software           | Approx. Lines | Pages (est.) |
| ------------------ | ------------- | ------------ |
| **XL Spreadsheet** | 8,318         | 309          |
| VisiCalc (1979)    | ~5,000        | ~185         |
| Early Lotus 1-2-3  | ~50,000       | ~1,850       |
| WordStar 1.0       | ~15,000       | ~555         |
| CP/M 2.2           | ~8,000        | ~296         |

XL is comparable in size to CP/M 2.2 and larger than VisiCalc due to
additional features (functions, variable widths, row/col operations, file I/O).

---

## Deployment 1.0 Release (2026-01-21)

### Overview

Deployment 1.0 removes dead code (COMMANDS.FOR - 203 lines of functions never called) and provides clean executables with only functioning code.

**Location:** `/Volumes/SECURE8/git/spreadsheet-fortran/deployment_1.0/`

### Executables

| Variant   | Cells | Size          | Reduction from Dev |
| --------- | ----- | ------------- | ------------------ |
| xl_small  | 100   | 150,088 bytes | -240 bytes         |
| xl_medium | 300   | 150,088 bytes | -240 bytes         |
| xl_large  | 2,000 | 150,088 bytes | -240 bytes         |

---

### Line Count Analysis (Active vs Comments vs Blank)

#### By Layer

| Layer                   | Active    | Comments  | Blank     | Total     |
| ----------------------- | ---------:| ---------:| ---------:| ---------:|
| **Layer 0: Utilities**  | 276       | 241       | 66        | 583       |
| **Layer 1: Engine**     | 1,130     | 613       | 364       | 2,107     |
| **Layer 2: UI & Files** | 1,438     | 654       | 408       | 2,500     |
| **Layer 3: Terminal**   | 465       | 357       | 196       | 1,018     |
| **Main Program**        | 594       | 236       | 154       | 984       |
| **TOTAL**               | **3,903** | **2,101** | **1,188** | **7,192** |

#### By File (Detail)

| File          | Active | Comments | Blank | Total |
| ------------- | ------:| --------:| -----:| -----:|
| **Layer 0**   |        |          |       |       |
| STRUTIL.FOR   | 276    | 241      | 66    | 583   |
| **Layer 1**   |        |          |       |       |
| CELLS.FOR     | 481    | 213      | 159   | 853   |
| PARSE.FOR     | 294    | 134      | 78    | 506   |
| DEPS.FOR      | 168    | 107      | 60    | 335   |
| EVAL.FOR      | 137    | 91       | 44    | 272   |
| RECALC.FOR    | 50     | 68       | 23    | 141   |
| **Layer 2**   |        |          |       |       |
| DISPLAY.FOR   | 415    | 203      | 116   | 734   |
| FILELOAD.FOR  | 389    | 118      | 96    | 603   |
| FILESAV.FOR   | 322    | 88       | 76    | 486   |
| MSG.FOR       | 149    | 61       | 44    | 254   |
| UI.FOR        | 117    | 77       | 50    | 244   |
| FILES.FOR     | 46     | 107      | 26    | 179   |
| **Layer 3**   |        |          |       |       |
| PROTVT100.FOR | 183    | 105      | 58    | 346   |
| TERMINAL.FOR  | 134    | 133      | 78    | 345   |
| FIOUNIX.FOR   | 98     | 50       | 30    | 178   |
| IOUNIX.FOR    | 50     | 69       | 30    | 149   |
| **Main**      |        |          |       |       |
| XLMAIN.FOR    | 560    | 207      | 139   | 906   |
| BRIDGE.FOR    | 34     | 29       | 15    | 78    |

---

### Excluded Dead Code

| File         | Active | Comments | Blank | Total | Reason                 |
| ------------ | ------:| --------:| -----:| -----:| ---------------------- |
| COMMANDS.FOR | 69     | 99       | 35    | 203   | Functions never called |

---

### Summary Statistics

| Metric                | Value         |
| --------------------- | ------------- |
| **Active code lines** | 3,903 (54.2%) |
| **Comment lines**     | 2,101 (29.2%) |
| **Blank lines**       | 1,188 (16.5%) |
| **Total lines**       | 7,192         |

---

### Printed Documentation (12pt Courier, double-spaced)

| Metric                         | Pages       |
| ------------------------------ | -----------:|
| **Active code only**           | 145         |
| **All lines**                  | 267         |
| **Stack height (active only)** | ~0.6 inches |
| **Stack height (all lines)**   | ~1.0 inches |

---

### Development vs Deployment 1.0 Comparison

| Metric          | Development | Deployment 1.0 | Saved     |
| --------------- | -----------:| --------------:| ---------:|
| Total lines     | 7,395       | 7,192          | 203       |
| Active code     | 3,972       | 3,903          | 69        |
| Executable size | 150,328     | 150,088        | 240 bytes |
| Dead code files | 1           | 0              | 1 file    |
