# XL Spreadsheet Development Analysis

## 2026-01-23 (Updated)

Product development metrics, implemented features, and release documentation.

**Latest refactoring:** Layer architecture reorganized. Layer 0 now contains platform-specific I/O (unix/, rsx/). Layer 1 contains portable core computation. Layer 2 contains portable application logic. Layer 3 contains terminal/device abstraction. XLMAIN.FOR slimmed from 1,443 to 114 lines with routines distributed to appropriate modules.

---

## Implemented Features

### Core Architecture

- **Multi-layered design** for portability:
  
  - Layer 0: Platform I/O (Unix, RSX-11M, CP-V specific) - **platform-dependent**
  - Layer 1: Core computation (cells, parsing, evaluation, dependencies, string utilities) - **portable FORTRAN IV/66**
  - Layer 2: Application logic (UI, display, commands, file I/O) - **portable FORTRAN IV/66**
  - Layer 3: Terminal/device abstraction (protocols, rendering) - **portable with platform I/O hooks**

- **Portability guarantee:** Layers 1 and 2 compile unmodified on any platform with FORTRAN IV/66 (CP-V, RSX-11M, Unix, CP/M, Apple II)

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

## Platform Portability Analysis

### Terminal I/O Models

Our architecture assumes character-mode terminal I/O (VT-100 style). Other platforms use fundamentally different models:

| Model                   | How It Works                                                    | Examples                 |
| ----------------------- | --------------------------------------------------------------- | ------------------------ |
| **Terminal Stream**     | Send escape sequences to position cursor, characters to display | VT-100, VT-52, ADM-3A    |
| **Memory-Mapped Video** | Write bytes directly to RAM addresses                           | Apple II, TRS-80, IBM PC |
| **Block-Mode Terminal** | Define fields, transmit whole screen on Enter                   | IBM 3270                 |

---

### Memory-Mapped Video Platforms

Personal computers of the late 1970s/early 1980s used memory-mapped video—write a byte to a specific RAM address and it appears on screen instantly.

#### Apple II+ (40×24)

```
Screen RAM: $0400-$07FF (1KB)
BUT: Interleaved in 8-line groups!

Row 0:  $0400    Row 8:  $0428    Row 16: $0450
Row 1:  $0480    Row 9:  $04A8    Row 17: $04D0
...
Address = base[row MOD 8] + (row DIV 8) * 40 + col
```

- No lowercase without hardware modification
- High bit controls inverse/flash video
- 40 columns only (80-column card was extra-cost option)
- **No arrow keys on original keyboard!**

#### TRS-80 Model I/III (64×16)

```
Screen RAM: $3C00-$3FFF (1KB)
Linear organization:
Address = $3C00 + (row * 64) + col
```

- Uppercase only on Model I
- Simple, predictable memory layout
- Semigraphics characters available

#### IBM PC (80×25)

```
Screen RAM: $B800:0000 (CGA) or $B000:0000 (MDA)
2 bytes per character (char + attribute):
Address = $B800:0000 + (row * 160) + (col * 2)
```

- Full per-character attributes (color, blink, intensity)
- 80 columns standard
- BIOS INT 10h available as alternative to direct memory

---

### IBM Mainframe 3270 Terminals (1978)

The IBM 3270 family (3278/3279 displays) used a fundamentally different **block-mode** architecture:

| Aspect             | IBM 3270                                   | VT-100                                 |
| ------------------ | ------------------------------------------ | -------------------------------------- |
| Mode               | Block-mode                                 | Character-mode                         |
| Transmission       | Entire screen sent on Enter                | Each keystroke sent immediately        |
| Connection         | Coax → cluster controller → channel        | Point-to-point RS-232 serial           |
| Character set      | EBCDIC                                     | ASCII                                  |
| Screen model       | Field-oriented (protected/unprotected)     | Character stream with escapes          |
| Local intelligence | Cursor movement, field tabbing, validation | Minimal escape sequence interpretation |

**The practical difference:** On a 3270, users fill out a form locally, then transmit the whole thing. On a VT-100, the host sees every character and must echo it back.

**Impact on XL:** The 3270's block-mode architecture would require a complete UI redesign—not just a new Layer 3, but rethinking how data entry and navigation work.

---

### Layer 3 Abstraction for Different Platforms

Our Layer 3 defines platform-independent primitives:

| Routine | VT-100                               | Memory-Mapped                               | 3270                      |
| ------- | ------------------------------------ | ------------------------------------------- | ------------------------- |
| TMINIT  | Set terminal raw mode                | Set video mode, clear screen                | Open VTAM session         |
| TMCLER  | Send ESC[2J                          | Fill screen RAM with spaces                 | Send erase/write          |
| TMGOTO  | Send ESC[row;colH                    | Calculate address for next write            | Set buffer address        |
| TMPUTC  | Serial character output              | POKE to screen memory                       | Build output buffer       |
| TMATTR  | Send ESC[attr;...m                   | Set attribute byte (PC) or high bit (Apple) | Set field attribute       |
| TMGETC  | Read serial + parse escape sequences | Read keyboard port/memory                   | **Receive entire screen** |
| TMFLSH  | Flush serial buffer                  | No-op (instant)                             | Transmit buffer           |

**Layers 0, 1, and 2 remain unchanged** for memory-mapped platforms. The 3270 would require Layer 2 UI changes.

---

### FORTRAN and Direct Memory Access

FORTRAN IV cannot write to arbitrary memory addresses. Memory-mapped platforms require **assembly language helpers**:

```z80
; Z80 (TRS-80): Write char to screen - ~15 bytes
POKSCR: LD HL,(ROW)      ; get row
        LD DE,64
        CALL MULT        ; row * 64
        LD DE,(COL)
        ADD HL,DE        ; + col
        LD DE,$3C00      ; + screen base
        ADD HL,DE
        LD A,(CHAR)
        LD (HL),A        ; poke to screen
        RET
```

```asm
; 8086 (IBM PC): Write char to screen - ~25 bytes
POKSCR  PROC
        MOV AX,[ROW]
        MOV BX,160
        MUL BX           ; row * 160
        MOV BX,[COL]
        SHL BX,1         ; col * 2
        ADD AX,BX
        MOV DI,AX
        MOV AX,0B800H
        MOV ES,AX
        MOV AL,[CHAR]
        STOSB            ; store char (skip attribute)
        RET
POKSCR  ENDP
```

The VT-100 approach requires only standard FORTRAN I/O (plus a small C helper for raw terminal mode on Unix).

---

### Keyboard Input Differences

| Platform  | Method                           | Arrow Keys            |
| --------- | -------------------------------- | --------------------- |
| VT-100    | Serial read, parse ESC sequences | ESC [ A/B/C/D         |
| Apple II+ | Read $C000, strobe $C010         | **None on original!** |
| TRS-80    | Memory-mapped keyboard matrix    | Direct scan codes     |
| IBM PC    | INT 16h or port 60h              | Extended scan codes   |
| IBM 3270  | Receive modified fields on Enter | **Local only**        |

**Apple II+ issue:** Original keyboard lacks arrow keys. Would need I/J/K/M or similar for navigation, or require aftermarket keyboard.

**IBM 3270 issue:** Arrow keys work locally for field navigation, but host doesn't see individual keystrokes—only the final screen state.

---

### Platform Portability Summary

| Platform        | Layers 0-1  | Layer 2            | Layer 3 | Assembly Needed | Estimate  |
| --------------- | ----------- | ------------------ | ------- | --------------- | --------- |
| **VT-100/Unix** | ✅ No change | ✅ No change        | ✅ Done  | ~50 lines C     | Done      |
| **IBM PC DOS**  | ✅ No change | ✅ No change        | Rewrite | ~200 lines ASM  | 1-2 weeks |
| **TRS-80 CP/M** | ✅ No change | ✅ No change        | Rewrite | ~150 lines ASM  | 2 weeks   |
| **Apple II+**   | ✅ No change | Minor (no arrows)  | Rewrite | ~300 lines ASM  | 3-4 weeks |
| **IBM 3270**    | ✅ No change | **Major redesign** | Rewrite | ~500 lines      | 4-6 weeks |

**Key insight:** Memory-mapped personal computers (Apple II, TRS-80, IBM PC) need assembly helpers for screen I/O, but the conceptual model matches VT-100—character-at-a-time interaction with immediate feedback. The IBM 3270's block-mode requires rethinking the entire user interaction model.

---

### IBM 3270 Research Questions

Porting to IBM mainframes (System/370, 303x series) requires answering:

1. **Character-mode alternatives:** Did VM/CMS support character-at-a-time I/O via DIAGNOSE? Were ASCII terminals supported via protocol converters (3708/7171)?

2. **FORTRAN terminal I/O:** What did IBM FORTRAN IV G/H compilers provide? Could FORTRAN call TPUT/TGET macros? When did GDDM ship?

3. **3270-native UI design:** How would a field-oriented spreadsheet work? Protected fields for display, unprotected for input, transmit on Enter?

4. **Existing examples:** How did XEDIT (CMS full-screen editor) and early ISPF handle 3270 interaction?

5. **EBCDIC conversion:** Character codes differ (A=193 vs 65), collating sequence differs (numbers after letters).

---

## Source Code Documentation

### Print Specifications

- **Paper:** 8.5" × 11" (US Letter)
- **Font:** 12 point Courier (monospace)
- **Spacing:** Double-spaced
- **Margins:** 1" all sides
- **Lines per page:** 27

---

### Layer 0: Platform I/O

Platform-specific implementations. Only one set linked per build.

**Unix Platform:**

| File             | Purpose          | Lines   | Pages  |
| ---------------- | ---------------- | -------:| ------:|
| unix/IOUNIX.FOR  | Unix console I/O | 174     | 6      |
| unix/FIOUNIX.FOR | Unix file I/O    | 343     | 13     |
| **Unix Total**   |                  | **517** | **19** |

**RSX-11M Platform:**

| File          | Purpose     | Lines   | Pages |
| ------------- | ----------- | -------:| -----:|
| rsx/IORSX.FOR | RSX-11M I/O | 159     | 6     |
| **RSX Total** |             | **159** | **6** |

---

### Layer 1: Core Computation (Portable)

| File              | Purpose                               | Lines     | Pages   |
| ----------------- | ------------------------------------- | ---------:| -------:|
| CELLS.FOR         | Cell storage, hash table, row/col ops | 1,449     | 54      |
| PARSE.FOR         | Formula parser, shunting-yard         | 1,083     | 40      |
| STRUTIL.FOR       | String utilities, conversion          | 666       | 25      |
| EVAL.FOR          | Expression evaluator                  | 431       | 16      |
| DEPS.FOR          | Dependency tracking                   | 362       | 13      |
| RECALC.FOR        | Recalculation engine                  | 189       | 7       |
| **Layer 1 Total** |                                       | **4,180** | **155** |

---

### Layer 2: Application Logic (Portable)

| File              | Purpose                            | Lines     | Pages   |
| ----------------- | ---------------------------------- | ---------:| -------:|
| COMMANDS.FOR      | Command mode handlers, copy, width | 782       | 29      |
| UI.FOR            | Navigation, entry, mode handlers   | 730       | 27      |
| FILELOAD.FOR      | JSON file parser, column widths    | 673       | 25      |
| FILESAV.FOR       | JSON file writer, column widths    | 599       | 22      |
| FILEBIN.FOR       | Binary file format                 | 408       | 15      |
| DISPLAY.FOR       | Screen rendering, variable widths  | 345       | 13      |
| MSG.FOR           | Status messages                    | 254       | 9       |
| FILES.FOR         | File utilities                     | 179       | 7       |
| XLMAIN.FOR        | Main program entry point           | 114       | 4       |
| FILEBINL.FOR      | Binary file loader                 | 71        | 3       |
| FILEBINS.FOR      | Binary file saver                  | 40        | 2       |
| **Layer 2 Total** |                                    | **4,195** | **156** |

*Note: XLMAIN.FOR refactored from 1,443 to 114 lines; routines distributed to COMMANDS.FOR and UI.FOR*

---

### Layer 3: Terminal/Device Abstraction

| File              | Purpose               | Lines     | Pages   |
| ----------------- | --------------------- | ---------:| -------:|
| vt100/RENDVT.FOR  | VT-100 rendering      | 790       | 29      |
| tty35/RENDTTY.FOR | Teletype rendering    | 545       | 20      |
| TERMCPV.FOR       | Xerox CP-V terminal   | 514       | 19      |
| TERMANSI.FOR      | ANSI terminal         | 466       | 17      |
| TERMNAT.FOR       | Native macOS terminal | 382       | 14      |
| TERMINAL.FOR      | Terminal driver       | 370       | 14      |
| PROTVT100.FOR     | VT-100 protocol       | 303       | 11      |
| PROTVT52.FOR      | VT-52 protocol        | 225       | 8       |
| **Layer 3 Total** |                       | **3,595** | **133** |

*Note: Only one terminal/protocol/renderer set linked per build (~1,463 lines for VT-100)*

---

### Summary by Layer (All Source)

| Layer     | Description  | Lines      | Pages   | % of Total |
| --------- | ------------ | ----------:| -------:| ----------:|
| Layer 0   | Platform I/O | 517        | 19      | 4%         |
| Layer 1   | Core         | 4,180      | 155     | 35%        |
| Layer 2   | Application  | 4,195      | 156     | 35%        |
| Layer 3   | Terminal*    | 3,595      | 133     | 30%        |
| **TOTAL** |              | **12,487** | **463** | **100%**   |

*Layer 0 shows Unix platform (517 lines); RSX is 159 lines. Layer 3 shows all variants; typical build uses ~1,463 lines.*

---

### Physical Document Characteristics (All Source)

| Metric               | Value                |
| -------------------- | -------------------- |
| Total Lines          | 12,487               |
| Total Pages          | 463                  |
| Paper Weight (20 lb) | ~2.0 lbs (0.9 kg)    |
| Stack Height         | ~1.8 inches (46 mm)  |
| Binder Size Needed   | 2" three-ring binder |

---

### Typical Build (One Terminal Variant)

For a single-platform Unix/VT-100 build:

| Component                                   | Lines      | Pages   |
| ------------------------------------------- | ----------:| -------:|
| Layer 0: Platform I/O (Unix)                | 517        | 19      |
| Layer 1: Core computation                   | 4,180      | 155     |
| Layer 2: Application logic                  | 4,195      | 156     |
| Layer 3: Terminal (TERMINAL+PROTVT100+REND) | 1,463      | 54      |
| **Build Total**                             | **10,355** | **384** |

A complete single-platform build prints to approximately **384 pages** or about **1.5 inches of paper**.

---

### Manual Entry Time Estimates (20 WPM)

Time to type the codebase manually at 20 words per minute (typical for careful code entry). Assumes ~10 words per FORTRAN line on average.

**By Layer (Typical Unix/VT-100 Build):**

| Component                  | Lines      | Est. Words  | Time @ 20 WPM | Hours        |
| -------------------------- | ----------:| -----------:| -------------:| ------------:|
| Layer 0: Platform (Unix)   | 517        | 5,170       | 258 min       | 4.3 hrs      |
| Layer 1: Core              | 4,180      | 41,800      | 2,090 min     | 34.8 hrs     |
| Layer 2: Application       | 4,195      | 41,950      | 2,098 min     | 35.0 hrs     |
| Layer 3: Terminal (VT-100) | 1,463      | 14,630      | 732 min       | 12.2 hrs     |
| **Total Build**            | **10,355** | **103,550** | **5,178 min** | **86.3 hrs** |

**Active Code Only** (excludes comments/blanks):

| Metric        | Value        |
| ------------- | ------------ |
| Active lines  | 5,893        |
| Est. words    | 58,930       |
| Time @ 20 WPM | 2,947 min    |
| **Hours**     | **49.1 hrs** |

**Working Days (8 hrs/day):**

| Scope       | Hours | Days |
| ----------- | -----:| ----:|
| All lines   | 86.3  | ~11  |
| Active code | 49.1  | ~6   |

*Comments and blank lines type faster (less precision needed), so actual time falls between these estimates.*

---

### Paper Tape Storage (ASR-33 Teletype)

Storage requirements for 8-bit paper tape at 10 characters/inch. Punch/read speed: 10 bytes/second. Rolled oiled paper tape (950 ft): $2.25 (1977 prices).

**By Layer (Typical Unix/VT-100 Build):**

| Layer                    | Bytes       | Tape Length  | Rolls    | Cost (1977) | Punch Time  |
| ------------------------ | -----------:| ------------:| --------:| -----------:| -----------:|
| Layer 0: Platform (Unix) | 14,185      | 118 ft       | 0.12     | $0.28       | 24 min      |
| Layer 1: Core            | 111,020     | 925 ft       | 0.97     | $2.19       | 3.1 hrs     |
| Layer 2: Application     | 112,064     | 934 ft       | 0.98     | $2.21       | 3.1 hrs     |
| Layer 3: Terminal        | 39,880      | 332 ft       | 0.35     | $0.79       | 1.1 hrs     |
| **Total Build**          | **277,149** | **2,310 ft** | **2.43** | **$5.47**   | **7.7 hrs** |

**Largest Individual Files:**

| File         | Bytes  | Tape Length | Punch Time |
| ------------ | ------:| -----------:| ----------:|
| CELLS.FOR    | 38,036 | 317 ft      | 63 min     |
| PARSE.FOR    | 30,064 | 251 ft      | 50 min     |
| RENDVT.FOR   | 22,684 | 189 ft      | 38 min     |
| COMMANDS.FOR | 22,222 | 185 ft      | 37 min     |
| UI.FOR       | 20,011 | 167 ft      | 33 min     |

**Physical Perspective:**

| Metric                | Value                            |
| --------------------- | -------------------------------- |
| Total tape length     | 2,310 ft (770 yards, 704 meters) |
| Distance              | ~0.4 miles                       |
| Rolls needed (950 ft) | 3 rolls                          |
| Media cost (1977)     | $6.75 (3 × $2.25)                |
| Punch/read time       | 7 hours 42 minutes               |

*The entire spreadsheet fits on 3 rolls of paper tape and costs under $7 in media—but takes a full workday to punch or read.*

---

### Comparison to Historical Software

| Software           | Approx. Lines | Pages (est.) |
| ------------------ | ------------- | ------------ |
| **XL Spreadsheet** | 10,355        | 384          |
| VisiCalc (1979)    | ~5,000        | ~185         |
| Early Lotus 1-2-3  | ~50,000       | ~1,850       |
| WordStar 1.0       | ~15,000       | ~555         |
| CP/M 2.2           | ~8,000        | ~296         |

XL is larger than VisiCalc and CP/M 2.2 due to comprehensive features (row/col ops, formula copy with reference adjustment, variable widths, multiple file formats, functions).

---

## Current Release (2026-01-23)

### Overview

Major architecture refactoring completed:

- **Layer 0** reorganized into platform subdirectories (unix/, rsx/)
- **XLMAIN.FOR** slimmed from 1,443 to 114 lines
- **COMMANDS.FOR** now fully implemented (782 lines, was 203-line stub)
- **STRUTIL.FOR** moved from Layer 0 to Layer 1 (portable core)
- **BRIDGE.FOR** deleted (no longer needed)

---

### Line Count Analysis (Active vs Comments vs Blank)

#### By Layer (Typical Unix/VT-100 Build)

| Layer                    | Active    | Comments  | Blank     | Total      |
| ------------------------ | ---------:| ---------:| ---------:| ----------:|
| **Layer 0: Platform**    | 264       | 154       | 99        | 517        |
| **Layer 1: Core**        | 2,403     | 1,139     | 638       | 4,180      |
| **Layer 2: Application** | 2,461     | 1,058     | 676       | 4,195      |
| **Layer 3: Terminal**    | 765       | 440       | 258       | 1,463      |
| **TOTAL**                | **5,893** | **2,791** | **1,671** | **10,355** |

#### By File (Detail)

| File             | Active | Comments | Blank | Total |
| ---------------- | ------:| --------:| -----:| -----:|
| **Layer 0**      |        |          |       |       |
| unix/IOUNIX.FOR  | 61     | 77       | 36    | 174   |
| unix/FIOUNIX.FOR | 203    | 77       | 63    | 343   |
| **Layer 1**      |        |          |       |       |
| CELLS.FOR        | 869    | 319      | 261   | 1,449 |
| PARSE.FOR        | 704    | 231      | 148   | 1,083 |
| STRUTIL.FOR      | 313    | 275      | 78    | 666   |
| EVAL.FOR         | 255    | 123      | 53    | 431   |
| DEPS.FOR         | 182    | 115      | 65    | 362   |
| RECALC.FOR       | 80     | 76       | 33    | 189   |
| **Layer 2**      |        |          |       |       |
| COMMANDS.FOR     | 531    | 150      | 101   | 782   |
| UI.FOR           | 414    | 189      | 127   | 730   |
| FILELOAD.FOR     | 434    | 132      | 107   | 673   |
| FILESAV.FOR      | 406    | 103      | 90    | 599   |
| FILEBIN.FOR      | 233    | 103      | 72    | 408   |
| DISPLAY.FOR      | 164    | 113      | 68    | 345   |
| MSG.FOR          | 149    | 61       | 44    | 254   |
| FILES.FOR        | 46     | 107      | 26    | 179   |
| XLMAIN.FOR       | 39     | 52       | 23    | 114   |
| FILEBINL.FOR     | 31     | 28       | 12    | 71    |
| FILEBINS.FOR     | 14     | 20       | 6     | 40    |
| **Layer 3**      |        |          |       |       |
| vt100/RENDVT.FOR | 468    | 202      | 120   | 790   |
| TERMINAL.FOR     | 146    | 140      | 84    | 370   |
| PROTVT100.FOR    | 151    | 98       | 54    | 303   |

---

### Summary Statistics

| Metric                | Value         |
| --------------------- | ------------- |
| **Active code lines** | 5,893 (56.9%) |
| **Comment lines**     | 2,791 (26.9%) |
| **Blank lines**       | 1,671 (16.1%) |
| **Total lines**       | 10,355        |

---

### Printed Documentation (12pt Courier, double-spaced)

| Metric                         | Pages       |
| ------------------------------ | -----------:|
| **Active code only**           | 218         |
| **All lines**                  | 384         |
| **Stack height (active only)** | ~0.9 inches |
| **Stack height (all lines)**   | ~1.5 inches |

---

### Key Refactoring Changes (1.0 → 2.0)

| File/Module  | Before     | After   | Change                                       |
| ------------ | ----------:| -------:| -------------------------------------------- |
| XLMAIN.FOR   | 1,443      | 114     | -1,329 lines (routines distributed)          |
| COMMANDS.FOR | 203 (stub) | 782     | +579 lines (fully implemented)               |
| UI.FOR       | 380        | 730     | +350 lines (added NAVKEY, ENTKEY, HLPSHW)    |
| PARSE.FOR    | 506        | 1,083   | +577 lines (added CPYADJ, parsing utilities) |
| STRUTIL.FOR  | layer0     | layer1  | Moved to portable layer                      |
| BRIDGE.FOR   | 78         | deleted | -78 lines (no longer needed)                 |
