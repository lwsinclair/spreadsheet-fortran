# XL Spreadsheet Development Analysis
## 2026-01-21

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
- Automatic recalculation when cell values change
- Propagates changes to dependent cells

### Display System (Layer 2 - DISPLAY.FOR)
- 24-line VT-52/VT-100 terminal layout:
  - Row 1: Status line (cell reference, mode)
  - Row 2: Column headers (A-H visible)
  - Row 3: Separator line
  - Rows 4-23: Grid (20 rows x 8 columns visible)
  - Row 24: Edit line / Status messages
- Viewport scrolling (follows cursor)
- Reverse video highlighting for current cell
- 9-character cell width (1 border + 8 content)
- Right-aligned numbers, left/right/center text

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

## Memory Analysis & Platform Requirements

### Configuration Parameters

| Parameter | Small | Medium | Large |
|-----------|-------|--------|-------|
| MAXCEL (cells) | 100 | 300 | 2,000 |
| HASHSZ (hash buckets) | 64 | 256 | 1,024 |
| MAXSTR (string pool) | 500 | 2,000 | 10,000 |
| MAXDEP (dependencies) | 50 | 150 | 1,000 |
| MAXTOK (tokens/formula) | 25 | 50 | 100 |
| MAXQUE (recalc queue) | 25 | 75 | 500 |

---

### Data Memory by Module (16-bit, 2-byte INTEGER, 4-byte REAL)

| Module | Small | Medium | Large |
|--------|-------|--------|-------|
| **Layer 0: STRUTIL** | 0.2 KB | 0.2 KB | 0.2 KB |
| **Layer 1: CELLS** | 3.4 KB | 11.4 KB | 68.4 KB |
| **Layer 1: DEPS** | 0.5 KB | 1.3 KB | 8.3 KB |
| **Layer 1: PARSE** | 0.2 KB | 0.4 KB | 0.9 KB |
| **Layer 1: EVAL** | 0.1 KB | 0.1 KB | 0.2 KB |
| **Layer 1: RECALC** | 0.2 KB | 0.4 KB | 2.0 KB |
| **Layer 2: UI/Display** | 0.5 KB | 0.5 KB | 0.5 KB |
| **Layer 2: Files** | 0.5 KB | 0.5 KB | 0.5 KB |
| **Layer 3: Terminal** | 0.3 KB | 0.3 KB | 0.3 KB |
| **Total Data** | **~6 KB** | **~15 KB** | **~82 KB** |

---

### Estimated Code Size by Layer

| Layer | Lines | 8-bit (Z80/6502) | 16-bit (PDP-11) |
|-------|-------|------------------|-----------------|
| Layer 0: Utilities | 583 | ~4 KB | ~5 KB |
| Layer 1: Engine | 2,107 | ~15 KB | ~20 KB |
| Layer 2: UI & Files | 2,500 | ~17 KB | ~23 KB |
| Layer 3: Terminal* | ~1,018 | ~7 KB | ~9 KB |
| XLMAIN | 984 | ~7 KB | ~9 KB |
| **Total Code** | ~7,192 | **~50 KB** | **~66 KB** |

*Only one terminal variant linked per build (~400-500 lines)

---

### Total Memory Requirements

| Variant | Data | Code (8-bit) | Code (16-bit) | **Total 8-bit** | **Total 16-bit** |
|---------|------|--------------|---------------|-----------------|------------------|
| Small | 6 KB | 46 KB | 62 KB | **52 KB** | **68 KB** |
| Medium | 15 KB | 46 KB | 62 KB | **61 KB** | **77 KB** |
| Large | 82 KB | 46 KB | 62 KB | **128 KB** | **144 KB** |

Add ~4-8 KB for FORTRAN runtime library and stack.

---

### Platform Compatibility

| Platform | RAM | Usable | Small | Medium | Large |
|----------|-----|--------|-------|--------|-------|
| **Apple II+ (6502)** | 48 KB | ~40 KB | ❌ No | ❌ No | ❌ No |
| **Apple IIe (6502)** | 64 KB | ~52 KB | ⚠️ Tight | ❌ No | ❌ No |
| **Apple IIe + Aux** | 128 KB | ~110 KB | ✅ Yes | ✅ Yes | ❌ No |
| **CP/M (Z80) 48KB** | 48 KB | ~44 KB | ❌ No | ❌ No | ❌ No |
| **CP/M (Z80) 64KB** | 64 KB | ~58 KB | ✅ Yes | ⚠️ Tight | ❌ No |
| **Kaypro, Osborne** | 64 KB | ~58 KB | ✅ Yes | ⚠️ Tight | ❌ No |
| **PDP-11/23 (64KB)** | 64 KB | ~56 KB | ❌ No | ❌ No | ❌ No |
| **PDP-11/23+ (256KB)** | 256 KB | ~200 KB | ✅ Yes | ✅ Yes | ✅ Yes |
| **PDP-11/73 RSX-11M** | 256+ KB | ~200 KB | ✅ Yes | ✅ Yes | ✅ Yes |
| **Xerox Sigma 7 CP-V** | 256+ KB | ~200 KB | ✅ Yes | ✅ Yes | ✅ Yes |
| **TRS-80 Model 4** | 64 KB | ~52 KB | ✅ Yes | ⚠️ Tight | ❌ No |
| **IBM PC (DOS)** | 640 KB | ~500 KB | ✅ Yes | ✅ Yes | ✅ Yes |

**Legend:** ✅ Runs comfortably | ⚠️ Marginal, may need optimization | ❌ Insufficient memory

---

### Platform Recommendations

| Target System | Recommended Variant | Notes |
|--------------|---------------------|-------|
| Apple IIe + 80-col | Small | Needs auxiliary memory card |
| CP/M 64KB | Small | Standard Z80 systems |
| PDP-11 RSX-11M | Medium or Large | Depends on partition size |
| Xerox Sigma 7 CP-V | Large | Plenty of memory |
| IBM PC DOS | Large | 640KB is overkill |

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

| File | Purpose | Lines | Pages |
|------|---------|------:|------:|
| STRUTIL.FOR | String utilities, conversion | 583 | 22 |
| **Layer 0 Total** | | **583** | **22** |

---

### Layer 1: Engine (Core Spreadsheet Logic)

| File | Purpose | Lines | Pages |
|------|---------|------:|------:|
| CELLS.FOR | Cell storage, hash table | 853 | 32 |
| PARSE.FOR | Formula parser, shunting-yard | 506 | 19 |
| DEPS.FOR | Dependency tracking | 335 | 12 |
| EVAL.FOR | Expression evaluator | 272 | 10 |
| RECALC.FOR | Recalculation engine | 141 | 5 |
| **Layer 1 Total** | | **2,107** | **78** |

---

### Layer 2: User Interface & File I/O

| File | Purpose | Lines | Pages |
|------|---------|------:|------:|
| DISPLAY.FOR | Screen rendering, grid | 734 | 27 |
| FILELOAD.FOR | JSON file parser | 603 | 22 |
| FILESAV.FOR | JSON file writer | 486 | 18 |
| MSG.FOR | Status messages | 254 | 9 |
| UI.FOR | UI state management | 244 | 9 |
| FILES.FOR | File utilities | 179 | 7 |
| **Layer 2 Total** | | **2,500** | **93** |

*Note: COMMANDS.FOR (203 lines) excluded as dead code*

---

### Layer 3: Terminal & Platform I/O

| File | Purpose | Lines | Pages |
|------|---------|------:|------:|
| TERMCPV.FOR | Xerox CP-V terminal | 514 | 19 |
| TERMANSI.FOR | ANSI terminal | 466 | 17 |
| TERMNAT.FOR | Native macOS terminal | 382 | 14 |
| PROTVT100.FOR | VT-100 protocol | 346 | 13 |
| TERMINAL.FOR | Terminal abstraction | 345 | 13 |
| PROTVT52.FOR | VT-52 protocol | 225 | 8 |
| FIOUNIX.FOR | Unix file I/O | 178 | 7 |
| IORSX.FOR | RSX-11M I/O | 159 | 6 |
| IOUNIX.FOR | Unix console I/O | 149 | 6 |
| **Layer 3 Total** | | **2,764** | **103** |

*Note: Only one terminal variant is linked per build (~400-500 lines)*

---

### Main Program

| File | Purpose | Lines | Pages |
|------|---------|------:|------:|
| XLMAIN.FOR | Main program, event loop, commands | 906 | 34 |
| BRIDGE.FOR | External function declarations | 78 | 3 |
| **Main Total** | | **984** | **37** |

---

### Summary by Layer (All Source)

| Layer | Description | Lines | Pages | % of Total |
|-------|-------------|------:|------:|----------:|
| Layer 0 | Utilities | 583 | 22 | 6% |
| Layer 1 | Engine | 2,107 | 78 | 23% |
| Layer 2 | UI & Files | 2,703 | 100 | 30% |
| Layer 3 | Terminal I/O | 2,764 | 103 | 31% |
| Main | Program | 984 | 37 | 10% |
| **TOTAL** | | **9,141** | **340** | **100%** |

---

### Physical Document Characteristics (All Source)

| Metric | Value |
|--------|-------|
| Total Lines | 9,141 |
| Total Pages | 340 |
| Paper Weight (20 lb) | ~1.5 lbs (0.7 kg) |
| Stack Height | ~1.3 inches (34 mm) |
| Binder Size Needed | 1" three-ring binder |

---

### Typical Build (One Terminal Variant)

For a single-platform build (excluding alternate terminal drivers):

| Component | Lines | Pages |
|-----------|------:|------:|
| Layer 0: Utilities | 583 | 22 |
| Layer 1: Engine | 2,107 | 78 |
| Layer 2: UI & Files | 2,500 | 93 |
| Layer 3: One terminal (~450 avg) | ~450 | ~17 |
| Main Program | 984 | 37 |
| **Build Total** | **~6,624** | **~247** |

A complete single-platform build prints to approximately **250 pages** or about **1 inch of paper**.

---

### Comparison to Historical Software

| Software | Approx. Lines | Pages (est.) |
|----------|--------------|--------------|
| **XL Spreadsheet** | 6,624 | 247 |
| VisiCalc (1979) | ~5,000 | ~185 |
| Early Lotus 1-2-3 | ~50,000 | ~1,850 |
| WordStar 1.0 | ~15,000 | ~555 |
| CP/M 2.2 | ~8,000 | ~296 |

XL is comparable in size to VisiCalc, the original microcomputer spreadsheet.

---

## Deployment 1.0 Release (2026-01-21)

### Overview

Deployment 1.0 removes dead code (COMMANDS.FOR - 203 lines of functions never called) and provides clean executables with only functioning code.

**Location:** `/Volumes/SECURE8/git/spreadsheet-fortran/deployment_1.0/`

### Executables

| Variant | Cells | Size | Reduction from Dev |
|---------|-------|------|-------------------|
| xl_small | 100 | 150,088 bytes | -240 bytes |
| xl_medium | 300 | 150,088 bytes | -240 bytes |
| xl_large | 2,000 | 150,088 bytes | -240 bytes |

---

### Line Count Analysis (Active vs Comments vs Blank)

#### By Layer

| Layer | Active | Comments | Blank | Total |
|-------|-------:|---------:|------:|------:|
| **Layer 0: Utilities** | 276 | 241 | 66 | 583 |
| **Layer 1: Engine** | 1,130 | 613 | 364 | 2,107 |
| **Layer 2: UI & Files** | 1,438 | 654 | 408 | 2,500 |
| **Layer 3: Terminal** | 465 | 357 | 196 | 1,018 |
| **Main Program** | 594 | 236 | 154 | 984 |
| **TOTAL** | **3,903** | **2,101** | **1,188** | **7,192** |

#### By File (Detail)

| File | Active | Comments | Blank | Total |
|------|-------:|---------:|------:|------:|
| **Layer 0** |
| STRUTIL.FOR | 276 | 241 | 66 | 583 |
| **Layer 1** |
| CELLS.FOR | 481 | 213 | 159 | 853 |
| PARSE.FOR | 294 | 134 | 78 | 506 |
| DEPS.FOR | 168 | 107 | 60 | 335 |
| EVAL.FOR | 137 | 91 | 44 | 272 |
| RECALC.FOR | 50 | 68 | 23 | 141 |
| **Layer 2** |
| DISPLAY.FOR | 415 | 203 | 116 | 734 |
| FILELOAD.FOR | 389 | 118 | 96 | 603 |
| FILESAV.FOR | 322 | 88 | 76 | 486 |
| MSG.FOR | 149 | 61 | 44 | 254 |
| UI.FOR | 117 | 77 | 50 | 244 |
| FILES.FOR | 46 | 107 | 26 | 179 |
| **Layer 3** |
| PROTVT100.FOR | 183 | 105 | 58 | 346 |
| TERMINAL.FOR | 134 | 133 | 78 | 345 |
| FIOUNIX.FOR | 98 | 50 | 30 | 178 |
| IOUNIX.FOR | 50 | 69 | 30 | 149 |
| **Main** |
| XLMAIN.FOR | 560 | 207 | 139 | 906 |
| BRIDGE.FOR | 34 | 29 | 15 | 78 |

---

### Excluded Dead Code

| File | Active | Comments | Blank | Total | Reason |
|------|-------:|---------:|------:|------:|--------|
| COMMANDS.FOR | 69 | 99 | 35 | 203 | Functions never called |

---

### Summary Statistics

| Metric | Value |
|--------|-------|
| **Active code lines** | 3,903 (54.2%) |
| **Comment lines** | 2,101 (29.2%) |
| **Blank lines** | 1,188 (16.5%) |
| **Total lines** | 7,192 |

---

### Printed Documentation (12pt Courier, double-spaced)

| Metric | Pages |
|--------|------:|
| **Active code only** | 145 |
| **All lines** | 267 |
| **Stack height (active only)** | ~0.6 inches |
| **Stack height (all lines)** | ~1.0 inches |

---

### Development vs Deployment 1.0 Comparison

| Metric | Development | Deployment 1.0 | Saved |
|--------|------------:|---------------:|------:|
| Total lines | 7,395 | 7,192 | 203 |
| Active code | 3,972 | 3,903 | 69 |
| Executable size | 150,328 | 150,088 | 240 bytes |
| Dead code files | 1 | 0 | 1 file |
