# XL Spreadsheet Development Analysis

## 2026-01-24 (Updated)

Product development metrics, implemented features, and release documentation.

**Latest updates:**

- Complete overlay system implementation for RSX-11M memory efficiency
- Multi-terminal support: 6 terminal drivers (VT-52, VT-100, ADM-3A, Hazeltine 1500, Wyse 50, TeleVideo 920)
- **Per-function overlays**: 36 individual function overlays (60-340 bytes each), not grouped
- **MRU eviction**: Most Recently Used eviction preserves dependency foundation
- **Small build uses binary-only file I/O** (no JSON option - saves ~3KB)
- Build configurations: Small (1 slot, CP/M 40KB), Medium (8 slots), Large (12 slots)
- FORTRAN IV/66 compliance verification and tooling
- ODL files for RSX-11M Task Builder: XL_SMALL.ODL, XL_MED.ODL, XL_LARGE.ODL

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

## Overlay System (pdp11/src)

### Overview

Per-function overlay architecture for memory-efficient operation across multiple platforms. Uses MRU (Most Recently Used) eviction to preserve dependency foundations when slots are full.

**Design Philosophy:**

- **Per-function overlays**: Each function is its own ~60-500 byte overlay (not grouped)
- **MRU eviction**: When slots are full, evict the most recently used function
- **Root-resident common functions**: @SUM and @COUNT always available (no overlay needed)
- **Configurable slot count**: 1, 8, or 12 slots depending on build size

### Why MRU Eviction?

When all slots are full and a new function is needed:

```
Scenario: Slots contain [SUM] [COUNT] [VAR], need @IF

LRU eviction (evict oldest):
  Evict SUM → [IF] [COUNT] [VAR]
  Problem: SUM computed foundations that COUNT/VAR depend on
  If recalc needed, must reload SUM, causing cascade

MRU eviction (evict most recent):
  Evict VAR → [SUM] [COUNT] [IF]
  Result: Preserves foundation, user loses only the "leaf" operation
  User can consciously choose: keep VAR or IF?
```

MRU preserves the dependency foundation and only removes the newest "experiment."

### Build Configurations

| Build  | Slots | Target RAM | Target Platform | Use Case                  |
| ------ | -----:| ---------- | --------------- | ------------------------- |
| Small  | 1     | ~40 KB     | 64KB CP/M       | Painful beyond 1 function |
| Medium | 8     | ~65 KB     | 256KB PDP-11    | Typical spreadsheets      |
| Large  | 12    | ~196 KB    | 1MB+ PDP-11     | Power users               |

**Platform Notes:**

- **Small (CP/M)**: 64KB machine with ~40KB available for program. Single function slot means heavy overlay swapping, but any function is usable.
- **Medium (PDP-11)**: 256KB partition allows 8 simultaneous function types. Handles typical business spreadsheets.
- **Large (PDP-11)**: 1MB+ systems with large partitions. Only 1-2 power users run simultaneously.

### Memory Layout (Medium Build)

```
+------------------+
|     ROOT         |  ~33 KB code + ~13 KB data = ~46 KB
+------------------+
|   TERMINAL       |  ~1.3 KB (one driver, loaded at startup)
+------------------+
|   FUNC SLOT 1    |  ~0.5 KB max
+------------------+
|   FUNC SLOT 2    |  ~0.5 KB max
+------------------+
|   ...            |
+------------------+
|   FUNC SLOT 8    |  ~0.5 KB max
+------------------+
|   FILE I/O       |  ~4 KB (temporary, during /SAVE or /LOAD)
+------------------+
```

### Root Segment Code (Always Resident)

Active code lines only (excluding comments and blanks):

| File         | Active Lines | Est. 8-bit | Est. 16-bit |
| ------------ | ------------:| ----------:| -----------:|
| CELLS.FOR    | 855          | 6.8 KB     | 8.6 KB      |
| PARSE.FOR    | 704          | 5.6 KB     | 7.0 KB      |
| COMMANDS.FOR | 632          | 5.1 KB     | 6.3 KB      |
| UI.FOR       | 365          | 2.9 KB     | 3.7 KB      |
| STRUTIL.FOR  | 313          | 2.5 KB     | 3.1 KB      |
| OVLMGR.FOR   | 278          | 2.2 KB     | 2.8 KB      |
| EVAL.FOR     | 228          | 1.8 KB     | 2.3 KB      |
| DEPS.FOR     | 182          | 1.5 KB     | 1.8 KB      |
| DISPLAY.FOR  | 161          | 1.3 KB     | 1.6 KB      |
| TERMINAL.FOR | 141          | 1.1 KB     | 1.4 KB      |
| MSG.FOR      | 140          | 1.1 KB     | 1.4 KB      |
| IORSX.FOR    | 127          | 1.0 KB     | 1.3 KB      |
| RECALC.FOR   | 80           | 0.6 KB     | 0.8 KB      |
| XLMAIN.FOR   | 41           | 0.3 KB     | 0.4 KB      |
| FNROOT.FOR   | 35           | 0.3 KB     | 0.4 KB      |
| **TOTAL**    | **4,282**    | **~34 KB** | **~43 KB**  |

*Estimates: 8 bytes/line (8-bit optimized), 10 bytes/line (16-bit PDP-11)*

### Terminal Overlays (overlay/term/)

| File          | Active Lines | Source Bytes | Est. Compiled |
| ------------- | ------------:| ------------:| -------------:|
| TERMVT100.FOR | 153          | 7,638        | ~1.5 KB       |
| TERMWY50.FOR  | 119          | 6,576        | ~1.2 KB       |
| TERMTVI9.FOR  | 117          | 6,391        | ~1.2 KB       |
| TERMVT52.FOR  | 100          | 6,277        | ~1.0 KB       |
| TERMHZ15.FOR  | 99           | 6,506        | ~1.0 KB       |
| TERMADM3.FOR  | 86           | 6,192        | ~0.9 KB       |

One terminal loaded at startup, stays resident forever.

### Individual Function Overlays (overlay/func/)

36 individual function files, each its own overlay segment:

**Statistical (5 functions):**

| File       | Active Lines | Source Bytes | Est. Compiled |
| ---------- | ------------:| ------------:| -------------:|
| FNSTDV.FOR | 34           | 1,259        | ~340 bytes    |
| FNVAR.FOR  | 34           | 1,229        | ~340 bytes    |
| FNAVG.FOR  | 25           | 860          | ~250 bytes    |
| FNMIN.FOR  | 24           | 816          | ~240 bytes    |
| FNMAX.FOR  | 24           | 817          | ~240 bytes    |

**Math (11 functions):**

| File        | Active Lines | Source Bytes | Est. Compiled |
| ----------- | ------------:| ------------:| -------------:|
| FNEXP.FOR   | 14           | 505          | ~140 bytes    |
| FNTAN.FOR   | 13           | 507          | ~130 bytes    |
| FNROUND.FOR | 12           | 552          | ~120 bytes    |
| FNMOD.FOR   | 11           | 466          | ~110 bytes    |
| FNSQRT.FOR  | 11           | 419          | ~110 bytes    |
| FNLOG.FOR   | 11           | 424          | ~110 bytes    |
| FNLN.FOR    | 11           | 428          | ~110 bytes    |
| FNINT.FOR   | 7            | 358          | ~70 bytes     |
| FNABS.FOR   | 7            | 338          | ~70 bytes     |
| FNSIN.FOR   | 7            | 328          | ~70 bytes     |
| FNCOS.FOR   | 7            | 332          | ~70 bytes     |

**Conditional (7 functions):**

| File         | Active Lines | Source Bytes | Est. Compiled |
| ------------ | ------------:| ------------:| -------------:|
| FNISNA.FOR   | 15           | 647          | ~150 bytes    |
| FNISERR.FOR  | 13           | 570          | ~130 bytes    |
| FNCHOOSE.FOR | 12           | 594          | ~120 bytes    |
| FNAND.FOR    | 9            | 444          | ~90 bytes     |
| FNOR.FOR     | 9            | 443          | ~90 bytes     |
| FNIF.FOR     | 8            | 433          | ~80 bytes     |
| FNNOT.FOR    | 8            | 394          | ~80 bytes     |

**Financial (5 functions):**

| File      | Active Lines | Source Bytes | Est. Compiled |
| --------- | ------------:| ------------:| -------------:|
| FNIRR.FOR | 33           | 1,466        | ~330 bytes    |
| FNNPV.FOR | 19           | 654          | ~190 bytes    |
| FNPMT.FOR | 16           | 779          | ~160 bytes    |
| FNPV.FOR  | 16           | 789          | ~160 bytes    |
| FNFV.FOR  | 16           | 775          | ~160 bytes    |

**String (8 functions):**

| File        | Active Lines | Source Bytes | Est. Compiled |
| ----------- | ------------:| ------------:| -------------:|
| FNTRIM.FOR  | 24           | 997          | ~240 bytes    |
| FNFIND.FOR  | 21           | 969          | ~210 bytes    |
| FNMID.FOR   | 16           | 848          | ~160 bytes    |
| FNUPPER.FOR | 13           | 696          | ~130 bytes    |
| FNLOWER.FOR | 13           | 689          | ~130 bytes    |
| FNRIGHT.FOR | 13           | 728          | ~130 bytes    |
| FNLEFT.FOR  | 12           | 641          | ~120 bytes    |
| FNLEN.FOR   | 6            | 323          | ~60 bytes     |

**Summary:** Functions range from 60 bytes (@LEN) to 340 bytes (@STDEV). Average ~150 bytes.

### Overlay Manager (OVLMGR.FOR)

| Routine | Purpose                                    |
| ------- | ------------------------------------------ |
| OVLINI  | Initialize overlay tracking state          |
| OVLCFG  | Set number of function slots (1, 8, or 12) |
| OVLTRM  | Load terminal overlay (once at startup)    |
| OVLFNL  | Load function with MRU eviction            |
| OVLFIO  | Load file I/O overlay                      |
| OVLFIU  | Unload file I/O overlay                    |
| OVLCHK  | Check if function is loaded                |
| OVLSLT  | Get slot status for debugging              |
| OVLSCN  | Scan formulas and preload needed functions |
| FNIDEN  | Identify function from name (1-36 code)    |

### ODL Files (Overlay Description Language)

| File         | Slots | Size    | Target       |
| ------------ | -----:| -------:| ------------ |
| XL_SMALL.ODL | 1     | 3.7 KB  | 64KB CP/M    |
| XL_MED.ODL   | 8     | 11.1 KB | 256KB PDP-11 |
| XL_LARGE.ODL | 12    | 15.8 KB | 1MB+ PDP-11  |

### Compiled Size Estimates (Root Only)

| Build  | Code (8-bit) | Code (16-bit) | Data   | Root Total (8-bit) | Root Total (16-bit) |
| ------ | ------------:| -------------:| ------:| ------------------:| -------------------:|
| Small  | ~27 KB       | ~37 KB        | ~6 KB  | ~33 KB             | ~43 KB              |
| Medium | ~27 KB       | ~37 KB        | ~15 KB | ~42 KB             | ~52 KB              |
| Large  | ~27 KB       | ~37 KB        | ~82 KB | ~109 KB            | ~119 KB             |

*Code at 6 bytes/active line (8-bit) or 8 bytes/active line (16-bit). Data scales with cell capacity.*

### Total Memory Including Overlay Regions

Overlay regions must be reserved even when empty. The runtime needs space for overlays to be loaded:

| Build  | Root   | Terminal | Func Slots      | File I/O | Stack | **Total (8-bit)** |
| ------ | ------:| --------:| ---------------:| --------:| -----:| -----------------:|
| Small  | 33 KB  | 1 KB     | 0.5 KB (1 slot) | 3.5 KB*  | 2 KB  | **~40 KB**        |
| Medium | 42 KB  | 1.5 KB   | 4 KB (8 slots)  | 4 KB     | 2 KB  | **~54 KB**        |
| Large  | 109 KB | 1.5 KB   | 6 KB (12 slots) | 4 KB     | 2 KB  | **~123 KB**       |

*Small build uses binary-only file I/O (3.5 KB) instead of JSON (4.5 KB).

**Note:** Function slots and file I/O regions could potentially share memory since they're never used simultaneously (file save/load doesn't evaluate formulas). This would save ~0.5 KB on small build, making it ~39.5 KB.

### Overlay Region Accounting

Each overlay region reserves memory for its largest segment:

| Region      | Small Build | Medium Build | Large Build | Purpose                  |
| ----------- | -----------:| ------------:| -----------:| ------------------------ |
| TERMRGN     | 1.0 KB      | 1.5 KB       | 1.5 KB      | One terminal driver      |
| FUNC1RGN    | 0.5 KB      | 0.5 KB       | 0.5 KB      | Function slot 1          |
| FUNC2-8RGN  | -           | 3.5 KB       | 3.5 KB      | Function slots 2-8       |
| FUNC9-12RGN | -           | -            | 2.0 KB      | Function slots 9-12      |
| FILERGN     | 3.5 KB      | 4.0 KB       | 4.0 KB      | File I/O (save or load)  |
| **Total**   | **5.0 KB**  | **9.5 KB**   | **11.5 KB** | *Reserved overlay space* |

The largest function overlay (~340 bytes for @STDEV) determines slot size.
The largest file overlay (~3.5-4 KB for FILELOAD) determines file region size.

---

## CP/M Deployment Analysis

### Target: 64KB CP/M Machine with ~40KB TPA

A 64KB CP/M-80 system has approximately 56-58KB TPA (Transient Program Area) after CP/M loads. To fit a spreadsheet in ~40KB of actual program space:

**Memory Budget:**

```
CP/M system:     ~6-8 KB
TPA available:   ~56-58 KB
Target program:  ~40 KB
Stack/runtime:   ~4 KB
═══════════════════════════
Available:       ~36 KB for code + data
```

**Small Build Memory Budget (Binary-Only File I/O):**

| Component                     | @ 6 bytes/line | @ 8 bytes/line |
| ----------------------------- | --------------:| --------------:|
| Root code (4,566 active)      | 27.4 KB        | 36.5 KB        |
| Data (small config)           | 6.0 KB         | 6.0 KB         |
| Terminal region (resident)    | 1.0 KB         | 1.0 KB         |
| Function slot region (1 slot) | 0.5 KB         | 0.5 KB         |
| File I/O region (binary)      | 3.5 KB         | 3.5 KB         |
| Stack/runtime                 | 2.0 KB         | 2.0 KB         |
| **TOTAL**                     | **~40.4 KB**   | **~49.5 KB**   |

**Verdict:** At 6 bytes/active line (optimized 8-bit FORTRAN), small build **just fits 40KB**. At 8 bytes/line (typical), it exceeds 40KB by ~9 KB and requires assembly optimization.

**Constraints:** Feature reduction and data reduction are NOT acceptable. All features must be retained. Small config uses 300 cells, not 100.

### Assembly Language Strategy for 40KB

The only path to 40KB is converting FORTRAN modules to hand-optimized assembly. The following modules are candidates, ordered by effort vs. savings:

**Tier 1: Easy wins (character/string operations)**

| Module                | FORTRAN Est. | Assembly Est. | Savings    | Complexity            |
| --------------------- | ------------:| -------------:| ----------:| --------------------- |
| STRUTIL (string ops)  | 1.7 KB       | 0.3 KB        | **1.4 KB** | Low - LDIR/CPIR loops |
| MSG (message output)  | 0.9 KB       | 0.2 KB        | **0.7 KB** | Low - table lookup    |
| Terminal I/O          | 1.1 KB       | 0.2 KB        | **0.9 KB** | Low - BDOS calls      |
| Protocol (VT escapes) | 0.8 KB       | 0.2 KB        | **0.6 KB** | Low - byte sequences  |
| **Tier 1 Total**      | **4.5 KB**   | **0.9 KB**    | **3.6 KB** |                       |

**Tier 2: Medium effort (core algorithms)**

| Module            | FORTRAN Est. | Assembly Est. | Savings    | Complexity              |
| ----------------- | ------------:| -------------:| ----------:| ----------------------- |
| Hash operations   | 0.5 KB       | 0.1 KB        | **0.4 KB** | Medium - multiply/mod   |
| Parser tokenizer  | 1.8 KB       | 0.6 KB        | **1.2 KB** | Medium - state machine  |
| Number formatting | 0.5 KB       | 0.2 KB        | **0.3 KB** | Medium - div/mod loops  |
| Command dispatch  | 0.5 KB       | 0.2 KB        | **0.3 KB** | Medium - string compare |
| **Tier 2 Total**  | **3.3 KB**   | **1.1 KB**    | **2.2 KB** |                         |

**Tier 3: Higher effort (computation)**

| Module           | FORTRAN Est. | Assembly Est. | Savings    | Complexity           |
| ---------------- | ------------:| -------------:| ----------:| -------------------- |
| Evaluator stack  | 0.8 KB       | 0.4 KB        | **0.4 KB** | High - FP operations |
| Display core     | 1.0 KB       | 0.5 KB        | **0.5 KB** | High - cursor logic  |
| Cell lookup      | 0.4 KB       | 0.2 KB        | **0.2 KB** | High - hash chains   |
| **Tier 3 Total** | **2.2 KB**   | **1.1 KB**    | **1.1 KB** |                      |

**Total Assembly Savings:**

| Tiers       | FORTRAN | Assembly | Savings    |
| ----------- | -------:| --------:| ----------:|
| Tier 1 only | 4.5 KB  | 0.9 KB   | 3.6 KB     |
| Tier 1+2    | 7.8 KB  | 2.0 KB   | **5.8 KB** |
| All tiers   | 10.0 KB | 3.1 KB   | **6.9 KB** |

**40KB Configuration (with Tier 1+2 assembly):**

| Component                | FORTRAN Only | With Assembly |
| ------------------------ | ------------:| -------------:|
| Root code (non-assembly) | 27.4 KB      | 21.6 KB       |
| Assembly routines        | -            | 2.0 KB        |
| Data (300 cells)         | 6.0 KB       | 6.0 KB        |
| Terminal region          | 1.0 KB       | 0.4 KB        |
| Function slot            | 0.5 KB       | 0.5 KB        |
| File I/O region          | 3.5 KB       | 3.5 KB        |
| Stack/runtime            | 2.0 KB       | 2.0 KB        |
| **TOTAL**                | **40.4 KB**  | **36.0 KB**   |

With Tier 1+2 assembly (~5.8 KB savings), small build fits comfortably in 40KB with 4 KB headroom. No feature or data reduction required.

**Assembly code estimate:** ~800-1000 lines of Z80/6502 assembly for Tier 1+2 modules.

---

## Floppy Disk Storage Analysis

### Source Code Distribution (for developers)

| Component          | Source Bytes | Source KB  |
| ------------------ | ------------:| ----------:|
| Root segment files | 205,478      | 201 KB     |
| Terminal overlays  | 39,580       | 39 KB      |
| Function overlays  | 24,051       | 23 KB      |
| Config files       | 9,247        | 9 KB       |
| ODL files          | 30,552       | 30 KB      |
| **TOTAL SOURCE**   | **308,908**  | **302 KB** |

### Compiled Distribution (for end users)

What ships on the distribution floppy:

| Build  | Main .TSK | Term Overlays | Func Overlays | File Overlays | **Total**   |
| ------ | ---------:| -------------:| -------------:| -------------:| -----------:|
| Small  | ~33 KB    | 6 KB (6×)     | 6 KB (36×)    | 3.5 KB        | **~49 KB**  |
| Medium | ~42 KB    | 9 KB (6×)     | 6 KB (36×)    | 4 KB          | **~61 KB**  |
| Large  | ~109 KB   | 9 KB (6×)     | 6 KB (36×)    | 4 KB          | **~128 KB** |

*All 6 terminal drivers ship with every build (~1-1.5 KB each). User selects terminal at startup or via config. Func Overlays: 36 files at ~60-340 bytes each. File Overlays: FILESAV + FILELOAD (or binary equivalents).*

**Why ship all terminals:**

- One distribution works for any terminal type
- User can switch terminals without needing a different build
- Only ~6-9 KB extra disk space; runtime memory unchanged (only 1 terminal loads)

**Note:** Runtime memory differs from disk distribution because:

- Disk needs all 36 function overlays (~6 KB total), but only 1 loads at a time (~0.5 KB max)
- Disk needs all 6 terminal overlays (~6-9 KB total), but only 1 loads at startup (~1 KB resident)

### Common Floppy Formats (1978-1982)

| Format                | Capacity | Source Fits? | Small (~49 KB) | Medium (~61 KB) | Large (~128 KB) |
| --------------------- | -------- | ------------ | -------------- | --------------- | --------------- |
| 8" SSSD (IBM 3740)    | 250 KB   | NO (needs 2) | YES            | YES             | YES             |
| 8" SSDD               | 500 KB   | YES          | YES            | YES             | YES             |
| 5.25" SSSD (Apple II) | 113 KB   | NO (needs 3) | YES            | YES             | NO (needs 2)    |
| 5.25" SSDD (TRS-80)   | 180 KB   | NO (needs 2) | YES            | YES             | YES             |
| 5.25" DSDD (IBM PC)   | 360 KB   | YES          | YES            | YES             | YES             |

### Distribution Scenarios

**Small Build on 5.25" SSSD (113 KB) - Single Disk:**

```
Compiled XL.TSK:    ~33 KB (main program)
Terminal overlays:   ~6 KB (all 6 drivers)
Function overlays:   ~6 KB (all 36 functions)
File I/O overlays:  ~3.5 KB
Documentation:       ~5 KB
───────────────────────────
Total:              ~54 KB (59 KB free for user files)
```

**Medium Build on 8" SSSD (250 KB) - Single Disk:**

```
Compiled XL.TSK:    ~42 KB
All overlays:       ~19 KB
Documentation:      ~10 KB
Sample spreadsheets: ~10 KB
───────────────────────────
Total:              ~81 KB (169 KB free for user files)
```

**Source Distribution on 8" SSDD (500 KB) - Single Disk:**

```
Source code:        ~302 KB
Compiled (medium):  ~61 KB
Documentation:      ~20 KB
Sample spreadsheets: ~20 KB
───────────────────────────
Total:              ~403 KB (fits with 97 KB free)
```

**Large Build on 5.25" DSDD (360 KB) - Single Disk:**

```
Compiled XL.TSK:    ~109 KB
All overlays:       ~19 KB
Documentation:      ~15 KB
Sample spreadsheets: ~15 KB
───────────────────────────
Total:              ~158 KB (202 KB free for user files)
```

---

## FORTRAN IV/66 Compliance

### Verification Tooling

Created `temp/check_f66.sh` script to verify FORTRAN 66 compatibility:

- Detects IF/THEN/ELSE blocks (not allowed in F66)
- Detects CHARACTER type declarations
- Detects CHAR() and ICHAR() intrinsics
- Detects inline ! comments
- Validates fixed-format column restrictions

### Code Standards

All code in pdp11/src follows strict FORTRAN IV/66:

- GO TO based control flow (no structured IF)
- INTEGER arrays for strings (no CHARACTER type)
- C-style comments in column 1 only
- Fixed format: columns 1-6 labels, column 6 continuation, columns 7-72 code

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

| File              | Purpose                           | Lines     | Pages   |
| ----------------- | --------------------------------- | ---------:| -------:|
| COMMANDS.FOR      | Command dispatcher + handlers     | 997       | 37      |
| UI.FOR            | Navigation, entry, mode handlers  | 688       | 25      |
| FILELOAD.FOR      | JSON file parser, column widths   | 673       | 25      |
| FILESAV.FOR       | JSON file writer, column widths   | 599       | 22      |
| FILEBIN.FOR       | Binary file format                | 408       | 15      |
| DISPLAY.FOR       | Screen rendering, variable widths | 345       | 13      |
| MSG.FOR           | Status messages                   | 237       | 9       |
| XLMAIN.FOR        | Main program entry point          | 114       | 4       |
| FILEBINL.FOR      | Binary file loader                | 71        | 3       |
| FILEBINS.FOR      | Binary file saver                 | 40        | 2       |
| **Layer 2 Total** |                                   | **4,172** | **155** |

*Note: COMMANDS.FOR refactored from 782-line monolith to 997 lines with modular handlers (better maintainability). FILES.FOR removed (unused stubs). UI.FOR and MSG.FOR had stubs removed.*

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
| Layer 1   | Core         | 4,180      | 155     | 34%        |
| Layer 2   | Application  | 4,172      | 155     | 33%        |
| Layer 3   | Terminal*    | 3,595      | 133     | 29%        |
| **TOTAL** |              | **12,464** | **462** | **100%**   |

*Layer 0 shows Unix platform (517 lines); RSX is 159 lines. Layer 3 shows all variants; typical build uses ~1,463 lines.*

---

### Physical Document Characteristics (All Source)

| Metric               | Value                |
| -------------------- | -------------------- |
| Total Lines          | 12,464               |
| Total Pages          | 462                  |
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
| Layer 2: Application logic                  | 4,172      | 155     |
| Layer 3: Terminal (TERMINAL+PROTVT100+REND) | 1,463      | 54      |
| **Build Total**                             | **10,332** | **383** |

A complete single-platform build prints to approximately **383 pages** or about **1.5 inches of paper**.

---

### Data Entry and Paper Tape Storage

Metrics for manual entry and ASR-33 Teletype paper tape storage.

**Assumptions:**

- Manual entry: 20 WPM (careful code typing), ~10 words per FORTRAN line
- Paper tape: 10 characters/inch, 10 characters/second (punch and read)
- Media cost: Rolled oiled tape 950 ft = $2.25 (1977 prices)

**By Layer (Typical Unix/VT-100 Build):**

| Layer           | Lines      | Bytes       | Entry Time   | Tape Length  | Tape Cost | Punch/Read  |
| --------------- | ----------:| -----------:| ------------:| ------------:| ---------:| -----------:|
| L0: Platform    | 517        | 14,185      | 4.3 hrs      | 118 ft       | $0.28     | 24 min      |
| L1: Core        | 4,180      | 111,020     | 34.8 hrs     | 925 ft       | $2.19     | 3.1 hrs     |
| L2: Application | 4,172      | 108,800     | 34.8 hrs     | 907 ft       | $2.15     | 3.0 hrs     |
| L3: Terminal    | 1,463      | 39,880      | 12.2 hrs     | 332 ft       | $0.79     | 1.1 hrs     |
| **Total**       | **10,332** | **273,885** | **86.1 hrs** | **2,282 ft** | **$5.41** | **7.6 hrs** |

**Largest Individual Files:**

| File         | Lines | Bytes  | Entry Time | Tape Length | Punch/Read |
| ------------ | -----:| ------:| ----------:| -----------:| ----------:|
| CELLS.FOR    | 1,449 | 38,036 | 12.1 hrs   | 317 ft      | 63 min     |
| PARSE.FOR    | 1,083 | 30,064 | 9.0 hrs    | 251 ft      | 50 min     |
| COMMANDS.FOR | 997   | 25,692 | 8.3 hrs    | 214 ft      | 43 min     |
| RENDVT.FOR   | 790   | 22,684 | 6.6 hrs    | 189 ft      | 38 min     |
| UI.FOR       | 688   | 18,786 | 5.7 hrs    | 157 ft      | 31 min     |

**Active Code Only** (excludes comments/blanks):

| Metric              | Value    |
| ------------------- | -------- |
| Active lines        | 5,893    |
| Entry time @ 20 WPM | 49.1 hrs |
| Working days        | ~6 days  |

**Summary (Complete Build):**

| Metric                | Value                         |
| --------------------- | ----------------------------- |
| Total lines           | 10,355                        |
| Total bytes           | 277,149                       |
| Manual entry time     | 86.3 hrs (~11 working days)   |
| Paper tape length     | 2,310 ft (770 yds, 0.4 miles) |
| Rolls needed (950 ft) | 3 rolls                       |
| Media cost (1977)     | $6.75                         |
| Punch time @ 10 cps   | 7 hrs 42 min                  |
| Read time @ 10 cps    | 7 hrs 42 min                  |

*The entire spreadsheet costs under $7 in tape media. Manual typing takes 11 days; paper tape transfer takes 1 day.*

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
- **COMMANDS.FOR** refactored from 782-line monolith to 997 lines with modular handlers
  - Split into dispatcher (CMDEXE) + 11 individual command handlers
  - Largest routine reduced from 487 to 162 lines
  - Better maintainability at cost of +215 lines
- **FILES.FOR** removed (unused stubs)
- **UI.FOR** and **MSG.FOR** had stubs removed
- **STRUTIL.FOR** moved from Layer 0 to Layer 1 (portable core)
- **BRIDGE.FOR** deleted (no longer needed)

---

### Line Count Analysis (Active vs Comments vs Blank)

#### By Layer (Typical Unix/VT-100 Build)

| Layer                    | Active    | Comments  | Blank     | Total      |
| ------------------------ | ---------:| ---------:| ---------:| ----------:|
| **Layer 0: Platform**    | 264       | 154       | 99        | 517        |
| **Layer 1: Core**        | 2,403     | 1,139     | 638       | 4,180      |
| **Layer 2: Application** | 2,519     | 965       | 688       | 4,172      |
| **Layer 3: Terminal**    | 765       | 440       | 258       | 1,463      |
| **TOTAL**                | **5,951** | **2,698** | **1,683** | **10,332** |

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
| COMMANDS.FOR     | 663    | 182      | 152   | 997   |
| UI.FOR           | 395    | 176      | 117   | 688   |
| FILELOAD.FOR     | 434    | 132      | 107   | 673   |
| FILESAV.FOR      | 406    | 103      | 90    | 599   |
| FILEBIN.FOR      | 233    | 103      | 72    | 408   |
| DISPLAY.FOR      | 164    | 113      | 68    | 345   |
| MSG.FOR          | 140    | 56       | 41    | 237   |
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
| **Active code lines** | 5,951 (57.6%) |
| **Comment lines**     | 2,698 (26.1%) |
| **Blank lines**       | 1,683 (16.3%) |
| **Total lines**       | 10,332        |

---

### Printed Documentation (12pt Courier, double-spaced)

| Metric                         | Pages       |
| ------------------------------ | -----------:|
| **Active code only**           | 220         |
| **All lines**                  | 383         |
| **Stack height (active only)** | ~0.9 inches |
| **Stack height (all lines)**   | ~1.5 inches |

---

### Key Refactoring Changes (1.0 → 2.0 → 2.1)

| File/Module  | v1.0       | v2.0    | v2.1    | Notes                               |
| ------------ | ----------:| -------:| -------:| ----------------------------------- |
| XLMAIN.FOR   | 1,443      | 114     | 114     | -1,329 lines (routines distributed) |
| COMMANDS.FOR | 203 (stub) | 782     | 997     | Refactored into modular handlers    |
| UI.FOR       | 380        | 730     | 688     | Stubs removed                       |
| MSG.FOR      | -          | 254     | 237     | Stubs removed                       |
| FILES.FOR    | -          | 179     | deleted | Unused stubs removed                |
| PARSE.FOR    | 506        | 1,083   | 1,083   | Added CPYADJ, parsing utilities     |
| STRUTIL.FOR  | layer0     | layer1  | layer1  | Moved to portable layer             |
| BRIDGE.FOR   | 78         | deleted | deleted | No longer needed                    |
