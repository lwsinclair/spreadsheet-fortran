# User Story: Terminal Support

**Epic**: Layer 3 - Terminal I/O
**Priority**: High
**Status**: Future (Layer 3)
**Created**: 2026-01-18

---

## Overview

XL spreadsheet must support period-appropriate terminals commonly used with Xerox Sigma and CP-V systems in 1978.

---

## Supported Terminals

### 1. ADM-3A (Lear Siegler)
**Status**: Required
**Common Name**: "Dumb Terminal"
**Prevalence**: Extremely common in late 1970s

**Technical Specs:**
- 24 lines × 80 columns
- ASCII display
- No attribute support (no bold/underline)
- Cursor positioning via ESC sequences
- No function keys

**Control Sequences:**
```
Cursor Home:     ESC H (0x1B 0x48)
Clear Screen:    ESC *  (0x1B 0x2A)
Cursor Position: ESC = row col (0x1B 0x3D <row+32> <col+32>)
Cursor Up:       Ctrl-K (0x0B)
Cursor Down:     Ctrl-J (0x0A) or LF
Cursor Left:     Ctrl-H (0x08) or BS
Cursor Right:    Ctrl-L (0x0C)
```

**XL Requirements:**
- Display grid at 80×24
- Status line at top
- Cell entry at bottom
- No color support needed
- Clear, simple display

### 2. VT52 (DEC)
**Status**: Required
**Common Name**: Early DEC terminal
**Prevalence**: Common in corporate/educational settings

**Technical Specs:**
- 24 lines × 80 columns
- ASCII display
- Limited graphics (line drawing)
- Cursor positioning via ESC sequences
- Limited function keys

**Control Sequences:**
```
Cursor Home:     ESC H
Clear Screen:    ESC J (from cursor to end)
                 ESC K (clear line from cursor)
Cursor Position: ESC Y row col (0x1B 0x59 <row+32> <col+32>)
Cursor Up:       ESC A
Cursor Down:     ESC B
Cursor Left:     ESC D
Cursor Right:    ESC C
Enter Graphics:  ESC F
Exit Graphics:   ESC G
```

**XL Requirements:**
- Display grid at 80×24
- Optional: Use graphics mode for grid lines
- Status line with basic formatting
- Cell entry area
- No color needed

### 3. VT100 (DEC) - Optional/Future
**Status**: Nice to have (introduced 1978, expensive)
**Note**: Less common in 1978, more prevalent by 1980

**Why Optional:**
- VT100 released August 1978
- Expensive ($1500+ initially)
- Most sites still using ADM-3A or VT52
- Full support can be added later

---

## User Stories

### Story 1: ADM-3A Terminal Support
**As a** user with an ADM-3A terminal
**I want to** run XL spreadsheet
**So that** I can use the most common terminal of the era

**Acceptance Criteria:**
- XL displays properly on 80×24 screen
- Cursor movement works (arrows, home)
- Clear screen clears entire display
- Status line visible at top
- Cell entry prompt at bottom
- Grid displays in ASCII (+ - | characters)
- Cell values display correctly
- No garbled output from unsupported sequences

**Technical Requirements:**
- TERMTEST.FOR uses ADM-3A control codes
- Cursor positioning: ESC = row col
- Clear screen: ESC *
- Screen layout fits 80×24
- Test on actual ADM-3A or emulator

### Story 2: VT52 Terminal Support
**As a** user with a VT52 terminal
**I want to** run XL spreadsheet
**So that** I can use my DEC terminal

**Acceptance Criteria:**
- XL displays properly on 80×24 screen
- Cursor movement works using VT52 sequences
- Clear screen works (ESC J)
- Status line visible
- Cell entry works
- Optional: Graphics mode for grid lines
- Cell values display correctly

**Technical Requirements:**
- TERMTEST.FOR supports VT52 control codes
- Cursor positioning: ESC Y row col
- Clear screen: ESC J
- Optional: ESC F/G for graphics mode
- Test on VT52 emulator

### Story 3: Terminal Auto-Detection
**As a** user
**I want** XL to detect my terminal type
**So that** I don't have to manually configure it

**Acceptance Criteria:**
- Check TERM environment variable (if available)
- Default to ADM-3A if unknown
- Allow manual override via command parameter
- Display terminal type on startup

**Technical Requirements:**
- Read TERM from environment
- Support: TERM=adm3a, TERM=vt52
- Default fallback to ADM-3A
- Command line: XL TERM=VT52

---

## Screen Layout (Both Terminals)

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ XL Spreadsheet  A1              READY                              [80 chars]│ ← Status line
├──────────────────────────────────────────────────────────────────────────────┤
│      A         B         C         D         E         F         G           │
│  1│          │          │          │          │          │          │         │
│  2│          │          │          │          │          │          │         │
│  3│          │          │          │          │          │          │         │
│  ...                                                                          │
│ 20│          │          │          │          │          │          │         │
├──────────────────────────────────────────────────────────────────────────────┤
│ A1:                                                                           │ ← Entry line
│ HELP: /H  COMMANDS: /  POINT: .                                              │ ← Help line
└──────────────────────────────────────────────────────────────────────────────┘
```

**Key:**
- Line 1: Status (cell, mode, message)
- Lines 2-21: Grid (20 rows visible)
- Line 22: Cell entry/formula
- Line 23: Context help
- Line 24: (blank/reserved)

---

## Grid Display Options

### ASCII Grid (ADM-3A, VT52 default)
```
   A      B      C      D
1│      │      │      │
2│  100 │  200 │  300 │
3│      │      │      │
```

**Characters:**
- Vertical: `│` (ASCII 179) or `|`
- Horizontal: `─` (ASCII 196) or `-`
- Cross: `┼` (ASCII 197) or `+`

### VT52 Graphics Mode (Optional)
Use VT52 graphics character set for cleaner lines:
```
ESC F    (enter graphics)
<draw grid with graphics chars>
ESC G    (exit graphics)
```

---

## Implementation Files

### Layer 3 Files to Implement:

**TERMCPV.FOR** - CP-V terminal driver
- Detect terminal type
- Send control sequences
- Handle keyboard input
- Cursor positioning
- Screen clear

**TERMTEST.FOR** - Test terminal driver
- Simulate terminal for automated tests
- Same interface as TERMCPV
- No actual terminal I/O

**Terminal Detection:**
```fortran
SUBROUTINE TRMDET(TTYPE)
C     Detect terminal type
C     Returns: TTYPE = 1 (ADM-3A)
C              TTYPE = 2 (VT52)
C              TTYPE = 3 (VT100)
```

**Cursor Positioning:**
```fortran
SUBROUTINE TRMPOS(ROW, COL)
C     Position cursor at ROW, COL
C     Uses appropriate sequence for terminal type
```

**Clear Screen:**
```fortran
SUBROUTINE TRMCLR()
C     Clear screen
C     Uses appropriate sequence for terminal type
```

---

## Testing Strategy

### Emulator Testing

**ADM-3A Emulation:**
- simh supports ADM-3A emulation
- Use telnet with ADM-3A terminal emulator
- Modern options:
  - `xterm` with TERM=adm3a
  - PuTTY with ADM-3A mode
  - Online ADM-3A emulator

**VT52 Emulation:**
- simh supports VT52
- Modern terminal emulators:
  - `xterm` with TERM=vt52
  - PuTTY with VT52 mode
  - macOS Terminal.app (partial VT52)

### Automated Tests

**Test Scenarios:**
1. Clear screen and display grid
2. Position cursor at specific cell
3. Enter value and display
4. Move cursor around grid
5. Update cell and refresh display
6. Display status messages
7. Handle screen boundaries

**Test Code:**
```python
def test_adm3a_clear_screen():
    """Test ADM-3A clear screen sequence"""
    terminal = TerminalSimulator(term_type='adm3a')
    terminal.clear()
    assert terminal.screen_is_blank()

def test_adm3a_cursor_position():
    """Test ADM-3A cursor positioning"""
    terminal = TerminalSimulator(term_type='adm3a')
    terminal.cursor_to(5, 10)
    assert terminal.cursor_row == 5
    assert terminal.cursor_col == 10
```

### Manual Testing Checklist

**ADM-3A Terminal:**
- [ ] Start XL on ADM-3A
- [ ] Screen clears and shows grid
- [ ] Status line displays at top
- [ ] Can navigate with arrow keys
- [ ] Can enter cell values
- [ ] Grid displays properly
- [ ] No control characters visible

**VT52 Terminal:**
- [ ] Start XL on VT52
- [ ] Screen clears (ESC J)
- [ ] Cursor positioning works (ESC Y)
- [ ] Arrow keys work (ESC A/B/C/D)
- [ ] Grid displays
- [ ] Optional: Graphics mode works

---

## Reference Documentation

### ADM-3A Resources
- Terminal specs: http://www.bitsavers.org/pdf/learSiegler/ADM3A_Owners_Manual.pdf
- Control codes: https://vt100.net/wiki/index.php/Lear_Siegler_ADM-3A
- Emulation: Use xterm with TERM=adm3a

### VT52 Resources
- Terminal specs: http://vt100.net/docs/vt52-ug/
- Control codes: https://vt100.net/wiki/index.php/VT52
- Emulation: Use xterm with TERM=vt52

### CP-V Terminal Support
- CP-V supports multiple terminal types
- COC (Communications Controller) handles terminal I/O
- Terminal type set in user profile
- Default: TTY (teletype)

---

## Timeline

**Layer 3 Implementation**: Week 8
**Terminal Detection**: Day 1
**ADM-3A Support**: Day 2-3
**VT52 Support**: Day 4
**Testing**: Day 5

---

## Acceptance Criteria Summary

### Must Have (Layer 3 Complete)
- ✅ ADM-3A fully supported
- ✅ VT52 fully supported
- ✅ 80×24 display works on both
- ✅ All control sequences correct
- ✅ Grid displays properly
- ✅ Cursor movement works
- ✅ Screen clear works
- ✅ Tested on emulators

### Nice to Have (Future)
- ⏳ VT100 support (1980+ timeframe)
- ⏳ Auto-detection from TERM variable
- ⏳ VT52 graphics mode for grid
- ⏳ Configurable grid characters

---

## Notes

**Historical Context:**
- 1978: ADM-3A is *the* standard terminal
- VT52 common in DEC shops
- VT100 just released (August 1978)
- Most CP-V sites have ADM-3A or equivalent

**Design Decision:**
- Primary target: ADM-3A
- Secondary: VT52
- Future: VT100

**Why ADM-3A First:**
- Most common terminal (1976-1982)
- Simplest control codes
- Used with Unix, CP/M, CP-V
- "$500 terminal" (affordable)
- No dependencies on advanced features

---

**Created**: 2026-01-18
**For**: Layer 3 implementation (Week 8)
**Dependencies**: Layer 2 (UI, DISPLAY) must be complete
**Testing**: Include in acceptance test framework
