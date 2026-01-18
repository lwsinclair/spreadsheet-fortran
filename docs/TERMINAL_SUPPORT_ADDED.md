# Terminal Support - User Stories and Tests Added

**Date**: 2026-01-18
**Status**: ✅ Documentation and test infrastructure ready
**Implementation**: Future (Layer 3, Week 8)

---

## Summary

Added comprehensive user stories and acceptance tests for period-appropriate terminal support. XL spreadsheet will support the two most common terminals from 1978: ADM-3A and VT52.

---

## Terminals Supported

### 1. ADM-3A (Lear Siegler) - PRIMARY
**Status**: Required for Layer 3
**Historical Context**: THE standard terminal of 1978

**Why ADM-3A:**
- Most common terminal (1976-1982)
- "$500 terminal" - affordable
- Used with Unix, CP/M, CP-V, TSO
- Simple control codes
- No dependencies on advanced features

**Technical Details:**
- 24 lines × 80 columns
- ASCII only, no graphics
- Cursor: ESC = row col
- Clear: ESC *
- Arrows: Ctrl-K/J/H/L

### 2. VT52 (DEC) - SECONDARY
**Status**: Required for Layer 3
**Historical Context**: Common in DEC installations

**Why VT52:**
- Common in corporate/educational DEC sites
- CP-V often ran on Sigma systems with DEC terminals
- Simple ESC-based control sequences
- Optional graphics mode for cleaner grid

**Technical Details:**
- 24 lines × 80 columns
- Cursor: ESC Y row col
- Clear: ESC H ESC J
- Arrows: ESC A/B/C/D
- Graphics: ESC F/G (optional)

### 3. VT100 (DEC) - FUTURE
**Status**: Nice to have (released August 1978)

**Why Future:**
- Just released in 1978, expensive initially
- Most sites still using ADM-3A or VT52
- Full support can be added after Layer 3 complete

---

## Files Created

### 1. User Story Documentation
**File**: `docs/USER_STORY_TERMINAL_SUPPORT.md`
**Size**: 600+ lines
**Contents**:
- Complete terminal specifications
- Control sequences for each terminal
- Screen layout design (80×24)
- User stories with acceptance criteria
- Testing strategy
- Manual test procedures
- Historical context
- References to terminal manuals

**Key Sections:**
- Terminal specs (ADM-3A, VT52, VT100)
- User stories (3 stories)
- Screen layout (80×24 grid design)
- Implementation requirements
- Testing procedures (automated + manual)
- Timeline for Layer 3

### 2. Acceptance Tests
**File**: `test/acceptance/scenarios/test_terminal_support.py`
**Size**: 350+ lines
**Contents**:
- 13 automated test scenarios (currently skipped)
- 2 manual test procedures
- Tests for both ADM-3A and VT52
- Terminal detection tests

**Test Classes:**
1. `TestADM3ATerminal` - 6 tests
   - Screen clear
   - Cursor positioning
   - Grid display (80×24)
   - Arrow keys (Ctrl-K/J/H/L)
   - Cell entry and display

2. `TestVT52Terminal` - 4 tests
   - Screen clear (ESC H ESC J)
   - Cursor positioning (ESC Y)
   - Arrow keys (ESC A/B/C/D)
   - Graphics mode (optional)

3. `TestTerminalDetection` - 3 tests
   - Detect ADM-3A from TERM env
   - Detect VT52 from TERM env
   - Default to ADM-3A if unknown

4. `TestTerminalManual` - 2 manual tests
   - ADM-3A manual testing procedure
   - VT52 manual testing procedure

### 3. Terminal Simulator Enhancements
**File**: `test/acceptance/framework/terminal_simulator.py`
**Changes**: Added terminal type support

**New Features:**
- Terminal type support (ADM3A, VT52, VT100)
- Control sequence logging
- Environment variable simulation
- Terminal-specific assertions

**New Methods:**
```python
__init__(width, height, term_type)  # Added term_type parameter
received_sequence(sequence)         # Check for control sequence
set_env(key, value)                 # Set TERM variable
get_env(key)                        # Get TERM variable
screen_is_mostly_blank()            # Verify clear screen
has_overflow()                      # Check 80×24 boundary
has_control_chars_in_display()      # Detect garbled output
screen_has_graphics_chars()         # VT52 graphics mode
log_control_sequence(sequence)      # Log received sequences
```

---

## Screen Layout Design

### 80×24 Layout
```
┌──────────────────────────────────────────────────────────────────────────────┐
│ XL Spreadsheet  A1              READY                              [80 chars]│ ← Line 1: Status
├──────────────────────────────────────────────────────────────────────────────┤
│      A         B         C         D         E         F         G           │ ← Line 2: Headers
│  1│          │          │          │          │          │          │         │
│  2│          │          │          │          │          │          │         │
│  3│          │          │          │          │          │          │         │
│  ...                                                                          │
│ 20│          │          │          │          │          │          │         │ ← Line 21: Last row
├──────────────────────────────────────────────────────────────────────────────┤
│ A1:                                                                           │ ← Line 22: Entry
│ HELP: /H  COMMANDS: /  POINT: .                                              │ ← Line 23: Help
└──────────────────────────────────────────────────────────────────────────────┘
```

**Distribution:**
- Status line: 1 line
- Grid: 20 lines (rows 2-21)
- Entry area: 2 lines (22-23)
- Reserved: 1 line (24)

---

## Control Sequences Reference

### ADM-3A Control Codes
```
Clear Screen:       ESC * (0x1B 0x2A)
Cursor Home:        ESC H (0x1B 0x48)
Cursor Position:    ESC = row col (0x1B 0x3D <row+32> <col+32>)
Cursor Up:          Ctrl-K (0x0B)
Cursor Down:        Ctrl-J (0x0A)
Cursor Left:        Ctrl-H (0x08)
Cursor Right:       Ctrl-L (0x0C)
```

### VT52 Control Codes
```
Clear to End:       ESC J (0x1B 0x4A)
Clear Line:         ESC K (0x1B 0x4B)
Cursor Home:        ESC H (0x1B 0x48)
Cursor Position:    ESC Y row col (0x1B 0x59 <row+32> <col+32>)
Cursor Up:          ESC A (0x1B 0x41)
Cursor Down:        ESC B (0x1B 0x42)
Cursor Right:       ESC C (0x1B 0x43)
Cursor Left:        ESC D (0x1B 0x44)
Enter Graphics:     ESC F (0x1B 0x46)
Exit Graphics:      ESC G (0x1B 0x47)
```

---

## Implementation Checklist

### Layer 3: TERMCPV.FOR (Week 8)
**Status**: Not started (future work)

**Functions to implement:**
- [ ] TRMDET - Detect terminal type from TERM variable
- [ ] TRMINI - Initialize terminal
- [ ] TRMCLR - Clear screen (terminal-specific)
- [ ] TRMPOS - Position cursor (terminal-specific)
- [ ] TRMPUT - Put character at cursor
- [ ] TRMGET - Get character from keyboard
- [ ] TRMARW - Get arrow key (terminal-specific)

**Terminal-Specific Code:**
```fortran
IF (TTYPE .EQ. 1) THEN
C   ADM-3A
    WRITE(*, '(A)') CHAR(27) // '*'
ELSE IF (TTYPE .EQ. 2) THEN
C   VT52
    WRITE(*, '(A)') CHAR(27) // 'H' // CHAR(27) // 'J'
ENDIF
```

### Testing
**Automated Tests**: 13 tests written, currently skipped
**Manual Tests**: 2 procedures documented

**When to unskip tests:**
- After TERMCPV.FOR implementation
- After DISPLAY.FOR can render to terminal
- After UI.FOR can handle keyboard input

---

## Testing Resources

### Emulators Available
**ADM-3A:**
- xterm with `TERM=adm3a`
- PuTTY with ADM-3A mode
- Online: https://www.masswerk.at/misc/adm3a/

**VT52:**
- xterm with `TERM=vt52`
- PuTTY with VT52 mode
- macOS Terminal.app (partial support)

### Reference Documents
**ADM-3A:**
- Manual: http://www.bitsavers.org/pdf/learSiegler/ADM3A_Owners_Manual.pdf
- Wiki: https://vt100.net/wiki/index.php/Lear_Siegler_ADM-3A

**VT52:**
- Manual: http://vt100.net/docs/vt52-ug/
- Wiki: https://vt100.net/wiki/index.php/VT52

---

## Integration Points

### Used By:
- **DISPLAY.FOR** - Calls TERMCPV to render grid
- **UI.FOR** - Calls TERMCPV to read keyboard
- **CALCSH.FOR** - Initializes terminal on startup

### Dependencies:
- **STRUTIL.FOR** ✅ Complete
- **Layer 2 (UI, DISPLAY)** - Must be complete first
- **CP-V terminal I/O** - simh provides terminal emulation

---

## Timeline

**Current Status**: Documentation and tests ready
**Layer 3 Start**: Week 8 (after Layers 1-2 complete)
**Implementation Time**: ~5 days
**Testing Time**: 2 days

**Milestones:**
- Day 1: Terminal detection and initialization
- Day 2-3: ADM-3A support
- Day 4: VT52 support
- Day 5: Testing and bug fixes

---

## Success Criteria

### Must Have (Layer 3 Complete)
- ✅ ADM-3A fully supported
- ✅ VT52 fully supported
- ✅ 80×24 display works correctly
- ✅ All control sequences work
- ✅ Grid displays properly
- ✅ Cursor movement correct
- ✅ 13 automated tests pass
- ✅ 2 manual tests pass

### Nice to Have (Future)
- ⏳ VT100 support
- ⏳ VT52 graphics mode for grid
- ⏳ Configurable grid characters
- ⏳ Color support (VT100)

---

## Benefits

1. **Historical Accuracy**: Supports actual 1978 terminals
2. **Broad Compatibility**: Works with most common terminals of era
3. **Testing Ready**: Complete test infrastructure in place
4. **Well Documented**: User stories and manual tests prepared
5. **Future Proof**: Easy to add VT100 later

---

## Summary

✅ **Complete user story documentation** (600+ lines)
✅ **13 automated acceptance tests** (ready for Layer 3)
✅ **2 manual test procedures** (documented)
✅ **Terminal simulator enhanced** (ADM-3A, VT52 support)
✅ **Screen layout designed** (80×24 optimized)
✅ **Control sequences documented** (both terminals)
✅ **Implementation plan ready** (Week 8)

**Status**: Ready for Layer 3 implementation!

When Layer 3 is implemented, just unskip the tests and they'll validate the terminal support automatically.

---

**Created**: 2026-01-18
**For**: Layer 3 - Terminal I/O (Week 8)
**Dependencies**: Layer 2 must complete first
**Tests**: 13 automated + 2 manual = 15 total
