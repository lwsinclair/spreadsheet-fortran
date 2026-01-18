# XL Spreadsheet - Interactive Readiness Assessment

## Current Status: Not Ready for Interactive Use

**Date:** 2026-01-18
**Question:** Can we simulate running this on the target machine with terminal emulator?
**Answer:** Not yet, but we can prepare for it now.

---

## What We Have ✅ (Layer 0 Only)

### String Utilities (STRUTIL.FOR)
```fortran
✅ STREQ   - String equality
✅ STRCPY  - String copy
✅ STRFND  - Find substring
✅ STRTRM  - Trim spaces
✅ ITOA    - Integer to string
✅ ATOI    - String to integer
✅ RTOA    - Real to string
✅ ATOR    - String to real
✅ COLTOA  - Column number to letters
✅ ATOCOL  - Letters to column number
✅ FMTCEL  - Format cell reference
✅ PARCEL  - Parse cell reference
```

**Status:** Foundation only - no user-facing functionality yet

---

## What We Need ❌ (Layers 1-3 + Main)

### Layer 1: Calculation Engine (0% Complete)
```fortran
❌ CELLS.FOR    - Cell storage with hash table
❌ PARSE.FOR    - Formula parser (shunting-yard)
❌ EVAL.FOR     - Expression evaluator
❌ DEPS.FOR     - Dependency graph
❌ RECALC.FOR   - Recalculation engine
```

**Required for:** Storing data, parsing formulas, calculating results

### Layer 2: Application Logic (0% Complete)
```fortran
❌ MSG.FOR      - Messages and prompts
❌ UI.FOR       - Mode state machine (NAV/ENTRY/POINT)
❌ DISPLAY.FOR  - Screen rendering and grid display
❌ COMMANDS.FOR - Command handlers (/B, /E, /C, /M, etc.)
❌ FILES.FOR    - Save/Load worksheets
```

**Required for:** User interaction, display, commands

### Layer 3: Terminal I/O (0% Complete)
```fortran
❌ TERMCPV.FOR  - CP-V terminal driver
❌ TERMTEST.FOR - Test harness terminal driver
```

**Required for:** Reading keyboard, writing screen

### Main Program (0% Complete)
```fortran
❌ CALCSH.FOR   - Main event loop
```

**Required for:** Running the program

---

## Dependency Chain

To run interactively, we need:

```
CALCSH.FOR (main)
    ↓
TERMCPV.FOR (I/O) + UI.FOR (mode machine)
    ↓
DISPLAY.FOR (screen) + COMMANDS.FOR (handlers)
    ↓
CELLS.FOR (storage) + EVAL.FOR (calculation)
    ↓
PARSE.FOR (formulas)
    ↓
STRUTIL.FOR (strings) ✅ ← WE ARE HERE
```

**Completion:** 1/8 layers (12.5%)

---

## What We Can Do NOW

While we can't run the full spreadsheet, we CAN:

### 1. Set Up Emulator Infrastructure
- Install simh Sigma emulator
- Download CP-V F00 (1978 release)
- Configure tape images for file transfer
- Test FORTRAN IV compiler on CP-V
- Create deployment scripts

### 2. Create User Acceptance Test Framework
- Define user story scenarios
- Build terminal interaction test harness
- Create automated acceptance tests
- Use tests to drive development (TDD at system level)

### 3. Build Headless Test Driver
- TERMTEST.FOR - simulates terminal I/O
- Allows testing without actual terminal
- Can run in CI/CD pipeline
- Validates user interactions programmatically

---

## User Acceptance Test Scenarios

Let me define the test scenarios that will drive development:

### Scenario 1: Basic Entry
```
User starts XL
User sees grid and prompt
User navigates to A1
User enters "100"
User presses Enter
User sees "100" in A1
```

### Scenario 2: Simple Formula
```
User navigates to A1, enters "10"
User navigates to A2, enters "20"
User navigates to A3, enters "+A1+A2"
User sees "30" in A3
User changes A1 to "15"
User sees "35" in A3 (recalculated)
```

### Scenario 3: Point-to-Reference Mode
```
User navigates to B1
User types "+."
User sees "+[A1]" with A1 highlighted
User presses → (arrow right)
User sees "+[B1]" with B1 highlighted
User presses .
User sees "+B1" (anchored)
User types "*2"
User presses Enter
User sees formula "+B1*2" evaluate
```

### Scenario 4: Save and Load
```
User creates worksheet with data
User types "/S"
User enters filename "TEST.CAL"
User sees "Saved" confirmation
User exits (/Q)
User restarts XL
User types "/L"
User enters "TEST.CAL"
User sees original data restored
```

### Scenario 5: Commands
```
User enters data in A1-A5
User types "/BR" (blank range)
User enters "A1:A5"
User sees cells cleared
User types "/U" (undo)
User sees data restored
```

---

## Implementation Plan

### Phase 1: Terminal Test Infrastructure (Now)
**Deliverable:** Headless test driver that can simulate user interactions

```python
# test/acceptance/test_user_stories.py
def test_basic_entry():
    """User can enter number in cell"""
    terminal = TerminalSimulator()
    xl = XLSpreadsheet(terminal)

    xl.start()
    xl.send_keys("100\n")  # Enter 100

    assert xl.cell_value(1, 1) == 100
    assert xl.screen_shows("100")
```

**Status:** Can build NOW (before Layers 1-3 complete)

### Phase 2: Emulator Setup (Now)
**Deliverable:** CP-V running on simh, can compile FORTRAN

**Steps:**
1. Install simh Sigma emulator
2. Download sigma-cpv-kit
3. Boot CP-V F00
4. Test FORTRAN IV compiler
5. Create file transfer scripts

**Status:** Can do NOW (independent of code)

### Phase 3: Implement to Pass Tests (Weeks 3-7)
**Deliverable:** Layers 1-3 implemented to pass acceptance tests

**Order:**
1. TERMTEST.FOR - headless terminal driver
2. CELLS.FOR - cell storage
3. PARSE.FOR - formula parsing
4. EVAL.FOR - evaluation
5. UI.FOR - mode machine
6. DISPLAY.FOR - screen rendering
7. COMMANDS.FOR - command handlers

### Phase 4: Integration Testing (Week 8)
**Deliverable:** Full interactive session on emulator

**Validation:**
- Start XL on CP-V emulator
- Connect via terminal emulator
- Execute all acceptance test scenarios manually
- Verify behavior matches specifications

---

## Acceptance Test Framework Design

### Terminal Simulator
```python
class TerminalSimulator:
    """Simulates VT100-style terminal for testing"""

    def __init__(self):
        self.screen = [[' '] * 80 for _ in range(24)]
        self.cursor_row = 0
        self.cursor_col = 0
        self.input_buffer = []

    def send_keys(self, keys: str):
        """Simulate user typing keys"""
        self.input_buffer.extend(list(keys))

    def read_char(self) -> str:
        """Read next character from input"""
        if self.input_buffer:
            return self.input_buffer.pop(0)
        return ''

    def write_char(self, row: int, col: int, char: str):
        """Write character to screen"""
        self.screen[row][col] = char

    def get_screen_text(self) -> str:
        """Get current screen contents"""
        return '\n'.join(''.join(row) for row in self.screen)

    def assert_screen_contains(self, text: str):
        """Verify screen contains text"""
        screen = self.get_screen_text()
        assert text in screen, f"'{text}' not found on screen"
```

### XL Test Harness
```python
class XLSpreadsheet:
    """Test harness for XL spreadsheet"""

    def __init__(self, terminal: TerminalSimulator):
        self.terminal = terminal
        self.fortran_process = None

    def start(self):
        """Start XL spreadsheet"""
        # Compile with TERMTEST.FOR (headless driver)
        # Launch in subprocess
        # Connect to terminal simulator
        pass

    def cell_value(self, col: int, row: int) -> any:
        """Get current value of cell"""
        # Send command to get cell value
        # Parse response
        pass

    def screen_shows(self, text: str) -> bool:
        """Check if text appears on screen"""
        return text in self.terminal.get_screen_text()
```

### Test Suite Structure
```
test/acceptance/
├── framework/
│   ├── terminal_simulator.py    # Terminal emulation
│   ├── xl_harness.py            # XL control
│   └── assertions.py            # Custom assertions
├── scenarios/
│   ├── test_basic_entry.py      # Scenario 1
│   ├── test_formulas.py         # Scenario 2
│   ├── test_point_mode.py       # Scenario 3
│   ├── test_save_load.py        # Scenario 4
│   └── test_commands.py         # Scenario 5
└── fixtures/
    └── worksheets/              # Sample .CAL files
```

---

## Timeline to Interactive Use

| Phase | Deliverable | Duration | Status |
|-------|-------------|----------|--------|
| ✅ Layer 0 | STRUTIL.FOR | Week 1-2 | COMPLETE |
| 🔄 Test Framework | Acceptance tests | Week 2 | CAN DO NOW |
| 🔄 Emulator Setup | CP-V running | Week 2 | CAN DO NOW |
| ❌ Layer 1 | Parser/Evaluator | Week 3-5 | Not started |
| ❌ Layer 2 | UI/Display | Week 6-7 | Not started |
| ❌ Layer 3 | Terminal I/O | Week 8-9 | Not started |
| ❌ Integration | End-to-end tests | Week 9 | Not started |

**Estimated time to interactive use:** 7-8 weeks from now

**What we can deliver NOW:**
- ✅ Test framework (this week)
- ✅ Emulator setup (this week)
- ✅ Acceptance test scenarios (this week)

---

## Next Steps (Immediate)

### Step 1: Create Acceptance Test Framework
```bash
mkdir -p test/acceptance/framework
mkdir -p test/acceptance/scenarios
mkdir -p test/acceptance/fixtures
```

Create:
- terminal_simulator.py
- xl_harness.py
- test_basic_entry.py (first scenario)

### Step 2: Set Up Emulator
```bash
cd emulator
./scripts/install_simh.sh
./scripts/download_cpv.sh
./scripts/boot_cpv.sh
```

### Step 3: Write First Acceptance Test
```python
# test/acceptance/scenarios/test_basic_entry.py
def test_user_enters_number():
    """
    GIVEN XL is running
    WHEN user navigates to A1 and enters "100"
    THEN cell A1 shows value 100
    """
    # This test will FAIL until Layers 1-3 are built
    # That's the point - it drives development
```

### Step 4: Use Tests to Drive Development
- Each layer implementation makes more tests pass
- Final integration: all acceptance tests pass
- Then we can run on emulator with real terminal

---

## Answer to Original Question

**Q:** Are we at a point where we could simulate running this on the target machine and have me use a terminal emulator to connect and use the spreadsheet as a user?

**A:** Not yet - we're at 12.5% completion (Layer 0 only). We need Layers 1-3 + main program.

**BUT:** We can absolutely prepare for it NOW by:
1. ✅ Creating the acceptance test framework
2. ✅ Setting up the CP-V emulator
3. ✅ Defining user story test scenarios
4. ✅ Building terminal simulation harness

These tests will FAIL initially (no implementation yet), but they'll guide development and tell us when we're ready for interactive use.

**Can we create test coverage for that user story?**

**A:** YES! Let me create it now. The tests will drive development of Layers 1-3.

---

**Recommendation:**
Let's build the acceptance test framework NOW, then use it to drive implementation of Layers 1-3. When all acceptance tests pass, we'll know the spreadsheet is ready for interactive use.

Shall I proceed with creating the terminal simulator and acceptance test framework?
