# XL Spreadsheet - Acceptance Test Framework

## Overview

**Status:** ✅ Test Framework Complete
**Purpose:** Drive development of Layers 1-3 using TDD at the system level
**Current State:** Tests written, will PASS when implementation complete

---

## What We Built

### 1. Terminal Simulator (terminal_simulator.py)
**Purpose:** Headless terminal emulation for automated testing

**Features:**
- 80x24 character grid (configurable)
- Cursor positioning
- Input buffer for simulated keystrokes
- Screen capture for assertions
- Support for special keys (arrows, ESC, Enter)
- Output logging for debugging

**Usage:**
```python
terminal = TerminalSimulator()
terminal.send_keys("100{ENTER}")
terminal.assert_screen_contains("100")
```

### 2. XL Test Harness (xl_harness.py)
**Purpose:** High-level interface for spreadsheet testing

**Features:**
- Compile FORTRAN with test mode (TERMTEST.FOR)
- Start/stop XL process
- Navigate to cells
- Enter values and formulas
- Send commands (/S, /L, /BR, etc.)
- Assert on cell values and screen contents

**Usage:**
```python
with XLSpreadsheet(terminal) as xl:
    xl.start()
    xl.enter_cell(col=1, row=1, value="100")
    xl.assert_cell_shows(col=1, row=1, expected="100")
```

### 3. Acceptance Test Scenarios
**Purpose:** Define user stories that drive implementation

**Scenarios Written:**
1. **Basic Entry** (test_basic_entry.py)
   - Enter single number
   - Enter multiple numbers
   - Overwrite cell value
   - Negative numbers
   - Decimal numbers

2. **Formulas** (test_formulas.py)
   - Simple addition
   - Automatic recalculation
   - Multiplication
   - Operator precedence
   - Parentheses
   - @SUM function
   - Formula chains
   - Circular reference detection

**Status:** All tests marked with `@pytest.mark.skip` until implementation complete

### 4. Emulator Setup Scripts
**Purpose:** Deploy and test on actual CP-V system

**Scripts:**
- `setup_emulator.sh` - Installs simh, downloads CP-V kit
- `make_tape.sh` - Creates tape images for file transfer
- `boot_cpv.ini` - Emulator boot configuration
- `QUICKSTART.md` - Usage guide

---

## Test-Driven Development Workflow

### Current State (Layer 0 Only)
```
Tests: ❌ FAIL (NotImplementedError)
Reason: Layers 1-3 not implemented yet
Expected: This is correct - tests drive implementation
```

### Development Flow

```
┌─────────────────────────────────────────────┐
│ 1. Acceptance tests written (NOW)          │
│    Status: FAIL - awaiting implementation  │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 2. Implement Layer 1 (CELLS, PARSE, EVAL)  │
│    Status: NOT STARTED                      │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 3. Some tests start PASSING               │
│    - Basic entry tests pass                │
│    - Simple formula tests pass             │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 4. Implement Layer 2 (UI, DISPLAY)        │
│    More tests pass                          │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 5. Implement Layer 3 (TERMTEST, TERMCPV)   │
│    All automated tests PASS                 │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 6. Manual testing on CP-V emulator         │
│    Run manual test procedures              │
└─────────────────────────────────────────────┘
              ↓
┌─────────────────────────────────────────────┐
│ 7. DONE: Interactive spreadsheet ready!    │
│    User can connect and use XL             │
└─────────────────────────────────────────────┘
```

---

## Running the Tests

### Now (Layer 0 Only)
```bash
cd test/acceptance
pytest scenarios/ -v
```

**Expected Output:**
```
test_basic_entry.py::test_user_enters_single_number SKIPPED
test_basic_entry.py::test_user_enters_multiple_numbers SKIPPED
test_formulas.py::test_simple_addition_formula SKIPPED
...

===== X skipped in X.XXs =====
```

**Why Skipped?** Layers 1-3 not implemented yet.

### After Layer 1 (CELLS, PARSE, EVAL)
```bash
pytest scenarios/test_formulas.py -v
```

**Expected:** Some tests will START PASSING as functionality is implemented.

### After Layers 2-3 (UI, DISPLAY, TERMINAL)
```bash
pytest scenarios/ -v
```

**Expected:** ALL automated tests PASS.

---

## Test Scenarios Defined

### Scenario 1: Basic Entry
```python
def test_user_enters_single_number():
    """
    GIVEN XL is running
    WHEN user enters "100" in A1
    THEN A1 displays "100"
    """
```

**Dependencies:**
- CELLS.FOR - Store cell value
- DISPLAY.FOR - Show value on screen
- TERMTEST.FOR - Handle I/O

**Status:** ❌ NotImplementedError

### Scenario 2: Simple Formula
```python
def test_simple_addition_formula():
    """
    GIVEN A1=10, A2=20
    WHEN user enters "+A1+A2" in A3
    THEN A3 displays "30"
    """
```

**Dependencies:**
- PARSE.FOR - Parse "+A1+A2"
- EVAL.FOR - Evaluate formula
- CELLS.FOR - Store and retrieve values
- DEPS.FOR - Track dependencies

**Status:** ❌ NotImplementedError

### Scenario 3: Automatic Recalculation
```python
def test_automatic_recalculation():
    """
    GIVEN A1=10, A2=20, A3=+A1+A2 (shows 30)
    WHEN user changes A1 to 15
    THEN A3 automatically updates to 35
    """
```

**Dependencies:**
- DEPS.FOR - Dependency tracking
- RECALC.FOR - Topological sort
- All of the above

**Status:** ❌ NotImplementedError

### Scenario 4: Circular Reference Detection
```python
def test_circular_reference_detection():
    """
    GIVEN A1=+A2, A2=+A1
    THEN display shows "CIRC" error
    """
```

**Dependencies:**
- DEPS.FOR - Circular detection algorithm

**Status:** ❌ NotImplementedError

---

## Manual Testing on CP-V

### Setup Emulator
```bash
cd emulator
./scripts/setup_emulator.sh
```

**This will:**
1. Install simh Sigma emulator
2. Download sigma-cpv-kit (CP-V F00)
3. Create boot configuration
4. Set up file transfer scripts

### Start CP-V
```bash
cd emulator
simh-sigma boot_cpv.ini
```

### Connect Terminal
In another terminal:
```bash
telnet localhost 5000
```

### Manual Test Procedure
```
1. Login as SYSTEM/SYSTEM
2. Transfer XL FORTRAN files to CP-V
3. Compile XL:
   $ RUN FORTRAN
   *SOURCE=CALCSH.FOR
   *OBJECT=XL.OBJ
   *LINK
   *GO

4. Run XL:
   $ RUN XL

5. Perform manual tests:
   - Enter "100" in A1 → verify displays "100"
   - Enter "200" in A2 → verify displays "200"
   - Enter "+A1+A2" in A3 → verify displays "300"
   - Change A1 to "150" → verify A3 updates to "350"
   - Test navigation with arrow keys
   - Test commands (/S, /L, /BR, etc.)
```

---

## When Can We Run Interactively?

### Current Progress: 12.5% (1/8 layers)
```
✅ Layer 0: STRUTIL.FOR      - COMPLETE
❌ Layer 1: CELLS.FOR        - NOT STARTED (3 weeks)
❌ Layer 1: PARSE.FOR        - NOT STARTED
❌ Layer 1: EVAL.FOR         - NOT STARTED
❌ Layer 2: UI.FOR           - NOT STARTED (2 weeks)
❌ Layer 2: DISPLAY.FOR      - NOT STARTED
❌ Layer 3: TERMTEST.FOR     - NOT STARTED (1 week)
❌ Main:    CALCSH.FOR       - NOT STARTED (1 week)
```

**Estimated Timeline:**
- Layer 1 (Parser/Evaluator): Weeks 3-5 (3 weeks)
- Layer 2 (UI/Display): Weeks 6-7 (2 weeks)
- Layer 3 (Terminal I/O): Week 8 (1 week)
- Integration: Week 9 (1 week)

**Total:** ~7 weeks from now

### What We Can Do NOW ✅
1. ✅ Run automated tests (they will SKIP until implementation)
2. ✅ Set up CP-V emulator
3. ✅ Test FORTRAN IV compilation on CP-V
4. ✅ Validate STRUTIL.FOR on CP-V (Layer 0 only)

### What We CAN'T Do Yet ❌
1. ❌ Run interactive spreadsheet
2. ❌ Enter formulas
3. ❌ See recalculation
4. ❌ Use slash commands
5. ❌ Save/load worksheets

---

## Test Coverage Map

### Layer 0: STRUTIL.FOR ✅ 100%
```
✅ 41 unit tests passing
✅ All functions tested
✅ Edge cases covered
✅ FORTRAN IV compliant
```

### Layer 1: Calculation Engine ❌ 0%
```
Acceptance tests defined:
  ❌ Basic entry (5 tests)
  ❌ Simple formulas (3 tests)
  ❌ Recalculation (1 test)
  ❌ Formula chains (1 test)
  ❌ Circular detection (1 test)

Status: All tests SKIP (awaiting implementation)
```

### Layer 2: Application ❌ 0%
```
Acceptance tests defined:
  ❌ Navigation (TBD)
  ❌ Commands (TBD)
  ❌ Point mode (TBD)
  ❌ Save/Load (TBD)

Status: Tests not yet written
```

### Layer 3: Terminal I/O ❌ 0%
```
Acceptance tests defined:
  ❌ Screen rendering (TBD)
  ❌ Keyboard input (TBD)
  ❌ Cursor movement (TBD)

Status: Tests not yet written
```

---

## Benefits of This Approach

### 1. Clear Definition of "Done"
Tests define exactly what needs to work for each layer.

### 2. Incremental Progress
Each function implemented makes more tests pass.

### 3. Regression Prevention
Once tests pass, they stay passing (or we know immediately if something breaks).

### 4. Documentation
Tests serve as executable specifications.

### 5. Confidence
When all tests pass, we KNOW the system works.

### 6. Focus
Tests tell us exactly what to build next.

---

## Next Steps

### Immediate (This Week)
1. ✅ Acceptance test framework complete
2. ✅ Emulator setup scripts ready
3. ⚠️ Set up CP-V emulator (optional, can do anytime)
4. ⚠️ Test STRUTIL.FOR on CP-V (validation only)

### Layer 1 Development (Weeks 3-5)
1. Write unit tests for CELLS.FOR
2. Implement CELLS.FOR (hash table)
3. Write unit tests for PARSE.FOR
4. Implement PARSE.FOR (shunting-yard)
5. Write unit tests for EVAL.FOR
6. Implement EVAL.FOR (postfix evaluation)
7. Implement DEPS.FOR and RECALC.FOR
8. Watch acceptance tests start PASSING!

### Layer 2 Development (Weeks 6-7)
1. Implement UI.FOR (mode machine)
2. Implement DISPLAY.FOR (screen rendering)
3. Implement COMMANDS.FOR (slash commands)
4. More acceptance tests PASS

### Layer 3 + Integration (Weeks 8-9)
1. Implement TERMTEST.FOR (headless)
2. Implement TERMCPV.FOR (CP-V terminal)
3. Implement CALCSH.FOR (main program)
4. ALL automated tests PASS
5. Manual testing on CP-V emulator
6. ✅ DONE: Interactive spreadsheet ready!

---

## Files Created

### Framework
```
test/acceptance/framework/
├── __init__.py
├── terminal_simulator.py     (200+ lines)
└── xl_harness.py             (300+ lines)
```

### Scenarios
```
test/acceptance/scenarios/
├── __init__.py
├── test_basic_entry.py       (6 tests)
└── test_formulas.py          (9 tests)
```

### Emulator
```
emulator/
├── scripts/
│   ├── setup_emulator.sh     (Install & configure)
│   └── make_tape.sh          (File transfer helper)
└── QUICKSTART.md             (Usage guide)
```

### Documentation
```
docs/
├── INTERACTIVE_READINESS.md  (Status assessment)
└── ACCEPTANCE_TEST_FRAMEWORK.md  (This file)
```

---

## Summary

### Question: Can we simulate running this on the target machine with terminal emulator?

**Answer:** Not yet - we're at 12.5% completion (Layer 0 only).

### Question: Can we create test coverage for that user story?

**Answer:** YES! ✅ We just did.

**What We Have:**
- ✅ Acceptance test framework (terminal simulator + harness)
- ✅ 15 acceptance tests defined (currently skipped)
- ✅ Emulator setup scripts ready
- ✅ Manual test procedures documented
- ✅ Clear path to interactive use

**What Happens Next:**
- Tests drive implementation of Layers 1-3
- Each layer makes more tests pass
- When all tests pass → interactive spreadsheet ready
- Then we can use it on CP-V emulator

**Timeline:** ~7 weeks to interactive use

**Current Status:** Foundation complete, acceptance tests ready, framework prepared for success! 🚀

---

**Created:** 2026-01-18
**Status:** ✅ Test Framework Complete
**Next:** Implement Layer 1 using TDD
