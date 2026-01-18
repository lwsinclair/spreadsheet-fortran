"""
Acceptance Test: Basic Entry

User Story:
    As a user
    I want to enter numbers into cells
    So that I can create a spreadsheet

Scenario: User enters a number
    GIVEN XL spreadsheet is running
    WHEN I navigate to cell A1
    AND I enter the number "100"
    THEN cell A1 should display "100"
    AND the value should be stored

Scenario: User enters multiple numbers
    GIVEN XL spreadsheet is running
    WHEN I enter "10" in A1
    AND I enter "20" in A2
    AND I enter "30" in A3
    THEN A1 shows "10"
    AND A2 shows "20"
    AND A3 shows "30"
"""

import sys
from pathlib import Path
import pytest

# Add framework to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from terminal_simulator import TerminalSimulator
from xl_harness import XLSpreadsheet


class TestBasicEntry:
    """Test basic cell entry functionality"""

    @pytest.mark.skip(reason="Awaiting implementation of Layers 1-3")
    def test_user_enters_single_number(self):
        """
        User can enter a single number in a cell.

        This test will PASS once:
        - Layer 1 (CELLS.FOR) stores cell values
        - Layer 2 (UI.FOR, DISPLAY.FOR) handles input and display
        - Layer 3 (TERMTEST.FOR) provides I/O
        - Main program (CALCSH.FOR) ties it together
        """
        terminal = TerminalSimulator()

        with XLSpreadsheet(terminal) as xl:
            # Start the spreadsheet
            xl.start()

            # User navigates to A1 and enters 100
            xl.enter_cell(col=1, row=1, value="100")

            # Verify cell displays 100
            xl.assert_cell_shows(col=1, row=1, expected="100")

            # Verify screen shows the value
            xl.assert_screen_shows("100")

    @pytest.mark.skip(reason="Awaiting implementation of Layers 1-3")
    def test_user_enters_multiple_numbers(self):
        """
        User can enter multiple numbers in different cells.

        This test validates:
        - Cell storage (CELLS.FOR)
        - Multiple cell navigation
        - Display updates correctly
        """
        terminal = TerminalSimulator()

        with XLSpreadsheet(terminal) as xl:
            xl.start()

            # Enter values in cells A1, A2, A3
            xl.enter_cell(col=1, row=1, value="10")
            xl.enter_cell(col=1, row=2, value="20")
            xl.enter_cell(col=1, row=3, value="30")

            # Verify all cells show correct values
            xl.assert_cell_shows(col=1, row=1, expected="10")
            xl.assert_cell_shows(col=1, row=2, expected="20")
            xl.assert_cell_shows(col=1, row=3, expected="30")

    @pytest.mark.skip(reason="Awaiting implementation of Layers 1-3")
    def test_user_overwrites_cell_value(self):
        """
        User can overwrite existing cell value.

        This test validates:
        - Cell update (not just insert)
        - Display refresh on change
        """
        terminal = TerminalSimulator()

        with XLSpreadsheet(terminal) as xl:
            xl.start()

            # Enter initial value
            xl.enter_cell(col=1, row=1, value="100")
            xl.assert_cell_shows(col=1, row=1, expected="100")

            # Overwrite with new value
            xl.enter_cell(col=1, row=1, value="200")
            xl.assert_cell_shows(col=1, row=1, expected="200")

    @pytest.mark.skip(reason="Awaiting implementation of Layers 1-3")
    def test_user_enters_negative_number(self):
        """
        User can enter negative numbers.

        This test validates:
        - Parser handles negative sign
        - Display shows negative correctly
        """
        terminal = TerminalSimulator()

        with XLSpreadsheet(terminal) as xl:
            xl.start()

            xl.enter_cell(col=1, row=1, value="-42")
            xl.assert_cell_shows(col=1, row=1, expected="-42")

    @pytest.mark.skip(reason="Awaiting implementation of Layers 1-3")
    def test_user_enters_decimal_number(self):
        """
        User can enter decimal numbers.

        This test validates:
        - Parser handles real numbers
        - Display formats decimals correctly
        """
        terminal = TerminalSimulator()

        with XLSpreadsheet(terminal) as xl:
            xl.start()

            xl.enter_cell(col=1, row=1, value="3.14")
            xl.assert_cell_shows(col=1, row=1, expected="3.14")


class TestBasicEntryManual:
    """
    Manual test procedures for validation on CP-V emulator.

    These describe the manual steps to perform when validating
    on the actual Xerox Sigma CP-V system.
    """

    def test_manual_basic_entry(self):
        """
        MANUAL TEST: Basic Entry

        Prerequisites:
        - XL compiled and running on CP-V
        - Terminal connected (VT100 or compatible)

        Steps:
        1. Start XL
        2. Observe empty grid with cursor at A1
        3. Type "100" and press Enter
        4. Observe "100" appears in cell A1
        5. Press Down arrow
        6. Type "200" and press Enter
        7. Observe "200" appears in cell A2
        8. Press Up arrow
        9. Observe cursor at A1 showing "100"

        Expected Results:
        - Cell A1 contains 100
        - Cell A2 contains 200
        - Navigation works correctly
        - Display updates properly

        Pass Criteria:
        - All values displayed correctly
        - Navigation smooth and responsive
        - No screen artifacts or corruption
        """
        pytest.skip("Manual test - run on CP-V emulator")


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
