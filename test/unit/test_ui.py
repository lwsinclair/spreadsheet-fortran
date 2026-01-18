"""
Unit Tests: UI.FOR - User Interface State Machine

Tests the UI state machine that manages application modes.

Modes:
    NAV (1)     - Navigation mode (moving cursor)
    ENTRY (2)   - Data entry mode (typing formula/value)
    POINT (3)   - Point mode (selecting cells for formula)
    COMMAND (4) - Command mode (executing commands)

Features:
    - Mode management and transitions
    - Keyboard input processing
    - Cursor position tracking
    - Input buffer management

Test Coverage:
    - Initialize UI
    - Mode transitions
    - Keyboard input handling
    - Cursor movement
    - Input buffer operations
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestUIBasic:
    """Basic UI operations"""

    def test_ui_init(self):
        """
        Initialize UI system.

        Should initialize to NAV mode at cell A1.
        """
        test_program = """
      PROGRAM TEST
      INTEGER MODE, COL, ROW

C     Initialize UI
      CALL UIINI

C     Get current mode and position
      CALL UIGET(MODE, COL, ROW)

C     Should be in NAV mode (1)
      IF (MODE .NE. 1) STOP 1

C     Should be at A1
      IF (COL .NE. 1) STOP 2
      IF (ROW .NE. 1) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_ui_set_mode(self):
        """
        Set UI mode.

        Action: UIMODE(2) - set to ENTRY mode
        Result: Mode changes to ENTRY
        """
        test_program = """
      PROGRAM TEST
      INTEGER MODE, COL, ROW

      CALL UIINI

C     Set to ENTRY mode (2)
      CALL UIMODE(2)

C     Get current mode
      CALL UIGET(MODE, COL, ROW)

C     Should be in ENTRY mode
      IF (MODE .NE. 2) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_ui_cursor_move(self):
        """
        Move cursor position.

        Setup: Cursor at A1
        Action: UIMOVE(2, 3) - move to B3
        Result: Cursor at B3
        """
        test_program = """
      PROGRAM TEST
      INTEGER MODE, COL, ROW

      CALL UIINI

C     Move to B3
      CALL UIMOVE(2, 3)

C     Get position
      CALL UIGET(MODE, COL, ROW)

C     Should be at B3
      IF (COL .NE. 2) STOP 1
      IF (ROW .NE. 3) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestUIModes:
    """Mode transition tests"""

    def test_ui_nav_to_entry(self):
        """
        Transition from NAV to ENTRY mode.

        Trigger: User types printable character
        Result: Enter ENTRY mode, character in buffer
        """
        test_program = """
      PROGRAM TEST
      INTEGER MODE, COL, ROW

      CALL UIINI

C     Should start in NAV mode
      CALL UIGET(MODE, COL, ROW)
      IF (MODE .NE. 1) STOP 1

C     Enter ENTRY mode
      CALL UIMODE(2)

C     Should be in ENTRY mode
      CALL UIGET(MODE, COL, ROW)
      IF (MODE .NE. 2) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_ui_entry_to_nav(self):
        """
        Transition from ENTRY to NAV mode.

        Trigger: User presses ESC or RETURN
        Result: Return to NAV mode
        """
        test_program = """
      PROGRAM TEST
      INTEGER MODE, COL, ROW

      CALL UIINI

C     Enter ENTRY mode
      CALL UIMODE(2)

C     Return to NAV mode
      CALL UIMODE(1)

C     Should be in NAV mode
      CALL UIGET(MODE, COL, ROW)
      IF (MODE .NE. 1) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestUIBuffer:
    """Input buffer tests"""

    def test_ui_buffer_add(self):
        """
        Add character to input buffer.

        Action: UIBUF(1, 65) - add 'A'
        Result: Buffer contains 'A'
        """
        test_program = """
      PROGRAM TEST
      INTEGER BUF(80), BLEN

      CALL UIINI

C     Add 'A' to buffer (ASCII 65)
      CALL UIBUF(1, 65)

C     Get buffer
      CALL UIGETB(BUF, BLEN)

C     Should have 1 character
      IF (BLEN .NE. 1) STOP 1

C     Should be 'A'
      IF (BUF(1) .NE. 65) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_ui_buffer_clear(self):
        """
        Clear input buffer.

        Setup: Buffer contains text
        Action: UIBUF(3, 0) - clear buffer
        Result: Buffer empty
        """
        test_program = """
      PROGRAM TEST
      INTEGER BUF(80), BLEN

      CALL UIINI

C     Add some characters
      CALL UIBUF(1, 65)  ! A
      CALL UIBUF(1, 66)  ! B

C     Clear buffer
      CALL UIBUF(3, 0)

C     Get buffer
      CALL UIGETB(BUF, BLEN)

C     Should be empty
      IF (BLEN .NE. 0) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR', 'layer2/UI.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestUIKeyboard:
    """Keyboard input tests"""

    @pytest.mark.skip(reason="Keyboard handler - implement after basic modes")
    def test_ui_key_arrow_right(self):
        """
        Handle arrow right key.

        Mode: NAV
        Key: Arrow right
        Result: Cursor moves right
        """
        pass

    @pytest.mark.skip(reason="Keyboard handler - implement after basic modes")
    def test_ui_key_return(self):
        """
        Handle return key.

        Mode: ENTRY
        Key: RETURN
        Result: Accept input, return to NAV
        """
        pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
