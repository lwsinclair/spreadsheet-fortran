"""
Unit Tests: MSG.FOR - Message String Module

Tests the message string storage and retrieval system.

Purpose: Store and retrieve application messages (errors, help, prompts)

Features:
    - Store error messages
    - Store help text
    - Store prompt strings
    - Format messages with parameters
    - Message categories

Test Coverage:
    - Initialize message system
    - Get error messages
    - Get help text
    - Get prompt strings
    - Format messages with values
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestMsgBasic:
    """Basic message operations"""

    def test_msg_init(self):
        """
        Initialize message system.

        Should initialize successfully.
        """
        test_program = """
      PROGRAM TEST

C     Initialize message system
      CALL MSGINI

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_msg_get_error(self):
        """
        Get error message by code.

        Setup: Error code 1 = "Invalid formula"
        Action: MSGGET(1, 1, text, len)
        Result: Returns error message
        """
        test_program = """
      PROGRAM TEST
      INTEGER TEXT(80), TLEN
      INTEGER EXPECT(80), ELEN

C     Initialize
      CALL MSGINI

C     Get error message 1
      CALL MSGGET(1, 1, TEXT, TLEN)

C     Expected: "Invalid formula"
      DATA EXPECT /73, 110, 118, 97, 108, 105, 100, 32,
     &             102, 111, 114, 109, 117, 108, 97, 65*32/
      ELEN = 15

C     Check length
      IF (TLEN .NE. ELEN) STOP 1

C     Check content (first few chars)
      IF (TEXT(1) .NE. 73) STOP 2   ! I
      IF (TEXT(2) .NE. 110) STOP 3  ! n
      IF (TEXT(3) .NE. 118) STOP 4  ! v

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_msg_get_help(self):
        """
        Get help message.

        Setup: Help message 1 = brief help
        Action: MSGGET(2, 1, text, len)
        Result: Returns help text
        """
        test_program = """
      PROGRAM TEST
      INTEGER TEXT(80), TLEN

C     Initialize
      CALL MSGINI

C     Get help message 1
      CALL MSGGET(2, 1, TEXT, TLEN)

C     Should have non-zero length
      IF (TLEN .EQ. 0) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestMsgCategories:
    """Message category tests"""

    def test_msg_categories(self):
        """
        Test different message categories.

        Categories:
          1 = Errors
          2 = Help
          3 = Prompts
        """
        test_program = """
      PROGRAM TEST
      INTEGER TEXT(80), TLEN

      CALL MSGINI

C     Get error message (category 1)
      CALL MSGGET(1, 1, TEXT, TLEN)
      IF (TLEN .EQ. 0) STOP 1

C     Get help message (category 2)
      CALL MSGGET(2, 1, TEXT, TLEN)
      IF (TLEN .EQ. 0) STOP 2

C     Get prompt message (category 3)
      CALL MSGGET(3, 1, TEXT, TLEN)
      IF (TLEN .EQ. 0) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer2/MSG.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestMsgFormat:
    """Message formatting tests"""

    @pytest.mark.skip(reason="Format with parameters - implement after basic msgs")
    def test_msg_format_number(self):
        """
        Format message with number parameter.

        Template: "Value: %d"
        Parameter: 42
        Result: "Value: 42"
        """
        pass

    @pytest.mark.skip(reason="Format with parameters - implement after basic msgs")
    def test_msg_format_cell(self):
        """
        Format message with cell reference.

        Template: "Circular reference at %c"
        Parameter: A1
        Result: "Circular reference at A1"
        """
        pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
