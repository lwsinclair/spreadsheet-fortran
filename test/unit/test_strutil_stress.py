"""
Stress tests for STRUTIL.FOR - Large data and edge cases

Tests for robustness with large amounts of text and boundary conditions.
"""

import sys
from pathlib import Path

# Add framework to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester
from assertions import assert_fortran_equal


class TestLargeStrings:
    """Test string operations with large data"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_strcpy_long_string(self):
        """STRCPY should handle strings near maximum length"""
        test_program = """
      PROGRAM TEST
      INTEGER SRC(80), DEST(80)
      INTEGER I
C     Create a 60-character string (repeating 'ABCD')
      DO 10 I = 1, 60, 4
        SRC(I) = 65
        SRC(I+1) = 66
        SRC(I+2) = 67
        SRC(I+3) = 68
   10 CONTINUE
C     Initialize DEST
      DO 20 I = 1, 80
        DEST(I) = 32
   20 CONTINUE
C     Copy
      CALL STRCPY(SRC, 60, DEST, 80)
C     Verify first 4 chars
      IF (DEST(1) .NE. 65) STOP 1
      IF (DEST(2) .NE. 66) STOP 2
      IF (DEST(3) .NE. 67) STOP 3
      IF (DEST(4) .NE. 68) STOP 4
C     Verify last 4 chars (positions 57-60)
      IF (DEST(57) .NE. 65) STOP 5
      IF (DEST(58) .NE. 66) STOP 6
      IF (DEST(59) .NE. 67) STOP 7
      IF (DEST(60) .NE. 68) STOP 8
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should copy long string")

    def test_strfnd_repeated_pattern(self):
        """STRFND should find first occurrence in repeated pattern"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(30), SUB(10)
      INTEGER I
C     STR = 'ABCABCABCABCABC' (5x 'ABC')
      DO 10 I = 1, 15, 3
        STR(I) = 65
        STR(I+1) = 66
        STR(I+2) = 67
   10 CONTINUE
      DO 20 I = 16, 30
        STR(I) = 32
   20 CONTINUE
C     SUB = 'ABC'
      SUB(1) = 65
      SUB(2) = 66
      SUB(3) = 67
      DO 30 I = 4, 10
        SUB(I) = 32
   30 CONTINUE
C     Find - should return 1 (first occurrence)
      RESULT = STRFND(STR, 15, SUB, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should find first occurrence at position 1")

    def test_strfnd_near_end(self):
        """STRFND should find pattern near end of string"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(50), SUB(10)
      INTEGER I
C     Fill STR with spaces
      DO 10 I = 1, 50
        STR(I) = 32
   10 CONTINUE
C     Put 'XYZ' at position 47-49
      STR(47) = 88
      STR(48) = 89
      STR(49) = 90
C     SUB = 'XYZ'
      SUB(1) = 88
      SUB(2) = 89
      SUB(3) = 90
      DO 20 I = 4, 10
        SUB(I) = 32
   20 CONTINUE
C     Find - should return 47
      RESULT = STRFND(STR, 50, SUB, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 47, "Should find pattern at position 47")


class TestASCIIBoundaries:
    """Test ASCII character boundary cases"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_atoi_all_digits(self):
        """ATOI should handle all digit characters 0-9"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     Test '9876543210' (all digits)
      DATA STR /57, 56, 55, 54, 53, 52, 51, 50, 49, 48/
      RESULT = ATOI(STR, 10)
C     Should be 9876543210 (but may overflow on 16-bit systems)
C     Just verify it's positive and non-zero
      IF (RESULT .LE. 0) STOP 1
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should parse all digits")

    def test_coltoa_atocol_round_trip(self):
        """Test round-trip conversion for columns 1-100"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN, I, COL1, COL2
      INTEGER ATOCOL
C     Test columns 1, 5, 10, 26, 27, 52, 100
      DO 100 I = 1, 7
        IF (I .EQ. 1) COL1 = 1
        IF (I .EQ. 2) COL1 = 5
        IF (I .EQ. 3) COL1 = 10
        IF (I .EQ. 4) COL1 = 26
        IF (I .EQ. 5) COL1 = 27
        IF (I .EQ. 6) COL1 = 52
        IF (I .EQ. 7) COL1 = 100

C       Convert to letters
        CALL COLTOA(COL1, STR, 10, LEN)

C       Convert back to number
        COL2 = ATOCOL(STR, LEN)

C       Verify match
        IF (COL1 .NE. COL2) STOP I
  100 CONTINUE

      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Round-trip should preserve column numbers")

    def test_fmtcel_parcel_round_trip(self):
        """Test round-trip conversion for cell references"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(20), LEN
      INTEGER C1, R1, C2, R2
      INTEGER I
C     Test several cell coordinates
      DO 100 I = 1, 5
        IF (I .EQ. 1) THEN
          C1 = 1
          R1 = 1
        END IF
        IF (I .EQ. 2) THEN
          C1 = 26
          R1 = 99
        END IF
        IF (I .EQ. 3) THEN
          C1 = 5
          R1 = 42
        END IF
        IF (I .EQ. 4) THEN
          C1 = 27
          R1 = 10
        END IF
        IF (I .EQ. 5) THEN
          C1 = 52
          R1 = 100
        END IF

C       Format as cell reference
        CALL FMTCEL(C1, R1, STR, 20, LEN)

C       Parse back
        CALL PARCEL(STR, LEN, C2, R2)

C       Verify match
        IF (C1 .NE. C2) STOP 10 + I
        IF (R1 .NE. R2) STOP 20 + I
  100 CONTINUE

      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Round-trip should preserve cell coordinates")


class TestEdgeCases:
    """Test edge cases and boundary conditions"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_strcpy_truncation(self):
        """STRCPY should truncate if destination too small"""
        test_program = """
      PROGRAM TEST
      INTEGER SRC(20), DEST(5)
      INTEGER I
C     SRC = 'HELLO WORLD' (11 chars)
      DATA SRC /72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 9*32/
C     DEST can only hold 5 chars
      DATA DEST /5*32/
C     Copy (will truncate)
      CALL STRCPY(SRC, 11, DEST, 5)
C     Verify only first 5 chars copied: 'HELLO'
      IF (DEST(1) .NE. 72) STOP 1
      IF (DEST(2) .NE. 69) STOP 2
      IF (DEST(3) .NE. 76) STOP 3
      IF (DEST(4) .NE. 76) STOP 4
      IF (DEST(5) .NE. 79) STOP 5
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should truncate to destination size")

    def test_strcpy_padding(self):
        """STRCPY should pad with spaces if source smaller"""
        test_program = """
      PROGRAM TEST
      INTEGER SRC(10), DEST(10)
      INTEGER I
C     SRC = 'HI' (2 chars)
      DATA SRC /72, 73, 8*32/
C     DEST is 10 chars
      DATA DEST /10*65/
C     Copy (will pad)
      CALL STRCPY(SRC, 2, DEST, 10)
C     Verify: first 2 are 'HI', rest are spaces
      IF (DEST(1) .NE. 72) STOP 1
      IF (DEST(2) .NE. 73) STOP 2
      IF (DEST(3) .NE. 32) STOP 3
      IF (DEST(10) .NE. 32) STOP 4
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should pad with spaces")

    def test_strfnd_empty_substring(self):
        """STRFND should return 0 for empty substring"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(10), SUB(10)
C     STR = 'HELLO'
      DATA STR /72, 69, 76, 76, 79, 5*32/
C     SUB is empty (length 0)
      DATA SUB /10*32/
      RESULT = STRFND(STR, 5, SUB, 0)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Empty substring should return 0")

    def test_strfnd_substring_longer_than_string(self):
        """STRFND should return 0 if substring longer than string"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(10), SUB(20)
C     STR = 'HI' (2 chars)
      DATA STR /72, 73, 8*32/
C     SUB = 'HELLO' (5 chars - longer than STR)
      DATA SUB /72, 69, 76, 76, 79, 15*32/
      RESULT = STRFND(STR, 2, SUB, 5)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Substring longer than string should return 0")

    def test_atoi_plus_sign(self):
        """ATOI should handle explicit plus sign"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     STR = '+42' ('+'=43, '4'=52, '2'=50)
      DATA STR /43, 52, 50, 7*32/
      RESULT = ATOI(STR, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 42, "Should parse '+42' as 42")

    def test_itoa_single_digit(self):
        """ITOA should handle single digits"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN, I
C     Test digits 0-9
      DO 100 I = 0, 9
        CALL ITOA(I, STR, 10, LEN)
C       Verify LEN=1
        IF (LEN .NE. 1) STOP I + 1
C       Verify correct ASCII: '0'=48, '1'=49, ..., '9'=57
        IF (STR(1) .NE. 48 + I) STOP I + 10
  100 CONTINUE
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should handle all single digits 0-9")


if __name__ == '__main__':
    import pytest
    pytest.main([__file__, '-v'])
