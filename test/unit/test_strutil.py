"""
Unit tests for STRUTIL.FOR - String utilities layer

Tests all string manipulation and conversion functions.
Following TDD: write tests first, then implement functions.
"""

import sys
from pathlib import Path

# Add framework to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester
from assertions import assert_fortran_equal


class TestSTREQ:
    """Test STREQ - String equality comparison"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_streq_identical_strings(self):
        """STREQ should return 1 for identical strings"""
        test_program = """
      PROGRAM TEST
      INTEGER STREQ, RESULT
      INTEGER STR1(10), STR2(10)
      INTEGER I
C     Initialize strings as 'HELLO' (ASCII values)
      DATA STR1 /72, 69, 76, 76, 79, 5*32/
      DATA STR2 /72, 69, 76, 76, 79, 5*32/
      RESULT = STREQ(STR1, 5, STR2, 5)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Identical strings should be equal")

    def test_streq_different_strings(self):
        """STREQ should return 0 for different strings"""
        test_program = """
      PROGRAM TEST
      INTEGER STREQ, RESULT
      INTEGER STR1(10), STR2(10)
C     STR1 = 'HELLO' (72,69,76,76,79), STR2 = 'WORLD' (87,79,82,76,68)
      DATA STR1 /72, 69, 76, 76, 79, 5*32/
      DATA STR2 /87, 79, 82, 76, 68, 5*32/
      RESULT = STREQ(STR1, 5, STR2, 5)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Different strings should not be equal")

    def test_streq_different_lengths(self):
        """STREQ should return 0 for strings of different lengths"""
        test_program = """
      PROGRAM TEST
      INTEGER STREQ, RESULT
      INTEGER STR1(10), STR2(10)
C     STR1 = 'HELLO' (5 chars: 72,69,76,76,79), STR2 = 'HI' (2 chars: 72,73)
      DATA STR1 /72, 69, 76, 76, 79, 5*32/
      DATA STR2 /72, 73, 8*32/
      RESULT = STREQ(STR1, 5, STR2, 2)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Strings of different length should not be equal")

    def test_streq_empty_strings(self):
        """STREQ should return 1 for two empty strings"""
        test_program = """
      PROGRAM TEST
      INTEGER STREQ, RESULT
      INTEGER STR1(10), STR2(10)
C     Both strings empty (length 0)
      DATA STR1 /10*32/
      DATA STR2 /10*32/
      RESULT = STREQ(STR1, 0, STR2, 0)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Empty strings should be equal")


class TestSTRCPY:
    """Test STRCPY - String copy"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_strcpy_basic_copy(self):
        """STRCPY should copy string from source to destination"""
        test_program = """
      PROGRAM TEST
      INTEGER SRC(10), DEST(10)
      INTEGER I
C     SRC = 'TEST' (ASCII: 84,69,83,84)
      DATA SRC /84, 69, 83, 84, 6*32/
C     Initialize DEST to spaces
      DATA DEST /10*32/
      CALL STRCPY(SRC, 4, DEST, 10)
C     Verify copy - check each character
      IF (DEST(1) .NE. 84) STOP 1
      IF (DEST(2) .NE. 69) STOP 2
      IF (DEST(3) .NE. 83) STOP 3
      IF (DEST(4) .NE. 84) STOP 4
C     Success
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Copy should succeed")


class TestSTRFND:
    """Test STRFND - Find substring position"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_strfnd_substring_found(self):
        """STRFND should return position of substring"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(20), SUB(10)
C     STR = 'HELLO WORLD' (H=72,E=69,L=76,O=79,space=32,W=87,R=82,D=68)
      DATA STR /72, 69, 76, 76, 79, 32, 87, 79, 82, 76, 68, 9*32/
C     SUB = 'WORLD'
      DATA SUB /87, 79, 82, 76, 68, 5*32/
      RESULT = STRFND(STR, 11, SUB, 5)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        # 'WORLD' starts at position 7 (1-indexed)
        assert_fortran_equal(result, 7, "Should find WORLD at position 7")

    def test_strfnd_substring_not_found(self):
        """STRFND should return 0 when substring not found"""
        test_program = """
      PROGRAM TEST
      INTEGER STRFND, RESULT
      INTEGER STR(20), SUB(10)
C     STR = 'HELLO' (H=72,E=69,L=76,O=79), SUB = 'XYZ' (X=88,Y=89,Z=90)
      DATA STR /72, 69, 76, 76, 79, 15*32/
      DATA SUB /88, 89, 90, 7*32/
      RESULT = STRFND(STR, 5, SUB, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Should return 0 when substring not found")


class TestConversionFunctions:
    """Test conversion functions (ITOA, ATOI, etc.)"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_itoa_positive_number(self):
        """ITOA should convert positive integer to string"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
C     Convert 42 to string (should be '4'=52, '2'=50)
      CALL ITOA(42, STR, 10, LEN)
C     Verify: LEN=2, STR(1)='4'=52, STR(2)='2'=50
      IF (LEN .NE. 2) STOP 1
      IF (STR(1) .NE. 52) STOP 2
      IF (STR(2) .NE. 50) STOP 3
C     Success
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 42 to '42'")

    def test_atoi_positive_number(self):
        """ATOI should convert string to positive integer"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     STR = '123' (ASCII: '1'=49, '2'=50, '3'=51)
      DATA STR /49, 50, 51, 7*32/
      RESULT = ATOI(STR, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 123, "Should convert '123' to 123")

    def test_atoi_negative_number(self):
        """ATOI should convert string to negative integer"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     STR = '-45' (ASCII: '-'=45, '4'=52, '5'=53)
      DATA STR /45, 52, 53, 7*32/
      RESULT = ATOI(STR, 3)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, -45, "Should convert '-45' to -45")


if __name__ == '__main__':
    import pytest
    pytest.main([__file__, '-v'])
