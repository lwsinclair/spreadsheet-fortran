"""
Extended unit tests for STRUTIL.FOR - Comprehensive coverage

Tests for remaining functions and edge cases.
"""

import sys
from pathlib import Path

# Add framework to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester
from assertions import assert_fortran_equal


class TestSTRTRM:
    """Test STRTRM - Trim trailing spaces"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_strtrm_with_trailing_spaces(self):
        """STRTRM should return length excluding trailing spaces"""
        test_program = """
      PROGRAM TEST
      INTEGER STRTRM, RESULT
      INTEGER STR(10)
C     STR = 'HELLO   ' (5 chars + 5 spaces)
      DATA STR /72, 69, 76, 76, 79, 5*32/
      RESULT = STRTRM(STR, 10)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 5, "Should trim to length 5")

    def test_strtrm_no_trailing_spaces(self):
        """STRTRM should return full length if no trailing spaces"""
        test_program = """
      PROGRAM TEST
      INTEGER STRTRM, RESULT
      INTEGER STR(10)
C     STR = 'HELLO' (all 5 chars, no spaces)
      DATA STR /72, 69, 76, 76, 79, 5*65/
      RESULT = STRTRM(STR, 5)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 5, "Should return full length")

    def test_strtrm_all_spaces(self):
        """STRTRM should return 0 for all spaces"""
        test_program = """
      PROGRAM TEST
      INTEGER STRTRM, RESULT
      INTEGER STR(10)
C     All spaces
      DATA STR /10*32/
      RESULT = STRTRM(STR, 10)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Should return 0 for all spaces")


class TestITOAExtended:
    """Extended tests for ITOA"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_itoa_zero(self):
        """ITOA should handle zero"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL ITOA(0, STR, 10, LEN)
C     Verify: LEN=1, STR(1)='0'=48
      IF (LEN .NE. 1) STOP 1
      IF (STR(1) .NE. 48) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 0 to '0'")

    def test_itoa_negative(self):
        """ITOA should handle negative numbers"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL ITOA(-123, STR, 10, LEN)
C     Verify: LEN=4, STR='-123' (45,49,50,51)
      IF (LEN .NE. 4) STOP 1
      IF (STR(1) .NE. 45) STOP 2
      IF (STR(2) .NE. 49) STOP 3
      IF (STR(3) .NE. 50) STOP 4
      IF (STR(4) .NE. 51) STOP 5
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert -123 to '-123'")

    def test_itoa_large_number(self):
        """ITOA should handle large numbers"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL ITOA(9999, STR, 10, LEN)
C     Verify: LEN=4
      IF (LEN .NE. 4) STOP 1
C     Verify digits: '9'=57
      IF (STR(1) .NE. 57) STOP 2
      IF (STR(2) .NE. 57) STOP 3
      IF (STR(3) .NE. 57) STOP 4
      IF (STR(4) .NE. 57) STOP 5
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 9999 to '9999'")


class TestATOIExtended:
    """Extended tests for ATOI"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_atoi_with_leading_spaces(self):
        """ATOI should skip leading spaces"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     STR = '  42' (2 spaces + '42')
      DATA STR /32, 32, 52, 50, 6*32/
      RESULT = ATOI(STR, 4)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 42, "Should parse '  42' as 42")

    def test_atoi_zero(self):
        """ATOI should parse zero"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOI, RESULT
      INTEGER STR(10)
C     STR = '0'
      DATA STR /48, 9*32/
      RESULT = ATOI(STR, 1)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 0, "Should parse '0' as 0")


class TestCOLTOA:
    """Test COLTOA - Column number to letters"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_coltoa_single_letter(self):
        """COLTOA should convert column 1 to 'A'"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL COLTOA(1, STR, 10, LEN)
C     Verify: LEN=1, STR(1)='A'=65
      IF (LEN .NE. 1) STOP 1
      IF (STR(1) .NE. 65) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 1 to 'A'")

    def test_coltoa_column_26(self):
        """COLTOA should convert column 26 to 'Z'"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL COLTOA(26, STR, 10, LEN)
C     Verify: LEN=1, STR(1)='Z'=90
      IF (LEN .NE. 1) STOP 1
      IF (STR(1) .NE. 90) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 26 to 'Z'")

    def test_coltoa_column_27(self):
        """COLTOA should convert column 27 to 'AA'"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL COLTOA(27, STR, 10, LEN)
C     Verify: LEN=2, STR='AA' (65,65)
      IF (LEN .NE. 2) STOP 1
      IF (STR(1) .NE. 65) STOP 2
      IF (STR(2) .NE. 65) STOP 3
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 27 to 'AA'")


class TestATOCOL:
    """Test ATOCOL - Letters to column number"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_atocol_single_letter(self):
        """ATOCOL should convert 'A' to 1"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOCOL, RESULT
      INTEGER STR(10)
C     STR = 'A' (65)
      DATA STR /65, 9*32/
      RESULT = ATOCOL(STR, 1)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should convert 'A' to 1")

    def test_atocol_z(self):
        """ATOCOL should convert 'Z' to 26"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOCOL, RESULT
      INTEGER STR(10)
C     STR = 'Z' (90)
      DATA STR /90, 9*32/
      RESULT = ATOCOL(STR, 1)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 26, "Should convert 'Z' to 26")

    def test_atocol_aa(self):
        """ATOCOL should convert 'AA' to 27"""
        test_program = """
      PROGRAM TEST
      INTEGER ATOCOL, RESULT
      INTEGER STR(10)
C     STR = 'AA' (65,65)
      DATA STR /65, 65, 8*32/
      RESULT = ATOCOL(STR, 2)
      WRITE(*,*) RESULT
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 27, "Should convert 'AA' to 27")


class TestFMTCEL:
    """Test FMTCEL - Format cell reference"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_fmtcel_a1(self):
        """FMTCEL should format col=1, row=1 as 'A1'"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL FMTCEL(1, 1, STR, 10, LEN)
C     Verify: LEN=2, STR='A1' (65,49)
      IF (LEN .NE. 2) STOP 1
      IF (STR(1) .NE. 65) STOP 2
      IF (STR(2) .NE. 49) STOP 3
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should format as 'A1'")

    def test_fmtcel_z99(self):
        """FMTCEL should format col=26, row=99 as 'Z99'"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), LEN
      CALL FMTCEL(26, 99, STR, 10, LEN)
C     Verify: LEN=3, STR='Z99' (90,57,57)
      IF (LEN .NE. 3) STOP 1
      IF (STR(1) .NE. 90) STOP 2
      IF (STR(2) .NE. 57) STOP 3
      IF (STR(3) .NE. 57) STOP 4
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should format as 'Z99'")


class TestPARCEL:
    """Test PARCEL - Parse cell reference"""

    def setup_method(self):
        """Setup test harness"""
        self.tester = FortranTester()

    def test_parcel_a1(self):
        """PARCEL should parse 'A1' as col=1, row=1"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), COL, ROW
C     STR = 'A1' (65,49)
      DATA STR /65, 49, 8*32/
      CALL PARCEL(STR, 2, COL, ROW)
C     Verify: COL=1, ROW=1
      IF (COL .NE. 1) STOP 1
      IF (ROW .NE. 1) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should parse 'A1' correctly")

    def test_parcel_z99(self):
        """PARCEL should parse 'Z99' as col=26, row=99"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), COL, ROW
C     STR = 'Z99' (90,57,57)
      DATA STR /90, 57, 57, 7*32/
      CALL PARCEL(STR, 3, COL, ROW)
C     Verify: COL=26, ROW=99
      IF (COL .NE. 26) STOP 1
      IF (ROW .NE. 99) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should parse 'Z99' correctly")

    def test_parcel_aa10(self):
        """PARCEL should parse 'AA10' as col=27, row=10"""
        test_program = """
      PROGRAM TEST
      INTEGER STR(10), COL, ROW
C     STR = 'AA10' (65,65,49,48)
      DATA STR /65, 65, 49, 48, 6*32/
      CALL PARCEL(STR, 4, COL, ROW)
C     Verify: COL=27, ROW=10
      IF (COL .NE. 27) STOP 1
      IF (ROW .NE. 10) STOP 2
      WRITE(*,*) 1
      STOP
      END
"""
        stdout, _ = self.tester.compile_and_run(['layer0/STRUTIL.FOR'], test_program)
        result = int(stdout.strip())
        assert_fortran_equal(result, 1, "Should parse 'AA10' correctly")


if __name__ == '__main__':
    import pytest
    pytest.main([__file__, '-v'])
