"""
Unit Tests: CELLS.FOR - Cell Storage

Tests the cell storage module using hash table with open chaining.

Test Coverage:
    - Basic cell storage and retrieval
    - Hash collision handling
    - Cell deletion and reuse
    - String pool for formulas
    - Capacity limits
    - Empty cell handling
"""

import sys
from pathlib import Path
import pytest

# Add framework to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestCellsBasic:
    """Basic cell storage and retrieval tests"""

    def test_celput_celget_number(self):
        """
        Store and retrieve a numeric cell value.

        CELPUT stores: COL=1, ROW=1, TYPE=1 (number), VALUE=100
        CELGET retrieves: should return TYPE=1, VALUE=100
        """
        test_program = """
      PROGRAM TEST
      INTEGER COL, ROW, TYPE
      REAL VALUE

C     Initialize cell storage
      CALL CELINI

C     Store number 100 in A1 (COL=1, ROW=1)
      COL = 1
      ROW = 1
      TYPE = 1
      VALUE = 100.0
      CALL CELPUT(COL, ROW, TYPE, VALUE)

C     Retrieve the value
      TYPE = 0
      VALUE = 0.0
      CALL CELGET(COL, ROW, TYPE, VALUE)

C     Verify TYPE=1 (number)
      IF (TYPE .NE. 1) STOP 1

C     Verify VALUE=100
      IF (ABS(VALUE - 100.0) .GT. 0.01) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_celput_multiple_cells(self):
        """
        Store multiple cells and retrieve them.

        Tests that cells don't interfere with each other.
        """
        test_program = """
      PROGRAM TEST
      INTEGER COL, ROW, TYPE
      REAL VALUE

      CALL CELINI

C     Store three cells
      CALL CELPUT(1, 1, 1, 100.0)
      CALL CELPUT(2, 1, 1, 200.0)
      CALL CELPUT(1, 2, 1, 300.0)

C     Retrieve and verify A1 (1,1)
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (TYPE .NE. 1) STOP 1
      IF (ABS(VALUE - 100.0) .GT. 0.01) STOP 2

C     Retrieve and verify B1 (2,1)
      CALL CELGET(2, 1, TYPE, VALUE)
      IF (TYPE .NE. 1) STOP 3
      IF (ABS(VALUE - 200.0) .GT. 0.01) STOP 4

C     Retrieve and verify A2 (1,2)
      CALL CELGET(1, 2, TYPE, VALUE)
      IF (TYPE .NE. 1) STOP 5
      IF (ABS(VALUE - 300.0) .GT. 0.01) STOP 6

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_celput_overwrite(self):
        """
        Overwrite existing cell value.

        CELPUT on same cell should update the value.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store initial value
      CALL CELPUT(1, 1, 1, 100.0)

C     Retrieve and verify
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 100.0) .GT. 0.01) STOP 1

C     Overwrite with new value
      CALL CELPUT(1, 1, 1, 200.0)

C     Retrieve and verify updated value
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 200.0) .GT. 0.01) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_celget_empty_cell(self):
        """
        Retrieve non-existent cell.

        CELGET should return TYPE=0 (empty) for cells that don't exist.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Try to get empty cell
      TYPE = 99
      VALUE = 999.0
      CALL CELGET(5, 5, TYPE, VALUE)

C     Should return TYPE=0 (empty)
      IF (TYPE .NE. 0) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestCellsHash:
    """Hash table collision handling tests"""

    def test_hash_collisions(self):
        """
        Store cells that hash to same bucket.

        Hash function: (COL*257 + ROW) MOD 1024
        Find two cells with same hash, verify both stored correctly.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     These should collide: (1*257+1) MOD 1024 = 258
C                            (1*257+1025) MOD 1024 = 258
C     Store both
      CALL CELPUT(1, 1, 1, 100.0)
      CALL CELPUT(1, 1025, 1, 200.0)

C     Retrieve first cell
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 100.0) .GT. 0.01) STOP 1

C     Retrieve second cell (collision)
      CALL CELGET(1, 1025, TYPE, VALUE)
      IF (ABS(VALUE - 200.0) .GT. 0.01) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestCellsDelete:
    """Cell deletion tests"""

    def test_celdel_basic(self):
        """
        Delete a cell.

        After CELDEL, CELGET should return TYPE=0 (empty).
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store cell
      CALL CELPUT(1, 1, 1, 100.0)

C     Verify stored
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (TYPE .NE. 1) STOP 1

C     Delete cell
      CALL CELDEL(1, 1)

C     Verify deleted (TYPE=0)
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (TYPE .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_celdel_and_reuse(self):
        """
        Delete cell and reuse the slot.

        After deletion, should be able to store new value.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store, delete, store again
      CALL CELPUT(1, 1, 1, 100.0)
      CALL CELDEL(1, 1)
      CALL CELPUT(1, 1, 1, 200.0)

C     Verify new value
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 200.0) .GT. 0.01) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestCellsFormulas:
    """Formula storage tests (string pool)"""

    @pytest.mark.skip(reason="Formula storage - implement after basic cells work")
    def test_celput_formula(self):
        """
        Store cell with formula.

        TYPE=2 means formula, VALUE field points to formula string pool.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE, FMLIDX
      REAL VALUE
      INTEGER FMLSTR(10)
      INTEGER FMLLEN

      CALL CELINI

C     Formula: "+A1+A2" (ASCII values)
      DATA FMLSTR /43,65,49,43,65,50,5*32/
      FMLLEN = 6

C     Add formula to pool
      CALL FMLADD(FMLSTR, FMLLEN, FMLIDX)

C     Store formula cell
      VALUE = REAL(FMLIDX)
      CALL CELPUT(1, 3, 2, VALUE)

C     Retrieve and verify TYPE=2 (formula)
      CALL CELGET(1, 3, TYPE, VALUE)
      IF (TYPE .NE. 2) STOP 1
      IF (INT(VALUE) .NE. FMLIDX) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestCellsDecimalPrecision:
    """Decimal precision tests - verify REAL values stored correctly"""

    def test_decimal_precision_pi(self):
        """
        Store and retrieve decimal value (pi approximation).

        Tests that decimal values like 3.14 are stored with full
        REAL precision, not truncated to integers.

        This test FAILED before the CELLV fix (stored 3.0 instead of 3.14).
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store pi approximation (3.14)
      CALL CELPUT(1, 1, 1, 3.14)

C     Retrieve value
      CALL CELGET(1, 1, TYPE, VALUE)

C     Verify decimal precision preserved
C     Should be 3.14, not 3.0 (as it was with INT() bug)
      IF (ABS(VALUE - 3.14) .GT. 0.0001) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_decimal_precision_negative(self):
        """
        Store and retrieve negative decimal value.

        Tests that negative decimals like -2.71 are stored correctly.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store negative decimal (-2.71)
      CALL CELPUT(1, 1, 1, -2.71)

C     Retrieve value
      CALL CELGET(1, 1, TYPE, VALUE)

C     Verify decimal precision preserved
      IF (ABS(VALUE - (-2.71)) .GT. 0.0001) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_decimal_precision_small_fraction(self):
        """
        Store and retrieve small fractional value.

        Tests that small fractions like 0.001 are preserved.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store small fraction (0.001)
      CALL CELPUT(1, 1, 1, 0.001)

C     Retrieve value
      CALL CELGET(1, 1, TYPE, VALUE)

C     Verify decimal precision preserved
      IF (ABS(VALUE - 0.001) .GT. 0.00001) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_decimal_precision_formula_result(self):
        """
        Store and retrieve formula result with decimals.

        Tests that CELRES stores decimal results correctly.
        Uses CELLR array instead of INT() conversion.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE, RESULT

      CALL CELINI

C     Create a formula cell (TYPE=2)
      CALL CELPUT(1, 1, 2, 0.0)

C     Store formula result (3.14159)
      RESULT = 3.14159
      CALL CELRES(1, 1, RESULT)

C     Retrieve result
      CALL CELGET(1, 1, TYPE, VALUE)

C     Verify TYPE=2 (formula)
      IF (TYPE .NE. 2) STOP 1

C     Verify decimal result preserved
      IF (ABS(VALUE - 3.14159) .GT. 0.00001) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_decimal_precision_overwrite(self):
        """
        Overwrite cell with different decimal value.

        Tests that overwriting preserves decimal precision.
        """
        test_program = """
      PROGRAM TEST
      INTEGER TYPE
      REAL VALUE

      CALL CELINI

C     Store initial decimal
      CALL CELPUT(1, 1, 1, 1.23)

C     Verify first value
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 1.23) .GT. 0.0001) STOP 1

C     Overwrite with different decimal
      CALL CELPUT(1, 1, 1, 4.56)

C     Verify overwritten value
      CALL CELGET(1, 1, TYPE, VALUE)
      IF (ABS(VALUE - 4.56) .GT. 0.0001) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestCellsCapacity:
    """Capacity and limits tests"""

    @pytest.mark.skip(reason="Stress test - implement after basic functionality")
    def test_max_cells(self):
        """
        Fill to MAXCEL capacity.

        Should be able to store MAXCEL cells without error.
        """
        pass

    @pytest.mark.skip(reason="Error handling - implement later")
    def test_exceed_max_cells(self):
        """
        Try to exceed MAXCEL.

        Should return error code when capacity exceeded.
        """
        pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
