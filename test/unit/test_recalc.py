"""
Unit Tests: RECALC.FOR - Recalculation Engine

Tests the recalculation engine that propagates changes through dependencies.

Purpose: Automatically update cells when their dependencies change.

Algorithm: Topological sort with propagation
  - Get dependents from DEPS
  - Calculate in correct order
  - Propagate to next level
  - Repeat until all updated

Features:
  - Automatic vs manual recalc modes
  - Recalculate one cell
  - Recalculate all cells
  - Topological ordering
  - Circular reference handling

Test Coverage:
    - Initialize recalc engine
    - Recalculate single cell
    - Linear dependency chains
    - Wide fan dependencies
    - Manual vs auto modes
    - Circular reference handling
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestRecalcBasic:
    """Basic recalculation tests"""

    def test_recalc_init(self):
        """
        Initialize recalc engine.

        Should initialize successfully.
        """
        test_program = """
      PROGRAM TEST

C     Initialize all modules
      CALL CELINI
      CALL DEPSINI
      CALL PRSINI
      CALL EVLINI
      CALL RECINI

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR',
             'layer1/DEPS.FOR', 'layer1/RECALC.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_recalc_single_cell(self):
        """
        Recalculate single cell with simple formula.

        Setup:
          A1 = 10
          A2 = 20
          A3 = +A1+A2

        Action: RECCEL(A3)
        Result: A3 = 30
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR, CTYPE
      REAL VAL

C     Initialize
      CALL CELINI
      CALL DEPSINI
      CALL PRSINI
      CALL EVLINI
      CALL RECINI

C     Store values
      CALL CELPUT(1, 1, 1, 10.0)  ! A1 = 10
      CALL CELPUT(1, 2, 1, 20.0)  ! A2 = 20

C     Store formula in A3: "+A1+A2"
C     Tokens: A1 A2 +
      NTOK = 3
      TOKENS(1,1) = 2  ! cell
      TOKENS(1,2) = 0
      TOKENS(1,3) = 1  ! A
      TOKENS(1,4) = 1  ! 1

      TOKENS(2,1) = 2  ! cell
      TOKENS(2,2) = 0
      TOKENS(2,3) = 1  ! A
      TOKENS(2,4) = 2  ! 2

      TOKENS(3,1) = 3  ! operator
      TOKENS(3,2) = 1  ! +

C     Store formula tokens in A3
      CALL CELPTK(1, 3, TOKENS, NTOK)

C     Add dependencies
      CALL DEPSADD(1, 1, 1, 3)  ! A3 depends on A1
      CALL DEPSADD(1, 2, 1, 3)  ! A3 depends on A2

C     Recalculate A3
      CALL RECCEL(1, 3)

C     Get result
      CALL CELGET(1, 3, CTYPE, VAL)

C     Should be 30.0
      IF (ABS(VAL - 30.0) .GT. 0.01) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR',
             'layer1/DEPS.FOR', 'layer1/RECALC.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestRecalcChains:
    """Dependency chain tests"""

    def test_recalc_linear_chain(self):
        """
        Recalculate linear dependency chain.

        Setup:
          A1 = 10
          A2 = +A1      (should be 10)
          A3 = +A2      (should be 10)
          A4 = +A3      (should be 10)

        Action: Change A1 to 20, RECCEL(A1)
        Result: A2=20, A3=20, A4=20 (all propagated)
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL VAL
      INTEGER CTYPE

C     Initialize
      CALL CELINI
      CALL DEPSINI
      CALL PRSINI
      CALL EVLINI
      CALL RECINI

C     A1 = 10
      CALL CELPUT(1, 1, 1, 10.0)

C     A2 = +A1 (tokens: A1)
      NTOK = 1
      TOKENS(1,1) = 2
      TOKENS(1,3) = 1
      TOKENS(1,4) = 1
      CALL CELPTK(1, 2, TOKENS, NTOK)
      CALL DEPSADD(1, 1, 1, 2)

C     A3 = +A2
      TOKENS(1,4) = 2
      CALL CELPTK(1, 3, TOKENS, NTOK)
      CALL DEPSADD(1, 2, 1, 3)

C     A4 = +A3
      TOKENS(1,4) = 3
      CALL CELPTK(1, 4, TOKENS, NTOK)
      CALL DEPSADD(1, 3, 1, 4)

C     Recalculate all
      CALL RECCEL(1, 2)
      CALL RECCEL(1, 3)
      CALL RECCEL(1, 4)

C     Change A1 to 20
      CALL CELPUT(1, 1, 1, 20.0)

C     Recalculate chain from A1
      CALL RECCEL(1, 1)

C     Check A2 (should be 20)
      CALL CELGET(1, 2, CTYPE, VAL)
      IF (ABS(VAL - 20.0) .GT. 0.01) STOP 1

C     Check A3 (should be 20)
      CALL CELGET(1, 3, CTYPE, VAL)
      IF (ABS(VAL - 20.0) .GT. 0.01) STOP 2

C     Check A4 (should be 20)
      CALL CELGET(1, 4, CTYPE, VAL)
      IF (ABS(VAL - 20.0) .GT. 0.01) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR',
             'layer1/DEPS.FOR', 'layer1/RECALC.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_recalc_wide_fan(self):
        """
        Recalculate wide fan dependency.

        Setup:
          A1 = 10
          A2 = +A1      (10)
          A3 = +A1      (10)
          A4 = +A1      (10)

        Action: Change A1 to 5, RECCEL(A1)
        Result: A2=5, A3=5, A4=5
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL VAL
      INTEGER CTYPE

C     Initialize
      CALL CELINI
      CALL DEPSINI
      CALL PRSINI
      CALL EVLINI
      CALL RECINI

C     A1 = 10
      CALL CELPUT(1, 1, 1, 10.0)

C     A2 = +A1
      NTOK = 1
      TOKENS(1,1) = 2
      TOKENS(1,3) = 1
      TOKENS(1,4) = 1
      CALL CELPTK(1, 2, TOKENS, NTOK)
      CALL DEPSADD(1, 1, 1, 2)

C     A3 = +A1
      CALL CELPTK(1, 3, TOKENS, NTOK)
      CALL DEPSADD(1, 1, 1, 3)

C     A4 = +A1
      CALL CELPTK(1, 4, TOKENS, NTOK)
      CALL DEPSADD(1, 1, 1, 4)

C     Initial calc
      CALL RECCEL(1, 2)
      CALL RECCEL(1, 3)
      CALL RECCEL(1, 4)

C     Change A1 to 5
      CALL CELPUT(1, 1, 1, 5.0)

C     Recalculate
      CALL RECCEL(1, 1)

C     Check all updated
      CALL CELGET(1, 2, CTYPE, VAL)
      IF (ABS(VAL - 5.0) .GT. 0.01) STOP 1

      CALL CELGET(1, 3, CTYPE, VAL)
      IF (ABS(VAL - 5.0) .GT. 0.01) STOP 2

      CALL CELGET(1, 4, CTYPE, VAL)
      IF (ABS(VAL - 5.0) .GT. 0.01) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR',
             'layer1/DEPS.FOR', 'layer1/RECALC.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestRecalcModes:
    """Recalc mode tests"""

    @pytest.mark.skip(reason="Auto/manual modes - implement after basic recalc")
    def test_recalc_auto_mode(self):
        """
        Test automatic recalculation mode.

        Mode: Auto
        Action: Change A1
        Result: Dependents automatically recalculated
        """
        pass

    @pytest.mark.skip(reason="Auto/manual modes - implement after basic recalc")
    def test_recalc_manual_mode(self):
        """
        Test manual recalculation mode.

        Mode: Manual
        Action: Change A1
        Result: Dependents NOT recalculated until RECALL
        """
        pass


class TestRecalcAll:
    """Full spreadsheet recalc tests"""

    @pytest.mark.skip(reason="Recalc all - implement after basic recalc")
    def test_recalc_all(self):
        """
        Recalculate entire spreadsheet.

        Setup: Multiple formulas
        Action: RECALL
        Result: All cells recalculated in correct order
        """
        pass


class TestRecalcCircular:
    """Circular reference handling"""

    @pytest.mark.skip(reason="Circular handling - implement after basic recalc")
    def test_recalc_circular(self):
        """
        Handle circular reference gracefully.

        Setup:
          A1 = +A2
          A2 = +A1

        Result: Error or special handling (CIRC value)
        """
        pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
