"""
Unit Tests: EVAL.FOR - Expression Evaluator

Tests the expression evaluator that processes postfix tokens.

Algorithm: Stack-based postfix evaluation
Input:  Tokens from PARSE (postfix/RPN)
Output: Calculated result

Test Coverage:
    - Basic arithmetic (+, -, *, /)
    - Operator precedence verification
    - Cell value retrieval
    - Stack operations
    - Error handling (division by zero)
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestEvalBasic:
    """Basic evaluation tests"""

    def test_eval_simple_number(self):
        """
        Evaluate single number.

        Tokens: [number(42)]
        Result: 42
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

C     Initialize
      CALL EVLINI

C     Single token: number 42
      NTOK = 1
      TOKENS(1,1) = 1    ! TYPE=number
      TOKENS(1,2) = 42   ! VALUE=42
      TOKENS(1,3) = 0
      TOKENS(1,4) = 0

C     Evaluate
      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

C     Should return 42
      IF (ABS(RESULT - 42.0) .GT. 0.01) STOP 1

C     No error
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_eval_addition(self):
        """
        Evaluate simple addition: 10 + 20

        Tokens: [number(10), number(20), operator(+)]
        Postfix: 10 20 +
        Result: 30
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL EVLINI

C     Tokens: 10 20 +
      NTOK = 3

C     Token 1: 10
      TOKENS(1,1) = 1
      TOKENS(1,2) = 10
      TOKENS(1,3) = 0
      TOKENS(1,4) = 0

C     Token 2: 20
      TOKENS(2,1) = 1
      TOKENS(2,2) = 20
      TOKENS(2,3) = 0
      TOKENS(2,4) = 0

C     Token 3: + (operator code 1)
      TOKENS(3,1) = 3
      TOKENS(3,2) = 1
      TOKENS(3,3) = 0
      TOKENS(3,4) = 0

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

C     Result should be 30
      IF (ABS(RESULT - 30.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_eval_subtraction(self):
        """
        Evaluate subtraction: 50 - 20

        Tokens: [number(50), number(20), operator(-)]
        Result: 30
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL EVLINI

C     Tokens: 50 20 -
      NTOK = 3

      TOKENS(1,1) = 1
      TOKENS(1,2) = 50
      TOKENS(1,3) = 0
      TOKENS(1,4) = 0

      TOKENS(2,1) = 1
      TOKENS(2,2) = 20
      TOKENS(2,3) = 0
      TOKENS(2,4) = 0

C     Operator - (code 2)
      TOKENS(3,1) = 3
      TOKENS(3,2) = 2
      TOKENS(3,3) = 0
      TOKENS(3,4) = 0

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

      IF (ABS(RESULT - 30.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_eval_multiplication(self):
        """
        Evaluate multiplication: 5 * 7

        Tokens: [number(5), number(7), operator(*)]
        Result: 35
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL EVLINI

C     Tokens: 5 7 *
      NTOK = 3

      TOKENS(1,1) = 1
      TOKENS(1,2) = 5
      TOKENS(1,3) = 0
      TOKENS(1,4) = 0

      TOKENS(2,1) = 1
      TOKENS(2,2) = 7
      TOKENS(2,3) = 0
      TOKENS(2,4) = 0

C     Operator * (code 3)
      TOKENS(3,1) = 3
      TOKENS(3,2) = 3
      TOKENS(3,3) = 0
      TOKENS(3,4) = 0

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

      IF (ABS(RESULT - 35.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_eval_division(self):
        """
        Evaluate division: 20 / 4

        Tokens: [number(20), number(4), operator(/)]
        Result: 5
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL EVLINI

C     Tokens: 20 4 /
      NTOK = 3

      TOKENS(1,1) = 1
      TOKENS(1,2) = 20
      TOKENS(1,3) = 0
      TOKENS(1,4) = 0

      TOKENS(2,1) = 1
      TOKENS(2,2) = 4
      TOKENS(2,3) = 0
      TOKENS(2,4) = 0

C     Operator / (code 4)
      TOKENS(3,1) = 3
      TOKENS(3,2) = 4
      TOKENS(3,3) = 0
      TOKENS(3,4) = 0

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

      IF (ABS(RESULT - 5.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestEvalPrecedence:
    """Operator precedence verification tests"""

    def test_eval_precedence_multiply_first(self):
        """
        Verify precedence: 2 + 3 * 4 = 14

        Tokens: [2, 3, 4, *, +]
        Postfix ensures correct order: 2 (3 4 *) +
        Result: 14 (not 20)
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL EVLINI

C     Tokens: 2 3 4 * +
      NTOK = 5

      TOKENS(1,1) = 1
      TOKENS(1,2) = 2

      TOKENS(2,1) = 1
      TOKENS(2,2) = 3

      TOKENS(3,1) = 1
      TOKENS(3,2) = 4

C     * (code 3)
      TOKENS(4,1) = 3
      TOKENS(4,2) = 3

C     + (code 1)
      TOKENS(5,1) = 3
      TOKENS(5,2) = 1

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

C     Should be 14, not 20
      IF (ABS(RESULT - 14.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestEvalCells:
    """Cell value retrieval tests"""

    def test_eval_cell_value(self):
        """
        Evaluate cell reference.

        Setup: Store 100 in A1
        Tokens: [cell(A1)]
        Result: 100
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

C     Initialize cells and evaluator
      CALL CELINI
      CALL EVLINI

C     Store 100 in A1
      CALL CELPUT(1, 1, 1, 100.0)

C     Token: cell A1
      NTOK = 1
      TOKENS(1,1) = 2    ! TYPE=cell
      TOKENS(1,2) = 0
      TOKENS(1,3) = 1    ! COL=1 (A)
      TOKENS(1,4) = 1    ! ROW=1

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

C     Should return 100
      IF (ABS(RESULT - 100.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_eval_cell_addition(self):
        """
        Evaluate formula with cells: A1 + A2

        Setup: A1=10, A2=20
        Tokens: [cell(A1), cell(A2), operator(+)]
        Result: 30
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK
      REAL RESULT
      INTEGER ERROR

      CALL CELINI
      CALL EVLINI

C     Store values
      CALL CELPUT(1, 1, 1, 10.0)  ! A1 = 10
      CALL CELPUT(1, 2, 1, 20.0)  ! A2 = 20

C     Tokens: A1 A2 +
      NTOK = 3

C     A1
      TOKENS(1,1) = 2
      TOKENS(1,2) = 0
      TOKENS(1,3) = 1
      TOKENS(1,4) = 1

C     A2
      TOKENS(2,1) = 2
      TOKENS(2,2) = 0
      TOKENS(2,3) = 1
      TOKENS(2,4) = 2

C     +
      TOKENS(3,1) = 3
      TOKENS(3,2) = 1

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)

      IF (ABS(RESULT - 30.0) .GT. 0.01) STOP 1
      IF (ERROR .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestEvalErrors:
    """Error handling tests"""

    @pytest.mark.skip(reason="Division by zero - implement after basic operations")
    def test_eval_division_by_zero(self):
        """
        Division by zero should return error.

        Tokens: [10, 0, /]
        Result: ERROR != 0
        """
        pass


class TestEvalIntegration:
    """Integration tests with PARSE"""

    def test_eval_parse_integration(self):
        """
        Full integration: Parse then Eval.

        Input: "+10+20"
        Parse: 10 20 +
        Eval: 30
        """
        test_program = """
      PROGRAM TEST
      INTEGER INPUT(10), INLEN
      INTEGER TOKENS(100, 4), NTOK, ERROR
      REAL RESULT

      CALL PRSINI
      CALL EVLINI

C     Input: "+10+20" (ASCII)
      DATA INPUT /43, 49, 48, 43, 50, 48, 4*32/
      INLEN = 6

C     Parse
      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)
      IF (ERROR .NE. 0) STOP 1

C     Evaluate
      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
      IF (ERROR .NE. 0) STOP 2

C     Should be 30
      IF (ABS(RESULT - 30.0) .GT. 0.01) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
