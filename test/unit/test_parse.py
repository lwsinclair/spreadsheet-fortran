"""
Unit Tests: PARSE.FOR - Formula Parser

Tests the formula parser that converts infix formulas to postfix (RPN).

Algorithm: Shunting-yard (Dijkstra)
Input:  "+A1*2+B2"  (infix)
Output: A1 2 * B2 + (postfix tokens)

Test Coverage:
    - Tokenization (numbers, cells, operators)
    - Operator precedence (^ > */ > +- > comparisons)
    - Parentheses grouping
    - Cell references (A1, B2, etc.)
    - Functions (@SUM, @AVG, etc.)
    - Error handling
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestParseTokenize:
    """Tokenization tests"""

    def test_tokenize_simple_number(self):
        """
        Tokenize a simple number.

        Input: "100"
        Output: 1 token, TYPE=1 (number), VALUE=100
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

C     Initialize parser
      CALL PRSINI

C     Input: "100" (ASCII: 49,48,48)
      DATA INPUT /49, 48, 48, 7*32/
      INLEN = 3

C     Parse
      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 1 token
      IF (NTOK .NE. 1) STOP 1

C     Token should be TYPE=1 (number)
      IF (TOKENS(1,1) .NE. 1) STOP 2

C     Value should be 100
      IF (TOKENS(1,2) .NE. 100) STOP 3

C     No error
      IF (ERROR .NE. 0) STOP 4

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_tokenize_cell_reference(self):
        """
        Tokenize a cell reference.

        Input: "A1"
        Output: 1 token, TYPE=2 (cell), COL=1, ROW=1
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

      CALL PRSINI

C     Input: "A1" (ASCII: 65,49)
      DATA INPUT /65, 49, 8*32/
      INLEN = 2

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 1 token
      IF (NTOK .NE. 1) STOP 1

C     Token TYPE=2 (cell)
      IF (TOKENS(1,1) .NE. 2) STOP 2

C     Column=1 (A)
      IF (TOKENS(1,3) .NE. 1) STOP 3

C     Row=1
      IF (TOKENS(1,4) .NE. 1) STOP 4

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_tokenize_operator(self):
        """
        Tokenize an operator.

        Input: "*"
        Output: 1 token, TYPE=3 (operator), VALUE=OP_MUL

        Note: Using * instead of + since + is formula indicator
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

      CALL PRSINI

C     Input: "*" (ASCII: 42)
      DATA INPUT /42, 9*32/
      INLEN = 1

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 1 token
      IF (NTOK .NE. 1) STOP 1

C     Token TYPE=3 (operator)
      IF (TOKENS(1,1) .NE. 3) STOP 2

C     Operator code for * (3)
      IF (TOKENS(1,2) .NE. 3) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestParseSimple:
    """Simple expression parsing tests"""

    def test_parse_addition(self):
        """
        Parse simple addition: +1+2

        Input:  "+1+2"
        Infix:  1 + 2
        Postfix: 1 2 +

        Output: 3 tokens (number, number, operator)
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

      CALL PRSINI

C     Input: "+1+2" (ASCII: 43,49,43,50)
      DATA INPUT /43, 49, 43, 50, 6*32/
      INLEN = 4

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 3 tokens (1, 2, +)
      IF (NTOK .NE. 3) STOP 1

C     Token 1: number 1
      IF (TOKENS(1,1) .NE. 1) STOP 2
      IF (TOKENS(1,2) .NE. 1) STOP 3

C     Token 2: number 2
      IF (TOKENS(2,1) .NE. 1) STOP 4
      IF (TOKENS(2,2) .NE. 2) STOP 5

C     Token 3: operator + (code 1)
      IF (TOKENS(3,1) .NE. 3) STOP 6
      IF (TOKENS(3,2) .NE. 1) STOP 7

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_parse_cell_reference_formula(self):
        """
        Parse formula with cell reference: +A1+10

        Input:  "+A1+10"
        Postfix: A1 10 +

        Output: 3 tokens (cell, number, operator)
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

      CALL PRSINI

C     Input: "+A1+10" (ASCII: 43,65,49,43,49,48)
      DATA INPUT /43, 65, 49, 43, 49, 48, 4*32/
      INLEN = 6

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 3 tokens
      IF (NTOK .NE. 3) STOP 1

C     Token 1: cell A1
      IF (TOKENS(1,1) .NE. 2) STOP 2
      IF (TOKENS(1,3) .NE. 1) STOP 3
      IF (TOKENS(1,4) .NE. 1) STOP 4

C     Token 2: number 10
      IF (TOKENS(2,1) .NE. 1) STOP 5
      IF (TOKENS(2,2) .NE. 10) STOP 6

C     Token 3: operator +
      IF (TOKENS(3,1) .NE. 3) STOP 7

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestParsePrecedence:
    """Operator precedence tests"""

    def test_precedence_multiply_over_add(self):
        """
        Test operator precedence: * before +

        Input:  "+1+2*3"
        Infix:  1 + 2 * 3
        Postfix: 1 2 3 * +

        Result should be 1 + (2*3) = 7, not (1+2)*3 = 9
        """
        test_program = """
      PROGRAM TEST
      INTEGER TOKENS(100, 4), NTOK, ERROR
      INTEGER INPUT(10)
      INTEGER INLEN

      CALL PRSINI

C     Input: "+1+2*3"
      DATA INPUT /43, 49, 43, 50, 42, 51, 4*32/
      INLEN = 6

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Should have 5 tokens: 1 2 3 * +
      IF (NTOK .NE. 5) STOP 1

C     Token 1: 1
      IF (TOKENS(1,2) .NE. 1) STOP 2

C     Token 2: 2
      IF (TOKENS(2,2) .NE. 2) STOP 3

C     Token 3: 3
      IF (TOKENS(3,2) .NE. 3) STOP 4

C     Token 4: * (code 3)
      IF (TOKENS(4,2) .NE. 3) STOP 5

C     Token 5: + (code 1)
      IF (TOKENS(5,2) .NE. 1) STOP 6

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR', 'layer1/PARSE.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    @pytest.mark.skip(reason="Exponentiation - implement after basic operators")
    def test_precedence_power_over_multiply(self):
        """
        Test operator precedence: ^ before *

        Input:  "+2*3^2"
        Infix:  2 * 3 ^ 2
        Postfix: 2 3 2 ^ *

        Result should be 2 * (3^2) = 18, not (2*3)^2 = 36
        """
        pass


class TestParseParentheses:
    """Parentheses grouping tests"""

    @pytest.mark.skip(reason="Parentheses - implement after basic precedence")
    def test_parse_parentheses_grouping(self):
        """
        Parse parentheses for grouping: +(1+2)*3

        Input:  "+(1+2)*3"
        Infix:  (1 + 2) * 3
        Postfix: 1 2 + 3 *

        Parentheses change precedence.
        """
        pass


class TestParseFunctions:
    """Function parsing tests"""

    @pytest.mark.skip(reason="Functions - implement after basic parsing")
    def test_parse_sum_function(self):
        """
        Parse @SUM function: @SUM(A1:A5)

        Output: Function token with range
        """
        pass


class TestParseErrors:
    """Error handling tests"""

    @pytest.mark.skip(reason="Error handling - implement after basic parsing")
    def test_parse_invalid_syntax(self):
        """
        Invalid syntax should return error.

        Input: "+++"
        Output: ERROR != 0
        """
        pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
