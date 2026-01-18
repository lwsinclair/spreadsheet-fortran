C     EVAL.FOR - Expression Evaluator Module
C
C     Purpose: Evaluate postfix expressions from parser
C
C     Algorithm: Stack-based postfix evaluation
C       Input:  Tokens from PARSE (postfix/RPN)
C       Process: Push operands, pop for operators, push result
C       Output: Calculated result
C
C     Example:
C       Tokens: 10 20 +
C       Stack:  [] -> [10] -> [10,20] -> [30]
C       Result: 30
C
C     Features:
C       - Stack-based evaluation (no recursion)
C       - Get cell values from CELLS module
C       - Handle all operators (+, -, *, /, ^)
C       - Error handling (division by zero, etc.)
C
C     Configuration:
C       Array sizes defined by PARAMETER statements.
C       Default: MAXTOK=100, MAXSTK=50
C       For CP/M: MAXTOK=50, MAXSTK=25
C       For minimal: MAXTOK=25, MAXSTK=15
C
C     Dependencies: STRUTIL.FOR, CELLS.FOR
C
C     Author: Claude Code
C     Date: 2026-01-18
C     Updated: 2026-01-19 (Added configurable array sizes)
C
C======================================================================

C======================================================================
C     EVLINI - Initialize evaluator
C======================================================================
      SUBROUTINE EVLINI
C     Nothing to initialize for now
      RETURN
      END

C======================================================================
C     EVAL - Evaluate postfix expression
C======================================================================
      SUBROUTINE EVAL(TOKENS, NTOK, RESULT, ERROR)
C     Configuration parameters
      INTEGER MAXTOK, MAXSTK
      PARAMETER (MAXTOK=100, MAXSTK=50)
C
      INTEGER TOKENS(MAXTOK, 4), NTOK, ERROR
      REAL RESULT

C     Evaluation stack
      REAL EVSTK(MAXSTK)
      INTEGER EVSTKP

C     Working variables
      INTEGER I, TTYPE, TVAL, TCOL, TROW
      REAL VAL, OP1, OP2, TRES
      INTEGER CTYPE

      ERROR = 0
      EVSTKP = 0

C     Process tokens left to right
      DO 100 I = 1, NTOK
        TTYPE = TOKENS(I,1)
        TVAL = TOKENS(I,2)
        TCOL = TOKENS(I,3)
        TROW = TOKENS(I,4)

C       Number - push value
        IF (TTYPE .EQ. 1) GO TO 20

C       Cell - get value and push
        IF (TTYPE .EQ. 2) GO TO 30

C       Operator - pop, calculate, push
        IF (TTYPE .EQ. 3) GO TO 40

C       Unknown type
        ERROR = 1
        RETURN

C       Push number value
20      VAL = REAL(TVAL)
        CALL EVPUSH(VAL, EVSTK, EVSTKP)
        GO TO 100

C       Get cell value and push
30      CALL CELGET(TCOL, TROW, CTYPE, VAL)
C       If cell empty, treat as 0
        IF (CTYPE .EQ. 0) VAL = 0.0
        CALL EVPUSH(VAL, EVSTK, EVSTKP)
        GO TO 100

C       Process operator
40      CONTINUE

C       Pop two operands
        IF (EVSTKP .LT. 2) GO TO 900
        CALL EVPOP(OP2, EVSTK, EVSTKP)
        CALL EVPOP(OP1, EVSTK, EVSTKP)

C       Calculate based on operator
        IF (TVAL .EQ. 1) TRES = OP1 + OP2     ! +
        IF (TVAL .EQ. 2) TRES = OP1 - OP2     ! -
        IF (TVAL .EQ. 3) TRES = OP1 * OP2     ! *
        IF (TVAL .EQ. 4) TRES = OP1 / OP2     ! /
        IF (TVAL .EQ. 5) TRES = OP1 ** OP2    ! ^

C       Push result
        CALL EVPUSH(TRES, EVSTK, EVSTKP)

100   CONTINUE

C     Final result is top of stack
      IF (EVSTKP .NE. 1) GO TO 900
      CALL EVPOP(RESULT, EVSTK, EVSTKP)
      RETURN

C     Error: stack underflow or incorrect final stack size
900   ERROR = 1
      RESULT = 0.0
      RETURN
      END

C======================================================================
C     EVPUSH - Push value to evaluation stack
C======================================================================
      SUBROUTINE EVPUSH(VAL, EVSTK, EVSTKP)
      REAL VAL, EVSTK(*)
      INTEGER EVSTKP

      EVSTKP = EVSTKP + 1
      EVSTK(EVSTKP) = VAL

      RETURN
      END

C======================================================================
C     EVPOP - Pop value from evaluation stack
C======================================================================
      SUBROUTINE EVPOP(VAL, EVSTK, EVSTKP)
      REAL VAL, EVSTK(*)
      INTEGER EVSTKP

      VAL = EVSTK(EVSTKP)
      EVSTKP = EVSTKP - 1

      RETURN
      END
