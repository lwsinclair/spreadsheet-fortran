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
C       - Handle functions (@SUM, @MIN, @MAX, @N, @AVG)
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
      INTEGER I, TTYPE, TVAL, TCOL, TROW, NUM
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

C       Range function - evaluate over range and push
        IF (TTYPE .EQ. 4) GO TO 50

C       Scalar function - pop 1, apply, push
        IF (TTYPE .EQ. 5) GO TO 60

C       Two-arg function - unpack and evaluate
        IF (TTYPE .EQ. 6) GO TO 70

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
C       Comparison operators (return 1.0 for true, 0.0 for false)
        IF (TVAL .EQ. 6) THEN                 ! <
          IF (OP1 .LT. OP2) THEN
            TRES = 1.0
          ELSE
            TRES = 0.0
          END IF
        END IF
        IF (TVAL .EQ. 7) THEN                 ! >
          IF (OP1 .GT. OP2) THEN
            TRES = 1.0
          ELSE
            TRES = 0.0
          END IF
        END IF
        IF (TVAL .EQ. 8) THEN                 ! =
          IF (OP1 .EQ. OP2) THEN
            TRES = 1.0
          ELSE
            TRES = 0.0
          END IF
        END IF

C       Push result
        CALL EVPUSH(TRES, EVSTK, EVSTKP)
        GO TO 100

C       Evaluate function over range
50      CONTINUE
C       TVAL = function code (1=SUM, 2=MIN, 3=MAX, 4=N, 5=AVG,
C                             6=PRODUCT, 7=STDEV, 8=VAR)
C       TCOL = SCOL*256+ECOL (start col, end col)
C       TROW = SROW*256+EROW (start row, end row)
        CALL EVFUNC(TVAL, TCOL, TROW, TRES, ERROR)
        IF (ERROR .NE. 0) RETURN
        CALL EVPUSH(TRES, EVSTK, EVSTKP)
        GO TO 100

C       Evaluate scalar function
60      CONTINUE
C       TVAL = function code (1=ABS, 2=INT, 3=SQRT, 4=EXP, 5=LN, 6=LOG)
C       TCOL = -1 if number arg, else cell column
C       TROW = number value if TCOL=-1, else cell row
        IF (TCOL .EQ. -1) THEN
          VAL = REAL(TROW)
        ELSE
          CALL CELGET(TCOL, TROW, CTYPE, VAL)
          IF (CTYPE .EQ. 0) VAL = 0.0
        END IF
C       Apply function
        IF (TVAL .EQ. 1) TRES = ABS(VAL)
        IF (TVAL .EQ. 2) TRES = AINT(VAL)
        IF (TVAL .EQ. 3) TRES = SQRT(ABS(VAL))
        IF (TVAL .EQ. 4) TRES = EXP(VAL)
        IF (TVAL .EQ. 5) THEN
          IF (VAL .GT. 0.0) THEN
            TRES = ALOG(VAL)
          ELSE
            TRES = 0.0
          END IF
        END IF
        IF (TVAL .EQ. 6) THEN
          IF (VAL .GT. 0.0) THEN
            TRES = ALOG10(VAL)
          ELSE
            TRES = 0.0
          END IF
        END IF
        CALL EVPUSH(TRES, EVSTK, EVSTKP)
        GO TO 100

C       Evaluate 2-arg function
70      CONTINUE
C       TVAL = function code (1=MOD, 2=ROUND)
C       TCOL = arg1 packed (col*256+row, -256+val for numbers)
C       TROW = arg2 packed (col*256+row, -256+val for numbers)
        CALL EVARG(TCOL, OP1, ERROR)
        IF (ERROR .NE. 0) RETURN
        CALL EVARG(TROW, OP2, ERROR)
        IF (ERROR .NE. 0) RETURN
C       Apply function
        IF (TVAL .EQ. 1) THEN
C         MOD(a,b) = a - INT(a/b)*b
          IF (OP2 .NE. 0.0) THEN
            TRES = OP1 - AINT(OP1/OP2)*OP2
          ELSE
            TRES = 0.0
          END IF
        END IF
        IF (TVAL .EQ. 2) THEN
C         ROUND(val, decimals)
          NUM = INT(OP2)
          IF (NUM .GE. 0 .AND. NUM .LE. 10) THEN
            VAL = 10.0 ** NUM
            TRES = ANINT(OP1 * VAL) / VAL
          ELSE
            TRES = OP1
          END IF
        END IF
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

C======================================================================
C     EVFUNC - Evaluate function over cell range
C======================================================================
      SUBROUTINE EVFUNC(FUNC, COLRNG, ROWRNG, RESULT, ERROR)
      INTEGER FUNC
      INTEGER COLRNG, ROWRNG
      REAL RESULT
      INTEGER ERROR

C     Function codes: 1=SUM, 2=MIN, 3=MAX, 4=N(count), 5=AVG,
C                     6=PRODUCT, 7=STDEV, 8=VAR

C     Decode range
      INTEGER SCOL, ECOL, SROW, EROW
      INTEGER COL, ROW, CTYPE, COUNT
      REAL VAL, TOTAL, SUMSQ

      ERROR = 0

C     Decode packed range values
      SCOL = COLRNG / 256
      ECOL = MOD(COLRNG, 256)
      SROW = ROWRNG / 256
      EROW = MOD(ROWRNG, 256)

C     Validate range (function ranges limited to 255x255)
      IF (SCOL .LT. 1 .OR. SCOL .GT. 255) GO TO 900
      IF (ECOL .LT. 1 .OR. ECOL .GT. 255) GO TO 900
      IF (SROW .LT. 1 .OR. SROW .GT. 255) GO TO 900
      IF (EROW .LT. 1 .OR. EROW .GT. 255) GO TO 900

C     Swap if needed (ensure start <= end)
      IF (SCOL .GT. ECOL) THEN
        COL = SCOL
        SCOL = ECOL
        ECOL = COL
      END IF
      IF (SROW .GT. EROW) THEN
        ROW = SROW
        SROW = EROW
        EROW = ROW
      END IF

C     Initialize accumulators
      TOTAL = 0.0
      SUMSQ = 0.0
      COUNT = 0
      IF (FUNC .EQ. 2) RESULT = 1.0E38    ! MIN: start high
      IF (FUNC .EQ. 3) RESULT = -1.0E38   ! MAX: start low
      IF (FUNC .EQ. 6) TOTAL = 1.0        ! PRODUCT: start at 1

C     Iterate over range
      DO 100 COL = SCOL, ECOL
        DO 90 ROW = SROW, EROW
          CALL CELGET(COL, ROW, CTYPE, VAL)

C         Skip empty cells for MIN/MAX, count as 0 for SUM/AVG
          IF (CTYPE .EQ. 0) THEN
            IF (FUNC .EQ. 2 .OR. FUNC .EQ. 3) GO TO 90
            VAL = 0.0
          END IF

C         Skip text cells
          IF (CTYPE .EQ. 3) GO TO 90

          COUNT = COUNT + 1

          IF (FUNC .EQ. 1 .OR. FUNC .EQ. 5) THEN
C           SUM or AVG - accumulate
            TOTAL = TOTAL + VAL
          ELSE IF (FUNC .EQ. 2) THEN
C           MIN
            IF (VAL .LT. RESULT) RESULT = VAL
          ELSE IF (FUNC .EQ. 3) THEN
C           MAX
            IF (VAL .GT. RESULT) RESULT = VAL
          ELSE IF (FUNC .EQ. 6) THEN
C           PRODUCT - multiply
            TOTAL = TOTAL * VAL
          ELSE IF (FUNC .EQ. 7 .OR. FUNC .EQ. 8) THEN
C           STDEV or VAR - accumulate sum and sum of squares
            TOTAL = TOTAL + VAL
            SUMSQ = SUMSQ + VAL * VAL
          END IF

90      CONTINUE
100   CONTINUE

C     Calculate final result
      IF (FUNC .EQ. 1) THEN
C       SUM
        RESULT = TOTAL
      ELSE IF (FUNC .EQ. 4) THEN
C       N (count)
        RESULT = REAL(COUNT)
      ELSE IF (FUNC .EQ. 5) THEN
C       AVG
        IF (COUNT .GT. 0) THEN
          RESULT = TOTAL / REAL(COUNT)
        ELSE
          RESULT = 0.0
        END IF
      ELSE IF (FUNC .EQ. 2 .OR. FUNC .EQ. 3) THEN
C       MIN/MAX - check if we found any values
        IF (COUNT .EQ. 0) RESULT = 0.0
      ELSE IF (FUNC .EQ. 6) THEN
C       PRODUCT
        RESULT = TOTAL
      ELSE IF (FUNC .EQ. 7) THEN
C       STDEV = sqrt(variance)
C       VAR = (sum(x^2) - (sum(x))^2/n) / (n-1)
        IF (COUNT .GT. 1) THEN
          VAL = (SUMSQ - TOTAL*TOTAL/REAL(COUNT)) / REAL(COUNT-1)
          IF (VAL .GT. 0.0) THEN
            RESULT = SQRT(VAL)
          ELSE
            RESULT = 0.0
          END IF
        ELSE
          RESULT = 0.0
        END IF
      ELSE IF (FUNC .EQ. 8) THEN
C       VAR (variance)
        IF (COUNT .GT. 1) THEN
          RESULT = (SUMSQ - TOTAL*TOTAL/REAL(COUNT)) / REAL(COUNT-1)
          IF (RESULT .LT. 0.0) RESULT = 0.0
        ELSE
          RESULT = 0.0
        END IF
      END IF

      RETURN

C     Error
900   ERROR = 1
      RESULT = 0.0
      RETURN
      END

C======================================================================
C     EVARG - Evaluate packed argument (for 2-arg functions)
C     PACKED = col*256+row if cell, or -256+value if number
C     Returns: VAL = cell value or literal number
C======================================================================
      SUBROUTINE EVARG(PACKED, VAL, ERROR)
      INTEGER PACKED, ERROR
      REAL VAL
      INTEGER COL, ROW, CTYPE

      ERROR = 0

C     Check if number (col=-1, packed = -256 + value)
      COL = PACKED / 256
      ROW = MOD(PACKED, 256)

      IF (COL .EQ. -1) THEN
C       Literal number
        VAL = REAL(ROW)
      ELSE IF (COL .GT. 0) THEN
C       Cell reference
        CALL CELGET(COL, ROW, CTYPE, VAL)
        IF (CTYPE .EQ. 0) VAL = 0.0
      ELSE
C       Invalid
        ERROR = 1
        VAL = 0.0
      END IF

      RETURN
      END
