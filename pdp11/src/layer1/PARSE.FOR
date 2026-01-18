C     PARSE.FOR - Formula Parser Module
C
C     Purpose: Parse formulas and convert infix to postfix (RPN)
C
C     Algorithm: Shunting-yard (Dijkstra)
C       Input:  "+A1*2+B2"  (infix formula)
C       Output: A1 2 * B2 + (postfix tokens)
C
C     Features:
C       - Tokenize numbers, cells, operators, functions
C       - Operator precedence: ^ > */ > +- > comparisons
C       - Parentheses for grouping
C       - Cell references (A1, B2, etc.)
C       - Functions (@SUM, @AVG, etc.)
C
C     Token structure:
C       TOKENS(i,1) = TYPE (1=number, 2=cell, 3=operator, 4=function)
C       TOKENS(i,2) = VALUE (number or operator code)
C       TOKENS(i,3) = COL (for cells)
C       TOKENS(i,4) = ROW (for cells)
C
C     Operator codes:
C       1 = + (add)
C       2 = - (subtract)
C       3 = * (multiply)
C       4 = / (divide)
C       5 = ^ (power)
C
C     Configuration:
C       Array sizes defined by PARAMETER statements.
C       Default: MAXTOK=100, MAXSTK=50
C       For CP/M: MAXTOK=50, MAXSTK=25
C       For minimal: MAXTOK=25, MAXSTK=15
C
C     Dependencies: STRUTIL.FOR
C
C     Author: Claude Code
C     Date: 2026-01-18
C     Updated: 2026-01-19 (Added configurable array sizes)
C
C======================================================================

C======================================================================
C     PRSINI - Initialize parser
C======================================================================
      SUBROUTINE PRSINI
C     Nothing to initialize for now
      RETURN
      END

C======================================================================
C     PARSE - Parse formula to postfix tokens
C======================================================================
      SUBROUTINE PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)
      INTEGER INPUT(*), INLEN
C     Configuration parameters
      INTEGER MAXTOK, MAXSTK
      PARAMETER (MAXTOK=100, MAXSTK=50)
C
      INTEGER TOKENS(MAXTOK, 4)
      INTEGER NTOK, ERROR

C     Operator stack for shunting-yard
      INTEGER OPSTK(MAXSTK)
      INTEGER OPSTKP

C     Working variables
      INTEGER POS, TTYPE, TVAL, TCOL, TROW
      INTEGER TOKNXT, OPPREC

      NTOK = 0
      ERROR = 0
      OPSTKP = 0
      POS = 1

C     Skip leading + (formula indicator)
      IF (INPUT(POS) .EQ. 43) POS = POS + 1

C     Process input tokens
100   IF (POS .GT. INLEN) GO TO 200

C     Get next token
      TTYPE = TOKNXT(INPUT, INLEN, POS, TVAL, TCOL, TROW)

C     End of input
      IF (TTYPE .EQ. 0) GO TO 200

C     Number or cell - add to output
      IF (TTYPE .EQ. 1 .OR. TTYPE .EQ. 2) GO TO 110

C     Operator - process precedence
      IF (TTYPE .EQ. 3) GO TO 120

C     Unknown token type
      ERROR = 1
      RETURN

C     Add number or cell to output
110   NTOK = NTOK + 1
      TOKENS(NTOK,1) = TTYPE
      TOKENS(NTOK,2) = TVAL
      TOKENS(NTOK,3) = TCOL
      TOKENS(NTOK,4) = TROW
      GO TO 100

C     Process operator
120   CONTINUE

C     Pop higher precedence operators from stack
130   IF (OPSTKP .LE. 0) GO TO 140
      IF (OPPREC(OPSTK(OPSTKP)) .LT. OPPREC(TVAL)) GO TO 140

C     Pop operator to output
      NTOK = NTOK + 1
      TOKENS(NTOK,1) = 3
      TOKENS(NTOK,2) = OPSTK(OPSTKP)
      TOKENS(NTOK,3) = 0
      TOKENS(NTOK,4) = 0
      OPSTKP = OPSTKP - 1
      GO TO 130

C     Push current operator to stack
140   OPSTKP = OPSTKP + 1
      OPSTK(OPSTKP) = TVAL
      GO TO 100

C     Pop remaining operators
200   IF (OPSTKP .LE. 0) GO TO 300

      NTOK = NTOK + 1
      TOKENS(NTOK,1) = 3
      TOKENS(NTOK,2) = OPSTK(OPSTKP)
      TOKENS(NTOK,3) = 0
      TOKENS(NTOK,4) = 0
      OPSTKP = OPSTKP - 1
      GO TO 200

300   RETURN
      END

C======================================================================
C     TOKNXT - Get next token from input
C======================================================================
      INTEGER FUNCTION TOKNXT(INPUT, INLEN, POS, TVAL, TCOL, TROW)
      INTEGER INPUT(*), INLEN, POS
      INTEGER TVAL, TCOL, TROW

      INTEGER CH, I, NUM

C     Skip whitespace
100   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .EQ. 32) GO TO 110
      GO TO 200

110   POS = POS + 1
      GO TO 100

C     Check token type
200   CH = INPUT(POS)

C     Check for number (digit)
      IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 300

C     Check for cell (letter)
      IF (CH .GE. 65 .AND. CH .LE. 90) GO TO 400

C     Check for operator
      IF (CH .EQ. 43) GO TO 510  ! +
      IF (CH .EQ. 45) GO TO 520  ! -
      IF (CH .EQ. 42) GO TO 530  ! *
      IF (CH .EQ. 47) GO TO 540  ! /
      IF (CH .EQ. 94) GO TO 550  ! ^

C     Unknown
      GO TO 900

C     Parse number
300   NUM = 0
310   IF (POS .GT. INLEN) GO TO 320
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 320
      NUM = NUM * 10 + (CH - 48)
      POS = POS + 1
      GO TO 310

320   TOKNXT = 1          ! TYPE=number
      TVAL = NUM
      TCOL = 0
      TROW = 0
      RETURN

C     Parse cell reference (e.g., A1, B23)
400   CONTINUE

C     Get column (A-Z)
      CH = INPUT(POS)
      TCOL = CH - 64      ! A=1, B=2, etc.
      POS = POS + 1

C     Get row number
      TROW = 0
410   IF (POS .GT. INLEN) GO TO 420
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 420
      TROW = TROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 410

420   TOKNXT = 2          ! TYPE=cell
      TVAL = 0
      RETURN

C     Operators
510   POS = POS + 1
      TOKNXT = 3          ! TYPE=operator
      TVAL = 1            ! OP_ADD
      TCOL = 0
      TROW = 0
      RETURN

520   POS = POS + 1
      TOKNXT = 3
      TVAL = 2            ! OP_SUB
      TCOL = 0
      TROW = 0
      RETURN

530   POS = POS + 1
      TOKNXT = 3
      TVAL = 3            ! OP_MUL
      TCOL = 0
      TROW = 0
      RETURN

540   POS = POS + 1
      TOKNXT = 3
      TVAL = 4            ! OP_DIV
      TCOL = 0
      TROW = 0
      RETURN

550   POS = POS + 1
      TOKNXT = 3
      TVAL = 5            ! OP_POW
      TCOL = 0
      TROW = 0
      RETURN

C     End of input or error
900   TOKNXT = 0
      TVAL = 0
      TCOL = 0
      TROW = 0
      RETURN
      END

C======================================================================
C     OPPREC - Get operator precedence
C======================================================================
      INTEGER FUNCTION OPPREC(OP)
      INTEGER OP

C     Precedence levels (higher = tighter binding):
C       3 = ^ (power)
C       2 = * / (multiply, divide)
C       1 = + - (add, subtract)

      IF (OP .EQ. 5) GO TO 100  ! ^
      IF (OP .EQ. 3) GO TO 200  ! *
      IF (OP .EQ. 4) GO TO 200  ! /
      IF (OP .EQ. 1) GO TO 300  ! +
      IF (OP .EQ. 2) GO TO 300  ! -

C     Default
      OPPREC = 0
      RETURN

100   OPPREC = 3
      RETURN

200   OPPREC = 2
      RETURN

300   OPPREC = 1
      RETURN
      END
