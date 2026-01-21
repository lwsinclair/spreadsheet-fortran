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
C       TOKENS(i,1) = TYPE (1=number, 2=cell, 3=operator, 4=function,
C                          5=left paren, 6=right paren)
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
C     Function codes (TYPE=4):
C       1 = @SUM (sum of range)
C       2 = @MIN (minimum of range)
C       3 = @MAX (maximum of range)
C       4 = @N   (count of range)
C       5 = @AVG (average of range)
C       Range encoded: TCOL=SCOL*256+ECOL, TROW=SROW*256+EROW
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

C     Skip leading = or + (formula indicator)
      IF (INPUT(POS) .EQ. 61) POS = POS + 1
      IF (INPUT(POS) .EQ. 43) POS = POS + 1

C     Process input tokens
100   IF (POS .GT. INLEN) GO TO 200

C     Get next token
      TTYPE = TOKNXT(INPUT, INLEN, POS, TVAL, TCOL, TROW)

C     End of input
      IF (TTYPE .EQ. 0) GO TO 200

C     Number, cell, or function - add to output
      IF (TTYPE .EQ. 1 .OR. TTYPE .EQ. 2 .OR. TTYPE .EQ. 4) GO TO 110

C     Operator - process precedence
      IF (TTYPE .EQ. 3) GO TO 120

C     Left parenthesis - push to stack
      IF (TTYPE .EQ. 5) GO TO 150

C     Right parenthesis - pop until left paren
      IF (TTYPE .EQ. 6) GO TO 160

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

C     Pop higher precedence operators from stack (but not left paren)
130   IF (OPSTKP .LE. 0) GO TO 140
      IF (OPSTK(OPSTKP) .EQ. -1) GO TO 140
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

C     Left parenthesis - push marker (-1) to stack
150   OPSTKP = OPSTKP + 1
      OPSTK(OPSTKP) = -1
      GO TO 100

C     Right parenthesis - pop operators until left paren
160   IF (OPSTKP .LE. 0) GO TO 165
      IF (OPSTK(OPSTKP) .EQ. -1) GO TO 165

C     Pop operator to output
      NTOK = NTOK + 1
      TOKENS(NTOK,1) = 3
      TOKENS(NTOK,2) = OPSTK(OPSTKP)
      TOKENS(NTOK,3) = 0
      TOKENS(NTOK,4) = 0
      OPSTKP = OPSTKP - 1
      GO TO 160

C     Discard the left parenthesis marker
165   IF (OPSTKP .GT. 0) THEN
        IF (OPSTK(OPSTKP) .EQ. -1) OPSTKP = OPSTKP - 1
      END IF
      GO TO 100

C     Pop remaining operators
200   IF (OPSTKP .LE. 0) GO TO 300
      IF (OPSTK(OPSTKP) .EQ. -1) THEN
C       Unmatched left paren - error
        ERROR = 2
        RETURN
      END IF

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
      INTEGER SCOL, SROW, ECOL, EROW

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

C     Check for function (@SUM, @MIN, etc.)
      IF (CH .EQ. 64) GO TO 600

C     Check for absolute cell reference ($A1, $A$1)
      IF (CH .EQ. 36) GO TO 400

C     Check for cell (letter A-Z or a-z)
      IF (CH .GE. 65 .AND. CH .LE. 90) GO TO 400
      IF (CH .GE. 97 .AND. CH .LE. 122) GO TO 400

C     Check for operator
      IF (CH .EQ. 43) GO TO 510  ! +
      IF (CH .EQ. 45) GO TO 520  ! -
      IF (CH .EQ. 42) GO TO 530  ! *
      IF (CH .EQ. 47) GO TO 540  ! /
      IF (CH .EQ. 94) GO TO 550  ! ^

C     Check for parentheses
      IF (CH .EQ. 40) GO TO 560  ! (
      IF (CH .EQ. 41) GO TO 570  ! )

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

C     Parse cell reference (e.g., A1, B23, $A$1, $A1, A$1)
400   CONTINUE

C     Skip $ before column if present
      CH = INPUT(POS)
      IF (CH .EQ. 36) THEN
        POS = POS + 1
        IF (POS .GT. INLEN) GO TO 900
        CH = INPUT(POS)
      END IF

C     Get column (A-Z or a-z, convert to 1-26)
      IF (CH .GE. 97 .AND. CH .LE. 122) THEN
C       Lowercase - convert to uppercase equivalent
        TCOL = CH - 96    ! a=1, b=2, etc.
      ELSE IF (CH .GE. 65 .AND. CH .LE. 90) THEN
C       Uppercase
        TCOL = CH - 64    ! A=1, B=2, etc.
      ELSE
        GO TO 900
      END IF
      POS = POS + 1

C     Skip $ before row if present
      IF (POS .LE. INLEN .AND. INPUT(POS) .EQ. 36) THEN
        POS = POS + 1
      END IF

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

C     Left parenthesis
560   POS = POS + 1
      TOKNXT = 5          ! TYPE=left paren
      TVAL = 0
      TCOL = 0
      TROW = 0
      RETURN

C     Right parenthesis
570   POS = POS + 1
      TOKNXT = 6          ! TYPE=right paren
      TVAL = 0
      TCOL = 0
      TROW = 0
      RETURN

C     Function: @SUM, @MIN, @MAX, @N, @AVG
C     Format: @FUNC(A1:B5)
C     Returns TYPE=4 with range encoded in TCOL/TROW
600   POS = POS + 1       ! Skip @
      IF (POS .GT. INLEN) GO TO 900

C     Parse function name
      CH = INPUT(POS)
C     Convert to uppercase if needed
      IF (CH .GE. 97) CH = CH - 32

C     Check for SUM (S=83)
      IF (CH .EQ. 83) GO TO 610
C     Check for MIN (M=77)
      IF (CH .EQ. 77) GO TO 620
C     Check for AVG (A=65)
      IF (CH .EQ. 65) GO TO 630
C     Check for N (N=78) - count function
      IF (CH .EQ. 78) GO TO 640
      GO TO 900

C     Parse SUM
610   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 85) GO TO 900      ! U
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 77) GO TO 900      ! M
      TVAL = 1                       ! SUM = 1
      POS = POS + 3
      GO TO 650

C     Parse MIN or MAX
620   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 73) GO TO 621      ! I for MIN
      IF (CH .EQ. 65) GO TO 622      ! A for MAX
      GO TO 900

621   CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 78) GO TO 900      ! N
      TVAL = 2                       ! MIN = 2
      POS = POS + 3
      GO TO 650

622   CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 88) GO TO 900      ! X
      TVAL = 3                       ! MAX = 3
      POS = POS + 3
      GO TO 650

C     Parse AVG
630   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 86) GO TO 900      ! V
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 71) GO TO 900      ! G
      TVAL = 5                       ! AVG = 5
      POS = POS + 3
      GO TO 650

C     Parse N (count)
640   TVAL = 4                       ! N/COUNT = 4
      POS = POS + 1
      GO TO 650

C     Now parse (range) - expect (A1:B5)
650   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .NE. 40) GO TO 900   ! Must have (
      POS = POS + 1

C     Parse start cell column
      IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 900
      SCOL = CH - 64
      POS = POS + 1

C     Parse start cell row
      SROW = 0
660   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 665
      SROW = SROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 660

C     Expect colon
665   IF (CH .NE. 58) GO TO 900       ! Must have :
      POS = POS + 1

C     Parse end cell column
      IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 900
      ECOL = CH - 64
      POS = POS + 1

C     Parse end cell row
      EROW = 0
670   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 675
      EROW = EROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 670

C     Expect closing paren
675   IF (CH .NE. 41) GO TO 900       ! Must have )
      POS = POS + 1

C     Encode range: TCOL = SCOL*256+ECOL, TROW = SROW*256+EROW
      TCOL = SCOL * 256 + ECOL
      TROW = SROW * 256 + EROW
      TOKNXT = 4          ! TYPE=function
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
