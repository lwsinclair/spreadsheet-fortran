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
C       TOKENS(i,1) = TYPE (1=number, 2=cell, 3=operator, 4=range func,
C                          5=scalar func, 6=2-arg func, 7=@IF,
C                          8=left paren, 9=right paren)
C       TOKENS(i,2) = VALUE (number, operator code, or function code)
C       TOKENS(i,3) = COL (for cells) or packed range for functions
C       TOKENS(i,4) = ROW (for cells) or packed range for functions
C
C     Operator codes (TYPE=3):
C       1 = + (add)
C       2 = - (subtract)
C       3 = * (multiply)
C       4 = / (divide)
C       5 = ^ (power)
C       6 = < (less than)      - for @IF conditions
C       7 = > (greater than)   - for @IF conditions
C       8 = = (equal)          - for @IF conditions
C
C     Range function codes (TYPE=4) - take (A1:B9) range:
C       1 = @SUM (sum of range)
C       2 = @MIN (minimum of range)
C       3 = @MAX (maximum of range)
C       4 = @N   (count of range)
C       5 = @AVG (average of range)
C       6 = @PRODUCT (product of range)
C       7 = @STDEV (standard deviation)
C       8 = @VAR (variance)
C       Range encoded: TCOL=SCOL*256+ECOL, TROW=SROW*256+EROW
C
C     Scalar function codes (TYPE=5) - pop 1 value, push result:
C       1 = @ABS (absolute value)
C       2 = @INT (truncate to integer)
C       3 = @SQRT (square root)
C       4 = @EXP (e^x)
C       5 = @LN (natural log)
C       6 = @LOG (log base 10)
C
C     Two-arg function codes (TYPE=6) - pop 2 values, push result:
C       1 = @MOD (remainder: MOD(a,b) = a - INT(a/b)*b)
C       2 = @ROUND (round to n decimals)
C
C     Conditional (TYPE=7):
C       @IF - pops 3 values: condition, true_val, false_val
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

C     Scalar function - add to output (TYPE=5)
      IF (TTYPE .EQ. 5) GO TO 110

C     Two-arg function - add to output (TYPE=6)
      IF (TTYPE .EQ. 6) GO TO 110

C     Left parenthesis - push to stack (TYPE=8)
      IF (TTYPE .EQ. 8) GO TO 150

C     Right parenthesis - pop until left paren (TYPE=9)
      IF (TTYPE .EQ. 9) GO TO 160

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

C     Check for comparison operators (for @IF conditions)
      IF (CH .EQ. 60) GO TO 580  ! <
      IF (CH .EQ. 62) GO TO 582  ! >
      IF (CH .EQ. 61) GO TO 584  ! =

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

C     Parse cell reference (e.g., A1, AA1, $A$1, $AA$1)
C     Supports 1-2 letter columns (A-ZZ = 1-702)
400   CONTINUE

C     Skip $ before column if present
      CH = INPUT(POS)
      IF (CH .EQ. 36) THEN
        POS = POS + 1
        IF (POS .GT. INLEN) GO TO 900
        CH = INPUT(POS)
      END IF

C     Get column letters (1-2 letters, base-26)
C     Convert to uppercase and calculate column number
      TCOL = 0
405   IF (POS .GT. INLEN) GO TO 408
      CH = INPUT(POS)
C     Convert lowercase to uppercase
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
C     Check if letter A-Z
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 408
C     Accumulate column number (base-26, 1-based)
      TCOL = TCOL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 405

408   IF (TCOL .EQ. 0) GO TO 900

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
      TOKNXT = 8          ! TYPE=left paren
      TVAL = 0
      TCOL = 0
      TROW = 0
      RETURN

C     Right parenthesis
570   POS = POS + 1
      TOKNXT = 9          ! TYPE=right paren
      TVAL = 0
      TCOL = 0
      TROW = 0
      RETURN

C     Comparison operators (lowest precedence, for @IF)
580   POS = POS + 1
      TOKNXT = 3
      TVAL = 6            ! OP_LT (<)
      TCOL = 0
      TROW = 0
      RETURN

582   POS = POS + 1
      TOKNXT = 3
      TVAL = 7            ! OP_GT (>)
      TCOL = 0
      TROW = 0
      RETURN

584   POS = POS + 1
      TOKNXT = 3
      TVAL = 8            ! OP_EQ (=)
      TCOL = 0
      TROW = 0
      RETURN

C     Functions - Parse @NAME and dispatch by type
C     Range functions (TYPE=4): @SUM, @MIN, @MAX, @N, @AVG, @PRODUCT,
C                               @STDEV, @VAR - take (A1:B5) range
C     Scalar functions (TYPE=5): @ABS, @INT, @SQRT, @EXP, @LN, @LOG
C                               - take single (value)
C     Two-arg functions (TYPE=6): @MOD, @ROUND - take (val1, val2)
C     Conditional (TYPE=7): @IF - takes (cond, true_val, false_val)
C
600   POS = POS + 1       ! Skip @
      IF (POS .GT. INLEN) GO TO 900

C     Get first letter of function name
      CH = INPUT(POS)
      IF (CH .GE. 97) CH = CH - 32    ! Uppercase

C     Dispatch based on first letter
      IF (CH .EQ. 83) GO TO 610       ! S: SUM, STDEV, SQRT
      IF (CH .EQ. 77) GO TO 620       ! M: MIN, MAX, MOD
      IF (CH .EQ. 65) GO TO 630       ! A: AVG, ABS
      IF (CH .EQ. 78) GO TO 640       ! N: N (count)
      IF (CH .EQ. 80) GO TO 645       ! P: PRODUCT
      IF (CH .EQ. 69) GO TO 700       ! E: EXP (scalar)
      IF (CH .EQ. 76) GO TO 710       ! L: LN, LOG (scalar)
      IF (CH .EQ. 73) GO TO 720       ! I: IF, INT
      IF (CH .EQ. 82) GO TO 730       ! R: ROUND (2-arg)
      IF (CH .EQ. 86) GO TO 740       ! V: VAR (range)
      GO TO 900

C     S: SUM, STDEV, SQRT
610   IF (POS + 1 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 85) GO TO 611       ! U for SUM
      IF (CH .EQ. 84) GO TO 612       ! T for STDEV
      IF (CH .EQ. 81) GO TO 613       ! Q for SQRT
      GO TO 900

611   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 77) GO TO 900       ! M
      TVAL = 1                        ! SUM = 1
      POS = POS + 3
      GO TO 650

612   IF (POS + 4 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 68) GO TO 900       ! D
      CH = INPUT(POS+3)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 69) GO TO 900       ! E
      CH = INPUT(POS+4)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 86) GO TO 900       ! V
      TVAL = 7                        ! STDEV = 7
      POS = POS + 5
      GO TO 650

613   IF (POS + 3 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 82) GO TO 900       ! R
      CH = INPUT(POS+3)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 84) GO TO 900       ! T
      TVAL = 3                        ! SQRT = 3 (scalar)
      POS = POS + 4
      GO TO 750

C     M: MIN, MAX, MOD
620   IF (POS + 1 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 73) GO TO 621       ! I for MIN
      IF (CH .EQ. 65) GO TO 622       ! A for MAX
      IF (CH .EQ. 79) GO TO 623       ! O for MOD
      GO TO 900

621   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 78) GO TO 900       ! N
      TVAL = 2                        ! MIN = 2
      POS = POS + 3
      GO TO 650

622   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 88) GO TO 900       ! X
      TVAL = 3                        ! MAX = 3
      POS = POS + 3
      GO TO 650

623   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 68) GO TO 900       ! D
      TVAL = 1                        ! MOD = 1 (2-arg)
      POS = POS + 3
      GO TO 760

C     A: AVG, ABS
630   IF (POS + 1 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 86) GO TO 631       ! V for AVG
      IF (CH .EQ. 66) GO TO 632       ! B for ABS
      GO TO 900

631   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 71) GO TO 900       ! G
      TVAL = 5                        ! AVG = 5
      POS = POS + 3
      GO TO 650

632   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 83) GO TO 900       ! S
      TVAL = 1                        ! ABS = 1 (scalar)
      POS = POS + 3
      GO TO 750

C     N: N (count)
640   TVAL = 4                        ! N/COUNT = 4
      POS = POS + 1
      GO TO 650

C     P: PRODUCT
645   IF (POS + 6 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 82) GO TO 900       ! R
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 79) GO TO 900       ! O
      CH = INPUT(POS+3)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 68) GO TO 900       ! D
      CH = INPUT(POS+4)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 85) GO TO 900       ! U
      CH = INPUT(POS+5)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 67) GO TO 900       ! C
      CH = INPUT(POS+6)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 84) GO TO 900       ! T
      TVAL = 6                        ! PRODUCT = 6
      POS = POS + 7
      GO TO 650

C     E: EXP (scalar)
700   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 88) GO TO 900       ! X
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 80) GO TO 900       ! P
      TVAL = 4                        ! EXP = 4 (scalar)
      POS = POS + 3
      GO TO 750

C     L: LN, LOG (scalar)
710   IF (POS + 1 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 78) GO TO 711       ! N for LN
      IF (CH .EQ. 79) GO TO 712       ! O for LOG
      GO TO 900

711   TVAL = 5                        ! LN = 5 (scalar)
      POS = POS + 2
      GO TO 750

712   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 71) GO TO 900       ! G
      TVAL = 6                        ! LOG = 6 (scalar)
      POS = POS + 3
      GO TO 750

C     I: IF, INT
720   IF (POS + 1 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .EQ. 70) GO TO 721       ! F for IF
      IF (CH .EQ. 78) GO TO 722       ! N for INT
      GO TO 900

721   TVAL = 1                        ! IF = 1 (@IF)
      POS = POS + 2
      GO TO 770

722   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 84) GO TO 900       ! T
      TVAL = 2                        ! INT = 2 (scalar)
      POS = POS + 3
      GO TO 750

C     R: ROUND (2-arg)
730   IF (POS + 4 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 79) GO TO 900       ! O
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 85) GO TO 900       ! U
      CH = INPUT(POS+3)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 78) GO TO 900       ! N
      CH = INPUT(POS+4)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 68) GO TO 900       ! D
      TVAL = 2                        ! ROUND = 2 (2-arg)
      POS = POS + 5
      GO TO 760

C     V: VAR (range)
740   IF (POS + 2 .GT. INLEN) GO TO 900
      CH = INPUT(POS+1)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 65) GO TO 900       ! A
      CH = INPUT(POS+2)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .NE. 82) GO TO 900       ! R
      TVAL = 8                        ! VAR = 8 (range)
      POS = POS + 3
      GO TO 650

C     Now parse (range) - expect (A1:B5) or (AA1:ZZ100)
C     Note: Function ranges limited to cols 1-255, rows 1-255
C           due to packed encoding (SCOL*256+ECOL, etc.)
650   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .NE. 40) GO TO 900   ! Must have (
      POS = POS + 1

C     Parse start cell column (1-2 letters)
      SCOL = 0
652   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 654
      SCOL = SCOL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 652
654   IF (SCOL .EQ. 0 .OR. SCOL .GT. 255) GO TO 900

C     Parse start cell row
      SROW = 0
660   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 665
      SROW = SROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 660
665   IF (SROW .EQ. 0 .OR. SROW .GT. 255) GO TO 900

C     Expect colon
      IF (CH .NE. 58) GO TO 900       ! Must have :
      POS = POS + 1

C     Parse end cell column (1-2 letters)
      ECOL = 0
667   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .GE. 97) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 668
      ECOL = ECOL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 667
668   IF (ECOL .EQ. 0 .OR. ECOL .GT. 255) GO TO 900

C     Parse end cell row
      EROW = 0
670   IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 675
      EROW = EROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 670
675   IF (EROW .EQ. 0 .OR. EROW .GT. 255) GO TO 900

C     Expect closing paren
      IF (CH .NE. 41) GO TO 900       ! Must have )
      POS = POS + 1

C     Encode range: TCOL = SCOL*256+ECOL, TROW = SROW*256+EROW
      TCOL = SCOL * 256 + ECOL
      TROW = SROW * 256 + EROW
      TOKNXT = 4          ! TYPE=range function
      RETURN

C     Scalar function: @ABS(cell), @INT(A1), @SQRT(5), etc.
C     Parse (cell_or_num) - single argument
C     Returns TYPE=5 with function code in TVAL
C     TCOL/TROW hold cell reference if argument is cell
750   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .NE. 40) GO TO 900   ! Must have (
      POS = POS + 1

C     Skip whitespace
751   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 751
      END IF

C     Check if argument is number or cell
      CH = INPUT(POS)

C     Number argument
      IF (CH .GE. 48 .AND. CH .LE. 57) THEN
        NUM = 0
752     IF (POS .GT. INLEN) GO TO 754
        CH = INPUT(POS)
        IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 754
        NUM = NUM * 10 + (CH - 48)
        POS = POS + 1
        GO TO 752
754     TCOL = -1           ! Flag: argument is number
        TROW = NUM          ! Number value
        GO TO 758
      END IF

C     Cell argument (letter or $)
      IF (CH .GE. 65 .AND. CH .LE. 90) GO TO 755
      IF (CH .GE. 97 .AND. CH .LE. 122) GO TO 755
      IF (CH .EQ. 36) GO TO 755
      GO TO 900

C     Parse cell reference
755   IF (CH .EQ. 36) THEN
        POS = POS + 1
        IF (POS .GT. INLEN) GO TO 900
        CH = INPUT(POS)
      END IF
      TCOL = 0
756   IF (POS .GT. INLEN) GO TO 757
      CH = INPUT(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 757
      TCOL = TCOL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 756
757   IF (TCOL .EQ. 0) GO TO 900
      IF (POS .LE. INLEN .AND. INPUT(POS) .EQ. 36) POS = POS + 1
      TROW = 0
      IF (POS .GT. INLEN) GO TO 900
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 900
7571  IF (POS .GT. INLEN) GO TO 758
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 758
      TROW = TROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 7571

C     Skip whitespace and expect closing paren
758   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 758
      END IF
      IF (INPUT(POS) .NE. 41) GO TO 900   ! Must have )
      POS = POS + 1
      TOKNXT = 5          ! TYPE=scalar function
      RETURN

C     Two-arg function: @MOD(a,b), @ROUND(val,decimals)
C     Parse (cell_or_num, cell_or_num)
C     Returns TYPE=6 with function code in TVAL
C     TCOL = arg1 (packed: hi=col or -1, lo=row/val)
C     TROW = arg2 (packed: hi=col or -1, lo=row/val)
760   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .NE. 40) GO TO 900   ! Must have (
      POS = POS + 1

C     Parse first argument
      CALL PRSA1(INPUT, INLEN, POS, SCOL, SROW, I)
      IF (I .NE. 0) GO TO 900

C     Expect comma
761   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 761
      END IF
      IF (INPUT(POS) .NE. 44) GO TO 900   ! Must have ,
      POS = POS + 1

C     Parse second argument
      CALL PRSA1(INPUT, INLEN, POS, ECOL, EROW, I)
      IF (I .NE. 0) GO TO 900

C     Expect closing paren
762   IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 762
      END IF
      IF (INPUT(POS) .NE. 41) GO TO 900   ! Must have )
      POS = POS + 1

C     Pack: col*256+row, -256+val for numbers
      TCOL = SCOL * 256 + SROW
      TROW = ECOL * 256 + EROW
      TOKNXT = 6          ! TYPE=2-arg function
      RETURN

C     @IF - not yet implemented (requires extended token structure)
C     For now, return error
770   GO TO 900

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
C       0 = < > = (comparison - lowest)

      IF (OP .EQ. 5) GO TO 100  ! ^
      IF (OP .EQ. 3) GO TO 200  ! *
      IF (OP .EQ. 4) GO TO 200  ! /
      IF (OP .EQ. 1) GO TO 300  ! +
      IF (OP .EQ. 2) GO TO 300  ! -
      IF (OP .EQ. 6) GO TO 400  ! <
      IF (OP .EQ. 7) GO TO 400  ! >
      IF (OP .EQ. 8) GO TO 400  ! =

C     Default
      OPPREC = 0
      RETURN

100   OPPREC = 3
      RETURN

200   OPPREC = 2
      RETURN

300   OPPREC = 1
      RETURN

400   OPPREC = 0
      RETURN
      END

C======================================================================
C     PRSA1 - Parse single argument (cell or number)
C     Returns: COL=-1 and ROW=value if number
C              COL=column and ROW=row if cell reference
C              ERR=0 success, ERR=1 error
C======================================================================
      SUBROUTINE PRSA1(INPUT, INLEN, POS, COL, ROW, ERR)
      INTEGER INPUT(*), INLEN, POS
      INTEGER COL, ROW, ERR
      INTEGER CH, NUM

      ERR = 0

C     Skip whitespace
10    IF (POS .GT. INLEN) GO TO 900
      IF (INPUT(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 10
      END IF

      CH = INPUT(POS)

C     Check for number (digit)
      IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 100

C     Check for cell reference (letter or $)
      IF (CH .GE. 65 .AND. CH .LE. 90) GO TO 200
      IF (CH .GE. 97 .AND. CH .LE. 122) GO TO 200
      IF (CH .EQ. 36) GO TO 200

C     Unknown - error
      GO TO 900

C     Parse number
100   NUM = 0
110   IF (POS .GT. INLEN) GO TO 120
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 120
      NUM = NUM * 10 + (CH - 48)
      POS = POS + 1
      GO TO 110
120   COL = -1
      ROW = NUM
      RETURN

C     Parse cell reference
200   IF (CH .EQ. 36) THEN
        POS = POS + 1
        IF (POS .GT. INLEN) GO TO 900
        CH = INPUT(POS)
      END IF

C     Parse column letters
      COL = 0
210   IF (POS .GT. INLEN) GO TO 220
      CH = INPUT(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 220
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 210
220   IF (COL .EQ. 0) GO TO 900

C     Skip $ before row
      IF (POS .LE. INLEN .AND. INPUT(POS) .EQ. 36) POS = POS + 1

C     Parse row number
      ROW = 0
230   IF (POS .GT. INLEN) GO TO 240
      CH = INPUT(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 240
      ROW = ROW * 10 + (CH - 48)
      POS = POS + 1
      GO TO 230
240   IF (ROW .EQ. 0) GO TO 900
      RETURN

C     Error
900   ERR = 1
      COL = 0
      ROW = 0
      RETURN
      END

C======================================================================
C     CPYADJ - Adjust cell references in formula by offset
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE CPYADJ(FSTR, FLEN, NFSTR, NFLEN, COLDIF, ROWDIF)
      INTEGER FSTR(*), FLEN
      INTEGER NFSTR(*), NFLEN
      INTEGER COLDIF, ROWDIF

C     Copy formula string, adjusting relative cell references
C     Absolute references ($AA$1) are preserved unchanged
C     Mixed references ($AA1, AA$1) adjust only the relative part

      INTEGER I, J, K, CH, COL, ROW, NEWCOL, NEWROW
      INTEGER ROWSTR(10), RLEN
      INTEGER COLSTR(10), CLEN
      INTEGER ABSCOL, ABSROW

      NFLEN = 0
      I = 1

100   IF (I .GT. FLEN) RETURN

      CH = FSTR(I)

C     Check for $ (start of absolute reference)
      ABSCOL = 0
      ABSROW = 0
      IF (CH .EQ. 36) THEN
C       $ found - check if followed by column letter
        IF (I + 1 .LE. FLEN) THEN
          CH = FSTR(I + 1)
          IF ((CH .GE. 65 .AND. CH .LE. 90) .OR.
     &        (CH .GE. 97 .AND. CH .LE. 122)) THEN
C           $A form - absolute column
            ABSCOL = 1
            I = I + 1
            CH = FSTR(I)
          END IF
        END IF
      END IF

C     Check for cell reference (letter A-Z or a-z)
      IF ((CH .GE. 65 .AND. CH .LE. 90) .OR.
     &    (CH .GE. 97 .AND. CH .LE. 122)) THEN
C       Parse column letters (1-2 letters, base-26)
        COL = 0
        J = I
105     IF (J .GT. FLEN) GO TO 107
        CH = FSTR(J)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 107
        COL = COL * 26 + (CH - 64)
        J = J + 1
        GO TO 105
107     IF (COL .EQ. 0) GO TO 150

C       Check if followed by $ (absolute row marker)
        IF (J .LE. FLEN .AND. FSTR(J) .EQ. 36) THEN
          ABSROW = 1
          J = J + 1
        END IF

C       Check if followed by digits (row number)
        ROW = 0
110     IF (J .GT. FLEN) GO TO 120
        CH = FSTR(J)
        IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 120
        ROW = ROW * 10 + (CH - 48)
        J = J + 1
        GO TO 110

120     IF (ROW .GT. 0) THEN
C         Valid cell reference - adjust based on absolute flags
          IF (ABSCOL .EQ. 0) THEN
            NEWCOL = COL + COLDIF
          ELSE
            NEWCOL = COL
          END IF
          IF (ABSROW .EQ. 0) THEN
            NEWROW = ROW + ROWDIF
          ELSE
            NEWROW = ROW
          END IF

C         Clamp to valid range (255 cols = A-IU, 255 rows)
          IF (NEWCOL .LT. 1) NEWCOL = 1
          IF (NEWCOL .GT. 255) NEWCOL = 255
          IF (NEWROW .LT. 1) NEWROW = 1
          IF (NEWROW .GT. 255) NEWROW = 255

C         Output reference with $ markers preserved
          IF (ABSCOL .EQ. 1) THEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = 36
          END IF

C         Convert column to letters (A-ZZ)
          CALL COLTOA(NEWCOL, COLSTR, 10, CLEN)
          DO 125 K = 1, CLEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = COLSTR(K)
125       CONTINUE

          IF (ABSROW .EQ. 1) THEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = 36
          END IF

C         Convert row to string
          CALL ITOA(NEWROW, ROWSTR, 10, RLEN)
          DO 130 K = 1, RLEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = ROWSTR(K)
130       CONTINUE

          I = J
          GO TO 100
        END IF
      END IF

C     Copy character as-is
150   NFLEN = NFLEN + 1
      NFSTR(NFLEN) = FSTR(I)
      I = I + 1
      GO TO 100

      END
