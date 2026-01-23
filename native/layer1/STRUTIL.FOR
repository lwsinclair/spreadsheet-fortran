C     STRUTIL.FOR - String Utilities (Layer 0)
C
C     Foundation layer providing string manipulation and conversion.
C     No dependencies - all other modules build on this.
C
C     FORTRAN IV (FORTRAN 66) compatible:
C     - Fixed format (columns 1-72)
C     - No CHARACTER type (use INTEGER arrays)
C     - No block IF/ELSE (use arithmetic IF or GO TO)
C     - Identifiers limited to 6 characters
C
C     String representation:
C     Strings stored as INTEGER arrays with ASCII values.
C     Length passed separately as INTEGER parameter.
C
C---------------------------------------------------------------------
C
C     STREQ - String equality comparison
C
C     Compare two strings for equality.
C
C     Parameters:
C       STR1(*)  - First string (INTEGER array)
C       LEN1     - Length of first string
C       STR2(*)  - Second string (INTEGER array)
C       LEN2     - Length of second string
C
C     Returns:
C       1 if strings are equal, 0 otherwise
C
      INTEGER FUNCTION STREQ(STR1, LEN1, STR2, LEN2)
      INTEGER STR1(*), STR2(*)
      INTEGER LEN1, LEN2
      INTEGER I

C     Different lengths means not equal
      IF (LEN1 .NE. LEN2) GO TO 900

C     Compare character by character
      DO 100 I = 1, LEN1
        IF (STR1(I) .NE. STR2(I)) GO TO 900
  100 CONTINUE

C     Equal
      STREQ = 1
      RETURN

C     Not equal
  900 STREQ = 0
      RETURN
      END

C---------------------------------------------------------------------
C
C     STRCPY - String copy
C
C     Copy string from source to destination.
C
C     Parameters:
C       SRC(*)   - Source string (INTEGER array)
C       SRCLEN   - Length of source string
C       DEST(*)  - Destination string (INTEGER array)
C       DSTLEN   - Maximum length of destination
C
C     Side effects:
C       Modifies DEST array
C       Truncates if source longer than destination
C       Pads with spaces if source shorter
C
      SUBROUTINE STRCPY(SRC, SRCLEN, DEST, DSTLEN)
      INTEGER SRC(*), DEST(*)
      INTEGER SRCLEN, DSTLEN
      INTEGER I, CPYLEN

C     Determine copy length (minimum of source and dest)
      CPYLEN = SRCLEN
      IF (DSTLEN .LT. SRCLEN) CPYLEN = DSTLEN

C     Copy characters
      DO 100 I = 1, CPYLEN
        DEST(I) = SRC(I)
  100 CONTINUE

C     Pad with spaces if destination larger
      IF (DSTLEN .LE. CPYLEN) GO TO 210
      DO 200 I = CPYLEN + 1, DSTLEN
        DEST(I) = 32
  200 CONTINUE
  210 CONTINUE

      RETURN
      END

C---------------------------------------------------------------------
C
C     STRFND - Find substring position
C
C     Find first occurrence of substring in string.
C
C     Parameters:
C       STR(*)   - String to search (INTEGER array)
C       STRLEN   - Length of string
C       SUB(*)   - Substring to find (INTEGER array)
C       SUBLEN   - Length of substring
C
C     Returns:
C       Position of first occurrence (1-based), or 0 if not found
C
      INTEGER FUNCTION STRFND(STR, STRLEN, SUB, SUBLEN)
      INTEGER STR(*), SUB(*)
      INTEGER STRLEN, SUBLEN
      INTEGER I, J, MATCH

C     Substring longer than string - not found
      IF (SUBLEN .GT. STRLEN) GO TO 900

C     Empty substring - not found
      IF (SUBLEN .EQ. 0) GO TO 900

C     Search for substring
      DO 200 I = 1, STRLEN - SUBLEN + 1
C       Try to match at position I
        MATCH = 1
        DO 100 J = 1, SUBLEN
          IF (STR(I + J - 1) .EQ. SUB(J)) GO TO 100
          MATCH = 0
          GO TO 110
  100   CONTINUE
  110   CONTINUE

C       Found match
        IF (MATCH .NE. 1) GO TO 200
        STRFND = I
        RETURN
  200 CONTINUE

C     Not found
  900 STRFND = 0
      RETURN
      END

C---------------------------------------------------------------------
C
C     STRTRM - Trim trailing spaces
C
C     Find length of string excluding trailing spaces.
C
C     Parameters:
C       STR(*)   - String (INTEGER array)
C       LEN      - Maximum length to check
C
C     Returns:
C       Length excluding trailing spaces
C
      INTEGER FUNCTION STRTRM(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
      INTEGER I

C     Scan backwards for first non-space
      DO 100 I = LEN, 1, -1
        IF (STR(I) .EQ. 32) GO TO 100
        STRTRM = I
        RETURN
  100 CONTINUE

C     All spaces (or empty)
      STRTRM = 0
      RETURN
      END

C---------------------------------------------------------------------
C
C     ITOA - Integer to ASCII string
C
C     Convert integer to string representation.
C
C     Parameters:
C       NUM      - Integer to convert
C       STR(*)   - Output string (INTEGER array)
C       MAXLEN   - Maximum length of output
C       LEN      - Actual length of result (output parameter)
C
C     Side effects:
C       Modifies STR and LEN
C
      SUBROUTINE ITOA(NUM, STR, MAXLEN, LEN)
      INTEGER NUM, STR(*), MAXLEN, LEN
      INTEGER N, I, J, TEMP, DIGIT, STRT

C     Handle zero
      IF (NUM .NE. 0) GO TO 50
      STR(1) = 48
      LEN = 1
      RETURN
   50 CONTINUE

C     Handle negative
      N = NUM
      STRT = 1
      IF (N .GE. 0) GO TO 60
      STR(1) = 45
      STRT = 2
      N = -N
   60 CONTINUE

C     Extract digits (right to left)
      I = STRT
  100 IF (N .LE. 0) GO TO 110
      DIGIT = MOD(N, 10)
      STR(I) = 48 + DIGIT
      I = I + 1
      N = N / 10
      GO TO 100
  110 LEN = I - 1

C     Reverse digits (preserve sign if negative)
      I = STRT
      J = LEN
  200 IF (I .GE. J) GO TO 210
      TEMP = STR(I)
      STR(I) = STR(J)
      STR(J) = TEMP
      I = I + 1
      J = J - 1
      GO TO 200
  210 CONTINUE

      RETURN
      END

C---------------------------------------------------------------------
C
C     ATOI - ASCII string to integer
C
C     Convert string to integer.
C
C     Parameters:
C       STR(*)   - String to convert (INTEGER array)
C       LEN      - Length of string
C
C     Returns:
C       Integer value
C
      INTEGER FUNCTION ATOI(STR, LEN)
      INTEGER STR(*), LEN
      INTEGER I, SIGN, RESULT

C     Initialize
      RESULT = 0
      SIGN = 1
      I = 1

C     Skip leading spaces
  100 IF (I .GT. LEN) GO TO 110
      IF (STR(I) .NE. 32) GO TO 110
      I = I + 1
      GO TO 100
  110 CONTINUE

C     Check for sign
      IF (I .GT. LEN) GO TO 150
      IF (STR(I) .NE. 45) GO TO 120
      SIGN = -1
      I = I + 1
      GO TO 150
  120 IF (STR(I) .NE. 43) GO TO 150
      I = I + 1
  150 CONTINUE

C     Convert digits
  200 IF (I .GT. LEN) GO TO 210
C     Check if digit (ASCII 48-57)
      IF (STR(I) .LT. 48 .OR. STR(I) .GT. 57) GO TO 210
      RESULT = RESULT * 10 + (STR(I) - 48)
      I = I + 1
      GO TO 200
  210 CONTINUE

      ATOI = RESULT * SIGN
      RETURN
      END

C---------------------------------------------------------------------
C
C     RTOA - Real to ASCII string
C
C     Convert real number to string with given precision.
C
C     Parameters:
C       NUM      - Real number to convert
C       STR(*)   - Output string (INTEGER array)
C       MAXLEN   - Maximum length of output
C       PREC     - Decimal places
C       LEN      - Actual length of result (output parameter)
C
C     Side effects:
C       Modifies STR and LEN
C
      SUBROUTINE RTOA(NUM, STR, MAXLEN, PREC, LEN)
      REAL NUM
      INTEGER STR(*), MAXLEN, PREC, LEN
      INTEGER INTPRT, I, SAVE
      REAL FRACPT, SCALE

C     Handle negative
      IF (NUM .GE. 0.0) GO TO 50
      STR(1) = 45
      LEN = 1
      INTPRT = INT(-NUM)
      FRACPT = -NUM - REAL(INTPRT)
      GO TO 60
   50 LEN = 0
      INTPRT = INT(NUM)
      FRACPT = NUM - REAL(INTPRT)
   60 CONTINUE

C     Convert integer part
      SAVE = LEN
      CALL ITOA(INTPRT, STR(LEN+1), MAXLEN-LEN, I)
      LEN = LEN + I

C     Add decimal point
      IF (PREC .LE. 0) GO TO 70
      IF (LEN .GE. MAXLEN) GO TO 70
      LEN = LEN + 1
      STR(LEN) = 46
   70 CONTINUE

C     Convert fractional part
      SCALE = 10.0
      DO 100 I = 1, PREC
        IF (LEN .GE. MAXLEN) GO TO 110
        INTPRT = INT(FRACPT * SCALE)
        STR(LEN + 1) = 48 + MOD(INTPRT, 10)
        LEN = LEN + 1
        SCALE = SCALE * 10.0
  100 CONTINUE
  110 CONTINUE

      RETURN
      END

C---------------------------------------------------------------------
C
C     ATOR - ASCII string to real
C
C     Convert string to real number.
C
C     Parameters:
C       STR(*)   - String to convert (INTEGER array)
C       LEN      - Length of string
C
C     Returns:
C       Real value
C
      REAL FUNCTION ATOR(STR, LEN)
      INTEGER STR(*), LEN
      INTEGER I, SIGN
      REAL RESULT, FRAC, SCALE

C     Initialize
      RESULT = 0.0
      FRAC = 0.0
      SCALE = 1.0
      SIGN = 1
      I = 1

C     Skip leading spaces
  100 IF (I .GT. LEN) GO TO 110
      IF (STR(I) .NE. 32) GO TO 110
      I = I + 1
      GO TO 100
  110 CONTINUE

C     Check for sign
      IF (I .GT. LEN) GO TO 150
      IF (STR(I) .NE. 45) GO TO 120
      SIGN = -1
      I = I + 1
      GO TO 150
  120 IF (STR(I) .NE. 43) GO TO 150
      I = I + 1
  150 CONTINUE

C     Convert integer part
  200 IF (I .GT. LEN) GO TO 210
      IF (STR(I) .LT. 48 .OR. STR(I) .GT. 57) GO TO 210
      RESULT = RESULT * 10.0 + REAL(STR(I) - 48)
      I = I + 1
      GO TO 200
  210 CONTINUE

C     Check for decimal point
      IF (I .GT. LEN) GO TO 400
      IF (STR(I) .NE. 46) GO TO 400
      I = I + 1

C     Convert fractional part
      SCALE = 0.1
  300 IF (I .GT. LEN) GO TO 400
      IF (STR(I) .LT. 48 .OR. STR(I) .GT. 57) GO TO 400
      FRAC = FRAC + REAL(STR(I) - 48) * SCALE
      SCALE = SCALE * 0.1
      I = I + 1
      GO TO 300
  400 CONTINUE

      ATOR = REAL(SIGN) * (RESULT + FRAC)
      RETURN
      END

C---------------------------------------------------------------------
C
C     COLTOA - Column number to letters (A, B, ..., Z, AA, ...)
C
C     Convert column number (1-based) to column letters.
C
C     Parameters:
C       COL      - Column number (1=A, 26=Z, 27=AA, etc.)
C       STR(*)   - Output string (INTEGER array)
C       MAXLEN   - Maximum length of output
C       LEN      - Actual length of result (output parameter)
C
C     Side effects:
C       Modifies STR and LEN
C
      SUBROUTINE COLTOA(COL, STR, MAXLEN, LEN)
      INTEGER COL, STR(*), MAXLEN, LEN
      INTEGER N, I, DIGIT

C     Handle invalid column
      IF (COL .GE. 1) GO TO 50
      LEN = 0
      RETURN
   50 CONTINUE

      N = COL
      LEN = 0

C     Extract letters (right to left, base-26)
  100 IF (N .LE. 0) GO TO 110
      LEN = LEN + 1
      IF (LEN .LE. MAXLEN) GO TO 105
      LEN = MAXLEN
      RETURN
  105 CONTINUE

C     Convert to 0-based, get digit, convert back to 1-based
      N = N - 1
      DIGIT = MOD(N, 26)
      STR(LEN) = 65 + DIGIT
      N = N / 26
      GO TO 100
  110 CONTINUE

C     Reverse letters
      I = 1
  200 IF (I .GE. LEN) GO TO 210
      N = STR(I)
      STR(I) = STR(LEN - I + 1)
      STR(LEN - I + 1) = N
      I = I + 1
      IF (I .LT. LEN - I + 1) GO TO 200
  210 CONTINUE

      RETURN
      END

C---------------------------------------------------------------------
C
C     ATOCOL - Letters to column number
C
C     Convert column letters to number.
C
C     Parameters:
C       STR(*)   - Column letters (INTEGER array)
C       LEN      - Length of string
C
C     Returns:
C       Column number (1-based), or 0 if invalid
C
      INTEGER FUNCTION ATOCOL(STR, LEN)
      INTEGER STR(*), LEN
      INTEGER I, RESULT, DIGIT

      RESULT = 0

C     Process each letter
      DO 100 I = 1, LEN
C       Check if uppercase letter (ASCII 65-90)
        IF (STR(I) .LT. 65 .OR. STR(I) .GT. 90) GO TO 900
        DIGIT = STR(I) - 65
        RESULT = RESULT * 26 + DIGIT + 1
  100 CONTINUE

      ATOCOL = RESULT
      RETURN

C     Invalid character
  900 ATOCOL = 0
      RETURN
      END

C---------------------------------------------------------------------
C
C     FMTCEL - Format cell reference (e.g., "A1", "B23")
C
C     Convert column and row numbers to cell reference string.
C
C     Parameters:
C       COL      - Column number (1-based)
C       ROW      - Row number (1-based)
C       STR(*)   - Output string (INTEGER array)
C       MAXLEN   - Maximum length of output
C       LEN      - Actual length of result (output parameter)
C
C     Side effects:
C       Modifies STR and LEN
C
      SUBROUTINE FMTCEL(COL, ROW, STR, MAXLEN, LEN)
      INTEGER COL, ROW, STR(*), MAXLEN, LEN
      INTEGER COLLEN, ROWLEN, I

C     Convert column to letters
      CALL COLTOA(COL, STR, MAXLEN, COLLEN)
      LEN = COLLEN

C     Convert row to digits
      IF (LEN .GE. MAXLEN) GO TO 100
      CALL ITOA(ROW, STR(LEN+1), MAXLEN-LEN, ROWLEN)
      LEN = LEN + ROWLEN
  100 CONTINUE

      RETURN
      END

C---------------------------------------------------------------------
C
C     PARCEL - Parse cell reference (e.g., "A1" -> col=1, row=1)
C
C     Parse cell reference string to column and row numbers.
C
C     Parameters:
C       STR(*)   - Cell reference string (INTEGER array)
C       LEN      - Length of string
C       COL      - Column number (output parameter)
C       ROW      - Row number (output parameter)
C
C     Returns (via COL, ROW):
C       COL=0, ROW=0 if parse fails
C
C     Side effects:
C       Modifies COL and ROW
C
      SUBROUTINE PARCEL(STR, LEN, COL, ROW)
      INTEGER STR(*), LEN, COL, ROW
      INTEGER I, COLSTR(10)
      INTEGER ATOI, ATOCOL

C     Find where letters end and digits begin
      I = 1
  100 IF (I .GT. LEN) GO TO 110
      IF (STR(I) .LT. 65 .OR. STR(I) .GT. 90) GO TO 110
      I = I + 1
      GO TO 100
  110 CONTINUE

C     Check for valid format (at least one letter, one digit)
      IF (I .NE. 1 .AND. I .LE. LEN) GO TO 120
      COL = 0
      ROW = 0
      RETURN
  120 CONTINUE

C     Parse column letters
      COL = ATOCOL(STR, I - 1)

C     Parse row number
      ROW = ATOI(STR(I), LEN - I + 1)

      RETURN
      END

C---------------------------------------------------------------------
C
C     PRSNUM - Parse integer from buffer starting at position
C
C     Parse a decimal number from a buffer starting at a given position.
C
C     Parameters:
C       BUF(*)   - Buffer containing digits (INTEGER array)
C       BLEN     - Length of buffer
C       START    - Starting position (1-based)
C       NUM      - Parsed number (output parameter)
C       ERR      - Error flag: 0=success, 1=error (output parameter)
C
C     Side effects:
C       Modifies NUM and ERR
C
      SUBROUTINE PRSNUM(BUF, BLEN, START, NUM, ERR)
      INTEGER BUF(*)
      INTEGER BLEN, START, NUM, ERR

      INTEGER POS, CH

      ERR = 0
      NUM = 0
      POS = START

C     Parse digits
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 20
      NUM = NUM * 10 + (CH - 48)
      POS = POS + 1
      GO TO 10

20    IF (NUM .EQ. 0) THEN
        ERR = 1
      END IF

      RETURN
      END

C---------------------------------------------------------------------
C
C     PRSCOL - Parse column letter(s) from buffer
C
C     Parse column letters (A-Z, AA-IU) from buffer starting at position.
C
C     Parameters:
C       BUF(*)   - Buffer containing letters (INTEGER array)
C       BLEN     - Length of buffer
C       START    - Starting position (1-based)
C       COL      - Parsed column number (output parameter)
C       ERR      - Error flag: 0=success, 1=error (output parameter)
C
C     Side effects:
C       Modifies COL and ERR
C
      SUBROUTINE PRSCOL(BUF, BLEN, START, COL, ERR)
      INTEGER BUF(*)
      INTEGER BLEN, START, COL, ERR

      INTEGER POS, CH

      ERR = 0
      COL = 0
      POS = START

C     Parse letters (1 or 2)
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 20
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 10

20    IF (COL .EQ. 0) THEN
        ERR = 1
      END IF

      RETURN
      END
