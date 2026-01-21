C     FILELOAD.FOR - Spreadsheet File Load Module
C
C     Purpose: Parse JSON-like format and populate spreadsheet
C
C     This module is portable - it reads lines of text from
C     the platform-specific Layer 3 file I/O.
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     FLLOAD - Load spreadsheet from file
C======================================================================
      SUBROUTINE FLLOAD(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     UI state for cursor position
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     File unit
      INTEGER FUNIT
      PARAMETER (FUNIT=20)

C     Line buffer
      INTEGER LINE(256), LLEN
      INTEGER EOF

C     Extension
      INTEGER EXT(2)
      DATA EXT /120, 108/

C     Working variables
      INTEGER I, J
      INTEGER FNBUF(80), FNLEN

C     Cell parsing state
      INTEGER CSTATE
      INTEGER CCOL, CROW, CTYPE
      REAL CVAL
      INTEGER CTEXT(80), CTLEN, CALIGN
      INTEGER CFRM(80), CFLEN

C     Parse state:
C     0 = looking for section
C     1 = in settings
C     2 = in cells array
C     3 = in cell object
      CSTATE = 0

      ERR = 0

C     Copy filename and add extension if needed
      FNLEN = FLEN
      DO 5 I = 1, FLEN
        FNBUF(I) = FNAME(I)
5     CONTINUE
      CALL FIOEXT(FNBUF, FNLEN, EXT, 2, 80)

C     Open file for reading
      CALL FIOOPN(FUNIT, FNBUF, FNLEN, 1, ERR)
      IF (ERR .NE. 0) RETURN

C     Clear spreadsheet
      CALL CLRALL

C     Initialize cell state
      CCOL = 0
      CROW = 0
      CTYPE = 0
      CVAL = 0.0
      CTLEN = 0
      CFLEN = 0
      CALIGN = 1

C     Read and parse file line by line
100   CALL FIORDL(FUNIT, LINE, 256, LLEN, EOF)
      IF (EOF .NE. 0) GO TO 900

C     Parse the line
      CALL FLPLIN(LINE, LLEN, CSTATE,
     &            CCOL, CROW, CTYPE, CVAL,
     &            CTEXT, CTLEN, CALIGN,
     &            CFRM, CFLEN,
     &            UICOL, UIROW)

C     If we just finished a cell, store it
      IF (CSTATE .EQ. 4) THEN
        CALL FLSTCL(CCOL, CROW, CTYPE, CVAL,
     &              CTEXT, CTLEN, CALIGN,
     &              CFRM, CFLEN)
C       Reset for next cell
        CCOL = 0
        CROW = 0
        CTYPE = 0
        CVAL = 0.0
        CTLEN = 0
        CFLEN = 0
        CALIGN = 1
        CSTATE = 2
      END IF

      GO TO 100

C     Close file
900   CALL FIOCLS(FUNIT)

      RETURN
      END

C======================================================================
C     CLRALL - Clear all cells (reset spreadsheet)
C======================================================================
      SUBROUTINE CLRALL
      INTEGER COL, ROW

C     We can't iterate all cells efficiently, so just reset the
C     hash table by reinitializing
      CALL CELINI

C     Reset column widths to defaults
      CALL CWINI

      RETURN
      END

C======================================================================
C     FLPLIN - Parse a single line and update state
C======================================================================
      SUBROUTINE FLPLIN(LINE, LLEN, STATE,
     &                  CCOL, CROW, CTYPE, CVAL,
     &                  CTEXT, CTLEN, CALIGN,
     &                  CFRM, CFLEN,
     &                  UCOL, UROW)
      INTEGER LINE(*), LLEN
      INTEGER STATE
      INTEGER CCOL, CROW, CTYPE
      REAL CVAL
      INTEGER CTEXT(*), CTLEN, CALIGN
      INTEGER CFRM(*), CFLEN
      INTEGER UCOL, UROW

      INTEGER I, J, POS
      INTEGER KEY(20), KLEN
      INTEGER VAL(256), VLEN

C     Skip empty lines
      IF (LLEN .EQ. 0) RETURN

C     Look for key patterns in line
      POS = 1

C     Skip whitespace
      DO 10 WHILE (POS .LE. LLEN .AND. LINE(POS) .LE. 32)
        POS = POS + 1
10    CONTINUE

      IF (POS .GT. LLEN) RETURN

C     Check for section markers
      IF (LINE(POS) .EQ. 123) THEN
C       { - object start
        IF (STATE .EQ. 2) STATE = 3
        RETURN
      END IF

      IF (LINE(POS) .EQ. 125) THEN
C       } - object end
        IF (STATE .EQ. 3) STATE = 4
        RETURN
      END IF

      IF (LINE(POS) .EQ. 93) THEN
C       ] - array end
        STATE = 0
        RETURN
      END IF

C     Look for "key": pattern
      IF (LINE(POS) .NE. 34) RETURN

C     Extract key
      POS = POS + 1
      KLEN = 0
      DO 20 WHILE (POS .LE. LLEN .AND. LINE(POS) .NE. 34)
        KLEN = KLEN + 1
        KEY(KLEN) = LINE(POS)
        POS = POS + 1
20    CONTINUE
      POS = POS + 1

C     Skip to value (past : and whitespace)
      DO 30 WHILE (POS .LE. LLEN .AND.
     &             (LINE(POS) .EQ. 58 .OR. LINE(POS) .LE. 32))
        POS = POS + 1
30    CONTINUE

C     Check for section starts
      IF (KLEN .EQ. 8 .AND.
     &    KEY(1) .EQ. 115 .AND. KEY(2) .EQ. 101 .AND.
     &    KEY(3) .EQ. 116 .AND. KEY(4) .EQ. 116 .AND.
     &    KEY(5) .EQ. 105 .AND. KEY(6) .EQ. 110 .AND.
     &    KEY(7) .EQ. 103 .AND. KEY(8) .EQ. 115) THEN
C       "settings"
        STATE = 1
        RETURN
      END IF

      IF (KLEN .EQ. 5 .AND.
     &    KEY(1) .EQ. 99 .AND. KEY(2) .EQ. 101 .AND.
     &    KEY(3) .EQ. 108 .AND. KEY(4) .EQ. 108 .AND.
     &    KEY(5) .EQ. 115) THEN
C       "cells"
        STATE = 2
        RETURN
      END IF

      IF (KLEN .EQ. 13 .AND.
     &    KEY(1) .EQ. 99 .AND. KEY(2) .EQ. 111 .AND.
     &    KEY(3) .EQ. 108 .AND. KEY(4) .EQ. 117 .AND.
     &    KEY(5) .EQ. 109 .AND. KEY(6) .EQ. 110 .AND.
     &    KEY(7) .EQ. 95 .AND. KEY(8) .EQ. 119 .AND.
     &    KEY(9) .EQ. 105 .AND. KEY(10) .EQ. 100 .AND.
     &    KEY(11) .EQ. 116 .AND. KEY(12) .EQ. 104 .AND.
     &    KEY(13) .EQ. 115) THEN
C       "column_widths"
        STATE = 5
        RETURN
      END IF

C     Handle column width values (in column_widths section)
      IF (STATE .EQ. 5) THEN
        CALL FLPCWD(KEY, KLEN, LINE, POS, LLEN)
        RETURN
      END IF

C     Handle settings values
      IF (STATE .EQ. 1) THEN
        IF (KLEN .EQ. 10 .AND.
     &      KEY(1) .EQ. 99 .AND. KEY(2) .EQ. 117 .AND.
     &      KEY(3) .EQ. 114 .AND. KEY(4) .EQ. 115 .AND.
     &      KEY(5) .EQ. 111 .AND. KEY(6) .EQ. 114 .AND.
     &      KEY(7) .EQ. 95 .AND. KEY(8) .EQ. 99 .AND.
     &      KEY(9) .EQ. 111 .AND. KEY(10) .EQ. 108) THEN
C         "cursor_col"
          CALL FLPNUM(LINE, POS, LLEN, UCOL)
        ELSE IF (KLEN .EQ. 10 .AND.
     &           KEY(1) .EQ. 99 .AND. KEY(2) .EQ. 117 .AND.
     &           KEY(3) .EQ. 114 .AND. KEY(4) .EQ. 115 .AND.
     &           KEY(5) .EQ. 111 .AND. KEY(6) .EQ. 114 .AND.
     &           KEY(7) .EQ. 95 .AND. KEY(8) .EQ. 114 .AND.
     &           KEY(9) .EQ. 111 .AND. KEY(10) .EQ. 119) THEN
C         "cursor_row"
          CALL FLPNUM(LINE, POS, LLEN, UROW)
        END IF
        RETURN
      END IF

C     Handle cell values
      IF (STATE .EQ. 3) THEN
C       "ref" - cell reference
        IF (KLEN .EQ. 3 .AND.
     &      KEY(1) .EQ. 114 .AND. KEY(2) .EQ. 101 .AND.
     &      KEY(3) .EQ. 102) THEN
          CALL FLPREF(LINE, POS, LLEN, CCOL, CROW)
          RETURN
        END IF

C       "type" - cell type
        IF (KLEN .EQ. 4 .AND.
     &      KEY(1) .EQ. 116 .AND. KEY(2) .EQ. 121 .AND.
     &      KEY(3) .EQ. 112 .AND. KEY(4) .EQ. 101) THEN
          CALL FLPTYP(LINE, POS, LLEN, CTYPE)
          RETURN
        END IF

C       "value" - cell value
        IF (KLEN .EQ. 5 .AND.
     &      KEY(1) .EQ. 118 .AND. KEY(2) .EQ. 97 .AND.
     &      KEY(3) .EQ. 108 .AND. KEY(4) .EQ. 117 .AND.
     &      KEY(5) .EQ. 101) THEN
          CALL FLPVAL(LINE, POS, LLEN, CVAL)
          RETURN
        END IF

C       "formula" - formula string
        IF (KLEN .EQ. 7 .AND.
     &      KEY(1) .EQ. 102 .AND. KEY(2) .EQ. 111 .AND.
     &      KEY(3) .EQ. 114 .AND. KEY(4) .EQ. 109 .AND.
     &      KEY(5) .EQ. 117 .AND. KEY(6) .EQ. 108 .AND.
     &      KEY(7) .EQ. 97) THEN
          CALL FLPSTR(LINE, POS, LLEN, CFRM, 80, CFLEN)
          RETURN
        END IF

C       "text" - text content
        IF (KLEN .EQ. 4 .AND.
     &      KEY(1) .EQ. 116 .AND. KEY(2) .EQ. 101 .AND.
     &      KEY(3) .EQ. 120 .AND. KEY(4) .EQ. 116) THEN
          CALL FLPSTR(LINE, POS, LLEN, CTEXT, 80, CTLEN)
          RETURN
        END IF

C       "align" - text alignment
        IF (KLEN .EQ. 5 .AND.
     &      KEY(1) .EQ. 97 .AND. KEY(2) .EQ. 108 .AND.
     &      KEY(3) .EQ. 105 .AND. KEY(4) .EQ. 103 .AND.
     &      KEY(5) .EQ. 110) THEN
          CALL FLPALG(LINE, POS, LLEN, CALIGN)
          RETURN
        END IF
      END IF

      RETURN
      END

C======================================================================
C     FLPNUM - Parse integer from line
C======================================================================
      SUBROUTINE FLPNUM(LINE, POS, LLEN, NUM)
      INTEGER LINE(*), POS, LLEN
      INTEGER NUM

      INTEGER SIGN

      NUM = 0
      SIGN = 1

      IF (POS .LE. LLEN .AND. LINE(POS) .EQ. 45) THEN
        SIGN = -1
        POS = POS + 1
      END IF

      DO 10 WHILE (POS .LE. LLEN .AND.
     &             LINE(POS) .GE. 48 .AND. LINE(POS) .LE. 57)
        NUM = NUM * 10 + (LINE(POS) - 48)
        POS = POS + 1
10    CONTINUE

      NUM = NUM * SIGN

      RETURN
      END

C======================================================================
C     FLPVAL - Parse real value from line
C======================================================================
      SUBROUTINE FLPVAL(LINE, POS, LLEN, VAL)
      INTEGER LINE(*), POS, LLEN
      REAL VAL

      INTEGER IPART, FPART, FDIG
      INTEGER SIGN
      REAL FRAC

      VAL = 0.0
      IPART = 0
      FPART = 0
      FDIG = 0
      SIGN = 1

C     Handle negative
      IF (POS .LE. LLEN .AND. LINE(POS) .EQ. 45) THEN
        SIGN = -1
        POS = POS + 1
      END IF

C     Integer part
      DO 10 WHILE (POS .LE. LLEN .AND.
     &             LINE(POS) .GE. 48 .AND. LINE(POS) .LE. 57)
        IPART = IPART * 10 + (LINE(POS) - 48)
        POS = POS + 1
10    CONTINUE

C     Decimal point
      IF (POS .LE. LLEN .AND. LINE(POS) .EQ. 46) THEN
        POS = POS + 1

C       Fractional part
        DO 20 WHILE (POS .LE. LLEN .AND.
     &               LINE(POS) .GE. 48 .AND. LINE(POS) .LE. 57)
          FPART = FPART * 10 + (LINE(POS) - 48)
          FDIG = FDIG + 1
          POS = POS + 1
20      CONTINUE
      END IF

C     Build value
      VAL = REAL(IPART)
      IF (FDIG .GT. 0) THEN
        FRAC = REAL(FPART)
        DO 30 I = 1, FDIG
          FRAC = FRAC / 10.0
30      CONTINUE
        VAL = VAL + FRAC
      END IF

      VAL = VAL * REAL(SIGN)

      RETURN
      END

C======================================================================
C     FLPREF - Parse cell reference (e.g., "A1", "AA123")
C     Supports 1-2 letter columns (A-ZZ = 1-702)
C======================================================================
      SUBROUTINE FLPREF(LINE, POS, LLEN, COL, ROW)
      INTEGER LINE(*), POS, LLEN
      INTEGER COL, ROW

      INTEGER CH

      COL = 0
      ROW = 0

C     Skip to opening quote
      DO 5 WHILE (POS .LE. LLEN .AND. LINE(POS) .NE. 34)
        POS = POS + 1
5     CONTINUE
      POS = POS + 1

C     Get column letters (1-2 letters, base-26)
7     IF (POS .GT. LLEN) RETURN
      CH = LINE(POS)
C     Convert lowercase to uppercase
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
C     Check if letter A-Z
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 8
C     Accumulate column number (base-26, 1-based)
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 7

8     IF (COL .EQ. 0) RETURN

C     Get row number
      DO 10 WHILE (POS .LE. LLEN .AND.
     &             LINE(POS) .GE. 48 .AND. LINE(POS) .LE. 57)
        ROW = ROW * 10 + (LINE(POS) - 48)
        POS = POS + 1
10    CONTINUE

      RETURN
      END

C======================================================================
C     FLPTYP - Parse cell type
C======================================================================
      SUBROUTINE FLPTYP(LINE, POS, LLEN, CTYPE)
      INTEGER LINE(*), POS, LLEN
      INTEGER CTYPE

      INTEGER TSTR(20), TLEN, I

C     Extract type string
      CALL FLPSTR(LINE, POS, LLEN, TSTR, 20, TLEN)

C     Match type
      IF (TLEN .EQ. 6 .AND.
     &    TSTR(1) .EQ. 110 .AND. TSTR(2) .EQ. 117 .AND.
     &    TSTR(3) .EQ. 109 .AND. TSTR(4) .EQ. 98 .AND.
     &    TSTR(5) .EQ. 101 .AND. TSTR(6) .EQ. 114) THEN
C       "number"
        CTYPE = 1
      ELSE IF (TLEN .EQ. 7 .AND.
     &         TSTR(1) .EQ. 102 .AND. TSTR(2) .EQ. 111 .AND.
     &         TSTR(3) .EQ. 114 .AND. TSTR(4) .EQ. 109 .AND.
     &         TSTR(5) .EQ. 117 .AND. TSTR(6) .EQ. 108 .AND.
     &         TSTR(7) .EQ. 97) THEN
C       "formula"
        CTYPE = 2
      ELSE IF (TLEN .EQ. 4 .AND.
     &         TSTR(1) .EQ. 116 .AND. TSTR(2) .EQ. 101 .AND.
     &         TSTR(3) .EQ. 120 .AND. TSTR(4) .EQ. 116) THEN
C       "text"
        CTYPE = 3
      ELSE
        CTYPE = 0
      END IF

      RETURN
      END

C======================================================================
C     FLPALG - Parse alignment
C======================================================================
      SUBROUTINE FLPALG(LINE, POS, LLEN, ALIGN)
      INTEGER LINE(*), POS, LLEN
      INTEGER ALIGN

      INTEGER ASTR(20), ALEN

C     Extract alignment string
      CALL FLPSTR(LINE, POS, LLEN, ASTR, 20, ALEN)

C     Match alignment
      IF (ALEN .EQ. 4 .AND.
     &    ASTR(1) .EQ. 108 .AND. ASTR(2) .EQ. 101 .AND.
     &    ASTR(3) .EQ. 102 .AND. ASTR(4) .EQ. 116) THEN
C       "left"
        ALIGN = 1
      ELSE IF (ALEN .EQ. 5 .AND.
     &         ASTR(1) .EQ. 114 .AND. ASTR(2) .EQ. 105 .AND.
     &         ASTR(3) .EQ. 103 .AND. ASTR(4) .EQ. 104 .AND.
     &         ASTR(5) .EQ. 116) THEN
C       "right"
        ALIGN = 2
      ELSE IF (ALEN .EQ. 6 .AND.
     &         ASTR(1) .EQ. 99 .AND. ASTR(2) .EQ. 101 .AND.
     &         ASTR(3) .EQ. 110 .AND. ASTR(4) .EQ. 116 .AND.
     &         ASTR(5) .EQ. 101 .AND. ASTR(6) .EQ. 114) THEN
C       "center"
        ALIGN = 3
      ELSE
        ALIGN = 1
      END IF

      RETURN
      END

C======================================================================
C     FLPSTR - Parse quoted string from line
C======================================================================
      SUBROUTINE FLPSTR(LINE, POS, LLEN, STR, MAXLEN, SLEN)
      INTEGER LINE(*), POS, LLEN
      INTEGER STR(*), MAXLEN, SLEN

      SLEN = 0

C     Skip to opening quote
      DO 5 WHILE (POS .LE. LLEN .AND. LINE(POS) .NE. 34)
        POS = POS + 1
5     CONTINUE
      POS = POS + 1

C     Extract string content
      DO 10 WHILE (POS .LE. LLEN .AND. LINE(POS) .NE. 34)
C       Handle escaped characters
        IF (LINE(POS) .EQ. 92 .AND. POS + 1 .LE. LLEN) THEN
C         Backslash - skip and take next char
          POS = POS + 1
        END IF

        IF (SLEN .LT. MAXLEN) THEN
          SLEN = SLEN + 1
          STR(SLEN) = LINE(POS)
        END IF
        POS = POS + 1
10    CONTINUE

      RETURN
      END

C======================================================================
C     FLSTCL - Store parsed cell data
C======================================================================
      SUBROUTINE FLSTCL(COL, ROW, CTYPE, CVAL,
     &                  CTEXT, CTLEN, CALIGN,
     &                  CFRM, CFLEN)
      INTEGER COL, ROW, CTYPE
      REAL CVAL
      INTEGER CTEXT(*), CTLEN, CALIGN
      INTEGER CFRM(*), CFLEN

      INTEGER TOKENS(100, 4), NTOK, ERROR
      REAL RESULT
      INTEGER DEPS(100, 2), NDEPS, I

C     Validate cell position (255 cols = A-IU, 255 rows)
      IF (COL .LT. 1 .OR. COL .GT. 255) RETURN
      IF (ROW .LT. 1 .OR. ROW .GT. 255) RETURN

C     Store based on type
      IF (CTYPE .EQ. 1) THEN
C       Number
        CALL CELPUT(COL, ROW, 1, CVAL)

      ELSE IF (CTYPE .EQ. 2) THEN
C       Formula - store formula string, then evaluate
        IF (CFLEN .GT. 0) THEN
C         Store formula string
          CALL CELPFS(COL, ROW, CFRM, CFLEN)

C         Parse and evaluate
          CALL PARSE(CFRM, CFLEN, TOKENS, NTOK, ERROR)
          IF (ERROR .EQ. 0 .AND. NTOK .GT. 0) THEN
            CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
            IF (ERROR .EQ. 0) THEN
              CALL CELRES(COL, ROW, RESULT)
            END IF

C           Extract dependencies from tokens (type 2 = cell references)
            NDEPS = 0
            DO 10 I = 1, NTOK
              IF (TOKENS(I, 1) .EQ. 2) THEN
C               Cell reference - add to dependencies
                NDEPS = NDEPS + 1
                IF (NDEPS .LE. 100) THEN
                  DEPS(NDEPS, 1) = TOKENS(I, 3)
                  DEPS(NDEPS, 2) = TOKENS(I, 4)
                END IF
              END IF
10          CONTINUE

C           Register dependencies
            IF (NDEPS .GT. 0) THEN
              CALL DEPSAD(COL, ROW, DEPS, NDEPS)
            END IF
          END IF
        END IF

      ELSE IF (CTYPE .EQ. 3) THEN
C       Text
        CALL CELTXT(COL, ROW, CTEXT, CTLEN, CALIGN)

      END IF

      RETURN
      END

C======================================================================
C     FLPCWD - Parse column width entry
C======================================================================
      SUBROUTINE FLPCWD(KEY, KLEN, LINE, POS, LLEN)
      INTEGER KEY(*), KLEN
      INTEGER LINE(*), POS, LLEN

      INTEGER COL, WIDTH, CH

C     KEY contains the column name (e.g., "A", "AA")
C     Parse column from key
      COL = 0
      DO 10 I = 1, KLEN
        CH = KEY(I)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .GE. 65 .AND. CH .LE. 90) THEN
          COL = COL * 26 + (CH - 64)
        END IF
10    CONTINUE

C     Parse width value
      WIDTH = 0
      DO 20 WHILE (POS .LE. LLEN)
        CH = LINE(POS)
        IF (CH .GE. 48 .AND. CH .LE. 57) THEN
          WIDTH = WIDTH * 10 + (CH - 48)
        ELSE IF (CH .NE. 32 .AND. CH .NE. 44) THEN
          GO TO 30
        END IF
        POS = POS + 1
20    CONTINUE

30    CONTINUE

C     Set column width if valid
      IF (COL .GE. 1 .AND. COL .LE. 255 .AND.
     &    WIDTH .GE. 3 .AND. WIDTH .LE. 40) THEN
        CALL CWSET(COL, WIDTH)
      END IF

      RETURN
      END

