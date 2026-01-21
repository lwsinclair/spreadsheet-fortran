C     FILESAV.FOR - Spreadsheet File Save Module
C
C     Purpose: Serialize spreadsheet to JSON-like format
C
C     This module is portable - it generates lines of text that
C     the platform-specific Layer 3 file I/O writes to disk.
C
C     File format (.xl):
C       {
C         "version": "1.0",
C         "settings": { "cursor_col": 1, "cursor_row": 1 },
C         "cells": [
C           { "ref": "A1", "type": "number", "value": 123.45 },
C           { "ref": "B1", "type": "formula", "formula": "=A1*2", "value": 246.9 },
C           { "ref": "C1", "type": "text", "text": "Hello", "align": "left" }
C         ]
C       }
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     FLSAVE - Save spreadsheet to file
C======================================================================
      SUBROUTINE FLSAVE(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     Configuration parameters
      INTEGER MAXCEL
      PARAMETER (MAXCEL=300)

C     UI state for cursor position
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     File unit
      INTEGER FUNIT
      PARAMETER (FUNIT=20)

C     Line buffer
      INTEGER LINE(256), LLEN

C     Extension
      INTEGER EXT(2)
      DATA EXT /120, 108/

C     Working variables
      INTEGER I, COL, ROW, CTYPE, ALIGN
      INTEGER FIRST
      REAL VALUE
      INTEGER TXTBUF(80), TXTLEN
      INTEGER FSTR(80), FSLEN
      INTEGER FNBUF(80), FNLEN

      ERR = 0

C     Copy filename and add extension
      FNLEN = FLEN
      DO 5 I = 1, FLEN
        FNBUF(I) = FNAME(I)
5     CONTINUE
      CALL FIOEXT(FNBUF, FNLEN, EXT, 2, 80)

C     Open file for writing
      CALL FIOOPN(FUNIT, FNBUF, FNLEN, 2, ERR)
      IF (ERR .NE. 0) RETURN

C     Write opening brace
      LINE(1) = 123
      CALL FIOWTL(FUNIT, LINE, 1)

C     Write version
      CALL FLWSTR(FUNIT, '  "version": "1.0",')

C     Write settings
      CALL FLWSTR(FUNIT, '  "settings": {')
      CALL FLWCUR(FUNIT, UICOL, UIROW)
      CALL FLWSTR(FUNIT, '  },')

C     Write cells array start
      CALL FLWSTR(FUNIT, '  "cells": [')

C     Iterate through all possible cells and write non-empty ones
      FIRST = 1
      DO 100 COL = 1, 26
        DO 90 ROW = 1, 254
          CALL CELGET(COL, ROW, CTYPE, VALUE)

          IF (CTYPE .EQ. 0) GO TO 90

C         Write comma before all but first cell
          IF (FIRST .EQ. 0) THEN
            CALL FLWSTR(FUNIT, '    },')
          END IF
          FIRST = 0

C         Write cell opening
          LINE(1) = 32
          LINE(2) = 32
          LINE(3) = 32
          LINE(4) = 32
          LINE(5) = 123
          CALL FIOWTL(FUNIT, LINE, 5)

C         Write cell reference
          CALL FLWREF(FUNIT, COL, ROW)

C         Write cell type and data
          IF (CTYPE .EQ. 1) THEN
C           Number
            CALL FLWSTR(FUNIT, '      "type": "number",')
            CALL FLWVAL(FUNIT, VALUE)

          ELSE IF (CTYPE .EQ. 2) THEN
C           Formula
            CALL FLWSTR(FUNIT, '      "type": "formula",')
            CALL CELGFS(COL, ROW, FSTR, 80, FSLEN)
            CALL FLWFRM(FUNIT, FSTR, FSLEN)
            CALL FLWVAL(FUNIT, VALUE)

          ELSE IF (CTYPE .EQ. 3) THEN
C           Text
            CALL FLWSTR(FUNIT, '      "type": "text",')
            CALL CELGTX(COL, ROW, TXTBUF, 80, TXTLEN, ALIGN)
            CALL FLWTXT(FUNIT, TXTBUF, TXTLEN, ALIGN)

          END IF

90      CONTINUE
100   CONTINUE

C     Close last cell if any were written
      IF (FIRST .EQ. 0) THEN
        CALL FLWSTR(FUNIT, '    }')
      END IF

C     Write cells array end
      CALL FLWSTR(FUNIT, '  ],')

C     Write empty sheets array (placeholder)
      CALL FLWSTR(FUNIT, '  "sheets": []')

C     Write closing brace
      LINE(1) = 125
      CALL FIOWTL(FUNIT, LINE, 1)

C     Close file
      CALL FIOCLS(FUNIT)

      RETURN
      END

C======================================================================
C     FLWSTR - Write string literal to file
C======================================================================
      SUBROUTINE FLWSTR(UNIT, STR)
      INTEGER UNIT
      CHARACTER*(*) STR

      INTEGER LINE(256), I, SLEN

      SLEN = LEN_TRIM(STR)
      IF (SLEN .GT. 256) SLEN = 256

      DO 10 I = 1, SLEN
        LINE(I) = ICHAR(STR(I:I))
10    CONTINUE

      CALL FIOWTL(UNIT, LINE, SLEN)

      RETURN
      END

C======================================================================
C     FLWREF - Write cell reference line
C======================================================================
      SUBROUTINE FLWREF(UNIT, COL, ROW)
      INTEGER UNIT, COL, ROW

      INTEGER LINE(80), LLEN
      INTEGER ROWSTR(10), RLEN

C     Build: "      "ref": "A1","
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 32
      LINE(6) = 32
      LINE(7) = 34
      LINE(8) = 114
      LINE(9) = 101
      LINE(10) = 102
      LINE(11) = 34
      LINE(12) = 58
      LINE(13) = 32
      LINE(14) = 34
      LINE(15) = 64 + COL
      LLEN = 15

C     Add row number
      CALL ITOA(ROW, ROWSTR, 10, RLEN)
      DO 10 I = 1, RLEN
        LLEN = LLEN + 1
        LINE(LLEN) = ROWSTR(I)
10    CONTINUE

C     Close quote and comma
      LLEN = LLEN + 1
      LINE(LLEN) = 34
      LLEN = LLEN + 1
      LINE(LLEN) = 44

      CALL FIOWTL(UNIT, LINE, LLEN)

      RETURN
      END

C======================================================================
C     FLWVAL - Write value line
C======================================================================
      SUBROUTINE FLWVAL(UNIT, VALUE)
      INTEGER UNIT
      REAL VALUE

      INTEGER LINE(80), LLEN
      INTEGER VALSTR(20), VLEN
      INTEGER I

C     Build: "      "value": 123.45"
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 32
      LINE(6) = 32
      LINE(7) = 34
      LINE(8) = 118
      LINE(9) = 97
      LINE(10) = 108
      LINE(11) = 117
      LINE(12) = 101
      LINE(13) = 34
      LINE(14) = 58
      LINE(15) = 32
      LLEN = 15

C     Convert value to string (2 decimal places)
      CALL RTOA(VALUE, VALSTR, 20, 2, VLEN)
      DO 10 I = 1, VLEN
        LLEN = LLEN + 1
        LINE(LLEN) = VALSTR(I)
10    CONTINUE

      CALL FIOWTL(UNIT, LINE, LLEN)

      RETURN
      END

C======================================================================
C     FLWFRM - Write formula line
C======================================================================
      SUBROUTINE FLWFRM(UNIT, FSTR, FSLEN)
      INTEGER UNIT
      INTEGER FSTR(*)
      INTEGER FSLEN

      INTEGER LINE(256), LLEN
      INTEGER I

C     Build: "      "formula": "=A1+B1","
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 32
      LINE(6) = 32
      LINE(7) = 34
      LINE(8) = 102
      LINE(9) = 111
      LINE(10) = 114
      LINE(11) = 109
      LINE(12) = 117
      LINE(13) = 108
      LINE(14) = 97
      LINE(15) = 34
      LINE(16) = 58
      LINE(17) = 32
      LINE(18) = 34
      LLEN = 18

C     Copy formula
      DO 10 I = 1, FSLEN
        LLEN = LLEN + 1
        LINE(LLEN) = FSTR(I)
10    CONTINUE

C     Close quote and comma
      LLEN = LLEN + 1
      LINE(LLEN) = 34
      LLEN = LLEN + 1
      LINE(LLEN) = 44

      CALL FIOWTL(UNIT, LINE, LLEN)

      RETURN
      END

C======================================================================
C     FLWTXT - Write text and align lines
C======================================================================
      SUBROUTINE FLWTXT(UNIT, TXT, TLEN, ALIGN)
      INTEGER UNIT
      INTEGER TXT(*)
      INTEGER TLEN, ALIGN

      INTEGER LINE(256), LLEN
      INTEGER I

C     Build: "      "text": "Hello","
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 32
      LINE(6) = 32
      LINE(7) = 34
      LINE(8) = 116
      LINE(9) = 101
      LINE(10) = 120
      LINE(11) = 116
      LINE(12) = 34
      LINE(13) = 58
      LINE(14) = 32
      LINE(15) = 34
      LLEN = 15

C     Copy text (escape quotes)
      DO 10 I = 1, TLEN
        IF (TXT(I) .EQ. 34) THEN
C         Escape quote with backslash
          LLEN = LLEN + 1
          LINE(LLEN) = 92
        END IF
        LLEN = LLEN + 1
        LINE(LLEN) = TXT(I)
10    CONTINUE

C     Close quote and comma
      LLEN = LLEN + 1
      LINE(LLEN) = 34
      LLEN = LLEN + 1
      LINE(LLEN) = 44

      CALL FIOWTL(UNIT, LINE, LLEN)

C     Write align line: "      "align": "left""
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 32
      LINE(6) = 32
      LINE(7) = 34
      LINE(8) = 97
      LINE(9) = 108
      LINE(10) = 105
      LINE(11) = 103
      LINE(12) = 110
      LINE(13) = 34
      LINE(14) = 58
      LINE(15) = 32
      LINE(16) = 34
      LLEN = 16

      IF (ALIGN .EQ. 1) THEN
C       left
        LINE(17) = 108
        LINE(18) = 101
        LINE(19) = 102
        LINE(20) = 116
        LLEN = 20
      ELSE IF (ALIGN .EQ. 2) THEN
C       right
        LINE(17) = 114
        LINE(18) = 105
        LINE(19) = 103
        LINE(20) = 104
        LINE(21) = 116
        LLEN = 21
      ELSE
C       center
        LINE(17) = 99
        LINE(18) = 101
        LINE(19) = 110
        LINE(20) = 116
        LINE(21) = 101
        LINE(22) = 114
        LLEN = 22
      END IF

C     Close quote (no comma - last field)
      LLEN = LLEN + 1
      LINE(LLEN) = 34

      CALL FIOWTL(UNIT, LINE, LLEN)

      RETURN
      END

C======================================================================
C     FLWCUR - Write cursor position
C======================================================================
      SUBROUTINE FLWCUR(UNIT, COL, ROW)
      INTEGER UNIT, COL, ROW

      INTEGER LINE(80), LLEN
      INTEGER NUMSTR(10), NLEN
      INTEGER I

C     "    "cursor_col": 1, "cursor_row": 1"
      LINE(1) = 32
      LINE(2) = 32
      LINE(3) = 32
      LINE(4) = 32
      LINE(5) = 34
      LINE(6) = 99
      LINE(7) = 117
      LINE(8) = 114
      LINE(9) = 115
      LINE(10) = 111
      LINE(11) = 114
      LINE(12) = 95
      LINE(13) = 99
      LINE(14) = 111
      LINE(15) = 108
      LINE(16) = 34
      LINE(17) = 58
      LINE(18) = 32
      LLEN = 18

      CALL ITOA(COL, NUMSTR, 10, NLEN)
      DO 10 I = 1, NLEN
        LLEN = LLEN + 1
        LINE(LLEN) = NUMSTR(I)
10    CONTINUE

C     ", "cursor_row": "
      LINE(LLEN+1) = 44
      LINE(LLEN+2) = 32
      LINE(LLEN+3) = 34
      LINE(LLEN+4) = 99
      LINE(LLEN+5) = 117
      LINE(LLEN+6) = 114
      LINE(LLEN+7) = 115
      LINE(LLEN+8) = 111
      LINE(LLEN+9) = 114
      LINE(LLEN+10) = 95
      LINE(LLEN+11) = 114
      LINE(LLEN+12) = 111
      LINE(LLEN+13) = 119
      LINE(LLEN+14) = 34
      LINE(LLEN+15) = 58
      LINE(LLEN+16) = 32
      LLEN = LLEN + 16

      CALL ITOA(ROW, NUMSTR, 10, NLEN)
      DO 20 I = 1, NLEN
        LLEN = LLEN + 1
        LINE(LLEN) = NUMSTR(I)
20    CONTINUE

      CALL FIOWTL(UNIT, LINE, LLEN)

      RETURN
      END

C     Note: ITOA and RTOA are provided by STRUTIL.FOR
