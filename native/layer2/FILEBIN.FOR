C     FILEBIN.FOR - Binary File I/O Module
C
C     Purpose: Compact binary save/load for spreadsheets
C
C     Binary format (.xlb):
C       Header:
C         Bytes 0-1:  Magic 'XL' (0x58, 0x4C)
C         Byte 2:     Version (1)
C         Bytes 3-4:  Cursor col (16-bit LE)
C         Bytes 5-6:  Cursor row (16-bit LE)
C       Column widths:
C         Bytes 0-1:  Count of non-default widths
C         For each:   col (1 byte), width (1 byte)
C       Cells:
C         Bytes 0-1:  Cell count
C         For each cell:
C           col (2 bytes), row (2 bytes)
C           type (1 byte): 1=number, 2=formula, 3=text
C           align (1 byte): 1=left, 2=right, 3=center
C           value (4 bytes, native REAL)
C           strlen (1 byte)
C           string (strlen bytes)
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

C======================================================================
C     FLBSAV - Save spreadsheet to binary file
C======================================================================
      SUBROUTINE FLBSAV(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     Configuration parameters
      INTEGER MAXCEL
      PARAMETER (MAXCEL=2000)

C     UI state for cursor position
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Column widths
      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

C     File unit
      INTEGER FUNIT
      PARAMETER (FUNIT=21)

C     Extension .xlb
      INTEGER EXT(3)
      DATA EXT /120, 108, 98/

C     Working variables
      INTEGER I, COL, ROW, CTYPE, ALIGN
      INTEGER NWID, NCEL
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
      CALL FIOEXT(FNBUF, FNLEN, EXT, 3, 80)

C     Open file for binary writing
      CALL FIOOPB(FUNIT, FNBUF, FNLEN, 2, ERR)
      IF (ERR .NE. 0) RETURN

C     Write magic 'XL'
      CALL FIOWRB(FUNIT, 88)
      CALL FIOWRB(FUNIT, 76)

C     Write version (1)
      CALL FIOWRB(FUNIT, 1)

C     Write cursor position
      CALL FIOWR2(FUNIT, UICOL)
      CALL FIOWR2(FUNIT, UIROW)

C     Count non-default column widths
      NWID = 0
      DO 10 I = 1, 255
        IF (COLWID(I) .NE. 8) NWID = NWID + 1
10    CONTINUE

C     Write width count and widths
      CALL FIOWR2(FUNIT, NWID)
      DO 20 I = 1, 255
        IF (COLWID(I) .NE. 8) THEN
          CALL FIOWRB(FUNIT, I)
          CALL FIOWRB(FUNIT, COLWID(I))
        END IF
20    CONTINUE

C     Count non-empty cells
      NCEL = 0
      DO 40 COL = 1, 255
        DO 30 ROW = 1, 255
          CALL CELGET(COL, ROW, CTYPE, VALUE)
          IF (CTYPE .NE. 0) NCEL = NCEL + 1
30      CONTINUE
40    CONTINUE

C     Write cell count
      CALL FIOWR2(FUNIT, NCEL)

C     Write each cell
      DO 100 COL = 1, 255
        DO 90 ROW = 1, 255
          CALL CELGET(COL, ROW, CTYPE, VALUE)
          IF (CTYPE .EQ. 0) GO TO 90

C         Write col, row
          CALL FIOWR2(FUNIT, COL)
          CALL FIOWR2(FUNIT, ROW)

C         Write type
          CALL FIOWRB(FUNIT, CTYPE)

C         Get alignment (for text cells)
          ALIGN = 1
          IF (CTYPE .EQ. 3) THEN
            CALL CELGTX(COL, ROW, TXTBUF, 80, TXTLEN, ALIGN)
          END IF
          CALL FIOWRB(FUNIT, ALIGN)

C         Write value
          CALL FIOWRR(FUNIT, VALUE)

C         Write string (formula or text)
          IF (CTYPE .EQ. 2) THEN
C           Formula - get formula string
            CALL CELGFS(COL, ROW, FSTR, 80, FSLEN)
            CALL FIOWRB(FUNIT, FSLEN)
            DO 50 I = 1, FSLEN
              CALL FIOWRB(FUNIT, FSTR(I))
50          CONTINUE
          ELSE IF (CTYPE .EQ. 3) THEN
C           Text - already got it above
            CALL FIOWRB(FUNIT, TXTLEN)
            DO 60 I = 1, TXTLEN
              CALL FIOWRB(FUNIT, TXTBUF(I))
60          CONTINUE
          ELSE
C           Number - no string
            CALL FIOWRB(FUNIT, 0)
          END IF

90      CONTINUE
100   CONTINUE

C     Close file
      CALL FIOCLS(FUNIT)

      RETURN
      END

C======================================================================
C     FLBLOD - Load spreadsheet from binary file
C======================================================================
      SUBROUTINE FLBLOD(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     Configuration parameters
      INTEGER MAXCEL
      PARAMETER (MAXCEL=2000)

C     UI state for cursor position
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Column widths
      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

C     File unit
      INTEGER FUNIT
      PARAMETER (FUNIT=21)

C     Extension .xlb
      INTEGER EXT(3)
      DATA EXT /120, 108, 98/

C     Working variables
      INTEGER I, J, COL, ROW, CTYPE, ALIGN
      INTEGER NWID, NCEL, SLEN
      INTEGER MAGIC1, MAGIC2, VER
      INTEGER EOF
      REAL VALUE
      INTEGER STRBUF(80)
      INTEGER FNBUF(80), FNLEN

      ERR = 0

C     Copy filename and add extension
      FNLEN = FLEN
      DO 5 I = 1, FLEN
        FNBUF(I) = FNAME(I)
5     CONTINUE
      CALL FIOEXT(FNBUF, FNLEN, EXT, 3, 80)

C     Open file for binary reading
      CALL FIOOPB(FUNIT, FNBUF, FNLEN, 1, ERR)
      IF (ERR .NE. 0) RETURN

C     Read and verify magic
      CALL FIORDB(FUNIT, MAGIC1, EOF)
      IF (EOF .NE. 0) GO TO 900
      CALL FIORDB(FUNIT, MAGIC2, EOF)
      IF (EOF .NE. 0) GO TO 900

      IF (MAGIC1 .NE. 88 .OR. MAGIC2 .NE. 76) THEN
C       Not a valid XL binary file
        ERR = 2
        GO TO 910
      END IF

C     Read version
      CALL FIORDB(FUNIT, VER, EOF)
      IF (EOF .NE. 0) GO TO 900
      IF (VER .NE. 1) THEN
C       Unsupported version
        ERR = 3
        GO TO 910
      END IF

C     Clear existing data
      CALL CELINI
      CALL CWINI

C     Read cursor position
      CALL FIORD2(FUNIT, UICOL, EOF)
      IF (EOF .NE. 0) GO TO 900
      CALL FIORD2(FUNIT, UIROW, EOF)
      IF (EOF .NE. 0) GO TO 900

C     Read column widths
      CALL FIORD2(FUNIT, NWID, EOF)
      IF (EOF .NE. 0) GO TO 900

      DO 20 I = 1, NWID
        CALL FIORDB(FUNIT, COL, EOF)
        IF (EOF .NE. 0) GO TO 900
        CALL FIORDB(FUNIT, ALIGN, EOF)
        IF (EOF .NE. 0) GO TO 900
        IF (COL .GE. 1 .AND. COL .LE. 255) THEN
          COLWID(COL) = ALIGN
        END IF
20    CONTINUE

C     Read cell count
      CALL FIORD2(FUNIT, NCEL, EOF)
      IF (EOF .NE. 0) GO TO 900

C     Read each cell
      DO 100 I = 1, NCEL
C       Read col, row
        CALL FIORD2(FUNIT, COL, EOF)
        IF (EOF .NE. 0) GO TO 900
        CALL FIORD2(FUNIT, ROW, EOF)
        IF (EOF .NE. 0) GO TO 900

C       Read type
        CALL FIORDB(FUNIT, CTYPE, EOF)
        IF (EOF .NE. 0) GO TO 900

C       Read alignment
        CALL FIORDB(FUNIT, ALIGN, EOF)
        IF (EOF .NE. 0) GO TO 900

C       Read value
        CALL FIORDR(FUNIT, VALUE, EOF)
        IF (EOF .NE. 0) GO TO 900

C       Read string length
        CALL FIORDB(FUNIT, SLEN, EOF)
        IF (EOF .NE. 0) GO TO 900

C       Read string if present
        IF (SLEN .GT. 0 .AND. SLEN .LE. 80) THEN
          DO 50 J = 1, SLEN
            CALL FIORDB(FUNIT, STRBUF(J), EOF)
            IF (EOF .NE. 0) GO TO 900
50        CONTINUE
        END IF

C       Store cell based on type
        IF (CTYPE .EQ. 1) THEN
C         Numeric cell
          CALL CELPUT(COL, ROW, 1, VALUE)
        ELSE IF (CTYPE .EQ. 2) THEN
C         Formula cell - store formula then value
          CALL CELPFS(COL, ROW, STRBUF, SLEN)
          CALL CELPUT(COL, ROW, 2, VALUE)
        ELSE IF (CTYPE .EQ. 3) THEN
C         Text cell
          CALL CELTXT(COL, ROW, STRBUF, SLEN, ALIGN)
        END IF

100   CONTINUE

C     Close file and return success
      CALL FIOCLS(FUNIT)
      RETURN

C     Error handlers
900   ERR = 1
910   CALL FIOCLS(FUNIT)
      RETURN

      END

C======================================================================
C     FLBCHK - Check if file is binary format (by magic number)
C======================================================================
      SUBROUTINE FLBCHK(FNAME, FLEN, ISBIN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ISBIN
      INTEGER ERR

C     File unit
      INTEGER FUNIT
      PARAMETER (FUNIT=21)

C     Working variables
      INTEGER MAGIC1, MAGIC2, EOF

      ISBIN = 0
      ERR = 0

C     Try to open file for binary reading
      CALL FIOOPB(FUNIT, FNAME, FLEN, 1, ERR)
      IF (ERR .NE. 0) RETURN

C     Read first two bytes
      CALL FIORDB(FUNIT, MAGIC1, EOF)
      IF (EOF .NE. 0) GO TO 100
      CALL FIORDB(FUNIT, MAGIC2, EOF)

C     Check for 'XL' magic
      IF (MAGIC1 .EQ. 88 .AND. MAGIC2 .EQ. 76) THEN
        ISBIN = 1
      END IF

100   CALL FIOCLS(FUNIT)
      RETURN
      END

C======================================================================
C     FLXEXT - Check if filename has .xlb extension
C======================================================================
      SUBROUTINE FLXEXT(FNAME, FLEN, ISXLB)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ISXLB

C     Look for .xlb or .XLB at end of filename
      INTEGER C1, C2, C3, C4

      ISXLB = 0

C     Need at least 4 chars for .xlb
      IF (FLEN .LT. 4) RETURN

C     Get last 4 characters
      C1 = FNAME(FLEN - 3)
      C2 = FNAME(FLEN - 2)
      C3 = FNAME(FLEN - 1)
      C4 = FNAME(FLEN)

C     Convert to uppercase for comparison
      IF (C2 .GE. 97 .AND. C2 .LE. 122) C2 = C2 - 32
      IF (C3 .GE. 97 .AND. C3 .LE. 122) C3 = C3 - 32
      IF (C4 .GE. 97 .AND. C4 .LE. 122) C4 = C4 - 32

C     Check for .XLB (46='.', 88='X', 76='L', 66='B')
      IF (C1 .EQ. 46 .AND. C2 .EQ. 88 .AND.
     &    C3 .EQ. 76 .AND. C4 .EQ. 66) THEN
        ISXLB = 1
      END IF

      RETURN
      END

