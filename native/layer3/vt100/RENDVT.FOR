C     RENDVT.FOR - VT-100/VT-52 CRT Renderer
C
C     Purpose: CRT-specific rendering for screen-based terminals
C
C     This module implements the RDXXX renderer interface for CRT
C     terminals with full-screen capabilities (cursor positioning,
C     screen clear, reverse video).
C
C     Display Layout (24-line terminal):
C       Row 1:     Status line (cell position, mode)
C       Row 2:     Column headers (A B C D E F G H)
C       Row 3:     Separator line
C       Rows 4-23: Grid (20 visible rows)
C       Row 24:    Edit/message line
C
C     Interface:
C       RDINIT              - Initialize CRT display
C       RDEND               - Cleanup CRT display
C       RDCAPS(CURSOR, CLEAR, RVIDEO, ROWS, COLS) - Get capabilities
C       RDFUL               - Full screen refresh
C       RDSTS(COL, ROW, MODE) - Render status line
C       RDEDT(BUF, LEN, MODE) - Render edit line
C       RDMSG(MSGNUM)       - Render status message
C       RDCLC(MSGNUM)       - Render calculation indicator
C       RDHLP               - Render help screen
C       RDHDR(LEFT)         - Render column headers
C       RDSEP               - Render separator line
C       RDGROW(DROW, ROW, LEFT, CURCOL, CURROW) - Render grid row
C       RDCELL(COL, ROW, CURCOL, CURROW, WIDTH) - Render single cell
C
C     Dependencies: TERMINAL.FOR, CELLS.FOR, UI.FOR
C
C     Author: Claude Code
C     Date: 2026-01-23
C
C======================================================================

C======================================================================
C     RDINIT - Initialize CRT display
C======================================================================
      SUBROUTINE RDINIT
C     Initialize CRT terminal for full-screen operation

C     Initialize terminal subsystem
      CALL TMINIT

C     Clear screen to known state
      CALL TMCLR

      RETURN
      END

C======================================================================
C     RDEND - Cleanup CRT display
C======================================================================
      SUBROUTINE RDEND
C     Restore terminal before exit

      CALL TMCLR
      CALL TMHOME
      CALL TMREST

      RETURN
      END

C======================================================================
C     RDCAPS - Get CRT capabilities
C======================================================================
      SUBROUTINE RDCAPS(CURSOR, CLEAR, RVIDEO, ROWS, COLS)
      INTEGER CURSOR, CLEAR, RVIDEO, ROWS, COLS
C     Return device capabilities
C     CURSOR = 1 if cursor positioning supported
C     CLEAR = 1 if screen clear supported
C     RVIDEO = 1 if reverse video supported
C     ROWS = visible rows for grid
C     COLS = visible columns for grid

      CURSOR = 1
      CLEAR = 1
      RVIDEO = 1
      ROWS = 20
      COLS = 8

      RETURN
      END

C======================================================================
C     RDFUL - Full screen refresh
C======================================================================
      SUBROUTINE RDFUL
C     Full CRT screen redraw - called by layer 2

C     Clear screen first
      CALL TMCLR

      RETURN
      END

C======================================================================
C     RDSTS - Render status line
C======================================================================
      SUBROUTINE RDSTS(COL, ROW, MODE)
      INTEGER COL, ROW, MODE
C     Render status line at row 1

      INTEGER CELRF(10), CLEN

C     Position at row 1, column 1
      CALL TMCURS(1, 1)

C     Format cell reference (e.g., "A1")
      CALL FMTCEL(COL, ROW, CELRF, 10, CLEN)

C     Write cell reference
      CALL TMPUTS(CELRF, CLEN)

C     Write separator
      CALL TMPREP(32, 2)

C     Write mode name
      IF (MODE .EQ. 1) THEN
C       POINT mode
        CALL TMPUTC(80)
        CALL TMPUTC(79)
        CALL TMPUTC(73)
        CALL TMPUTC(78)
        CALL TMPUTC(84)
      ELSE IF (MODE .EQ. 2) THEN
C       TYPE mode
        CALL TMPUTC(84)
        CALL TMPUTC(89)
        CALL TMPUTC(80)
        CALL TMPUTC(69)
      ELSE IF (MODE .EQ. 3) THEN
C       SELECT mode
        CALL TMPUTC(83)
        CALL TMPUTC(69)
        CALL TMPUTC(76)
        CALL TMPUTC(69)
        CALL TMPUTC(67)
        CALL TMPUTC(84)
      ELSE IF (MODE .EQ. 4) THEN
C       COMMAND mode
        CALL TMPUTC(67)
        CALL TMPUTC(79)
        CALL TMPUTC(77)
        CALL TMPUTC(77)
        CALL TMPUTC(65)
        CALL TMPUTC(78)
        CALL TMPUTC(68)
      ELSE
C       Unknown
        CALL TMPUTC(63)
      END IF

C     Clear rest of status line
      CALL TMCEOL

      RETURN
      END

C======================================================================
C     RDEDT - Render edit line
C======================================================================
      SUBROUTINE RDEDT(COL, ROW, MODE, BUF, BLEN)
      INTEGER COL, ROW, MODE
      INTEGER BUF(80), BLEN
C     Render edit line at row 24

      INTEGER CTYPE, ALIGN, I
      REAL VALUE
      INTEGER TXTBUF(80), TXTLEN

C     Position at row 24, column 1
      CALL TMCURS(24, 1)

C     Check mode
      IF (MODE .EQ. 1) THEN
C       NAV mode - show current cell contents
        CALL CELGET(COL, ROW, CTYPE, VALUE)

        IF (CTYPE .EQ. 0) THEN
C         Empty cell - show nothing

        ELSE IF (CTYPE .EQ. 1) THEN
C         Numeric - show value
          CALL IOPUTR(VALUE, 2)

        ELSE IF (CTYPE .EQ. 2) THEN
C         Formula - show the formula string
          CALL CELGFS(COL, ROW, TXTBUF, 80, TXTLEN)
          IF (TXTLEN .GT. 0) THEN
            CALL TMPUTS(TXTBUF, TXTLEN)
          END IF

        ELSE IF (CTYPE .EQ. 3) THEN
C         Text - show with alignment prefix
          CALL CELGTX(COL, ROW, TXTBUF, 80, TXTLEN, ALIGN)
          IF (ALIGN .EQ. 1) THEN
            CALL TMPUTC(39)
          ELSE IF (ALIGN .EQ. 2) THEN
            CALL TMPUTC(34)
          ELSE IF (ALIGN .EQ. 3) THEN
            CALL TMPUTC(94)
          END IF
          IF (TXTLEN .GT. 0) THEN
            CALL TMPUTS(TXTBUF, TXTLEN)
          END IF

        END IF

      ELSE IF (MODE .EQ. 2 .OR. MODE .EQ. 3) THEN
C       ENTRY or POINT mode - show input buffer
        IF (BLEN .GT. 0) THEN
          CALL TMPUTS(BUF, BLEN)
        END IF

      ELSE IF (MODE .EQ. 4) THEN
C       COMMAND mode - show buffer with '/' prefix
        CALL TMPUTC(47)
        IF (BLEN .GT. 0) THEN
          CALL TMPUTS(BUF, BLEN)
        END IF

      END IF

C     Clear rest of edit line
      CALL TMCEOL

      RETURN
      END

C======================================================================
C     RDHDR - Render column headers
C======================================================================
      SUBROUTINE RDHDR(LEFT)
      INTEGER LEFT
C     Render column headers at row 2

      INTEGER COL, USED, CWIDTH, PADL, PADR
      INTEGER CNAME(10)
      INTEGER CLEN
      INTEGER CWGET

C     Position at row 2, column 1
      CALL TMCURS(2, 1)

C     Write 8 spaces for row number column
      CALL TMPREP(32, 8)

C     Draw column headers using variable widths
      USED = 0
      COL = LEFT

10    IF (COL .GT. 255) GO TO 90
      CWIDTH = CWGET(COL)

C     Check if column fits (need width+1 for border)
      IF (USED + CWIDTH + 1 .GT. 72) GO TO 90

C     Convert column number to letter(s)
      CALL COLTOA(COL, CNAME, 10, CLEN)

C     Write column header centered in width+1 chars
      CALL TMPUTC(32)
      PADL = (CWIDTH - CLEN) / 2
      PADR = CWIDTH - CLEN - PADL
      CALL TMPREP(32, PADL)
      CALL TMPUTS(CNAME, CLEN)
      CALL TMPREP(32, PADR)

      USED = USED + CWIDTH + 1
      COL = COL + 1
      GO TO 10

90    CONTINUE

      RETURN
      END

C======================================================================
C     RDSEP - Render separator line
C======================================================================
      SUBROUTINE RDSEP
C     Render separator line at row 3

C     Position at row 3, column 1
      CALL TMCURS(3, 1)

C     Draw line across screen (80 chars)
      CALL TMLINE(80)

      RETURN
      END

C======================================================================
C     RDGROW - Render one grid row
C======================================================================
      SUBROUTINE RDGROW(DROW, ROW, LEFT, CURCOL, CURROW)
      INTEGER DROW, ROW, LEFT, CURCOL, CURROW
C     Render one grid row
C     DROW = display row (1-20)
C     ROW = actual spreadsheet row
C     LEFT = leftmost visible column

      INTEGER COL, USED, CWIDTH, SROW
      INTEGER CWGET

C     Screen row (4-23 for grid)
      SROW = DROW + 3

C     Position at start of row
      CALL TMCURS(SROW, 1)

C     Draw row number right-aligned in 8 chars
      CALL TMPREP(32, 5)
      CALL IOPUTN3(ROW)

C     Draw cells in this row with variable widths
      USED = 0
      COL = LEFT

15    IF (COL .GT. 255) GO TO 20
      CWIDTH = CWGET(COL)

C     Check if column fits
      IF (USED + CWIDTH + 1 .GT. 72) GO TO 20

C     Draw one cell
      CALL RDCELL(COL, ROW, CURCOL, CURROW, CWIDTH)

      USED = USED + CWIDTH + 1
      COL = COL + 1
      GO TO 15

20    CONTINUE

      RETURN
      END

C======================================================================
C     RDCELL - Render single cell
C======================================================================
      SUBROUTINE RDCELL(COL, ROW, CURCOL, CURROW, CWIDTH)
      INTEGER COL, ROW, CURCOL, CURROW, CWIDTH
C     Render single cell with optional cursor highlight

      INTEGER CTYPE
      REAL VALUE
      INTEGER I
      INTEGER ISCUR
      INTEGER TXTBUF(80), TXTLEN, ALIGN
      INTEGER PADL, PADR, DISPLEN

C     Retrieve cell contents
      CALL CELGET(COL, ROW, CTYPE, VALUE)

C     Check if this is cursor position
      ISCUR = 0
      IF (COL .EQ. CURCOL .AND. ROW .EQ. CURROW) THEN
        ISCUR = 1
        CALL TMRVON
      END IF

C     Draw cell border
      CALL TMPUTC(32)

C     Draw cell contents
      IF (CTYPE .EQ. 0) THEN
C       Empty cell
        DO 10 I = 1, CWIDTH
          CALL TMPUTC(32)
10      CONTINUE

      ELSE IF (CTYPE .EQ. 1) THEN
C       Numeric value - right-align
        CALL IOPUTRW(VALUE, 2, CWIDTH)

      ELSE IF (CTYPE .EQ. 2) THEN
C       Formula - show result right-aligned
        CALL IOPUTRW(VALUE, 2, CWIDTH)

      ELSE IF (CTYPE .EQ. 3) THEN
C       Text cell - display with alignment
        CALL CELGTX(COL, ROW, TXTBUF, 80, TXTLEN, ALIGN)

C       Limit display to CWIDTH chars
        DISPLEN = TXTLEN
        IF (DISPLEN .GT. CWIDTH) DISPLEN = CWIDTH

C       Calculate padding based on alignment
        IF (ALIGN .EQ. 1) THEN
C         Left align
          PADL = 0
          PADR = CWIDTH - DISPLEN
        ELSE IF (ALIGN .EQ. 2) THEN
C         Right align
          PADL = CWIDTH - DISPLEN
          PADR = 0
        ELSE IF (ALIGN .EQ. 3) THEN
C         Center align
          PADL = (CWIDTH - DISPLEN) / 2
          PADR = CWIDTH - DISPLEN - PADL
        ELSE
C         Default left align
          PADL = 0
          PADR = CWIDTH - DISPLEN
        END IF

C       Output with padding
        DO 50 I = 1, PADL
          CALL TMPUTC(32)
50      CONTINUE
        DO 60 I = 1, DISPLEN
          CALL TMPUTC(TXTBUF(I))
60      CONTINUE
        DO 70 I = 1, PADR
          CALL TMPUTC(32)
70      CONTINUE

      ELSE
C       Unknown type - show spaces
        DO 40 I = 1, CWIDTH
          CALL TMPUTC(32)
40      CONTINUE

      END IF

C     Turn off reverse video if cursor cell
      IF (ISCUR .EQ. 1) THEN
        CALL TMRVOF
      END IF

      RETURN
      END

C======================================================================
C     RDMSG - Render status message
C======================================================================
      SUBROUTINE RDMSG(MSGNUM)
      INTEGER MSGNUM
C     Display status message at row 24

C     Position at bottom
      CALL TMCURS(24, 1)

C     Show message based on code
      IF (MSGNUM .EQ. 1) THEN
C       "File saved."
        CALL RDMSTR(70,105,108,101,32,115,97,118,101,100,46)
      ELSE IF (MSGNUM .EQ. 2) THEN
C       "Save error!"
        CALL RDMSTR(83,97,118,101,32,101,114,114,111,114,33)
      ELSE IF (MSGNUM .EQ. 3) THEN
C       "File loaded."
        CALL RDMS12(70,105,108,101,32,108,111,97,100,101,100,46)
      ELSE IF (MSGNUM .EQ. 4) THEN
C       "Load error!"
        CALL RDMSTR(76,111,97,100,32,101,114,114,111,114,33)
      ELSE IF (MSGNUM .EQ. 5) THEN
C       "Range copied."
        CALL RDMS13(82,97,110,103,101,32,99,111,112,105,101,100,46)
      ELSE IF (MSGNUM .EQ. 6) THEN
C       "Copy error!"
        CALL RDMSTR(67,111,112,121,32,101,114,114,111,114,33)
      ELSE IF (MSGNUM .EQ. 7) THEN
C       "Recalculated."
        CALL RDMS13(82,101,99,97,108,99,117,108,97,116,101,100,46)
      ELSE IF (MSGNUM .EQ. 8) THEN
C       "Recalc: Auto"
        CALL RDMS12(82,101,99,97,108,99,58,32,65,117,116,111)
      ELSE IF (MSGNUM .EQ. 9) THEN
C       "Recalc: Manual"
        CALL RDMS14(82,101,99,97,108,99,58,32,77,97,110,117,97,108)
      ELSE IF (MSGNUM .EQ. 10) THEN
C       "Width set."
        CALL RDMS10(87,105,100,116,104,32,115,101,116,46)
      ELSE IF (MSGNUM .EQ. 11) THEN
C       "Width error!"
        CALL RDMS12(87,105,100,116,104,32,101,114,114,111,114,33)
      ELSE IF (MSGNUM .EQ. 12) THEN
C       "Row inserted."
        CALL RDMS13(82,111,119,32,105,110,115,101,114,116,101,100,46)
      ELSE IF (MSGNUM .EQ. 13) THEN
C       "Invalid row!"
        CALL RDMS12(73,110,118,97,108,105,100,32,114,111,119,33)
      ELSE IF (MSGNUM .EQ. 14) THEN
C       "Row deleted."
        CALL RDMS12(82,111,119,32,100,101,108,101,116,101,100,46)
      ELSE IF (MSGNUM .EQ. 15) THEN
C       "Col inserted."
        CALL RDMS13(67,111,108,32,105,110,115,101,114,116,101,100,46)
      ELSE IF (MSGNUM .EQ. 16) THEN
C       "Invalid col!"
        CALL RDMS12(73,110,118,97,108,105,100,32,99,111,108,33)
      ELSE IF (MSGNUM .EQ. 17) THEN
C       "Col deleted."
        CALL RDMS12(67,111,108,32,100,101,108,101,116,101,100,46)
      END IF

C     Clear rest of line
      CALL TMCEOL

C     Ring bell for errors
      IF (MSGNUM .EQ. 2 .OR. MSGNUM .EQ. 4 .OR. MSGNUM .EQ. 6
     &    .OR. MSGNUM .EQ. 11 .OR. MSGNUM .EQ. 13
     &    .OR. MSGNUM .EQ. 16) THEN
        CALL TMBELL
      END IF

C     Flush output
      CALL TMFLSH

      RETURN
      END

C======================================================================
C     Message output helpers (to avoid 80+ column lines)
C======================================================================
      SUBROUTINE RDMS10(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      CALL TMPUTC(C9)
      CALL TMPUTC(C10)
      RETURN
      END

      SUBROUTINE RDMSTR(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      CALL TMPUTC(C9)
      CALL TMPUTC(C10)
      CALL TMPUTC(C11)
      RETURN
      END

      SUBROUTINE RDMS12(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      CALL TMPUTC(C9)
      CALL TMPUTC(C10)
      CALL TMPUTC(C11)
      CALL TMPUTC(C12)
      RETURN
      END

      SUBROUTINE RDMS13(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      CALL TMPUTC(C9)
      CALL TMPUTC(C10)
      CALL TMPUTC(C11)
      CALL TMPUTC(C12)
      CALL TMPUTC(C13)
      RETURN
      END

      SUBROUTINE RDMS14(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      CALL TMPUTC(C9)
      CALL TMPUTC(C10)
      CALL TMPUTC(C11)
      CALL TMPUTC(C12)
      CALL TMPUTC(C13)
      CALL TMPUTC(C14)
      RETURN
      END

C======================================================================
C     RDCLC - Render calculation indicator
C======================================================================
      SUBROUTINE RDCLC(MSGNUM)
      INTEGER MSGNUM
C     Display calculation indicator on status line

C     Position at end of status line
      CALL TMCURS(1, 15)

C     Show message based on MSGNUM (0-5)
      IF (MSGNUM .EQ. 0) THEN
C       thinking
        CALL RDMS08(116,104,105,110,107,105,110,103)
      ELSE IF (MSGNUM .EQ. 1) THEN
C       musing
        CALL RDMS08(109,117,115,105,110,103,32,32)
      ELSE IF (MSGNUM .EQ. 2) THEN
C       calculat
        CALL RDMS08(99,97,108,99,117,108,97,116)
      ELSE IF (MSGNUM .EQ. 3) THEN
C       working
        CALL RDMS08(119,111,114,107,105,110,103,32)
      ELSE IF (MSGNUM .EQ. 4) THEN
C       crunchin
        CALL RDMS08(99,114,117,110,99,104,105,110)
      ELSE
C       ponderin
        CALL RDMS08(112,111,110,100,101,114,105,110)
      END IF

C     Clear rest and flush
      CALL TMCEOL
      CALL TMFLSH

      RETURN
      END

      SUBROUTINE RDMS08(C1,C2,C3,C4,C5,C6,C7,C8)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8
      CALL TMPUTC(C1)
      CALL TMPUTC(C2)
      CALL TMPUTC(C3)
      CALL TMPUTC(C4)
      CALL TMPUTC(C5)
      CALL TMPUTC(C6)
      CALL TMPUTC(C7)
      CALL TMPUTC(C8)
      RETURN
      END

C======================================================================
C     RDHLP - Render help screen
C======================================================================
      SUBROUTINE RDHLP
C     Display help screen and wait for keypress

      INTEGER KEY

C     Line 1: "              XL SPREADSHEET"
      INTEGER H1(50)
      DATA H1 /32,32,32,32,32,32,32,32,32,32,32,32,32,32,88,76,32,
     &         83,80,82,69,65,68,83,72,69,69,84,32,32,32,32,32,32,
     &         32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 3: "  MOVE   Arrows  Tab  Enter  /? or /HELP"
      INTEGER H3(50)
      DATA H3 /32,32,77,79,86,69,32,32,32,65,114,114,111,119,115,
     &         32,32,84,97,98,32,32,69,110,116,101,114,32,32,47,63,
     &         32,111,114,32,47,72,69,76,80,32,32,32,32,32,32,32,
     &         32,32,32/

C     Line 4: "  EDIT   Del=clear  Esc=cancel"
      INTEGER H4(50)
      DATA H4 /32,32,69,68,73,84,32,32,32,68,101,108,61,99,108,101,
     &         97,114,32,32,69,115,99,61,99,97,110,99,101,108,32,32,
     &         32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 6: "  DATA   42.5   =$A1+B$2*3+C1^2"
      INTEGER H6(50)
      DATA H6 /32,32,68,65,84,65,32,32,32,52,50,46,53,32,32,32,61,
     &         36,65,49,43,66,36,50,42,51,43,67,49,94,50,32,32,32,
     &         32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 7: "  TEXT   'Left  "Right  ^Center"
      INTEGER H7(50)
      DATA H7 /32,32,84,69,88,84,32,32,32,39,76,101,102,116,32,32,
     &         34,82,105,103,104,116,32,32,94,67,101,110,116,101,
     &         114,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
     &         32,32,32/

C     Line 8: "  FUNC   =@SUM(A1:A9)  @AVG @MIN @MAX @N"
      INTEGER H8(50)
      DATA H8 /32,32,70,85,78,67,32,32,32,61,64,83,85,77,40,65,49,
     &         58,65,57,41,32,32,64,65,86,71,32,64,77,73,78,32,64,
     &         77,65,88,32,64,78,32,32,32,32,32,32,32,32,32,32/

C     Line 10: "  /SAVE test.xlb        /OPEN test.xlb"
      INTEGER H10(50)
      DATA H10 /32,32,47,83,65,86,69,32,116,101,115,116,46,120,108,
     &          98,32,32,32,32,32,32,32,32,47,79,80,69,78,32,116,
     &          101,115,116,46,120,108,98,32,32,32,32,32,32,32,32,
     &          32,32,32,32/

C     Line 11: "  /COPY A1:A9 B1:B9     /WIDTH A 12"
      INTEGER H11(50)
      DATA H11 /32,32,47,67,79,80,89,32,65,49,58,65,57,32,66,49,58,
     &          66,57,32,32,32,32,32,47,87,73,68,84,72,32,65,32,49,
     &          50,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 12: "  /IR /DR /IC /DC       insert/delete row/col"
      INTEGER H12(50)
      DATA H12 /32,32,47,73,82,32,47,68,82,32,47,73,67,32,47,68,67,
     &          32,32,32,32,32,32,32,105,110,115,101,114,116,47,100,
     &          101,108,101,116,101,32,114,111,119,47,99,111,108,32,
     &          32,32,32,32/

C     Line 13: "  /RECALC  /RECALC AUTO  /RECALC MAN"
      INTEGER H13(50)
      DATA H13 /32,32,47,82,69,67,65,76,67,32,32,47,82,69,67,65,76,
     &          67,32,65,85,84,79,32,32,47,82,69,67,65,76,67,32,77,
     &          65,78,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 14: "  /QUIT"
      INTEGER H14(50)
      DATA H14 /32,32,47,81,85,73,84,32,32,32,32,32,32,32,32,32,32,
     &          32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
     &          32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32/

C     Line 16: "           press any key"
      INTEGER H16(50)
      DATA H16 /32,32,32,32,32,32,32,32,32,32,32,112,114,101,115,
     &          115,32,97,110,121,32,107,101,121,32,32,32,32,32,32,
     &          32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
     &          32,32,32/

C     Clear screen
      CALL TMCLR

C     Draw help
      CALL TMCURS(1, 1)
      CALL RDHLIN(H1, 50)
      CALL TMCURS(3, 1)
      CALL RDHLIN(H3, 50)
      CALL TMCURS(4, 1)
      CALL RDHLIN(H4, 50)
      CALL TMCURS(6, 1)
      CALL RDHLIN(H6, 50)
      CALL TMCURS(7, 1)
      CALL RDHLIN(H7, 50)
      CALL TMCURS(8, 1)
      CALL RDHLIN(H8, 50)
      CALL TMCURS(10, 1)
      CALL RDHLIN(H10, 50)
      CALL TMCURS(11, 1)
      CALL RDHLIN(H11, 50)
      CALL TMCURS(12, 1)
      CALL RDHLIN(H12, 50)
      CALL TMCURS(13, 1)
      CALL RDHLIN(H13, 50)
      CALL TMCURS(14, 1)
      CALL RDHLIN(H14, 50)
      CALL TMCURS(16, 1)
      CALL RDHLIN(H16, 50)

      CALL IOFLSH
      CALL TMWAIT(KEY)

      RETURN
      END

C======================================================================
C     RDHLIN - Output one help line
C======================================================================
      SUBROUTINE RDHLIN(LINE, LEN)
      INTEGER LINE(*)
      INTEGER LEN

      INTEGER I

      DO 10 I = 1, LEN
        CALL IOPUTC(LINE(I))
10    CONTINUE

      RETURN
      END
