C     DISPLAY.FOR - Screen Rendering Module
C
C     Purpose: Render spreadsheet grid to terminal
C
C     Features:
C       - Clear screen and draw border
C       - Render spreadsheet grid (20×8 viewport)
C       - Show cell values and formulas
C       - Status line (position, mode, indicators)
C       - Edit line for formula entry
C       - Cursor positioning
C
C     Display Layout (24-line VT-52 terminal):
C       Row 1: Status line (position, mode)
C       Row 2: Column headers (A B C D E F G H)
C       Row 3: Separator line
C       Rows 4-23: Grid (20 rows × 8 cols visible)
C       Row 24: Edit line
C
C     Viewport:
C       Shows 20 rows and 8 columns at a time
C       Scrolls to follow cursor
C
C     Dependencies: STRUTIL.FOR, MSG.FOR, UI.FOR, CELLS.FOR, TERMCPV.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C======================================================================

C======================================================================
C     DSPINI - Initialize display system
C======================================================================
      SUBROUTINE DSPINI
C     Initialize display state and clear screen

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD

      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

C     Initialize viewport to show row 1, column A
      DSPTOP = 1
      DSPLFT = 1

C     No special display mode
      DSPMOD = 0

C     Initialize terminal
      CALL TMINIT

C     Clear screen
      CALL TMCLR

      RETURN
      END

C======================================================================
C     DSPFUL - Full screen redraw
C======================================================================
      SUBROUTINE DSPFUL
C     Redraw entire screen
C     Called after major changes or mode switches

C     Clear screen
      CALL TMCLR

C     Draw all components
      CALL DSPSTS
      CALL DSPHDR
      CALL DSPSEP
      CALL DSPGRD
      CALL DSPEDT

      RETURN
      END

C======================================================================
C     DSPHDR - Draw column headers
C======================================================================
      SUBROUTINE DSPHDR
C     Draw column headers with variable widths
C     Row 2 of display
C
C     Layout: 8 chars for row nums, then variable width columns

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER COL, USED, CWIDTH, PADL, PADR
      INTEGER CNAME(10)
      INTEGER CLEN
      INTEGER CWGET

C     Position at row 2, column 1
      CALL TMCURS(2, 1)

C     Write 8 spaces for row number column
      CALL TMPREP(32, 8)

C     Draw column headers using variable widths
C     Available width = 72 chars (80 - 8 for row numbers)
      USED = 0
      COL = DSPLFT

10    IF (COL .GT. 255) GO TO 90
      CWIDTH = CWGET(COL)

C     Check if column fits (need width+1 for border)
      IF (USED + CWIDTH + 1 .GT. 72) GO TO 90

C     Convert column number to letter(s)
      CALL COLTOA(COL, CNAME, 10, CLEN)

C     Write column header centered in width+1 chars (1 border + width)
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
C     DSPSEP - Draw separator line
C======================================================================
      SUBROUTINE DSPSEP
C     Draw separator line under column headers
C     Row 3 of display

C     Position at row 3, column 1
      CALL TMCURS(3, 1)

C     Draw line across screen (80 chars)
      CALL TMLINE(80)

      RETURN
      END

C======================================================================
C     DSPGRD - Draw spreadsheet grid
C======================================================================
      SUBROUTINE DSPGRD
C     Draw spreadsheet grid with cell values
C     Rows 4-23 (20 visible rows)
C
C     Layout: 8 chars for row nums, then variable width columns

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

C     UI state (for cursor position)
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER DROW, ROW, COL, USED, CWIDTH
      INTEGER SROW
      INTEGER CWGET

C     Draw 20 rows
      DO 20 DROW = 1, 20
C       Actual row number
        ROW = DSPTOP + DROW - 1

C       Screen row (4-23)
        SROW = DROW + 3

C       Position at start of row
        CALL TMCURS(SROW, 1)

C       Draw row number right-aligned in 8 chars total
C       Format: 5 spaces + 3-digit number for rows up to 255
        CALL TMPREP(32, 5)
        CALL IOPUTN3(ROW)

C       Draw cells in this row with variable widths
        USED = 0
        COL = DSPLFT

15      IF (COL .GT. 255) GO TO 20
        CWIDTH = CWGET(COL)

C       Check if column fits
        IF (USED + CWIDTH + 1 .GT. 72) GO TO 20

C       Draw one cell with variable width
        CALL DSPCLL(COL, ROW, UICOL, UIROW, CWIDTH)

        USED = USED + CWIDTH + 1
        COL = COL + 1
        GO TO 15

20    CONTINUE

      RETURN
      END

C======================================================================
C     DSPCLL - Draw single cell
C======================================================================
      SUBROUTINE DSPCLL(COL, ROW, CURCOL, CURROW, CWIDTH)
      INTEGER COL, ROW
      INTEGER CURCOL, CURROW
      INTEGER CWIDTH
C     Draw single cell at COL, ROW with specified width
C     Highlight if this is current cursor position
C     width+1 characters total (1 border + width value)

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

C     Draw cell contents (CWIDTH chars)
      IF (CTYPE .EQ. 0) THEN
C       Empty cell - fill with spaces
        DO 10 I = 1, CWIDTH
          CALL TMPUTC(32)
10      CONTINUE

      ELSE IF (CTYPE .EQ. 1) THEN
C       Numeric value - right-align in CWIDTH chars
        CALL IOPUTRW(VALUE, 2, CWIDTH)

      ELSE IF (CTYPE .EQ. 2) THEN
C       Formula - show result right-aligned in CWIDTH chars
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

C       Output left padding
        DO 50 I = 1, PADL
          CALL TMPUTC(32)
50      CONTINUE

C       Output text
        DO 60 I = 1, DISPLEN
          CALL TMPUTC(TXTBUF(I))
60      CONTINUE

C       Output right padding
        DO 70 I = 1, PADR
          CALL TMPUTC(32)
70      CONTINUE

      ELSE
C       Unknown type - show spaces
        DO 40 I = 1, CWIDTH
          CALL TMPUTC(32)
40      CONTINUE

      END IF

C     Turn off reverse video if this was cursor cell
      IF (ISCUR .EQ. 1) THEN
        CALL TMRVOF
      END IF

      RETURN
      END

C======================================================================
C     DSPSTS - Update status line
C======================================================================
      SUBROUTINE DSPSTS
C     Update status line with position and mode
C     Row 1 of display

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER CELRF(10), CLEN
      INTEGER MNAME(10), MLEN
      INTEGER I

C     Position at row 1, column 1
      CALL TMCURS(1, 1)

C     Format cell reference (e.g., "A1")
      CALL FMTCEL(UICOL, UIROW, CELRF, 10, CLEN)

C     Write cell reference
      CALL TMPUTS(CELRF, CLEN)

C     Write separator
      CALL TMPREP(32, 2)

C     Write mode name
      IF (CUMODE .EQ. 1) THEN
C       POINT mode (was NAV) - P,O,I,N,T
        CALL TMPUTC(80)
        CALL TMPUTC(79)
        CALL TMPUTC(73)
        CALL TMPUTC(78)
        CALL TMPUTC(84)
      ELSE IF (CUMODE .EQ. 2) THEN
C       TYPE mode (was ENTRY) - T,Y,P,E
        CALL TMPUTC(84)
        CALL TMPUTC(89)
        CALL TMPUTC(80)
        CALL TMPUTC(69)
      ELSE IF (CUMODE .EQ. 3) THEN
C       SELECT mode (for range selection) - S,E,L,E,C,T
        CALL TMPUTC(83)
        CALL TMPUTC(69)
        CALL TMPUTC(76)
        CALL TMPUTC(69)
        CALL TMPUTC(67)
        CALL TMPUTC(84)
      ELSE IF (CUMODE .EQ. 4) THEN
C       COMMAND mode - C,O,M,M,A,N,D
        CALL TMPUTC(67)
        CALL TMPUTC(79)
        CALL TMPUTC(77)
        CALL TMPUTC(77)
        CALL TMPUTC(65)
        CALL TMPUTC(78)
        CALL TMPUTC(68)
      ELSE
C       Unknown - ?
        CALL TMPUTC(63)
      END IF

C     Clear rest of status line
      CALL TMCEOL

      RETURN
      END

C======================================================================
C     DSPEDT - Show edit line
C======================================================================
      SUBROUTINE DSPEDT
C     Show edit line with formula or cell contents
C     Row 24 (bottom of screen)

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER CTYPE, ALIGN, I
      REAL VALUE
      INTEGER TXTBUF(80), TXTLEN

C     Position at row 24, column 1
      CALL TMCURS(24, 1)

C     Check mode
      IF (CUMODE .EQ. 1) THEN
C       NAV mode - show current cell contents
        CALL CELGET(UICOL, UIROW, CTYPE, VALUE)

        IF (CTYPE .EQ. 0) THEN
C         Empty cell - show nothing

        ELSE IF (CTYPE .EQ. 1) THEN
C         Numeric - show value
          CALL IOPUTR(VALUE, 2)

        ELSE IF (CTYPE .EQ. 2) THEN
C         Formula - show the formula string
          CALL CELGFS(UICOL, UIROW, TXTBUF, 80, TXTLEN)
          IF (TXTLEN .GT. 0) THEN
            CALL TMPUTS(TXTBUF, TXTLEN)
          END IF

        ELSE IF (CTYPE .EQ. 3) THEN
C         Text - show with alignment prefix
          CALL CELGTX(UICOL, UIROW, TXTBUF, 80, TXTLEN, ALIGN)
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

      ELSE IF (CUMODE .EQ. 2 .OR. CUMODE .EQ. 3) THEN
C       ENTRY or POINT mode - show input buffer

        IF (UIBLEN .GT. 0) THEN
C         Show buffer contents
          CALL TMPUTS(INBUF, UIBLEN)
        END IF

      ELSE IF (CUMODE .EQ. 4) THEN
C       COMMAND mode - show buffer with '/' prefix

        CALL TMPUTC(47)

        IF (UIBLEN .GT. 0) THEN
          CALL TMPUTS(INBUF, UIBLEN)
        END IF

      END IF

C     Clear rest of edit line
      CALL TMCEOL

      RETURN
      END

C======================================================================
C     DSPCUR - Position cursor
C======================================================================
      SUBROUTINE DSPCUR(ROW, COL)
      INTEGER ROW, COL
C     Position cursor at grid cell (not screen row/col)
C     Converts grid position to screen position

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER SROW, SCOL

C     Calculate screen row (rows 4-23 for grid)
      SROW = (ROW - DSPTOP) + 4

C     Calculate screen column
C     Each cell is 9 chars wide, plus 8 for row number column
      SCOL = (COL - DSPLFT) * 9 + 9

C     Position cursor
      CALL TMCURS(SROW, SCOL)

      RETURN
      END

C======================================================================
C     DSPSCR - Scroll viewport
C======================================================================
      SUBROUTINE DSPSCR(COL, ROW)
      INTEGER COL, ROW
C     Scroll viewport to show cell at COL, ROW

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER REDRAW, USED, C, CWIDTH, VISIBLE
      INTEGER CWGET

      REDRAW = 0

C     Check if row is visible (20 visible rows)
      IF (ROW .LT. DSPTOP) THEN
C       Scroll up
        DSPTOP = ROW
        REDRAW = 1
      ELSE IF (ROW .GE. DSPTOP + 20) THEN
C       Scroll down
        DSPTOP = ROW - 19
        REDRAW = 1
      END IF

C     Check if column is visible with variable widths
      IF (COL .LT. DSPLFT) THEN
C       Column is to the left - scroll left
        DSPLFT = COL
        REDRAW = 1
      ELSE
C       Check if column fits in current viewport
        USED = 0
        VISIBLE = 0
        C = DSPLFT
10      IF (C .GT. 255) GO TO 20
        CWIDTH = CWGET(C)
        IF (USED + CWIDTH + 1 .GT. 72) GO TO 20
        IF (C .EQ. COL) VISIBLE = 1
        USED = USED + CWIDTH + 1
        C = C + 1
        GO TO 10
20      CONTINUE

        IF (VISIBLE .EQ. 0) THEN
C         Column not visible - scroll right so COL is leftmost
          DSPLFT = COL
          REDRAW = 1
        END IF
      END IF

C     Redraw if we scrolled
      IF (REDRAW .EQ. 1) THEN
        CALL DSPFUL
      END IF

      RETURN
      END

C======================================================================
C     DSPMSG - Show status message at bottom of screen
C======================================================================
      SUBROUTINE DSPMSG(MSGNUM)
      INTEGER MSGNUM
C     Display status message by code
C     1 = File saved
C     2 = Save error
C     3 = File loaded
C     4 = Load error
C     5 = Range copied
C     6 = Copy error

C     Position at bottom
      CALL TMCURS(24, 1)

C     Show message based on code
      IF (MSGNUM .EQ. 1) THEN
C       "File saved." (11 chars)
        CALL TMPUTC(70)
        CALL TMPUTC(105)
        CALL TMPUTC(108)
        CALL TMPUTC(101)
        CALL TMPUTC(32)
        CALL TMPUTC(115)
        CALL TMPUTC(97)
        CALL TMPUTC(118)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 2) THEN
C       "Save error!" (11 chars)
        CALL TMPUTC(83)
        CALL TMPUTC(97)
        CALL TMPUTC(118)
        CALL TMPUTC(101)
        CALL TMPUTC(32)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(114)
        CALL TMPUTC(111)
        CALL TMPUTC(114)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 3) THEN
C       "File loaded." (12 chars)
        CALL TMPUTC(70)
        CALL TMPUTC(105)
        CALL TMPUTC(108)
        CALL TMPUTC(101)
        CALL TMPUTC(32)
        CALL TMPUTC(108)
        CALL TMPUTC(111)
        CALL TMPUTC(97)
        CALL TMPUTC(100)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 4) THEN
C       "Load error!" (11 chars)
        CALL TMPUTC(76)
        CALL TMPUTC(111)
        CALL TMPUTC(97)
        CALL TMPUTC(100)
        CALL TMPUTC(32)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(114)
        CALL TMPUTC(111)
        CALL TMPUTC(114)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 5) THEN
C       "Range copied." (13 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(97)
        CALL TMPUTC(110)
        CALL TMPUTC(103)
        CALL TMPUTC(101)
        CALL TMPUTC(32)
        CALL TMPUTC(99)
        CALL TMPUTC(111)
        CALL TMPUTC(112)
        CALL TMPUTC(105)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 6) THEN
C       "Copy error!" (11 chars)
        CALL TMPUTC(67)
        CALL TMPUTC(111)
        CALL TMPUTC(112)
        CALL TMPUTC(121)
        CALL TMPUTC(32)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(114)
        CALL TMPUTC(111)
        CALL TMPUTC(114)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 7) THEN
C       "Recalculated." (13 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(101)
        CALL TMPUTC(99)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(99)
        CALL TMPUTC(117)
        CALL TMPUTC(108)
        CALL TMPUTC(97)
        CALL TMPUTC(116)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 8) THEN
C       "Recalc: Auto" (12 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(101)
        CALL TMPUTC(99)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(99)
        CALL TMPUTC(58)
        CALL TMPUTC(32)
        CALL TMPUTC(65)
        CALL TMPUTC(117)
        CALL TMPUTC(116)
        CALL TMPUTC(111)
      ELSE IF (MSGNUM .EQ. 9) THEN
C       "Recalc: Manual" (14 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(101)
        CALL TMPUTC(99)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(99)
        CALL TMPUTC(58)
        CALL TMPUTC(32)
        CALL TMPUTC(77)
        CALL TMPUTC(97)
        CALL TMPUTC(110)
        CALL TMPUTC(117)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
      ELSE IF (MSGNUM .EQ. 10) THEN
C       "Width set." (10 chars)
        CALL TMPUTC(87)
        CALL TMPUTC(105)
        CALL TMPUTC(100)
        CALL TMPUTC(116)
        CALL TMPUTC(104)
        CALL TMPUTC(32)
        CALL TMPUTC(115)
        CALL TMPUTC(101)
        CALL TMPUTC(116)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 11) THEN
C       "Width error!" (12 chars)
        CALL TMPUTC(87)
        CALL TMPUTC(105)
        CALL TMPUTC(100)
        CALL TMPUTC(116)
        CALL TMPUTC(104)
        CALL TMPUTC(32)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(114)
        CALL TMPUTC(111)
        CALL TMPUTC(114)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 12) THEN
C       "Row inserted." (13 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(111)
        CALL TMPUTC(119)
        CALL TMPUTC(32)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(115)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(116)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 13) THEN
C       "Invalid row!" (12 chars)
        CALL TMPUTC(73)
        CALL TMPUTC(110)
        CALL TMPUTC(118)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(105)
        CALL TMPUTC(100)
        CALL TMPUTC(32)
        CALL TMPUTC(114)
        CALL TMPUTC(111)
        CALL TMPUTC(119)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 14) THEN
C       "Row deleted." (12 chars)
        CALL TMPUTC(82)
        CALL TMPUTC(111)
        CALL TMPUTC(119)
        CALL TMPUTC(32)
        CALL TMPUTC(100)
        CALL TMPUTC(101)
        CALL TMPUTC(108)
        CALL TMPUTC(101)
        CALL TMPUTC(116)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 15) THEN
C       "Col inserted." (13 chars)
        CALL TMPUTC(67)
        CALL TMPUTC(111)
        CALL TMPUTC(108)
        CALL TMPUTC(32)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(115)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(116)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
      ELSE IF (MSGNUM .EQ. 16) THEN
C       "Invalid col!" (12 chars)
        CALL TMPUTC(73)
        CALL TMPUTC(110)
        CALL TMPUTC(118)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(105)
        CALL TMPUTC(100)
        CALL TMPUTC(32)
        CALL TMPUTC(99)
        CALL TMPUTC(111)
        CALL TMPUTC(108)
        CALL TMPUTC(33)
      ELSE IF (MSGNUM .EQ. 17) THEN
C       "Col deleted." (12 chars)
        CALL TMPUTC(67)
        CALL TMPUTC(111)
        CALL TMPUTC(108)
        CALL TMPUTC(32)
        CALL TMPUTC(100)
        CALL TMPUTC(101)
        CALL TMPUTC(108)
        CALL TMPUTC(101)
        CALL TMPUTC(116)
        CALL TMPUTC(101)
        CALL TMPUTC(100)
        CALL TMPUTC(46)
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
C     DSPCLC - Show calculation indicator
C======================================================================
      SUBROUTINE DSPCLC(MSGNUM)
      INTEGER MSGNUM
C     Display cycling calculation message on status line
C     Messages: thinking, musing, calculating, working, crunching, deliberating

C     Position at end of status line (after mode)
      CALL TMCURS(1, 15)

C     Show message based on MSGNUM (0-5)
      IF (MSGNUM .EQ. 0) THEN
C       thinking (8 chars)
        CALL TMPUTC(116)
        CALL TMPUTC(104)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(107)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(103)
      ELSE IF (MSGNUM .EQ. 1) THEN
C       musing (6 chars)
        CALL TMPUTC(109)
        CALL TMPUTC(117)
        CALL TMPUTC(115)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(103)
        CALL TMPUTC(32)
        CALL TMPUTC(32)
      ELSE IF (MSGNUM .EQ. 2) THEN
C       calculating (11 chars -> 8)
        CALL TMPUTC(99)
        CALL TMPUTC(97)
        CALL TMPUTC(108)
        CALL TMPUTC(99)
        CALL TMPUTC(117)
        CALL TMPUTC(108)
        CALL TMPUTC(97)
        CALL TMPUTC(116)
      ELSE IF (MSGNUM .EQ. 3) THEN
C       working (7 chars)
        CALL TMPUTC(119)
        CALL TMPUTC(111)
        CALL TMPUTC(114)
        CALL TMPUTC(107)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
        CALL TMPUTC(103)
        CALL TMPUTC(32)
      ELSE IF (MSGNUM .EQ. 4) THEN
C       crunching (9 chars -> 8)
        CALL TMPUTC(99)
        CALL TMPUTC(114)
        CALL TMPUTC(117)
        CALL TMPUTC(110)
        CALL TMPUTC(99)
        CALL TMPUTC(104)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
      ELSE
C       pondering (9 chars -> 8)
        CALL TMPUTC(112)
        CALL TMPUTC(111)
        CALL TMPUTC(110)
        CALL TMPUTC(100)
        CALL TMPUTC(101)
        CALL TMPUTC(114)
        CALL TMPUTC(105)
        CALL TMPUTC(110)
      END IF

C     Clear rest and flush
      CALL TMCEOL
      CALL TMFLSH

      RETURN
      END

C======================================================================
C     BLOCK DATA - Initialize display state
C======================================================================
      BLOCK DATA DSPIND
C     Initialize display viewport

      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD

      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      DATA DSPTOP /1/
      DATA DSPLFT /1/
      DATA DSPMOD /0/

      END
