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
C     Draw column headers (A B C D E F G H)
C     Row 2 of display

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER I, COL
      INTEGER CNAME(10)
      INTEGER CLEN

C     Position at row 2, column 1
      CALL TMCURS(2, 1)

C     Write "   " (row number space)
      CALL TMPUTS(32, 3)

C     Draw 8 column headers
      DO 10 I = 1, 8
        COL = DSPLFT + I - 1

C       Convert column number to letter(s)
        CALL COLTOA(COL, CNAME, 10, CLEN)

C       Write column name centered in 10-char width
        CALL TMPUTS(32, 4)
        CALL TMPUTS(CNAME, CLEN)
        CALL TMPUTS(32, 4 - CLEN)
10    CONTINUE

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

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

C     UI state (for cursor position)
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

      INTEGER DROW, ROW, COL
      INTEGER I, SROW

C     Draw 20 rows
      DO 20 DROW = 1, 20
C       Actual row number
        ROW = DSPTOP + DROW - 1

C       Screen row (4-23)
        SROW = DROW + 3

C       Position at start of row
        CALL TMCURS(SROW, 1)

C       Draw row number (right-aligned in 3 chars)
        IF (ROW .LT. 10) THEN
          CALL TMPUTS(32, 2)
          CALL TMPUTN(ROW)
        ELSE IF (ROW .LT. 100) THEN
          CALL TMPUTS(32, 1)
          CALL TMPUTN(ROW)
        ELSE
          CALL TMPUTN(ROW)
        END IF

C       Draw cells in this row (8 columns)
        DO 10 I = 1, 8
          COL = DSPLFT + I - 1

C         Draw one cell
          CALL DSPCLL(COL, ROW, UICOL, UIROW)

10      CONTINUE

20    CONTINUE

      RETURN
      END

C======================================================================
C     DSPCLL - Draw single cell
C======================================================================
      SUBROUTINE DSPCLL(COL, ROW, CURCOL, CURROW)
      INTEGER COL, ROW
      INTEGER CURCOL, CURROW
C     Draw single cell at COL, ROW
C     Highlight if this is current cursor position
C     10 characters wide per cell

      INTEGER CTYPE, CVAL(80), CLEN
      REAL VALUE
      INTEGER I

C     Retrieve cell contents
      CALL CELGET(COL, ROW, CTYPE, VALUE)

C     Get cell formula if it has one
      CALL CELGTK(COL, ROW, CVAL, 80, CLEN)

C     Check if this is cursor position
C     (For now, no highlighting since VT-52 doesn't support it)

C     Draw cell border
      CALL TMPUTC(32)

C     Draw cell contents
      IF (CTYPE .EQ. 0) THEN
C       Empty cell - fill with spaces
        DO 10 I = 1, 9
          CALL TMPUTC(32)
10      CONTINUE

      ELSE IF (CTYPE .EQ. 1) THEN
C       Numeric value - right-align
        CALL TMPUTR(VALUE, 2)

C       Pad remaining space
        DO 20 I = 1, 2
          CALL TMPUTC(32)
20      CONTINUE

      ELSE IF (CTYPE .EQ. 2) THEN
C       Formula - show result right-aligned
        CALL TMPUTR(VALUE, 2)

C       Pad
        DO 30 I = 1, 2
          CALL TMPUTC(32)
30      CONTINUE

      ELSE
C       Unknown type - show spaces
        DO 40 I = 1, 9
          CALL TMPUTC(32)
40      CONTINUE

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
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

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
      CALL TMPUTS(32, 2)

C     Write mode
      IF (CUMODE .EQ. 1) THEN
C       NAV mode
        DATA MNAME /78,65,86,32,32,32,32,32,32,32/
        MLEN = 3
      ELSE IF (CUMODE .EQ. 2) THEN
C       ENTRY mode
        DATA MNAME /69,78,84,82,89,32,32,32,32,32/
        MLEN = 5
      ELSE IF (CUMODE .EQ. 3) THEN
C       POINT mode
        DATA MNAME /80,79,73,78,84,32,32,32,32,32/
        MLEN = 5
      ELSE IF (CUMODE .EQ. 4) THEN
C       COMMAND mode
        DATA MNAME /67,79,77,77,65,78,68,32,32,32/
        MLEN = 7
      ELSE
C       Unknown
        DATA MNAME /63,63,63,32,32,32,32,32,32,32/
        MLEN = 3
      END IF

      CALL TMPUTS(MNAME, MLEN)

C     Clear rest of status line
      CALL TMCEOL

      RETURN
      END

C======================================================================
C     DSPEDT - Show edit line
C======================================================================
      SUBROUTINE DSPEDT
C     Show edit line with formula
C     Row 24 (bottom of screen)

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Position at row 24, column 1
      CALL TMCURS(24, 1)

C     Check mode
      IF (CUMODE .EQ. 2 .OR. CUMODE .EQ. 3) THEN
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
C     Each cell is 10 chars wide, plus 3 for row number
      SCOL = (COL - DSPLFT) * 10 + 4

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

      INTEGER REDRAW

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

C     Check if column is visible (8 visible columns)
      IF (COL .LT. DSPLFT) THEN
C       Scroll left
        DSPLFT = COL
        REDRAW = 1
      ELSE IF (COL .GE. DSPLFT + 8) THEN
C       Scroll right
        DSPLFT = COL - 7
        REDRAW = 1
      END IF

C     Redraw if we scrolled
      IF (REDRAW .EQ. 1) THEN
        CALL DSPFUL
      END IF

      RETURN
      END

C======================================================================
C     DSPMSG - Show message at bottom of screen
C======================================================================
      SUBROUTINE DSPMSG(MSG, LEN)
      INTEGER MSG(*)
      INTEGER LEN
C     Display message at row 24 (overwrites edit line temporarily)

C     Position at bottom
      CALL TMCURS(24, 1)

C     Show message
      CALL TMPUTS(MSG, LEN)

C     Clear rest of line
      CALL TMCEOL

C     Ring bell
      CALL TMBELL

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
