C     DISPLAY.FOR - Display Manager Module (Layer 2)
C
C     Purpose: Manage spreadsheet display state and coordinate rendering
C
C     This module handles:
C       - Viewport state (which rows/columns are visible)
C       - Scrolling logic (when to scroll to follow cursor)
C       - Coordinating full and partial screen updates
C       - Calling layer 3 renderer for actual output
C
C     Display State:
C       DSPTOP = top row of viewport
C       DSPLFT = leftmost column of viewport
C
C     Layer 3 Interface (RDXXX):
C       Rendering routines in layer3/vt100/RENDVT.FOR (CRT)
C       or layer3/tty35/RENDTTY.FOR (teletype)
C
C     Dependencies: CELLS.FOR, UI.FOR, layer 3 renderer
C
C     Author: Claude Code
C     Date: 2026-01-23
C
C======================================================================

C======================================================================
C     DSPINI - Initialize display system
C======================================================================
      SUBROUTINE DSPINI
C     Initialize display state and renderer

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD

      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

C     Initialize viewport to show row 1, column A
      DSPTOP = 1
      DSPLFT = 1

C     No special display mode
      DSPMOD = 0

C     Initialize renderer (layer 3)
      CALL RDINIT

      RETURN
      END

C======================================================================
C     DSPFUL - Full screen redraw
C======================================================================
      SUBROUTINE DSPFUL
C     Redraw entire screen
C     Called after major changes or mode switches

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

C     Tell renderer to start full refresh
      CALL RDFUL

C     Draw all components via renderer
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
C     Draw column headers - delegates to renderer

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

C     Call renderer with leftmost column
      CALL RDHDR(DSPLFT)

      RETURN
      END

C======================================================================
C     DSPSEP - Draw separator line
C======================================================================
      SUBROUTINE DSPSEP
C     Draw separator line - delegates to renderer

      CALL RDSEP

      RETURN
      END

C======================================================================
C     DSPGRD - Draw spreadsheet grid
C======================================================================
      SUBROUTINE DSPGRD
C     Draw spreadsheet grid with cell values

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

      INTEGER DROW, ROW
      INTEGER CURSOR, CLEAR, RVIDEO, NROWS, NCOLS

C     Get renderer capabilities (number of visible rows)
      CALL RDCAPS(CURSOR, CLEAR, RVIDEO, NROWS, NCOLS)

C     Draw visible rows via renderer
      DO 20 DROW = 1, NROWS
C       Actual row number
        ROW = DSPTOP + DROW - 1

C       Draw this row via renderer
        CALL RDGROW(DROW, ROW, DSPLFT, UICOL, UIROW)

20    CONTINUE

      RETURN
      END

C======================================================================
C     DSPCLL - Draw single cell (for incremental updates)
C======================================================================
      SUBROUTINE DSPCLL(COL, ROW, CURCOL, CURROW, CWIDTH)
      INTEGER COL, ROW
      INTEGER CURCOL, CURROW
      INTEGER CWIDTH
C     Draw single cell - delegates to renderer

      CALL RDCELL(COL, ROW, CURCOL, CURROW, CWIDTH)

      RETURN
      END

C======================================================================
C     DSPSTS - Update status line
C======================================================================
      SUBROUTINE DSPSTS
C     Update status line with position and mode

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Call renderer with current state
      CALL RDSTS(UICOL, UIROW, CUMODE)

      RETURN
      END

C======================================================================
C     DSPEDT - Show edit line
C======================================================================
      SUBROUTINE DSPEDT
C     Show edit line with formula or cell contents

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Call renderer with current state
      CALL RDEDT(UICOL, UIROW, CUMODE, INBUF, UIBLEN)

      RETURN
      END

C======================================================================
C     DSPCUR - Position cursor (deprecated - renderer handles this)
C======================================================================
      SUBROUTINE DSPCUR(ROW, COL)
      INTEGER ROW, COL
C     Position cursor at grid cell
C     Note: This is now handled by renderer in RDGROW/RDCELL

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER SROW, SCOL

C     Calculate screen row (rows 4-23 for grid in CRT mode)
      SROW = (ROW - DSPTOP) + 4

C     Calculate screen column
      SCOL = (COL - DSPLFT) * 9 + 9

C     Position cursor via terminal
      CALL TMCURS(SROW, SCOL)

      RETURN
      END

C======================================================================
C     DSPSCR - Scroll viewport
C======================================================================
      SUBROUTINE DSPSCR(COL, ROW)
      INTEGER COL, ROW
C     Scroll viewport to show cell at COL, ROW
C     This is application logic - stays in layer 2

C     Display state
      INTEGER DSPTOP, DSPLFT
      INTEGER DSPMOD
      COMMON /DSPDAT/ DSPTOP, DSPLFT, DSPMOD

      INTEGER REDRAW, USED, C, CWIDTH, VISIBLE
      INTEGER CURSOR, CLEAR, RVIDEO, NROWS, NCOLS
      INTEGER CWGET

      REDRAW = 0

C     Get renderer capabilities
      CALL RDCAPS(CURSOR, CLEAR, RVIDEO, NROWS, NCOLS)

C     Check if row is visible
      IF (ROW .LT. DSPTOP) THEN
C       Scroll up
        DSPTOP = ROW
        REDRAW = 1
      ELSE IF (ROW .GE. DSPTOP + NROWS) THEN
C       Scroll down
        DSPTOP = ROW - NROWS + 1
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
C     Display status message - delegates to renderer

      CALL RDMSG(MSGNUM)

      RETURN
      END

C======================================================================
C     DSPCLC - Show calculation indicator
C======================================================================
      SUBROUTINE DSPCLC(MSGNUM)
      INTEGER MSGNUM
C     Display calculation message - delegates to renderer

      CALL RDCLC(MSGNUM)

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
