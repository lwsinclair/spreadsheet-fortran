C     RENDTTY.FOR - Teletype Model 35 Renderer
C
C     Purpose: Line-oriented rendering for hardcopy terminals
C
C     This module implements the RDXXX renderer interface for
C     teletype terminals without cursor positioning or screen clear.
C
C     Characteristics:
C       - 110 baud, 10 cps
C       - 72 column paper width
C       - Uppercase only
C       - No cursor positioning
C       - No screen clear (hardcopy)
C       - Separate CR and LF keys
C
C     Display Model:
C       - Print only on explicit /D command
C       - Prompt and input on same line: "A1: 100"
C       - CR moves down, TAB moves right
C       - /P (point) for navigation
C       - /G (goto) for viewport
C
C     Interface:
C       RDINIT              - Initialize TTY display
C       RDEND               - Cleanup TTY display
C       RDCAPS(CURSOR, CLEAR, RVIDEO, ROWS, COLS) - Get capabilities
C       RDFUL               - Full refresh (no-op for TTY)
C       RDSTS(COL, ROW, MODE) - Render prompt
C       RDEDT(BUF, LEN, MODE) - No-op (input on same line as prompt)
C       RDMSG(MSGNUM)       - Print message on new line
C       RDCLC(MSGNUM)       - Print calculation indicator
C       RDHLP               - Print help text
C       RDHDR(LEFT)         - Print column headers
C       RDSEP               - Print separator line
C       RDGROW(DROW, ROW, LEFT, CURCOL, CURROW) - Print grid row
C       RDCELL(COL, ROW, CURCOL, CURROW, WIDTH) - Print cell
C
C     Dependencies: TERMINAL.FOR (TMPUTC, etc.), CELLS.FOR
C
C     Author: Claude Code
C     Date: 2026-01-23
C
C======================================================================

C======================================================================
C     TTY State - Track if we need newline before output
C======================================================================
      BLOCK DATA TTYIND
      INTEGER TTYNL
      COMMON /TTYDAT/ TTYNL
      DATA TTYNL /1/
      END

C======================================================================
C     RDINIT - Initialize TTY display
C======================================================================
      SUBROUTINE RDINIT
C     Initialize teletype terminal

      INTEGER TTYNL
      COMMON /TTYDAT/ TTYNL

C     Initialize terminal subsystem
      CALL TMINIT

C     We're at start of line
      TTYNL = 1

C     Print startup message
      CALL TTYNEW
      CALL TTYMSG(88,76,32,83,80,82,69,65,68,83,72,69,69,84)
      CALL TTYNEW

      RETURN
      END

C======================================================================
C     RDEND - Cleanup TTY display
C======================================================================
      SUBROUTINE RDEND
C     Restore terminal before exit

C     Print newline if needed
      CALL TTYNEW

C     Restore terminal
      CALL TMREST

      RETURN
      END

C======================================================================
C     RDCAPS - Get TTY capabilities
C======================================================================
      SUBROUTINE RDCAPS(CURSOR, CLEAR, RVIDEO, ROWS, COLS)
      INTEGER CURSOR, CLEAR, RVIDEO, ROWS, COLS
C     Return device capabilities

C     No cursor, no clear, no reverse video
      CURSOR = 0
      CLEAR = 0
      RVIDEO = 0

C     Display 10 rows at a time (reasonable for paper)
      ROWS = 10
      COLS = 5

      RETURN
      END

C======================================================================
C     RDFUL - Full screen refresh (no-op for TTY)
C======================================================================
      SUBROUTINE RDFUL
C     No auto-refresh for TTY - only print on /D command

      RETURN
      END

C======================================================================
C     RDSTS - Render cell prompt
C======================================================================
      SUBROUTINE RDSTS(COL, ROW, MODE)
      INTEGER COL, ROW, MODE
C     Print cell prompt: "A1: " on new line

      INTEGER CELRF(10), CLEN
      INTEGER I

C     Start new line
      CALL TTYNEW

C     Format cell reference (e.g., "A1")
      CALL FMTCEL(COL, ROW, CELRF, 10, CLEN)

C     Print cell reference (uppercase)
      DO 10 I = 1, CLEN
        CALL TTYUPC(CELRF(I))
10    CONTINUE

C     Print ": "
      CALL TMPUTC(58)
      CALL TMPUTC(32)

C     Flush to show prompt
      CALL TMFLSH

      RETURN
      END

C======================================================================
C     RDEDT - Render edit line (no-op for TTY)
C======================================================================
      SUBROUTINE RDEDT(COL, ROW, MODE, BUF, BLEN)
      INTEGER COL, ROW, MODE
      INTEGER BUF(80), BLEN
C     For TTY, input appears after prompt on same line
C     Nothing to do here - input handled by main loop

      RETURN
      END

C======================================================================
C     RDHDR - Render column headers
C======================================================================
      SUBROUTINE RDHDR(LEFT)
      INTEGER LEFT
C     Print column headers

      INTEGER COL, USED, CWIDTH, PADL, PADR
      INTEGER CNAME(10), CLEN, I
      INTEGER CWGET

C     Start new line
      CALL TTYNEW

C     Print 6 spaces for row number column
      DO 5 I = 1, 6
        CALL TMPUTC(32)
5     CONTINUE

C     Print column headers
      USED = 0
      COL = LEFT

10    IF (COL .GT. 255) GO TO 90
      CWIDTH = CWGET(COL)

C     Check if column fits (72 - 6 = 66 chars for data)
      IF (USED + CWIDTH + 1 .GT. 66) GO TO 90

C     Get column name
      CALL COLTOA(COL, CNAME, 10, CLEN)

C     Print column header centered
      CALL TMPUTC(32)
      PADL = (CWIDTH - CLEN) / 2
      PADR = CWIDTH - CLEN - PADL
      DO 20 I = 1, PADL
        CALL TMPUTC(32)
20    CONTINUE
      DO 30 I = 1, CLEN
        CALL TTYUPC(CNAME(I))
30    CONTINUE
      DO 40 I = 1, PADR
        CALL TMPUTC(32)
40    CONTINUE

      USED = USED + CWIDTH + 1
      COL = COL + 1
      GO TO 10

90    CONTINUE
      CALL TTYNEW

      RETURN
      END

C======================================================================
C     RDSEP - Render separator line
C======================================================================
      SUBROUTINE RDSEP
C     Print separator line (dashes)

      INTEGER I

C     Print 72 dashes
      DO 10 I = 1, 72
        CALL TMPUTC(45)
10    CONTINUE
      CALL TTYNEW

      RETURN
      END

C======================================================================
C     RDGROW - Render one grid row
C======================================================================
      SUBROUTINE RDGROW(DROW, ROW, LEFT, CURCOL, CURROW)
      INTEGER DROW, ROW, LEFT, CURCOL, CURROW
C     Print one grid row

      INTEGER COL, USED, CWIDTH
      INTEGER CWGET
      INTEGER ROWSTR(10), RLEN, I

C     Print row number right-aligned in 6 chars
      CALL ITOA(ROW, ROWSTR, 10, RLEN)
      DO 5 I = 1, 6 - RLEN
        CALL TMPUTC(32)
5     CONTINUE
      DO 8 I = 1, RLEN
        CALL TMPUTC(ROWSTR(I))
8     CONTINUE

C     Print cells
      USED = 0
      COL = LEFT

15    IF (COL .GT. 255) GO TO 20
      CWIDTH = CWGET(COL)
      IF (USED + CWIDTH + 1 .GT. 66) GO TO 20

C     Print cell (no cursor highlight for TTY)
      CALL RDCELL(COL, ROW, CURCOL, CURROW, CWIDTH)

      USED = USED + CWIDTH + 1
      COL = COL + 1
      GO TO 15

20    CONTINUE
      CALL TTYNEW

      RETURN
      END

C======================================================================
C     RDCELL - Render single cell
C======================================================================
      SUBROUTINE RDCELL(COL, ROW, CURCOL, CURROW, CWIDTH)
      INTEGER COL, ROW, CURCOL, CURROW, CWIDTH
C     Print single cell (no cursor highlight on TTY)

      INTEGER CTYPE
      REAL VALUE
      INTEGER I
      INTEGER TXTBUF(80), TXTLEN, ALIGN
      INTEGER PADL, PADR, DISPLEN

C     Get cell contents
      CALL CELGET(COL, ROW, CTYPE, VALUE)

C     Print border space
      CALL TMPUTC(32)

C     Print cell contents
      IF (CTYPE .EQ. 0) THEN
C       Empty cell
        DO 10 I = 1, CWIDTH
          CALL TMPUTC(32)
10      CONTINUE

      ELSE IF (CTYPE .EQ. 1 .OR. CTYPE .EQ. 2) THEN
C       Numeric or formula result
        CALL IOPUTRW(VALUE, 2, CWIDTH)

      ELSE IF (CTYPE .EQ. 3) THEN
C       Text cell
        CALL CELGTX(COL, ROW, TXTBUF, 80, TXTLEN, ALIGN)

        DISPLEN = TXTLEN
        IF (DISPLEN .GT. CWIDTH) DISPLEN = CWIDTH

C       Calculate padding
        IF (ALIGN .EQ. 1) THEN
          PADL = 0
          PADR = CWIDTH - DISPLEN
        ELSE IF (ALIGN .EQ. 2) THEN
          PADL = CWIDTH - DISPLEN
          PADR = 0
        ELSE IF (ALIGN .EQ. 3) THEN
          PADL = (CWIDTH - DISPLEN) / 2
          PADR = CWIDTH - DISPLEN - PADL
        ELSE
          PADL = 0
          PADR = CWIDTH - DISPLEN
        END IF

C       Output with padding (uppercase)
        DO 50 I = 1, PADL
          CALL TMPUTC(32)
50      CONTINUE
        DO 60 I = 1, DISPLEN
          CALL TTYUPC(TXTBUF(I))
60      CONTINUE
        DO 70 I = 1, PADR
          CALL TMPUTC(32)
70      CONTINUE

      ELSE
C       Unknown type
        DO 40 I = 1, CWIDTH
          CALL TMPUTC(32)
40      CONTINUE

      END IF

      RETURN
      END

C======================================================================
C     RDMSG - Render status message
C======================================================================
      SUBROUTINE RDMSG(MSGNUM)
      INTEGER MSGNUM
C     Print status message on new line

C     Start new line
      CALL TTYNEW

C     Print message based on code (uppercase)
      IF (MSGNUM .EQ. 1) THEN
        CALL TTYMSG(70,73,76,69,32,83,65,86,69,68,46,32,32,32)
      ELSE IF (MSGNUM .EQ. 2) THEN
        CALL TTYMSG(83,65,86,69,32,69,82,82,79,82,33,32,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 3) THEN
        CALL TTYMSG(70,73,76,69,32,76,79,65,68,69,68,46,32,32)
      ELSE IF (MSGNUM .EQ. 4) THEN
        CALL TTYMSG(76,79,65,68,32,69,82,82,79,82,33,32,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 5) THEN
        CALL TTYMSG(82,65,78,71,69,32,67,79,80,73,69,68,46,32)
      ELSE IF (MSGNUM .EQ. 6) THEN
        CALL TTYMSG(67,79,80,89,32,69,82,82,79,82,33,32,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 7) THEN
        CALL TTYMSG(82,69,67,65,76,67,85,76,65,84,69,68,46,32)
      ELSE IF (MSGNUM .EQ. 8) THEN
        CALL TTYMSG(82,69,67,65,76,67,58,32,65,85,84,79,32,32)
      ELSE IF (MSGNUM .EQ. 9) THEN
        CALL TTYMSG(82,69,67,65,76,67,58,32,77,65,78,85,65,76)
      ELSE IF (MSGNUM .EQ. 10) THEN
        CALL TTYMSG(87,73,68,84,72,32,83,69,84,46,32,32,32,32)
      ELSE IF (MSGNUM .EQ. 11) THEN
        CALL TTYMSG(87,73,68,84,72,32,69,82,82,79,82,33,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 12) THEN
        CALL TTYMSG(82,79,87,32,73,78,83,69,82,84,69,68,46,32)
      ELSE IF (MSGNUM .EQ. 13) THEN
        CALL TTYMSG(73,78,86,65,76,73,68,32,82,79,87,33,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 14) THEN
        CALL TTYMSG(82,79,87,32,68,69,76,69,84,69,68,46,32,32)
      ELSE IF (MSGNUM .EQ. 15) THEN
        CALL TTYMSG(67,79,76,32,73,78,83,69,82,84,69,68,46,32)
      ELSE IF (MSGNUM .EQ. 16) THEN
        CALL TTYMSG(73,78,86,65,76,73,68,32,67,79,76,33,32,32)
        CALL TMBELL
      ELSE IF (MSGNUM .EQ. 17) THEN
        CALL TTYMSG(67,79,76,32,68,69,76,69,84,69,68,46,32,32)
      END IF

      CALL TTYNEW
      CALL TMFLSH

      RETURN
      END

C======================================================================
C     RDCLC - Render calculation indicator
C======================================================================
      SUBROUTINE RDCLC(MSGNUM)
      INTEGER MSGNUM
C     Print calculation indicator (simple for TTY)

C     Print a dot to show activity
      CALL TMPUTC(46)
      CALL TMFLSH

      RETURN
      END

C======================================================================
C     RDHLP - Render help screen
C======================================================================
      SUBROUTINE RDHLP
C     Print help text on TTY

      INTEGER KEY

C     Start new line
      CALL TTYNEW

C     Print help text (uppercase, 72 columns max)
      CALL TTYMSG(88,76,32,83,80,82,69,65,68,83,72,69,69,84)
      CALL TTYNEW
      CALL TTYNEW

      CALL TTYMSG(67,79,77,77,65,78,68,83,58,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,68,32,32,32,32,32,32,32,68,73,83,80,76)
      CALL TTYMSG(65,89,32,71,82,73,68,32,32,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,80,32,65,49,32,32,32,80,79,73,78,84,32)
      CALL TTYMSG(84,79,32,67,69,76,76,32,32,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,71,32,65,49,32,32,32,71,79,84,79,32,86)
      CALL TTYMSG(73,69,87,80,79,82,84,32,32,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,67,32,65,49,32,32,32,83,72,79,87,32,67)
      CALL TTYMSG(69,76,76,32,68,69,84,65,73,76,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,83,65,86,69,32,70,73,76,69,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,79,80,69,78,32,70,73,76,69,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,82,69,67,65,76,67,32,32,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYMSG(47,81,85,73,84,32,32,32,32,32,32,32,32,32)
      CALL TTYNEW

      CALL TTYNEW
      CALL TTYMSG(69,78,84,82,89,58,32,67,82,61,68,79,87,78)
      CALL TTYMSG(32,84,65,66,61,82,73,71,72,84,32,32,32,32)
      CALL TTYNEW

      CALL TTYNEW
      CALL TMFLSH

C     Wait for keypress
      CALL TMWAIT(KEY)

      RETURN
      END

C======================================================================
C     Helper routines for TTY output
C======================================================================

C======================================================================
C     TTYNEW - Output newline (CR+LF)
C======================================================================
      SUBROUTINE TTYNEW
      INTEGER TTYNL
      COMMON /TTYDAT/ TTYNL

C     Output CR+LF
      CALL TMPUTC(13)
      CALL TMPUTC(10)
      TTYNL = 1

      RETURN
      END

C======================================================================
C     TTYUPC - Output character forced to uppercase
C======================================================================
      SUBROUTINE TTYUPC(CH)
      INTEGER CH
      INTEGER C

      C = CH

C     Convert lowercase to uppercase
      IF (C .GE. 97 .AND. C .LE. 122) THEN
        C = C - 32
      END IF

      CALL TMPUTC(C)

      RETURN
      END

C======================================================================
C     TTYMSG - Output 14-character message (uppercase)
C======================================================================
      SUBROUTINE TTYMSG(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14)
      INTEGER C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14

      CALL TTYUPC(C1)
      CALL TTYUPC(C2)
      CALL TTYUPC(C3)
      CALL TTYUPC(C4)
      CALL TTYUPC(C5)
      CALL TTYUPC(C6)
      CALL TTYUPC(C7)
      CALL TTYUPC(C8)
      CALL TTYUPC(C9)
      CALL TTYUPC(C10)
      CALL TTYUPC(C11)
      CALL TTYUPC(C12)
      CALL TTYUPC(C13)
      CALL TTYUPC(C14)

      RETURN
      END
