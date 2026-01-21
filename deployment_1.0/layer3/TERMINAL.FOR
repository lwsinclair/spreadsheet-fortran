C     TERMINAL.FOR - Generic Terminal Driver
C
C     Purpose: High-level terminal API combining protocol + I/O
C
C     This module implements the terminal driver API (TMINIT, TMCLR,
C     TMCURS, TMKEY, etc.) by calling:
C       - Protocol routines (PRCLR, PRCURS, etc.) for escape sequences
C       - I/O routines (IOPUTC, IOGETC, etc.) for byte transfer
C
C     Architecture:
C       Layer 2 (UI/Display) calls TMXXX routines
C           |
C       TERMINAL.FOR (this file) - combines protocol + I/O
C           |
C       +---+---+
C       |       |
C     PROTXXX   IOXXX
C     (VT-52    (Unix,
C      VT-100)   RSX, etc)
C
C     Build Configuration:
C       Link with ONE protocol file (PROTVT52 or PROTVT100)
C       Link with ONE I/O file (IOUNIX, IORSX, IOCPV, etc.)
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     TMINIT - Initialize terminal
C======================================================================
      SUBROUTINE TMINIT
C     Initialize terminal subsystem

C     Initialize platform I/O
      CALL IOINIT

C     Clear screen to known state
      CALL TMCLR

      RETURN
      END

C======================================================================
C     TMCLR - Clear screen
C======================================================================
      SUBROUTINE TMCLR
C     Clear entire screen and home cursor

      CALL PRCLR
      CALL IOFLSH

      RETURN
      END

C======================================================================
C     TMHOME - Home cursor
C======================================================================
      SUBROUTINE TMHOME
C     Move cursor to top-left (1,1)

      CALL PRHOME
      CALL IOFLSH

      RETURN
      END

C======================================================================
C     TMCURS - Position cursor
C======================================================================
      SUBROUTINE TMCURS(ROW, COL)
      INTEGER ROW, COL
C     Move cursor to row, col (1-based)

      CALL PRCURS(ROW, COL)
      CALL IOFLSH

      RETURN
      END

C======================================================================
C     TMCEOL - Clear to end of line
C======================================================================
      SUBROUTINE TMCEOL
C     Clear from cursor to end of line

      CALL PRCEOL
      CALL IOFLSH

      RETURN
      END

C======================================================================
C     TMCUP - Cursor up
C======================================================================
      SUBROUTINE TMCUP
C     Move cursor up one line

      CALL PRCUP

      RETURN
      END

C======================================================================
C     TMCDN - Cursor down
C======================================================================
      SUBROUTINE TMCDN
C     Move cursor down one line

      CALL PRCDN

      RETURN
      END

C======================================================================
C     TMCRT - Cursor right
C======================================================================
      SUBROUTINE TMCRT
C     Move cursor right one column

      CALL PRCRT

      RETURN
      END

C======================================================================
C     TMCLFT - Cursor left
C======================================================================
      SUBROUTINE TMCLFT
C     Move cursor left one column

      CALL PRCLFT

      RETURN
      END

C======================================================================
C     TMPUTC - Output single character
C======================================================================
      SUBROUTINE TMPUTC(CH)
      INTEGER CH
C     Write single character at cursor position

      CALL IOPUTC(CH)

      RETURN
      END

C======================================================================
C     TMPUTS - Output string
C======================================================================
      SUBROUTINE TMPUTS(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
C     Write string of characters

      CALL IOPUTS(STR, LEN)

      RETURN
      END

C======================================================================
C     TMPUTN - Output integer
C======================================================================
      SUBROUTINE TMPUTN(NUM)
      INTEGER NUM
C     Write integer at cursor position

      CALL IOPUTN(NUM)

      RETURN
      END

C======================================================================
C     TMPUTR - Output real number
C======================================================================
      SUBROUTINE TMPUTR(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Write real number with specified precision

      CALL IOPUTR(NUM, PREC)

      RETURN
      END

C======================================================================
C     TMKEY - Read single keystroke
C======================================================================
      SUBROUTINE TMKEY(KEY, VALID)
      INTEGER KEY, VALID
C     Read keystroke with escape sequence handling
C
C     Arrow keys returned as: 128=up, 129=down, 130=right, 131=left
C     Ctrl+Arrow keys: 132=up, 133=down, 134=right, 135=left

C     Escape sequence state (shared)
      INTEGER ESTATE
      COMMON /TMKDAT/ ESTATE

      INTEGER CH, DONE
      INTEGER RETRIES

C     Reset state at start to handle interrupted sequences
      IF (ESTATE .NE. 0) THEN
        CALL PRRSET(ESTATE)
      END IF

C     Loop to handle multi-byte escape sequences
      RETRIES = 0
10    CONTINUE

C     Try to read a character
      CALL IOGETC(CH, VALID)

      IF (VALID .EQ. 0) THEN
C       No character available
        IF (ESTATE .NE. 0) THEN
C         We were in middle of escape sequence but timed out
C         Reset state and try a few more times
          RETRIES = RETRIES + 1
          IF (RETRIES .LT. 3) GO TO 10
C         Give up - reset state
          CALL PRRSET(ESTATE)
        END IF
        KEY = 0
        RETURN
      END IF

C     Got a character - reset retry count
      RETRIES = 0

C     Process through protocol's key translator
      CALL PRKEY(CH, ESTATE, KEY, DONE)

      IF (DONE .EQ. 0) THEN
C       Need more characters for escape sequence
        GO TO 10
      END IF

C     Got a complete key
      RETURN
      END

C======================================================================
C     TMWAIT - Wait for keypress
C======================================================================
      SUBROUTINE TMWAIT(KEY)
      INTEGER KEY
C     Block until a key is pressed

      INTEGER VALID

10    CALL TMKEY(KEY, VALID)
      IF (VALID .EQ. 0) GO TO 10

      RETURN
      END

C======================================================================
C     TMBELL - Ring terminal bell
C======================================================================
      SUBROUTINE TMBELL
C     Sound the terminal bell

      CALL PRBELL

      RETURN
      END

C======================================================================
C     TMRVON - Reverse video on
C======================================================================
      SUBROUTINE TMRVON
C     Enable reverse video (if supported)

      CALL PRRVON

      RETURN
      END

C======================================================================
C     TMRVOF - Reverse video off
C======================================================================
      SUBROUTINE TMRVOF
C     Disable reverse video

      CALL PRRVOF

      RETURN
      END

C======================================================================
C     TMFLSH - Flush output
C======================================================================
      SUBROUTINE TMFLSH
C     Ensure all output is sent

      CALL IOFLSH

      RETURN
      END

C======================================================================
C     TMLINE - Draw horizontal line
C======================================================================
      SUBROUTINE TMLINE(LEN)
      INTEGER LEN
C     Draw line of dashes

      INTEGER I
      INTEGER DASH
      DASH = 45

      DO 10 I = 1, LEN
        CALL IOPUTC(DASH)
10    CONTINUE

      RETURN
      END

C======================================================================
C     TMREST - Restore terminal
C======================================================================
      SUBROUTINE TMREST
C     Restore terminal to normal state before exit

      CALL IOREST

      RETURN
      END

C======================================================================
C     BLOCK DATA - Initialize terminal state
C======================================================================
      BLOCK DATA TMINID
C     Initialize escape sequence state

      INTEGER ESTATE
      COMMON /TMKDAT/ ESTATE

      DATA ESTATE /0/

      END
