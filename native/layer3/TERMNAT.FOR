C     TERMNAT.FOR - Terminal Driver using C output functions
C
C     Purpose: Platform-specific terminal I/O using C library
C
C     This driver delegates all output to C functions that can
C     properly handle escape sequences and arbitrary ASCII codes.
C     FORTRAN's WRITE with A1 format cannot output ESC (ASCII 27)
C     correctly on modern systems.
C
C     Target: ANSI/VT-100 terminals via C stdio
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     TMINIT - Initialize terminal
C======================================================================
      SUBROUTINE TMINIT
C     Initialize terminal to known state

C     Initialize raw terminal mode (C function)
      CALL TMINITC

C     Clear screen
      CALL TMCLR

      RETURN
      END

C======================================================================
C     TMCLR - Clear screen
C======================================================================
      SUBROUTINE TMCLR
C     Clear entire screen and home cursor (via C)

      CALL TMCLRC

      RETURN
      END

C======================================================================
C     TMHOME - Home cursor (move to 1,1)
C======================================================================
      SUBROUTINE TMHOME
C     Move cursor to top-left corner (via C)

      CALL TMHOMEC

      RETURN
      END

C======================================================================
C     TMCURS - Position cursor
C======================================================================
      SUBROUTINE TMCURS(ROW, COL)
      INTEGER ROW, COL
C     Move cursor to specified position (via C)

      CALL TMCURSC(ROW, COL)

      RETURN
      END

C======================================================================
C     TMCEOL - Clear to end of line
C======================================================================
      SUBROUTINE TMCEOL
C     Erase from cursor to end of current line (via C)

      CALL TMCEOLC

      RETURN
      END

C======================================================================
C     TMCUP - Cursor up one line
C======================================================================
      SUBROUTINE TMCUP
C     Move cursor up one line
C     Not implemented via C yet - use direct output

      INTEGER ESC
      ESC = 27
      CALL TMPUTCC(ESC)
      CALL TMPUTCC(91)
      CALL TMPUTCC(65)

      RETURN
      END

C======================================================================
C     TMCDN - Cursor down one line
C======================================================================
      SUBROUTINE TMCDN
C     Move cursor down one line

      INTEGER ESC
      ESC = 27
      CALL TMPUTCC(ESC)
      CALL TMPUTCC(91)
      CALL TMPUTCC(66)

      RETURN
      END

C======================================================================
C     TMCRT - Cursor right one column
C======================================================================
      SUBROUTINE TMCRT
C     Move cursor right one column

      INTEGER ESC
      ESC = 27
      CALL TMPUTCC(ESC)
      CALL TMPUTCC(91)
      CALL TMPUTCC(67)

      RETURN
      END

C======================================================================
C     TMCLFT - Cursor left one column
C======================================================================
      SUBROUTINE TMCLFT
C     Move cursor left one column

      INTEGER ESC
      ESC = 27
      CALL TMPUTCC(ESC)
      CALL TMPUTCC(91)
      CALL TMPUTCC(68)

      RETURN
      END

C======================================================================
C     TMPUTC - Write single character at current position
C======================================================================
      SUBROUTINE TMPUTC(CH)
      INTEGER CH
C     Write character (via C)

      CALL TMPUTCC(CH)

      RETURN
      END

C======================================================================
C     TMPUTS - Write string at current position
C======================================================================
      SUBROUTINE TMPUTS(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
C     Write string of LENGTH characters (via C)

      CALL TMPUTSC(STR, LEN)

      RETURN
      END

C======================================================================
C     TMPUTN - Write integer at current position
C======================================================================
      SUBROUTINE TMPUTN(NUM)
      INTEGER NUM
C     Write integer value (via C)

      CALL TMPUTNC(NUM)

      RETURN
      END

C======================================================================
C     TMPUTR - Write REAL at current position
C======================================================================
      SUBROUTINE TMPUTR(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Write REAL value with PREC decimal places (via C)

      CALL TMPUTRC(NUM, PREC)

      RETURN
      END

C======================================================================
C     TMKEY - Read single keystroke
C======================================================================
      SUBROUTINE TMKEY(KEY, VALID)
      INTEGER KEY
      INTEGER VALID
C     Read a single keystroke from terminal
C     KEY: ASCII code of key pressed
C     VALID: 1 if key read, 0 if no key available
C
C     Special handling for ANSI arrow keys:
C       ESC [ A = Up arrow    -> 128
C       ESC [ B = Down arrow  -> 129
C       ESC [ C = Right arrow -> 130
C       ESC [ D = Left arrow  -> 131

C     Key state
      INTEGER LASKEY, ESCSQ
      COMMON /TMKDAT/ LASKEY, ESCSQ

      INTEGER CH

C     Loop to read characters (handles escape sequences)
10    CONTINUE

C     Read character using C raw terminal function
      CALL TMREADC(CH, VALID)

C     Check if character available
      IF (VALID .EQ. 0) THEN
        KEY = 0
        RETURN
      END IF

C     State machine for ANSI escape sequences
C     ESCSQ: 0=normal, 1=got ESC, 2=got ESC [

      IF (ESCSQ .EQ. 2) THEN
C       Third character of ANSI sequence (ESC [ X)
        ESCSQ = 0

C       Check for arrow keys
        IF (CH .EQ. 65) THEN
C         ESC [ A = Up arrow
          KEY = 128
        ELSE IF (CH .EQ. 66) THEN
C         ESC [ B = Down arrow
          KEY = 129
        ELSE IF (CH .EQ. 67) THEN
C         ESC [ C = Right arrow
          KEY = 130
        ELSE IF (CH .EQ. 68) THEN
C         ESC [ D = Left arrow
          KEY = 131
        ELSE
C         Unknown sequence, return ESC
          KEY = 27
        END IF

        RETURN

      ELSE IF (ESCSQ .EQ. 1) THEN
C       Second character after ESC
        IF (CH .EQ. 91) THEN
C         Got '[', continue to state 2
          ESCSQ = 2
          GO TO 10
        ELSE
C         Not '[', return ESC and this char
          ESCSQ = 0
          KEY = 27
        END IF

        RETURN

      END IF

C     Check if this starts an escape sequence
      IF (CH .EQ. 27) THEN
        ESCSQ = 1
        LASKEY = CH
C       Loop back to read next character
        GO TO 10
      END IF

C     Normal character
      KEY = CH

      RETURN
      END

C======================================================================
C     TMWAIT - Wait for keypress
C======================================================================
      SUBROUTINE TMWAIT(KEY)
      INTEGER KEY
C     Wait for a keypress and return it

      INTEGER VALID

10    CALL TMKEY(KEY, VALID)
      IF (VALID .EQ. 0) GO TO 10

      RETURN
      END

C======================================================================
C     TMBELL - Ring terminal bell
C======================================================================
      SUBROUTINE TMBELL
C     Sound terminal bell (ASCII 7)

      INTEGER BELL
      BELL = 7

      CALL TMPUTCC(BELL)

      RETURN
      END

C======================================================================
C     TMRVON - Reverse video ON
C======================================================================
      SUBROUTINE TMRVON
C     Enable reverse video (via C)

      CALL TMRVONC

      RETURN
      END

C======================================================================
C     TMRVOF - Reverse video OFF
C======================================================================
      SUBROUTINE TMRVOF
C     Disable reverse video (via C)

      CALL TMRVOFC

      RETURN
      END

C======================================================================
C     TMFLSH - Flush output buffer
C======================================================================
      SUBROUTINE TMFLSH
C     Ensure all output is sent to terminal (via C)

      CALL TMFLSHC

      RETURN
      END

C======================================================================
C     TMLINE - Draw horizontal line
C======================================================================
      SUBROUTINE TMLINE(LEN)
      INTEGER LEN
C     Draw horizontal line of dashes

      INTEGER I
      INTEGER DASH
      DASH = 45

      DO 10 I = 1, LEN
        CALL TMPUTCC(DASH)
10    CONTINUE

      RETURN
      END

C======================================================================
C     TMREST - Restore terminal to normal mode
C======================================================================
      SUBROUTINE TMREST
C     Restore terminal settings before exit

      CALL TMRSTC

      RETURN
      END

C======================================================================
C     BLOCK DATA - Initialize terminal state
C======================================================================
      BLOCK DATA TMINID
C     Initialize terminal key state

      INTEGER LASKEY, ESCSQ
      COMMON /TMKDAT/ LASKEY, ESCSQ

      DATA LASKEY /0/
      DATA ESCSQ /0/

      END
