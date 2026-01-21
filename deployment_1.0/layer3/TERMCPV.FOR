C     TERMCPV.FOR - Terminal Driver for CP-V with VT-52 CRT
C
C     Purpose: Platform-specific terminal I/O for Sigma CP-V
C
C     Target: VT-52 compatible CRT terminals (1978 era)
C
C     VT-52 Escape Sequences:
C       ESC H       - Home cursor (top-left)
C       ESC J       - Clear to end of screen
C       ESC K       - Clear to end of line
C       ESC Y r c   - Direct cursor address (row+32, col+32)
C       ESC A       - Cursor up
C       ESC B       - Cursor down
C       ESC C       - Cursor right
C       ESC D       - Cursor left
C       ESC I       - Reverse line feed
C
C     Special Keys:
C       ESC         - ASCII 27
C       RETURN      - ASCII 13
C       BACKSPACE   - ASCII 8 or 127
C       Arrow keys  - ESC followed by A/B/C/D
C
C     Dependencies: None (pure terminal I/O)
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C     FORTRAN IV Notes:
C       - Uses FORMAT with A1 for character I/O
C       - Terminal unit is 6 (standard output)
C       - Input unit is 5 (standard input)
C       - Non-advancing I/O via $ format (CP-V extension)
C
C======================================================================

C======================================================================
C     TMINIT - Initialize terminal
C======================================================================
      SUBROUTINE TMINIT
C     Initialize terminal to known state
C     Called once at program startup

C     ESC sequence buffer
      INTEGER ESC, CH1, CH2

C     ASCII codes
      ESC = 27

C     Initialize raw terminal mode (C function)
      CALL TMINITC

C     Send initialization sequence (if needed)
C     VT-52 doesn't require init, but we clear screen

      CALL TMCLR

      RETURN
      END

C======================================================================
C     TMCLR - Clear screen
C======================================================================
      SUBROUTINE TMCLR
C     Clear entire screen and home cursor
C
C     VT-52: ESC H (home) followed by ESC J (clear to end)

      INTEGER ESC, H, J

      ESC = 27
      H = 72
      J = 74

C     Write ESC H (home cursor)
      WRITE(6,100) ESC, H

C     Write ESC J (clear to end of screen)
      WRITE(6,100) ESC, J

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMHOME - Home cursor (move to 1,1)
C======================================================================
      SUBROUTINE TMHOME
C     Move cursor to top-left corner (1,1)
C
C     VT-52: ESC H

      INTEGER ESC, H

      ESC = 27
      H = 72

      WRITE(6,100) ESC, H

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMCURS - Position cursor
C======================================================================
      SUBROUTINE TMCURS(ROW, COL)
      INTEGER ROW, COL
C     Move cursor to specified position
C
C     VT-52: ESC Y row col
C     Where row and col are offset by 32 (space character)
C     Valid range: row 1-24, col 1-80

      INTEGER ESC, Y
      INTEGER RCHAR, CCHAR

      ESC = 27
      Y = 89

C     Convert row/col to VT-52 format (add 31 for base-0 + space offset)
      RCHAR = ROW + 31
      CCHAR = COL + 31

C     Write ESC Y row col
      WRITE(6,100) ESC, Y, RCHAR, CCHAR

100   FORMAT(4A1,$)

      RETURN
      END

C======================================================================
C     TMCEOL - Clear to end of line
C======================================================================
      SUBROUTINE TMCEOL
C     Erase from cursor to end of current line
C
C     VT-52: ESC K

      INTEGER ESC, K

      ESC = 27
      K = 75

      WRITE(6,100) ESC, K

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMCUP - Cursor up one line
C======================================================================
      SUBROUTINE TMCUP
C     Move cursor up one line
C
C     VT-52: ESC A

      INTEGER ESC, A

      ESC = 27
      A = 65

      WRITE(6,100) ESC, A

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMCDN - Cursor down one line
C======================================================================
      SUBROUTINE TMCDN
C     Move cursor down one line
C
C     VT-52: ESC B

      INTEGER ESC, B

      ESC = 27
      B = 66

      WRITE(6,100) ESC, B

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMCRT - Cursor right one column
C======================================================================
      SUBROUTINE TMCRT
C     Move cursor right one column
C
C     VT-52: ESC C

      INTEGER ESC, C

      ESC = 27
      C = 67

      WRITE(6,100) ESC, C

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMCLFT - Cursor left one column
C======================================================================
      SUBROUTINE TMCLFT
C     Move cursor left one column
C
C     VT-52: ESC D

      INTEGER ESC, D

      ESC = 27
      D = 68

      WRITE(6,100) ESC, D

100   FORMAT(2A1,$)

      RETURN
      END

C======================================================================
C     TMPUTC - Write single character at current position
C======================================================================
      SUBROUTINE TMPUTC(CH)
      INTEGER CH
C     Write character without advancing to next line
C     Character stays at current cursor position

      WRITE(6,100) CH

100   FORMAT(A1,$)

      RETURN
      END

C======================================================================
C     TMPUTS - Write string at current position
C======================================================================
      SUBROUTINE TMPUTS(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
C     Write string of LENGTH characters
C     Does not advance to next line

      INTEGER I

      DO 10 I = 1, LEN
        WRITE(6,100) STR(I)
10    CONTINUE

100   FORMAT(A1,$)

      RETURN
      END

C======================================================================
C     TMPUTN - Write integer at current position
C======================================================================
      SUBROUTINE TMPUTN(NUM)
      INTEGER NUM
C     Write integer value
C     Does not advance to next line

      WRITE(6,100) NUM

100   FORMAT(I6,$)

      RETURN
      END

C======================================================================
C     TMPUTR - Write REAL at current position
C======================================================================
      SUBROUTINE TMPUTR(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Write REAL value with PREC decimal places
C     Does not advance to next line
C     PREC: 0-6 decimal places

      IF (PREC .EQ. 0) THEN
        WRITE(6,100) NUM
      ELSE IF (PREC .EQ. 1) THEN
        WRITE(6,101) NUM
      ELSE IF (PREC .EQ. 2) THEN
        WRITE(6,102) NUM
      ELSE IF (PREC .EQ. 3) THEN
        WRITE(6,103) NUM
      ELSE IF (PREC .EQ. 4) THEN
        WRITE(6,104) NUM
      ELSE IF (PREC .EQ. 5) THEN
        WRITE(6,105) NUM
      ELSE
        WRITE(6,106) NUM
      END IF

100   FORMAT(F10.0,$)
101   FORMAT(F10.1,$)
102   FORMAT(F10.2,$)
103   FORMAT(F10.3,$)
104   FORMAT(F10.4,$)
105   FORMAT(F10.5,$)
106   FORMAT(F10.6,$)

      RETURN
      END

C======================================================================
C     TMKEY - Read single keystroke (non-blocking attempt)
C======================================================================
      SUBROUTINE TMKEY(KEY, VALID)
      INTEGER KEY
      INTEGER VALID
C     Read a single keystroke from terminal
C     KEY: ASCII code of key pressed
C     VALID: 1 if key read, 0 if no key available
C
C     Special handling:
C       - Arrow keys: ESC followed by A/B/C/D
C       - Returns special codes 128+ for arrows
C       - ESC alone: 27
C       - RETURN: 13
C       - BACKSPACE: 8 or 127
C
C     FORTRAN 66 compatible: Uses GO TO loop instead of recursion

C     Key state
      INTEGER LASKEY, ESCSQ
      COMMON /TMKDAT/ LASKEY, ESCSQ

      INTEGER CH
      INTEGER IOS

C     Loop to read characters (handles escape sequences)
10    CONTINUE

C     Read character using C raw terminal function
      CALL TMREADC(CH, VALID)

C     Check if character available
      IF (VALID .EQ. 0) THEN
        KEY = 0
        RETURN
      END IF

C     Check if we're in escape sequence
      IF (ESCSQ .EQ. 1) THEN
C       Second character of escape sequence
        ESCSQ = 0

C       Check for arrow keys
        IF (CH .EQ. 65) THEN
C         ESC A = Up arrow
          KEY = 128
        ELSE IF (CH .EQ. 66) THEN
C         ESC B = Down arrow
          KEY = 129
        ELSE IF (CH .EQ. 67) THEN
C         ESC C = Right arrow
          KEY = 130
        ELSE IF (CH .EQ. 68) THEN
C         ESC D = Left arrow
          KEY = 131
        ELSE
C         Unknown escape sequence, return ESC
          KEY = 27
        END IF

        RETURN
      END IF

C     Check if this starts an escape sequence
      IF (CH .EQ. 27) THEN
        ESCSQ = 1
        LASKEY = CH
C       Loop back to read next character (no recursion)
        GO TO 10
      END IF

C     Normal character
      KEY = CH

      RETURN
      END

C======================================================================
C     TMREST - Restore terminal to normal mode
C======================================================================
      SUBROUTINE TMREST
C     Restore terminal settings before exit
C     Called at program shutdown

      CALL TMRSTC

      RETURN
      END

C======================================================================
C     TMWAIT - Wait for keypress
C======================================================================
      SUBROUTINE TMWAIT(KEY)
      INTEGER KEY
C     Wait for a keypress and return it
C     Blocks until key is pressed

      INTEGER VALID

C     Loop until we get a key
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

      WRITE(6,100) BELL

100   FORMAT(A1,$)

      RETURN
      END

C======================================================================
C     TMRVON - Reverse video ON (if supported)
C======================================================================
      SUBROUTINE TMRVON
C     Enable reverse video
C     VT-52 doesn't have this, so no-op

      RETURN
      END

C======================================================================
C     TMRVOF - Reverse video OFF
C======================================================================
      SUBROUTINE TMRVOF
C     Disable reverse video
C     VT-52 doesn't have this, so no-op

      RETURN
      END

C======================================================================
C     TMFLSH - Flush output buffer
C======================================================================
      SUBROUTINE TMFLSH
C     Ensure all output is sent to terminal
C     Forces write to complete

C     CP-V specific: flush unit 6
C     In FORTRAN IV, this is typically automatic
C     but we provide the hook for platforms that need it

      RETURN
      END

C======================================================================
C     TMLINE - Draw horizontal line
C======================================================================
      SUBROUTINE TMLINE(LEN)
      INTEGER LEN
C     Draw horizontal line of dashes
C     Used for borders and separators

      INTEGER I
      INTEGER DASH

      DASH = 45

      DO 10 I = 1, LEN
        WRITE(6,100) DASH
10    CONTINUE

100   FORMAT(A1,$)

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
