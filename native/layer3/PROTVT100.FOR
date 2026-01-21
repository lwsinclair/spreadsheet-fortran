C     PROTVT100.FOR - VT-100/ANSI Terminal Protocol
C
C     Purpose: Define ANSI escape sequences for VT-100 terminals
C
C     This module builds escape sequence byte arrays but does NOT
C     perform any I/O. The I/O primitives (IOPUTC, etc.) are provided
C     by a separate platform-specific module.
C
C     VT-100/ANSI Escape Sequences:
C       ESC [ H         - Home cursor
C       ESC [ 2 J       - Clear entire screen
C       ESC [ K         - Clear to end of line
C       ESC [ row ; col H - Position cursor
C       ESC [ A/B/C/D   - Cursor up/down/right/left
C       ESC [ 7 m       - Reverse video on
C       ESC [ 0 m       - Normal video
C
C     Arrow Key Input (returns these after ESC [):
C       A = Up, B = Down, C = Right, D = Left
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     PRCLR - Output clear screen sequence
C======================================================================
      SUBROUTINE PRCLR
C     Clear screen: ESC [ H ESC [ 2 J

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(72)
      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(50)
      CALL IOPUTC(74)

      RETURN
      END

C======================================================================
C     PRHOME - Output home cursor sequence
C======================================================================
      SUBROUTINE PRHOME
C     Home cursor: ESC [ H

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(72)

      RETURN
      END

C======================================================================
C     PRCURS - Output cursor positioning sequence
C======================================================================
      SUBROUTINE PRCURS(ROW, COL)
      INTEGER ROW, COL
C     Position cursor: ESC [ row ; col H
C     Row and col are 1-based

      INTEGER R, C

      R = ROW
      C = COL

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL PRNUM(R)
      CALL IOPUTC(59)
      CALL PRNUM(C)
      CALL IOPUTC(72)

      RETURN
      END

C======================================================================
C     PRNUM - Output integer as ASCII digits
C======================================================================
      SUBROUTINE PRNUM(NUM)
      INTEGER NUM
C     Output number as decimal ASCII digits

      INTEGER N, D, STARTED

      N = NUM
      IF (N .LT. 0) N = 0
      IF (N .GT. 999) N = 999

C     Handle zero
      IF (N .EQ. 0) THEN
        CALL IOPUTC(48)
        RETURN
      END IF

C     Output digits
      STARTED = 0

      IF (N .GE. 100) THEN
        D = N / 100
        CALL IOPUTC(48 + D)
        N = N - D * 100
        STARTED = 1
      END IF

      IF (N .GE. 10 .OR. STARTED .EQ. 1) THEN
        D = N / 10
        CALL IOPUTC(48 + D)
        N = N - D * 10
      END IF

      CALL IOPUTC(48 + N)

      RETURN
      END

C======================================================================
C     PRCEOL - Output clear to end of line sequence
C======================================================================
      SUBROUTINE PRCEOL
C     Clear to EOL: ESC [ K

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(75)

      RETURN
      END

C======================================================================
C     PRCUP - Output cursor up sequence
C======================================================================
      SUBROUTINE PRCUP
C     Cursor up: ESC [ A

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(65)

      RETURN
      END

C======================================================================
C     PRCDN - Output cursor down sequence
C======================================================================
      SUBROUTINE PRCDN
C     Cursor down: ESC [ B

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(66)

      RETURN
      END

C======================================================================
C     PRCRT - Output cursor right sequence
C======================================================================
      SUBROUTINE PRCRT
C     Cursor right: ESC [ C

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(67)

      RETURN
      END

C======================================================================
C     PRCLFT - Output cursor left sequence
C======================================================================
      SUBROUTINE PRCLFT
C     Cursor left: ESC [ D

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(68)

      RETURN
      END

C======================================================================
C     PRRVON - Output reverse video on sequence
C======================================================================
      SUBROUTINE PRRVON
C     Reverse video: ESC [ 7 m

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(55)
      CALL IOPUTC(109)

      RETURN
      END

C======================================================================
C     PRRVOF - Output reverse video off sequence
C======================================================================
      SUBROUTINE PRRVOF
C     Normal video: ESC [ 0 m

      CALL IOPUTC(27)
      CALL IOPUTC(91)
      CALL IOPUTC(48)
      CALL IOPUTC(109)

      RETURN
      END

C======================================================================
C     PRBELL - Output bell character
C======================================================================
      SUBROUTINE PRBELL
C     Bell: ASCII 7

      CALL IOPUTC(7)

      RETURN
      END

C======================================================================
C     PRKEY - Translate input character to key code
C======================================================================
      SUBROUTINE PRKEY(CH, ESTATE, KEY, DONE)
      INTEGER CH, ESTATE, KEY, DONE
C     Process input character through escape sequence state machine
C
C     Input:
C       CH     - character just read
C       ESTATE - current escape state (0=normal, 1=got ESC, 2=got ESC[,
C                3=got ESC[1, 4=got ESC[1;, 5=got ESC[1;5)
C
C     Output:
C       KEY    - translated key code
C                Arrow: 128=up, 129=down, 130=right, 131=left
C                Ctrl+Arrow: 132=up, 133=down, 134=right, 135=left
C       DONE   - 1 if key is complete, 0 if need more input
C       ESTATE - updated state
C
C     ANSI arrow keys: ESC [ A/B/C/D
C     Ctrl+Arrow keys: ESC [ 1 ; 5 A/B/C/D

      DONE = 0
      KEY = 0

      IF (ESTATE .EQ. 5) THEN
C       Got ESC [ 1 ; 5, expecting A/B/C/D for Ctrl+Arrow
        ESTATE = 0
        DONE = 1

        IF (CH .EQ. 65) THEN
          KEY = 132
        ELSE IF (CH .EQ. 66) THEN
          KEY = 133
        ELSE IF (CH .EQ. 67) THEN
          KEY = 134
        ELSE IF (CH .EQ. 68) THEN
          KEY = 135
        ELSE
          KEY = CH
        END IF

      ELSE IF (ESTATE .EQ. 4) THEN
C       Got ESC [ 1 ;, expecting 5
        IF (CH .EQ. 53) THEN
          ESTATE = 5
        ELSE
          ESTATE = 0
          KEY = CH
          DONE = 1
        END IF

      ELSE IF (ESTATE .EQ. 3) THEN
C       Got ESC [ 1, expecting ;
        IF (CH .EQ. 59) THEN
          ESTATE = 4
        ELSE
          ESTATE = 0
          KEY = CH
          DONE = 1
        END IF

      ELSE IF (ESTATE .EQ. 2) THEN
C       Got ESC [, expecting A/B/C/D or 1 (for Ctrl+Arrow)
        IF (CH .EQ. 65) THEN
          ESTATE = 0
          DONE = 1
          KEY = 128
        ELSE IF (CH .EQ. 66) THEN
          ESTATE = 0
          DONE = 1
          KEY = 129
        ELSE IF (CH .EQ. 67) THEN
          ESTATE = 0
          DONE = 1
          KEY = 130
        ELSE IF (CH .EQ. 68) THEN
          ESTATE = 0
          DONE = 1
          KEY = 131
        ELSE IF (CH .EQ. 49) THEN
C         Got '1', might be Ctrl+Arrow sequence
          ESTATE = 3
        ELSE
          ESTATE = 0
          KEY = CH
          DONE = 1
        END IF

      ELSE IF (ESTATE .EQ. 1) THEN
C       Got ESC, expecting [
        IF (CH .EQ. 91) THEN
          ESTATE = 2
        ELSE
          ESTATE = 0
          KEY = 27
          DONE = 1
        END IF

      ELSE
C       Normal state
        IF (CH .EQ. 27) THEN
          ESTATE = 1
        ELSE
          KEY = CH
          DONE = 1
        END IF

      END IF

      RETURN
      END

C======================================================================
C     PRRSET - Reset escape sequence state
C======================================================================
      SUBROUTINE PRRSET(ESTATE)
      INTEGER ESTATE
C     Reset escape state machine (call on timeout)

      ESTATE = 0

      RETURN
      END
