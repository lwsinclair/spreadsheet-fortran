C     PROTVT52.FOR - VT-52 Terminal Protocol
C
C     Purpose: Define escape sequences for VT-52 terminals
C
C     This module builds escape sequence byte arrays but does NOT
C     perform any I/O. The I/O primitives (IOPUTC, etc.) are provided
C     by a separate platform-specific module.
C
C     VT-52 Escape Sequences:
C       ESC H           - Home cursor
C       ESC J           - Clear to end of screen
C       ESC K           - Clear to end of line
C       ESC Y row col   - Position cursor (row/col + 32)
C       ESC A/B/C/D     - Cursor up/down/right/left
C
C     Note: VT-52 has no reverse video - we skip it or use other means
C
C     Arrow Key Input (returns these after ESC):
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
C     Clear screen: ESC H (home) + ESC J (clear to end)

      CALL IOPUTC(27)
      CALL IOPUTC(72)
      CALL IOPUTC(27)
      CALL IOPUTC(74)

      RETURN
      END

C======================================================================
C     PRHOME - Output home cursor sequence
C======================================================================
      SUBROUTINE PRHOME
C     Home cursor: ESC H

      CALL IOPUTC(27)
      CALL IOPUTC(72)

      RETURN
      END

C======================================================================
C     PRCURS - Output cursor positioning sequence
C======================================================================
      SUBROUTINE PRCURS(ROW, COL)
      INTEGER ROW, COL
C     Position cursor: ESC Y row+32 col+32
C     Row and col are 1-based

      INTEGER R, C

      R = ROW + 31
      C = COL + 31

      IF (R .LT. 32) R = 32
      IF (R .GT. 127) R = 127
      IF (C .LT. 32) C = 32
      IF (C .GT. 127) C = 127

      CALL IOPUTC(27)
      CALL IOPUTC(89)
      CALL IOPUTC(R)
      CALL IOPUTC(C)

      RETURN
      END

C======================================================================
C     PRCEOL - Output clear to end of line sequence
C======================================================================
      SUBROUTINE PRCEOL
C     Clear to EOL: ESC K

      CALL IOPUTC(27)
      CALL IOPUTC(75)

      RETURN
      END

C======================================================================
C     PRCUP - Output cursor up sequence
C======================================================================
      SUBROUTINE PRCUP
C     Cursor up: ESC A

      CALL IOPUTC(27)
      CALL IOPUTC(65)

      RETURN
      END

C======================================================================
C     PRCDN - Output cursor down sequence
C======================================================================
      SUBROUTINE PRCDN
C     Cursor down: ESC B

      CALL IOPUTC(27)
      CALL IOPUTC(66)

      RETURN
      END

C======================================================================
C     PRCRT - Output cursor right sequence
C======================================================================
      SUBROUTINE PRCRT
C     Cursor right: ESC C

      CALL IOPUTC(27)
      CALL IOPUTC(67)

      RETURN
      END

C======================================================================
C     PRCLFT - Output cursor left sequence
C======================================================================
      SUBROUTINE PRCLFT
C     Cursor left: ESC D

      CALL IOPUTC(27)
      CALL IOPUTC(68)

      RETURN
      END

C======================================================================
C     PRRVON - Output reverse video on sequence
C======================================================================
      SUBROUTINE PRRVON
C     VT-52 has no standard reverse video
C     Some terminals support ESC p for reverse, ESC q for normal
C     We try it but it may not work on all VT-52s

      CALL IOPUTC(27)
      CALL IOPUTC(112)

      RETURN
      END

C======================================================================
C     PRRVOF - Output reverse video off sequence
C======================================================================
      SUBROUTINE PRRVOF
C     VT-52 reverse off: ESC q

      CALL IOPUTC(27)
      CALL IOPUTC(113)

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
C       ESTATE - current escape state (0=normal, 1=got ESC)
C
C     Output:
C       KEY    - translated key code (128=up, 129=down, 130=right, 131=left)
C       DONE   - 1 if key is complete, 0 if need more input
C       ESTATE - updated state
C
C     VT-52 arrow keys: ESC A/B/C/D (no '[' like ANSI)

      DONE = 0
      KEY = 0

      IF (ESTATE .EQ. 1) THEN
C       Got ESC, expecting A/B/C/D
        ESTATE = 0
        DONE = 1

        IF (CH .EQ. 65) THEN
          KEY = 128
        ELSE IF (CH .EQ. 66) THEN
          KEY = 129
        ELSE IF (CH .EQ. 67) THEN
          KEY = 130
        ELSE IF (CH .EQ. 68) THEN
          KEY = 131
        ELSE
          KEY = 27
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
