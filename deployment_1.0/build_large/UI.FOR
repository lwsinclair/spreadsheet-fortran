C     UI.FOR - User Interface State Machine Module
C
C     Purpose: Manage application modes and user input
C
C     Modes:
C       1 = NAV (navigation) - Moving cursor with arrow keys
C       2 = ENTRY (data entry) - Typing formula or value
C       3 = POINT (point mode) - Selecting cells for formula
C       4 = COMMAND (command mode) - Executing slash commands
C
C     State:
C       Current mode
C       Cursor position (column, row)
C       Input buffer (for ENTRY mode)
C       Formula being built (for POINT mode)
C
C     Mode Transitions:
C       NAV -> ENTRY: User types printable character or '+'
C       NAV -> COMMAND: User types '/'
C       ENTRY -> NAV: User presses ESC or RETURN
C       ENTRY -> POINT: User types '.' in formula
C       POINT -> ENTRY: User types '.' to anchor cell
C       COMMAND -> NAV: User presses ESC or RETURN
C
C     Dependencies: STRUTIL.FOR, MSG.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C======================================================================

C======================================================================
C     UIINI - Initialize UI system
C======================================================================
      SUBROUTINE UIINI
C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

      INTEGER I

C     Initialize to NAV mode at cell A1
      CUMODE = 1
      UICOL = 1
      UIROW = 1

C     Clear input buffer
      UIBLEN = 0
      DO 10 I = 1, 80
        INBUF(I) = 32
10    CONTINUE

C     Clear formula buffer
      UIFLEN = 0
      DO 20 I = 1, 80
        UIFRM(I) = 32
20    CONTINUE

      RETURN
      END

C======================================================================
C     UIGET - Get current UI state
C======================================================================
      SUBROUTINE UIGET(MODE, COL, ROW)
      INTEGER MODE, COL, ROW

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

      MODE = CUMODE
      COL = UICOL
      ROW = UIROW

      RETURN
      END

C======================================================================
C     UIMODE - Set current mode
C======================================================================
      SUBROUTINE UIMODE(MODE)
      INTEGER MODE

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Set mode
      CUMODE = MODE

C     Clear input buffer when entering ENTRY mode
      IF (MODE .EQ. 2) UIBLEN = 0

C     Clear formula buffer when entering POINT mode
      IF (MODE .EQ. 3) UIFLEN = 0

      RETURN
      END

C======================================================================
C     UIMOVE - Move cursor position
C======================================================================
      SUBROUTINE UIMOVE(COL, ROW)
      INTEGER COL, ROW

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Bounds checking
      IF (COL .LT. 1) COL = 1
      IF (COL .GT. 26) COL = 26
      IF (ROW .LT. 1) ROW = 1
      IF (ROW .GT. 254) ROW = 254

C     Set position
      UICOL = COL
      UIROW = ROW

      RETURN
      END

C======================================================================
C     UIBUF - Input buffer operations
C======================================================================
      SUBROUTINE UIBUF(OP, CHAR)
      INTEGER OP, CHAR

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Operations:
C       1 = Add character
C       2 = Backspace (delete last)
C       3 = Clear buffer

      IF (OP .EQ. 1) GO TO 100
      IF (OP .EQ. 2) GO TO 200
      IF (OP .EQ. 3) GO TO 300
      RETURN

C     Add character
100   IF (UIBLEN .GE. 80) RETURN
      UIBLEN = UIBLEN + 1
      INBUF(UIBLEN) = CHAR
      RETURN

C     Backspace
200   IF (UIBLEN .LE. 0) RETURN
      UIBLEN = UIBLEN - 1
      RETURN

C     Clear buffer
300   UIBLEN = 0
      RETURN
      END

C======================================================================
C     UIGETB - Get input buffer
C======================================================================
      SUBROUTINE UIGETB(BUF, BLEN)
      INTEGER BUF(80), BLEN

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

      INTEGER I

      BLEN = UIBLEN

      DO 10 I = 1, BLEN
        BUF(I) = INBUF(I)
10    CONTINUE

      RETURN
      END

C======================================================================
C     UIKEY - Process keystroke (STUB for future)
C======================================================================
      SUBROUTINE UIKEY(KEY, ACTION)
      INTEGER KEY, ACTION

C     TODO: Implement keyboard event handling
C     KEY = ASCII code or special key code
C     ACTION = what to do (move cursor, add to buffer, etc.)

C     For now, just return
      ACTION = 0

      RETURN
      END

C======================================================================
C     UINAV - Handle navigation key (STUB for future)
C======================================================================
      SUBROUTINE UINAV(KEY, NCOL, NROW)
      INTEGER KEY, NCOL, NROW

C     TODO: Implement navigation (arrow keys, PgUp/Dn, Home/End)
C     Returns new position in NCOL, NROW

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     For now, just return current position
      NCOL = UICOL
      NROW = UIROW

      RETURN
      END
