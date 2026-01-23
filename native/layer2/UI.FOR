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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      INTEGER I

C     Initialize to NAV mode at cell A1
      CUMODE = 1
      UICOL = 1
      UIROW = 1

C     Initialize recalc mode to AUTO (1=auto, 2=manual)
      RCMODE = 1

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

C     Initialize column widths
      CALL CWINI

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Bounds checking (255 cols = A-IU, 255 rows)
      IF (COL .LT. 1) COL = 1
      IF (COL .GT. 255) COL = 255
      IF (ROW .LT. 1) ROW = 1
      IF (ROW .GT. 255) ROW = 255

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

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
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     For now, just return current position
      NCOL = UICOL
      NROW = UIROW

      RETURN
      END

C======================================================================
C     UIRCGT - Get recalculation mode
C======================================================================
      SUBROUTINE UIRCGT(MODE)
      INTEGER MODE

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Return recalc mode (1=auto, 2=manual)
      MODE = RCMODE

      RETURN
      END

C======================================================================
C     UIRCST - Set recalculation mode
C======================================================================
      SUBROUTINE UIRCST(MODE)
      INTEGER MODE

      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE

      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Set recalc mode (1=auto, 2=manual)
      IF (MODE .EQ. 1 .OR. MODE .EQ. 2) THEN
        RCMODE = MODE
      END IF

      RETURN
      END

C======================================================================
C     CWINI - Initialize column widths
C======================================================================
      SUBROUTINE CWINI
C     Column width storage (separate COMMON to minimize memory overhead)
      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

      INTEGER I

C     Initialize all columns to default width (8 characters)
      DO 10 I = 1, 255
        COLWID(I) = 8
10    CONTINUE

      RETURN
      END

C======================================================================
C     CWGET - Get column width
C======================================================================
      INTEGER FUNCTION CWGET(COL)
      INTEGER COL

      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

C     Validate column and return width
      IF (COL .GE. 1 .AND. COL .LE. 255) THEN
        CWGET = COLWID(COL)
      ELSE
        CWGET = 8
      END IF

      RETURN
      END

C======================================================================
C     CWSET - Set column width
C======================================================================
      SUBROUTINE CWSET(COL, WIDTH)
      INTEGER COL, WIDTH

      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

C     Validate column
      IF (COL .LT. 1 .OR. COL .GT. 255) RETURN

C     Validate width (3-40 characters)
      IF (WIDTH .LT. 3) RETURN
      IF (WIDTH .GT. 40) RETURN

C     Set width
      COLWID(COL) = WIDTH

      RETURN
      END

C======================================================================
C     RECALC - Recalculate from cell with UI indicator
C======================================================================
      SUBROUTINE RECALC(COL, ROW)
      INTEGER COL, ROW
C     Wrapper for RECCEL that shows calculation indicator

C     Calculation indicator state
      INTEGER CALCST
      COMMON /CALCDAT/ CALCST
      DATA CALCST /0/

C     Show calculation indicator
      CALL DSPCLC(CALCST)

C     Cycle to next indicator message
      CALCST = CALCST + 1
      IF (CALCST .GT. 5) CALCST = 0

C     Do the actual recalculation
      CALL RECCEL(COL, ROW)

      RETURN
      END

C======================================================================
C     NAVKEY - Handle keystroke in NAV mode
C======================================================================
      SUBROUTINE NAVKEY(KEY, RUNNING)
      INTEGER KEY
      INTEGER RUNNING
C     Process keystroke in navigation mode
C
C     Arrow keys: 128=Up, 129=Down, 130=Right, 131=Left
C     /: Enter COMMAND mode
C     Printable char: Enter ENTRY mode

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Check for arrow keys
      IF (KEY .EQ. 128) THEN
C       Up arrow
        IF (UIROW .GT. 1) THEN
          UIROW = UIROW - 1
        END IF

      ELSE IF (KEY .EQ. 129) THEN
C       Down arrow
        IF (UIROW .LT. 255) THEN
          UIROW = UIROW + 1
        END IF

      ELSE IF (KEY .EQ. 130) THEN
C       Right arrow
        IF (UICOL .LT. 255) THEN
          UICOL = UICOL + 1
        END IF

      ELSE IF (KEY .EQ. 131) THEN
C       Left arrow
        IF (UICOL .GT. 1) THEN
          UICOL = UICOL - 1
        END IF

C     Check for slash (command mode)
      ELSE IF (KEY .EQ. 47) THEN
C       Enter COMMAND mode
        CUMODE = 4
        UIBLEN = 0

C     Check for Delete/Backspace (clear cell in NAV mode)
      ELSE IF (KEY .EQ. 127 .OR. KEY .EQ. 8) THEN
C       Delete current cell contents
        CALL CELDEL(UICOL, UIROW)

C     Check for ? (help shortcut, 63 = '?')
      ELSE IF (KEY .EQ. 63) THEN
        CALL HLPSHW

C     Check for printable characters (start ENTRY mode)
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
C       Enter ENTRY mode
        CUMODE = 2
        UIBLEN = 1
        INBUF(1) = KEY

      END IF

      RETURN
      END

C======================================================================
C     ENTKEY - Handle keystroke in ENTRY mode
C======================================================================
      SUBROUTINE ENTKEY(KEY)
      INTEGER KEY
C     Process keystroke in entry mode
C
C     RETURN (13): Finish entry, evaluate, return to NAV
C     ESC (27): Cancel entry, return to NAV
C     BACKSPACE (8/127): Delete last character
C     Printable: Add to buffer

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      REAL VALUE
      REAL ATOR
      INTEGER CTYPE
      INTEGER TOKENS(100, 4)
      INTEGER NTOK
      INTEGER DEPS(100, 2)
      INTEGER NDEPS
      INTEGER ERR
      INTEGER I, J
      INTEGER ISNUM, CH
      INTEGER ALIGN, TXTBUF(80), TXTLEN

C     Check for RETURN (CR=13 or LF=10, platform-dependent)
      IF (KEY .EQ. 13 .OR. KEY .EQ. 10) THEN
C       Finish entry

        IF (UIBLEN .GT. 0) THEN
C         Check for label prefix (Lotus 1-2-3 convention)
C         ' (39) = left align, " (34) = right align, ^ (94) = center
          ALIGN = 0
          IF (INBUF(1) .EQ. 39) THEN
C           Single quote = left align
            ALIGN = 1
          ELSE IF (INBUF(1) .EQ. 34) THEN
C           Double quote = right align
            ALIGN = 2
          ELSE IF (INBUF(1) .EQ. 94) THEN
C           Caret = center align
            ALIGN = 3
          END IF

C         If label prefix found, store as text cell
          IF (ALIGN .GT. 0) THEN
C           Copy text without the prefix
            TXTLEN = UIBLEN - 1
            IF (TXTLEN .GT. 0) THEN
              DO 3 J = 1, TXTLEN
                TXTBUF(J) = INBUF(J + 1)
3             CONTINUE
              CALL CELTXT(UICOL, UIROW, TXTBUF, TXTLEN, ALIGN)
            END IF
            GO TO 18
          END IF

C         Check if input is all whitespace - if so, clear cell
          ISNUM = 1
          DO 4 I = 1, UIBLEN
            IF (INBUF(I) .NE. 32) ISNUM = 0
4         CONTINUE
          IF (ISNUM .EQ. 1) THEN
C           All spaces - delete cell contents
            CALL CELDEL(UICOL, UIROW)
            GO TO 18
          END IF

C         Check if input is a plain number (digits, decimal, sign only)
          ISNUM = 1
          DO 5 I = 1, UIBLEN
            CH = INBUF(I)
            IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 5
            IF (CH .EQ. 46) GO TO 5
            IF (CH .EQ. 45) GO TO 5
            IF (CH .EQ. 43) GO TO 5
            IF (CH .EQ. 32) GO TO 5
            ISNUM = 0
5         CONTINUE

C         If plain number, use ATOR directly
          IF (ISNUM .EQ. 1) THEN
            VALUE = ATOR(INBUF, UIBLEN)
            CALL CELPUT(UICOL, UIROW, 1, VALUE)
            GO TO 18
          END IF

C         Try to parse as formula
          CALL PARSE(INBUF, UIBLEN, TOKENS, NTOK, ERR)

          IF (ERR .EQ. 0 .AND. NTOK .GT. 0) THEN
C           Extract dependencies from tokens (type 2 = cell references)
            NDEPS = 0
            DO 15 I = 1, NTOK
              IF (TOKENS(I, 1) .EQ. 2) THEN
C               Cell reference - add to dependencies
                NDEPS = NDEPS + 1
                DEPS(NDEPS, 1) = TOKENS(I, 3)
                DEPS(NDEPS, 2) = TOKENS(I, 4)
              END IF
15          CONTINUE

C           Valid formula - evaluate and store
            CALL EVAL(TOKENS, NTOK, VALUE, ERR)

            IF (ERR .EQ. 0) THEN
C             Store result
              CALL CELPUT(UICOL, UIROW, 2, VALUE)

C             Store formula string (for edit line display)
              CALL CELPFS(UICOL, UIROW, INBUF, UIBLEN)

C             Add dependencies
              CALL DEPSAD(UICOL, UIROW, DEPS, NDEPS)

C             Recalculate dependents (only if AUTO mode)
              CALL UIRCGT(CTYPE)
              IF (CTYPE .EQ. 1) THEN
                CALL RECALC(UICOL, UIROW)
              END IF
            ELSE
C             EVAL failed - treat as left-aligned text
              CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
            END IF
          ELSE
C           Not a number or formula - treat as left-aligned text
C           (Lotus 1-2-3 default behavior for plain text)
            CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
          END IF
        END IF

C       Done processing input
18      CONTINUE

C       Return to NAV mode and move cursor DOWN (Enter = down)
        CUMODE = 1
        UIBLEN = 0
        IF (UIROW .LT. 255) THEN
          UIROW = UIROW + 1
        END IF

C     Check for TAB (move RIGHT after entry)
      ELSE IF (KEY .EQ. 9) THEN
C       Store value and move right (same logic as RETURN)
        IF (UIBLEN .GT. 0) THEN
C         Check for label prefix
          ALIGN = 0
          IF (INBUF(1) .EQ. 39) THEN
            ALIGN = 1
          ELSE IF (INBUF(1) .EQ. 34) THEN
            ALIGN = 2
          ELSE IF (INBUF(1) .EQ. 94) THEN
            ALIGN = 3
          END IF

          IF (ALIGN .GT. 0) THEN
C           Label with prefix - store text without prefix
            TXTLEN = UIBLEN - 1
            IF (TXTLEN .GT. 0) THEN
              DO 23 J = 1, TXTLEN
                TXTBUF(J) = INBUF(J + 1)
23            CONTINUE
              CALL CELTXT(UICOL, UIROW, TXTBUF, TXTLEN, ALIGN)
            END IF
          ELSE
C           Check if all whitespace
            ISNUM = 1
            DO 24 I = 1, UIBLEN
              IF (INBUF(I) .NE. 32) ISNUM = 0
24          CONTINUE
            IF (ISNUM .EQ. 1) THEN
              CALL CELDEL(UICOL, UIROW)
            ELSE
C             Check if plain number
              ISNUM = 1
              DO 25 I = 1, UIBLEN
                CH = INBUF(I)
                IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 25
                IF (CH .EQ. 46) GO TO 25
                IF (CH .EQ. 45) GO TO 25
                IF (CH .EQ. 43) GO TO 25
                IF (CH .EQ. 32) GO TO 25
                ISNUM = 0
25            CONTINUE

              IF (ISNUM .EQ. 1) THEN
C               Plain number
                VALUE = ATOR(INBUF, UIBLEN)
                CALL CELPUT(UICOL, UIROW, 1, VALUE)
              ELSE
C               Not a number - store as left-aligned text
                CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
              END IF
            END IF
          END IF
        END IF
        CUMODE = 1
        UIBLEN = 0
        IF (UICOL .LT. 255) THEN
          UICOL = UICOL + 1
        END IF

C     Check for ESC
      ELSE IF (KEY .EQ. 27) THEN
C       Cancel entry
        CUMODE = 1
        UIBLEN = 0

C     Check for BACKSPACE
      ELSE IF (KEY .EQ. 8 .OR. KEY .EQ. 127) THEN
        IF (UIBLEN .GT. 0) THEN
          UIBLEN = UIBLEN - 1
        END IF

C     Check for printable characters
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
        IF (UIBLEN .LT. 80) THEN
          UIBLEN = UIBLEN + 1
          INBUF(UIBLEN) = KEY
        END IF

      END IF

      RETURN
      END

C======================================================================
C     HLPSHW - Display help screen
C======================================================================
      SUBROUTINE HLPSHW
C     Display help, wait for key, redraw spreadsheet
C     Delegates to layer 3 renderer for device-specific help

C     Show help via renderer (layer 3)
      CALL RDHLP

C     Redraw spreadsheet after help
      CALL DSPFUL

      RETURN
      END

C======================================================================
C     BLOCK DATA - Initialize column widths
C======================================================================
      BLOCK DATA CWIND
      INTEGER COLWID(255)
      COMMON /CWDAT/ COLWID

      DATA COLWID /255*8/

      END
