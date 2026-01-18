C     XLMAIN.FOR - XL Spreadsheet Main Program
C
C     Purpose: Interactive spreadsheet for CP-V with VT-52 terminal
C
C     Features:
C       - Full-screen grid display
C       - Cell navigation with arrow keys
C       - Formula entry and evaluation
C       - Automatic recalculation
C       - Slash commands (future)
C
C     Target: Xerox Sigma 7 CP-V with VT-52 CRT terminal
C
C     Usage:
C       $ RUN XL
C
C     Exit:
C       Type /QUIT and press RETURN
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C     FORTRAN IV Notes:
C       - Fixed-format source (columns 1-72)
C       - Arithmetic IF only
C       - GO TO for control flow
C       - No PARAMETER in main program (F66 limitation)
C
C======================================================================

      PROGRAM XL
C     XL Spreadsheet - Interactive calculation system

C     System state
      INTEGER MODE, COL, ROW
      INTEGER KEY, VALID
      INTEGER RUNNING

C     Welcome message
      INTEGER WELMSG(40)
      INTEGER WELLEN

C     Initialize all subsystems
      CALL CELINI
      CALL DEPINI
      CALL UIINI
      CALL DSPINI

C     Display welcome
      DATA WELMSG /88,76,32,83,112,114,101,97,100,115,
     &             104,101,101,116,32,118,49,46,48,32,
     &             45,32,67,80,45,86,47,86,84,45,53,50,
     &             32,32,32,32,32,32,32,32/
      WELLEN = 32

C     Show welcome at bottom of screen
      CALL DSPMSG(WELMSG, WELLEN)

C     Wait 2 seconds (poor man's delay - loop)
C     Note: This is platform-dependent, adjust for CP-V
      DO 5 KEY = 1, 100000
5     CONTINUE

C     Draw initial screen
      CALL DSPFUL

C     Main event loop
      RUNNING = 1

10    IF (RUNNING .EQ. 0) GO TO 999

C       Read keystroke
        CALL TMKEY(KEY, VALID)

C       Check if key available
        IF (VALID .EQ. 0) GO TO 10

C       Get current state
        CALL UIGET(MODE, COL, ROW)

C       Handle keystroke based on mode
        IF (MODE .EQ. 1) THEN
C         NAV mode
          CALL NAVKEY(KEY, RUNNING)

        ELSE IF (MODE .EQ. 2) THEN
C         ENTRY mode
          CALL ENTKEY(KEY)

        ELSE IF (MODE .EQ. 4) THEN
C         COMMAND mode
          CALL CMDKEY(KEY, RUNNING)

        END IF

C       Scroll viewport if needed
        CALL UIGET(MODE, COL, ROW)
        CALL DSPSCR(COL, ROW)

C       Refresh display
        CALL DSPFUL

C       Continue loop
        GO TO 10

999   CONTINUE

C     Clean shutdown
      CALL TMCLR
      CALL TMHOME

      STOP
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
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Check for arrow keys
      IF (KEY .EQ. 128) THEN
C       Up arrow
        IF (UIROW .GT. 1) THEN
          UIROW = UIROW - 1
        END IF

      ELSE IF (KEY .EQ. 129) THEN
C       Down arrow
        IF (UIROW .LT. 254) THEN
          UIROW = UIROW + 1
        END IF

      ELSE IF (KEY .EQ. 130) THEN
C       Right arrow
        IF (UICOL .LT. 26) THEN
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
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

      REAL VALUE
      INTEGER CTYPE
      INTEGER TOKENS(100, 4)
      INTEGER NTOK
      INTEGER DEPS(100, 2)
      INTEGER NDEPS
      INTEGER ERR

C     Check for RETURN
      IF (KEY .EQ. 13) THEN
C       Finish entry

        IF (UIBLEN .GT. 0) THEN
C         Try to parse as formula
          CALL PARSE(INBUF, UIBLEN, TOKENS, 100, NTOK,
     &               DEPS, 100, NDEPS, ERR)

          IF (ERR .EQ. 0 .AND. NTOK .GT. 0) THEN
C           Valid formula - evaluate and store
            CALL EVAL(TOKENS, NTOK, VALUE, ERR)

            IF (ERR .EQ. 0) THEN
C             Store result
              CALL CELPUT(UICOL, UIROW, 2, VALUE)

C             Store formula
              CALL CELPTK(UICOL, UIROW, INBUF, UIBLEN)

C             Add dependencies
              CALL DEPSAD(UICOL, UIROW, DEPS, NDEPS)

C             Recalculate dependents
              CALL RECALC(UICOL, UIROW)
            END IF
          ELSE
C           Try as plain number
            VALUE = ATOR(INBUF, UIBLEN)
            CALL CELPUT(UICOL, UIROW, 1, VALUE)
          END IF
        END IF

C       Return to NAV mode
        CUMODE = 1
        UIBLEN = 0

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
C     CMDKEY - Handle keystroke in COMMAND mode
C======================================================================
      SUBROUTINE CMDKEY(KEY, RUNNING)
      INTEGER KEY
      INTEGER RUNNING
C     Process keystroke in command mode
C
C     RETURN: Execute command
C     ESC: Cancel command
C     BACKSPACE: Delete character
C     Printable: Add to buffer

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN

C     Command: QUIT (Q,U,I,T = 81,85,73,84)
      INTEGER QCMD(4)
      DATA QCMD /81,85,73,84/

      INTEGER I, MATCH

C     Check for RETURN
      IF (KEY .EQ. 13) THEN
C       Execute command

C       Check for QUIT
        IF (UIBLEN .EQ. 4) THEN
          MATCH = 1
          DO 10 I = 1, 4
            IF (INBUF(I) .NE. QCMD(I)) THEN
              MATCH = 0
            END IF
10        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Quit command
            RUNNING = 0
            RETURN
          END IF
        END IF

C       Unknown command - ignore
        CUMODE = 1
        UIBLEN = 0

C     Check for ESC
      ELSE IF (KEY .EQ. 27) THEN
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
