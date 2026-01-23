C     XLMAIN.FOR - XL Spreadsheet Main Program
C
C     Purpose: Application entry point and main event loop
C
C     Features:
C       - Full-screen grid display
C       - Cell navigation with arrow keys
C       - Formula entry and evaluation
C       - Automatic recalculation
C       - Slash commands
C
C     Target: Multi-platform (Unix, CP-V, RSX-11M)
C
C     Usage:
C       $ RUN XL
C
C     Exit:
C       Type /QUIT and press RETURN
C
C     Author: Claude Code
C     Date: 2026-01-19
C     Updated: 2026-01-23 (Refactored - mode handlers moved to UI.FOR,
C                          commands moved to COMMANDS.FOR)
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

C     Clean shutdown via renderer
      CALL RDEND

      STOP
      END
