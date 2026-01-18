C     TESTTERM.FOR - Terminal Driver Test Program for RSX-11M
C
C     Purpose: Validate TERMRSX.FOR terminal driver functionality
C
C     Tests:
C       1. Screen clear and home
C       2. Cursor positioning
C       3. Character output
C       4. String output
C       5. Integer output
C       6. Real output
C       7. Line drawing
C       8. Cursor movement
C       9. Clear to end of line
C      10. Keyboard input
C      11. Arrow key detection
C      12. Bell
C
C     Usage: RUN TESTTERM
C
C     Expected behavior:
C       - Screen clears
C       - Various test patterns display
C       - Prompts for keyboard input
C       - Arrow keys recognized
C       - Clean exit on ESC
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C======================================================================

      PROGRAM TESTTERM
      INTEGER KEY, VALID
      INTEGER STR(20)
      INTEGER I
      REAL RNUM

C     Test 1: Initialize and clear screen
      CALL TMINIT
      CALL PAUSE(1)

C     Test 2: Cursor positioning
      CALL TMCURS(1, 1)
      CALL TMPUTS('Terminal Driver Test - RSX-11M', 31)

C     Test 3: Draw border
      CALL TMCURS(2, 1)
      CALL TMLINE(80)

C     Test 4: Character output
      CALL TMCURS(4, 1)
      CALL TMPUTS('Test 1: Character output... ', 29)
      DO 10 I = 65, 90
        CALL TMPUTC(I)
10    CONTINUE
      CALL TMPUTS(' OK', 3)

C     Test 5: Integer output
      CALL TMCURS(5, 1)
      CALL TMPUTS('Test 2: Integer output... ', 27)
      CALL TMPUTN(12345)
      CALL TMPUTS(' OK', 3)

C     Test 6: Real output
      CALL TMCURS(6, 1)
      CALL TMPUTS('Test 3: Real output... ', 24)
      RNUM = 123.456
      CALL TMPUTR(RNUM, 2)
      CALL TMPUTS(' OK', 3)

C     Test 7: Cursor movement
      CALL TMCURS(8, 1)
      CALL TMPUTS('Test 4: Cursor movement...', 27)
      CALL TMCURS(9, 5)
      CALL TMPUTS('Right', 5)
      CALL TMCRT
      CALL TMCRT
      CALL TMPUTS('Up', 2)
      CALL TMCUP
      CALL TMPUTS('Down', 4)
      CALL TMCDN
      CALL TMCDN
      CALL TMPUTS('Left', 4)
      CALL TMCLFT
      CALL TMCLFT
      CALL TMPUTS(' OK', 3)

C     Test 8: Clear to end of line
      CALL TMCURS(11, 1)
      CALL TMPUTS('Test 5: Clear EOL... XXXXXXXXXXXXX', 35)
      CALL TMCURS(11, 22)
      CALL TMCEOL
      CALL TMPUTS('OK', 2)

C     Test 9: Line drawing
      CALL TMCURS(13, 1)
      CALL TMPUTS('Test 6: Line drawing... ', 25)
      CALL TMLINE(40)
      CALL TMPUTS(' OK', 3)

C     Test 10: Bell
      CALL TMCURS(15, 1)
      CALL TMPUTS('Test 7: Bell... ', 17)
      CALL TMBELL
      CALL PAUSE(1)
      CALL TMPUTS('OK', 2)

C     Test 11: Keyboard input
      CALL TMCURS(17, 1)
      CALL TMPUTS('Test 8: Keyboard input', 23)
      CALL TMCURS(18, 1)
      CALL TMPUTS('Press arrow keys (ESC to quit):', 32)

C     Keyboard test loop
      CALL TMCURS(20, 1)
100   CALL TMKEY(KEY, VALID)

      IF (VALID .EQ. 1) THEN
        IF (KEY .EQ. 27) THEN
C         ESC pressed - exit
          GO TO 200
        ELSE IF (KEY .EQ. 128) THEN
C         Up arrow
          CALL TMPUTS('UP   ', 5)
        ELSE IF (KEY .EQ. 129) THEN
C         Down arrow
          CALL TMPUTS('DOWN ', 5)
        ELSE IF (KEY .EQ. 130) THEN
C         Right arrow
          CALL TMPUTS('RIGHT', 5)
        ELSE IF (KEY .EQ. 131) THEN
C         Left arrow
          CALL TMPUTS('LEFT ', 5)
        ELSE IF (KEY .EQ. 13) THEN
C         Return
          CALL TMPUTS('ENTER', 5)
        ELSE
C         Other key
          CALL TMPUTS('KEY=', 4)
          CALL TMPUTN(KEY)
          CALL TMPUTS(' ', 1)
        END IF
        CALL TMCURS(20, 1)
      END IF

      GO TO 100

C     Test complete
200   CALL TMCURS(22, 1)
      CALL TMLINE(80)
      CALL TMCURS(23, 1)
      CALL TMPUTS('All tests complete! Terminal driver OK.', 40)
      CALL TMCURS(24, 1)

      STOP
      END

C======================================================================
C     PAUSE - Delay for visual verification
C======================================================================
      SUBROUTINE PAUSE(SECS)
      INTEGER SECS
C     Simple delay loop (not accurate, just for visual pause)

      INTEGER I, J
      INTEGER LOOPS

      LOOPS = SECS * 100000

      DO 10 I = 1, LOOPS
        J = I + 1
10    CONTINUE

      RETURN
      END
