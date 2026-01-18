C     COMMANDS.FOR - Command Handler Module
C
C     Purpose: Parse and execute slash commands
C
C     Commands:
C       /B   - Blank cell (clear contents)
C       /E   - Edit cell formula
C       /BR  - Blank range
C       /C   - Copy cell/range
C       /M   - Move cell/range
C       /F   - Format cell (number of decimals)
C       /W   - Width of column
C       /R   - Recalc mode (auto/manual)
C       /!   - Force recalculation
C       /H   - Help
C       /Q   - Quit
C
C     Command Codes:
C       0 = Invalid command
C       1 = Blank (/B)
C       2 = Edit (/E)
C       3 = Blank range (/BR)
C       4 = Copy (/C)
C       5 = Move (/M)
C       6 = Format (/F)
C       7 = Width (/W)
C       8 = Help (/H)
C       9 = Quit (/Q)
C      10 = Recalc mode (/R)
C      11 = Force recalc (/!)
C
C     Dependencies: STRUTIL.FOR, MSG.FOR, CELLS.FOR, RECALC.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C======================================================================

C======================================================================
C     CMDINI - Initialize command system
C======================================================================
      SUBROUTINE CMDINI
C     Initialize command handler

C     Nothing to initialize for now
      RETURN
      END

C======================================================================
C     CMDPRS - Parse command string
C======================================================================
      SUBROUTINE CMDPRS(CMD, CMDLEN, CODE)
      INTEGER CMD(10)
      INTEGER CMDLEN
      INTEGER CODE

C     Parse command and return command code
C     CMD = command string (ASCII codes)
C     CMDLEN = length of command
C     CODE = command code (0 if invalid)

C     Check for empty command
      IF (CMDLEN .LE. 0) GO TO 900

C     Check first character
      IF (CMD(1) .EQ. 66) GO TO 100   ! 'B'
      IF (CMD(1) .EQ. 69) GO TO 200   ! 'E'
      IF (CMD(1) .EQ. 67) GO TO 400   ! 'C'
      IF (CMD(1) .EQ. 77) GO TO 500   ! 'M'
      IF (CMD(1) .EQ. 70) GO TO 600   ! 'F'
      IF (CMD(1) .EQ. 87) GO TO 700   ! 'W'
      IF (CMD(1) .EQ. 72) GO TO 800   ! 'H'
      IF (CMD(1) .EQ. 81) GO TO 900   ! 'Q' - actually should be 9
      IF (CMD(1) .EQ. 82) GO TO 1000  ! 'R'
      IF (CMD(1) .EQ. 33) GO TO 1100  ! '!'
      GO TO 900

C     /B - Blank
100   IF (CMDLEN .EQ. 1) GO TO 110
C     Check for /BR (blank range)
      IF (CMDLEN .NE. 2) GO TO 900
      IF (CMD(2) .EQ. 82) GO TO 300  ! 'R'
      GO TO 900

110   CODE = 1
      RETURN

C     /E - Edit
200   CODE = 2
      RETURN

C     /BR - Blank range
300   CODE = 3
      RETURN

C     /C - Copy
400   CODE = 4
      RETURN

C     /M - Move
500   CODE = 5
      RETURN

C     /F - Format
600   CODE = 6
      RETURN

C     /W - Width
700   CODE = 7
      RETURN

C     /H - Help
800   CODE = 8
      RETURN

C     /Q - Quit (but return 9 for code)
C900   CODE = 9
C      RETURN

C     /R - Recalc mode
1000  CODE = 10
      RETURN

C     /! - Force recalc
1100  CODE = 11
      RETURN

C     Invalid command
900   CODE = 0
      RETURN
      END

C======================================================================
C     CMDEXC - Execute command (STUB for future)
C======================================================================
      SUBROUTINE CMDEXC(CODE, STATUS)
      INTEGER CODE, STATUS

C     Execute command by code
C     CODE = command code
C     STATUS = 0 if success, error code if failure

C     TODO: Implement command execution
C       - /B: Call CELDEL to delete cell
C       - /E: Load cell formula into edit buffer
C       - /C: Copy cell to destination
C       - /M: Move cell to destination
C       - /H: Display help message
C       - etc.

      STATUS = 0
      RETURN
      END

C======================================================================
C     CMDBLN - Execute blank command (STUB for future)
C======================================================================
      SUBROUTINE CMDBLN(COL, ROW, STATUS)
      INTEGER COL, ROW, STATUS

C     Blank cell at COL, ROW

C     TODO: Call CELDEL(COL, ROW)
C     TODO: Call RECCEL to update dependents

      STATUS = 0
      RETURN
      END

C======================================================================
C     CMDEDT - Execute edit command (STUB for future)
C======================================================================
      SUBROUTINE CMDEDT(COL, ROW, BUF, BLEN)
      INTEGER COL, ROW
      INTEGER BUF(80), BLEN

C     Load cell formula into edit buffer

C     TODO: Get cell type
C     TODO: If formula, get formula string
C     TODO: If value, convert to string
C     TODO: Copy to BUF, set BLEN

      BLEN = 0
      RETURN
      END

C======================================================================
C     CMDCPY - Execute copy command (STUB for future)
C======================================================================
      SUBROUTINE CMDCPY(SC, SR, DC, DR, STATUS)
      INTEGER SC, SR, DC, DR, STATUS

C     Copy cell from SC,SR to DC,DR

C     TODO: Get source cell type and value
C     TODO: Adjust relative references
C     TODO: Store to destination
C     TODO: Recalculate dependents

      STATUS = 0
      RETURN
      END
