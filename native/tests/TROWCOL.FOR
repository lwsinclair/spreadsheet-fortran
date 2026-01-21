C     TROWCOL.FOR - Tests for Row/Column Insert/Delete
C
C     Purpose: TDD tests for row and column operations
C
C     Test cases:
C       1. Insert row shifts cells down
C       2. Insert row updates formula references
C       3. Delete row shifts cells up
C       4. Delete row updates formula references
C       5. Insert column shifts cells right
C       6. Insert column updates formula references
C       7. Delete column shifts cells left
C       8. Delete column updates formula references
C       9. Absolute references are NOT shifted
C      10. References to deleted row/col become invalid
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

C======================================================================
C     TROWCL - Run all row/column tests
C======================================================================
      SUBROUTINE TROWCL

      CALL TSTGRP('Row Insert Tests')
      CALL TRINS1
      CALL TRINS2
      CALL TRINS3

      CALL TSTGRP('Row Delete Tests')
      CALL TRDEL1
      CALL TRDEL2
      CALL TRDEL3

      CALL TSTGRP('Column Insert Tests')
      CALL TCINS1
      CALL TCINS2
      CALL TCINS3

      CALL TSTGRP('Column Delete Tests')
      CALL TCDEL1
      CALL TCDEL2
      CALL TCDEL3

      CALL TSTGRP('Absolute Reference Tests')
      CALL TABSR1
      CALL TABSR2

      RETURN
      END

C======================================================================
C     TRINS1 - Insert row: cells shift down
C======================================================================
      SUBROUTINE TRINS1
C     Test: Insert row at row 2 should shift cell at A2 to A3

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI
      CALL CWINI

C     Put value 100 at A2 (col=1, row=2)
      CALL CELPUT(1, 2, 1, 100.0)

C     Verify it's at A2
      CALL CELGET(1, 2, CTYPE, VALUE)
      CALL TSTEQ('A2 exists before insert', 1, CTYPE)
      CALL TSTREQ('A2 value before insert', 100.0, VALUE)

C     Insert row at row 2
      CALL ROWINS(2)

C     A2 should now be empty (new row)
      CALL CELGET(1, 2, CTYPE, VALUE)
      CALL TSTEQ('A2 empty after insert', 0, CTYPE)

C     Original A2 should now be at A3
      CALL CELGET(1, 3, CTYPE, VALUE)
      CALL TSTEQ('A3 exists after insert', 1, CTYPE)
      CALL TSTREQ('A3 value after insert', 100.0, VALUE)

      RETURN
      END

C======================================================================
C     TRINS2 - Insert row: cells above stay in place
C======================================================================
      SUBROUTINE TRINS2
C     Test: Insert row at row 3 should NOT move cell at A1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value 50 at A1 (col=1, row=1)
      CALL CELPUT(1, 1, 1, 50.0)

C     Insert row at row 3
      CALL ROWINS(3)

C     A1 should still have 50
      CALL CELGET(1, 1, CTYPE, VALUE)
      CALL TSTEQ('A1 type unchanged', 1, CTYPE)
      CALL TSTREQ('A1 value unchanged', 50.0, VALUE)

      RETURN
      END

C======================================================================
C     TRINS3 - Insert row: formula references update
C======================================================================
      SUBROUTINE TRINS3
C     Test: Insert row updates formula references
C     If A1 has =B2, and we insert row at 2, A1 should become =B3

      INTEGER FSTR(80), FLEN
      INTEGER TOKENS(100, 4), NTOK, ERR
      REAL VALUE

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put value 200 at B2
      CALL CELPUT(2, 2, 1, 200.0)

C     Put formula =B2 at A1
C     Formula string: "B2" (ASCII: 66, 50)
      FSTR(1) = 66
      FSTR(2) = 50
      FLEN = 2
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 200.0)

C     Insert row at row 2
      CALL ROWINS(2)

C     Get formula string from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should now be "B3" (ASCII: 66, 51)
      CALL TSTEQ('Formula length after insert', 2, FLEN)
      CALL TSTEQ('Formula col char', 66, FSTR(1))
      CALL TSTEQ('Formula row char', 51, FSTR(2))

      RETURN
      END

C======================================================================
C     TRDEL1 - Delete row: cells shift up
C======================================================================
      SUBROUTINE TRDEL1
C     Test: Delete row 2 should shift cell at A3 to A2

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value 300 at A3
      CALL CELPUT(1, 3, 1, 300.0)

C     Put value 999 at A2 (will be deleted)
      CALL CELPUT(1, 2, 1, 999.0)

C     Delete row 2
      CALL ROWDEL(2)

C     A3 should now be empty
      CALL CELGET(1, 3, CTYPE, VALUE)
      CALL TSTEQ('A3 empty after delete', 0, CTYPE)

C     Original A3 should now be at A2
      CALL CELGET(1, 2, CTYPE, VALUE)
      CALL TSTEQ('A2 exists after delete', 1, CTYPE)
      CALL TSTREQ('A2 value after delete', 300.0, VALUE)

      RETURN
      END

C======================================================================
C     TRDEL2 - Delete row: cells above stay in place
C======================================================================
      SUBROUTINE TRDEL2
C     Test: Delete row 3 should NOT move cell at A1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value at A1
      CALL CELPUT(1, 1, 1, 75.0)

C     Put value at A3 (will be deleted)
      CALL CELPUT(1, 3, 1, 999.0)

C     Delete row 3
      CALL ROWDEL(3)

C     A1 should be unchanged
      CALL CELGET(1, 1, CTYPE, VALUE)
      CALL TSTEQ('A1 type unchanged', 1, CTYPE)
      CALL TSTREQ('A1 value unchanged', 75.0, VALUE)

      RETURN
      END

C======================================================================
C     TRDEL3 - Delete row: formula references update
C======================================================================
      SUBROUTINE TRDEL3
C     Test: If A1 has =B3, and we delete row 2, A1 should become =B2

      INTEGER FSTR(80), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put formula =B3 at A1
      FSTR(1) = 66
      FSTR(2) = 51
      FLEN = 2
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 0.0)

C     Delete row 2
      CALL ROWDEL(2)

C     Get formula from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should now be "B2"
      CALL TSTEQ('Formula length after delete', 2, FLEN)
      CALL TSTEQ('Formula col char', 66, FSTR(1))
      CALL TSTEQ('Formula row char', 50, FSTR(2))

      RETURN
      END

C======================================================================
C     TCINS1 - Insert column: cells shift right
C======================================================================
      SUBROUTINE TCINS1
C     Test: Insert column at B should shift cell at B1 to C1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value 400 at B1 (col=2, row=1)
      CALL CELPUT(2, 1, 1, 400.0)

C     Insert column at col 2 (B)
      CALL COLINS(2)

C     B1 should now be empty
      CALL CELGET(2, 1, CTYPE, VALUE)
      CALL TSTEQ('B1 empty after insert', 0, CTYPE)

C     Original B1 should now be at C1
      CALL CELGET(3, 1, CTYPE, VALUE)
      CALL TSTEQ('C1 exists after insert', 1, CTYPE)
      CALL TSTREQ('C1 value after insert', 400.0, VALUE)

      RETURN
      END

C======================================================================
C     TCINS2 - Insert column: cells to left stay in place
C======================================================================
      SUBROUTINE TCINS2
C     Test: Insert column at C should NOT move cell at A1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value at A1
      CALL CELPUT(1, 1, 1, 60.0)

C     Insert column at C (col 3)
      CALL COLINS(3)

C     A1 should be unchanged
      CALL CELGET(1, 1, CTYPE, VALUE)
      CALL TSTEQ('A1 type unchanged', 1, CTYPE)
      CALL TSTREQ('A1 value unchanged', 60.0, VALUE)

      RETURN
      END

C======================================================================
C     TCINS3 - Insert column: formula references update
C======================================================================
      SUBROUTINE TCINS3
C     Test: If A1 has =B1, and we insert col at B, A1 should become =C1

      INTEGER FSTR(80), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put formula =B1 at A1
      FSTR(1) = 66
      FSTR(2) = 49
      FLEN = 2
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 0.0)

C     Insert column at B (col 2)
      CALL COLINS(2)

C     Get formula from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should now be "C1" (ASCII: 67, 49)
      CALL TSTEQ('Formula length after insert', 2, FLEN)
      CALL TSTEQ('Formula col char', 67, FSTR(1))
      CALL TSTEQ('Formula row char', 49, FSTR(2))

      RETURN
      END

C======================================================================
C     TCDEL1 - Delete column: cells shift left
C======================================================================
      SUBROUTINE TCDEL1
C     Test: Delete column B should shift cell at C1 to B1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value at C1 (col=3)
      CALL CELPUT(3, 1, 1, 500.0)

C     Put value at B1 (will be deleted)
      CALL CELPUT(2, 1, 1, 999.0)

C     Delete column B (col 2)
      CALL COLDEL(2)

C     C1 should now be empty
      CALL CELGET(3, 1, CTYPE, VALUE)
      CALL TSTEQ('C1 empty after delete', 0, CTYPE)

C     Original C1 should now be at B1
      CALL CELGET(2, 1, CTYPE, VALUE)
      CALL TSTEQ('B1 exists after delete', 1, CTYPE)
      CALL TSTREQ('B1 value after delete', 500.0, VALUE)

      RETURN
      END

C======================================================================
C     TCDEL2 - Delete column: cells to left stay in place
C======================================================================
      SUBROUTINE TCDEL2
C     Test: Delete column C should NOT move cell at A1

      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put value at A1
      CALL CELPUT(1, 1, 1, 85.0)

C     Put value at C1 (will be deleted)
      CALL CELPUT(3, 1, 1, 999.0)

C     Delete column C (col 3)
      CALL COLDEL(3)

C     A1 should be unchanged
      CALL CELGET(1, 1, CTYPE, VALUE)
      CALL TSTEQ('A1 type unchanged', 1, CTYPE)
      CALL TSTREQ('A1 value unchanged', 85.0, VALUE)

      RETURN
      END

C======================================================================
C     TCDEL3 - Delete column: formula references update
C======================================================================
      SUBROUTINE TCDEL3
C     Test: If A1 has =C1, and we delete col B, A1 should become =B1

      INTEGER FSTR(80), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put formula =C1 at A1
      FSTR(1) = 67
      FSTR(2) = 49
      FLEN = 2
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 0.0)

C     Delete column B (col 2)
      CALL COLDEL(2)

C     Get formula from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should now be "B1" (ASCII: 66, 49)
      CALL TSTEQ('Formula length after delete', 2, FLEN)
      CALL TSTEQ('Formula col char', 66, FSTR(1))
      CALL TSTEQ('Formula row char', 49, FSTR(2))

      RETURN
      END

C======================================================================
C     TABSR1 - Absolute row reference not shifted
C======================================================================
      SUBROUTINE TABSR1
C     Test: If A1 has =B$2, insert row at 2 should keep =B$2

      INTEGER FSTR(80), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put formula =B$2 at A1 (ASCII: B=66, $=36, 2=50)
      FSTR(1) = 66
      FSTR(2) = 36
      FSTR(3) = 50
      FLEN = 3
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 0.0)

C     Insert row at row 2
      CALL ROWINS(2)

C     Get formula from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should still be "B$2" (absolute row not shifted)
      CALL TSTEQ('Abs ref length', 3, FLEN)
      CALL TSTEQ('Abs ref col', 66, FSTR(1))
      CALL TSTEQ('Abs ref $', 36, FSTR(2))
      CALL TSTEQ('Abs ref row', 50, FSTR(3))

      RETURN
      END

C======================================================================
C     TABSR2 - Absolute column reference not shifted
C======================================================================
      SUBROUTINE TABSR2
C     Test: If A1 has =$B1, insert col at B should keep =$B1

      INTEGER FSTR(80), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     Put formula =$B1 at A1 (ASCII: $=36, B=66, 1=49)
      FSTR(1) = 36
      FSTR(2) = 66
      FSTR(3) = 49
      FLEN = 3
      CALL CELPFS(1, 1, FSTR, FLEN)
      CALL CELPUT(1, 1, 2, 0.0)

C     Insert column at B (col 2)
      CALL COLINS(2)

C     Get formula from A1
      CALL CELGFS(1, 1, FSTR, 80, FLEN)

C     Formula should still be "$B1" (absolute col not shifted)
      CALL TSTEQ('Abs ref length', 3, FLEN)
      CALL TSTEQ('Abs ref $', 36, FSTR(1))
      CALL TSTEQ('Abs ref col', 66, FSTR(2))
      CALL TSTEQ('Abs ref row', 49, FSTR(3))

      RETURN
      END
