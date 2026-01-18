C     RECALC.FOR - Recalculation Engine Module
C
C     Purpose: Propagate changes through dependency graph
C
C     Algorithm: Breadth-first propagation
C       - Get formula tokens from cell
C       - Evaluate formula
C       - Store result
C       - Get dependents
C       - Add dependents to queue
C       - Process queue until empty
C
C     Features:
C       - Recalculate single cell
C       - Propagate to all dependents
C       - Iterative (no recursion)
C       - Handles circular references
C
C     Configuration:
C       Array sizes defined by PARAMETER statements.
C       Default: MAXQUE=500, MAXTOK=100, MAXDPS=100
C       For CP/M: MAXQUE=75, MAXTOK=50, MAXDPS=50
C       For minimal: MAXQUE=25, MAXTOK=25, MAXDPS=25
C
C     Dependencies: CELLS.FOR, PARSE.FOR, EVAL.FOR, DEPS.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C     Updated: 2026-01-19 (Added configurable array sizes)
C
C======================================================================

C======================================================================
C     RECINI - Initialize recalculation engine
C======================================================================
      SUBROUTINE RECINI
C     Nothing to initialize for now
      RETURN
      END

C======================================================================
C     RECCEL - Recalculate cell and propagate to dependents
C======================================================================
      SUBROUTINE RECCEL(COL, ROW)
      INTEGER COL, ROW

C     Configuration parameters
      INTEGER MAXQUE, MAXTOK, MAXDPS
      PARAMETER (MAXQUE=500, MAXTOK=100, MAXDPS=100)
C
C     Work queue for propagation
      INTEGER QUEUE(MAXQUE, 2)
      INTEGER QHEAD, QTAIL

C     Working variables
      INTEGER CCOL, CROW
      INTEGER TOKENS(MAXTOK, 4), NTOK, ERROR
      REAL RESULT
      INTEGER CTYPE
      REAL VAL
      INTEGER DEPS(MAXDPS, 2), NDEPS
      INTEGER I

C     Initialize queue
      QHEAD = 1
      QTAIL = 1

C     Add starting cell to queue
      QUEUE(QTAIL,1) = COL
      QUEUE(QTAIL,2) = ROW
      QTAIL = QTAIL + 1

C     Process queue
100   IF (QHEAD .GE. QTAIL) RETURN

C     Get next cell from queue
      CCOL = QUEUE(QHEAD,1)
      CROW = QUEUE(QHEAD,2)
      QHEAD = QHEAD + 1

C     Get cell type to see if it's a formula
      CALL CELGET(CCOL, CROW, CTYPE, VAL)

C     If it's a formula, recalculate it
      IF (CTYPE .NE. 2) GO TO 200

C     Get formula tokens
      CALL CELGTK(CCOL, CROW, TOKENS, NTOK)
      IF (NTOK .EQ. 0) GO TO 200

C     Evaluate formula
      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
      IF (ERROR .NE. 0) GO TO 200

C     Store result (use CELRES to keep formula type)
      CALL CELRES(CCOL, CROW, RESULT)

C     Get dependents of this cell
200   CALL DEPSGET(CCOL, CROW, DEPS, NDEPS)

C     Add dependents to queue
      DO 300 I = 1, NDEPS
        QUEUE(QTAIL,1) = DEPS(I,1)
        QUEUE(QTAIL,2) = DEPS(I,2)
        QTAIL = QTAIL + 1
300   CONTINUE

C     Continue processing queue
      GO TO 100
      END

C======================================================================
C     RECMOD - Set recalculation mode (STUB for future)
C======================================================================
      SUBROUTINE RECMOD(MODE)
      INTEGER MODE

C     TODO: Implement auto/manual modes
C     0 = manual
C     1 = automatic

      RETURN
      END

C======================================================================
C     RECALL - Recalculate all cells (STUB for future)
C======================================================================
      SUBROUTINE RECALL

C     TODO: Implement full spreadsheet recalc
C     - Get all formula cells
C     - Topological sort
C     - Calculate in order

      RETURN
      END
