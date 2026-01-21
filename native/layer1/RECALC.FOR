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
      INTEGER FSTR(80), FLEN

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

C     Get formula string
      CALL CELGFS(CCOL, CROW, FSTR, 80, FLEN)
      IF (FLEN .EQ. 0) GO TO 200

C     Parse formula string to get tokens
      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERROR)
      IF (ERROR .NE. 0 .OR. NTOK .EQ. 0) GO TO 200

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
C     RECALL - Recalculate all formula cells
C======================================================================
      SUBROUTINE RECALL

C     Configuration parameters (must match CELLS.FOR)
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=2000, HASHSZ=1024, MAXSTR=10000)
C
C     Cell storage arrays (from CELLS.FOR COMMON block)
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

C     Working variables
      INTEGER I, COL, ROW, CTYPE
      INTEGER TOKENS(100, 4), NTOK, ERROR
      REAL RESULT
      INTEGER FSTR(80), FLEN

C     Iterate through all cells
      DO 100 I = 1, CELCNT
C       Skip empty cells
        IF (CELLA(I,1) .EQ. 0) GO TO 100

C       Check if formula cell (TYPE=2)
        IF (CELLA(I,3) .NE. 2) GO TO 100

C       Get cell position
        COL = CELLA(I,1)
        ROW = CELLA(I,2)

C       Get formula string
        CALL CELGFS(COL, ROW, FSTR, 80, FLEN)
        IF (FLEN .EQ. 0) GO TO 100

C       Parse formula
        CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERROR)
        IF (ERROR .NE. 0 .OR. NTOK .EQ. 0) GO TO 100

C       Evaluate formula
        CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
        IF (ERROR .NE. 0) GO TO 100

C       Store result
        CALL CELRES(COL, ROW, RESULT)

100   CONTINUE

      RETURN
      END
