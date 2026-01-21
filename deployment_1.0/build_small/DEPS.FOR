C     DEPS.FOR - Dependency Tracking Module
C
C     Purpose: Track which cells depend on other cells
C
C     Data Structure: Hash table with linked lists
C       Hash by source cell (the cell being referenced)
C       Each node stores one dependency relationship
C       Supports multiple dependents per source cell
C
C     Example: A3 = +A1+A2
C       Dependencies stored:
C         A1 -> [A3]  (A3 depends on A1)
C         A2 -> [A3]  (A3 depends on A2)
C
C     Storage:
C       DEPNOD(i,1) = Source column
C       DEPNOD(i,2) = Source row
C       DEPNOD(i,3) = Dependent column
C       DEPNOD(i,4) = Dependent row
C       DEPNOD(i,5) = Next pointer (linked list)
C
C     Configuration:
C       Array sizes defined by PARAMETER statements in each subroutine.
C       Default: FULL configuration (for large systems)
C       For CP/M: MAXDEP=50, DEPHSZ=32, MAXQUE=25, MAXDPS=25
C       For minimal: MAXDEP=50, DEPHSZ=32, MAXQUE=25, MAXDPS=25
C
C     Dependencies: None (standalone module)
C
C     Author: Claude Code
C     Date: 2026-01-18
C     Updated: 2026-01-19 (Added configurable array sizes)
C
C======================================================================

C======================================================================
C     DEPSINI - Initialize dependency graph
C======================================================================
      SUBROUTINE DEPSINI
C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ
      PARAMETER (MAXDEP=50, DEPHSZ=32)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE
      INTEGER I

C     Clear hash table
      DO 10 I = 1, DEPHSZ
        DEPHT(I) = 0
10    CONTINUE

C     Initialize free list
      DEPFRE = 1
      DO 20 I = 1, MAXDEP - 1
        DEPNOD(I,5) = I + 1
20    CONTINUE
      DEPNOD(MAXDEP,5) = 0

      RETURN
      END

C======================================================================
C     DEPSADD - Add dependency
C======================================================================
      SUBROUTINE DEPSADD(SCOL, SROW, DCOL, DROW)
      INTEGER SCOL, SROW, DCOL, DROW

C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ
      PARAMETER (MAXDEP=50, DEPHSZ=32)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE

      INTEGER HASH, DEPSHSH, NODE

C     Get hash for source cell
      HASH = DEPSHSH(SCOL, SROW)

C     Check if already exists
      CALL DEPSFND(SCOL, SROW, DCOL, DROW, NODE)
      IF (NODE .NE. 0) RETURN

C     Allocate new node from free list
      IF (DEPFRE .EQ. 0) RETURN
      NODE = DEPFRE
      DEPFRE = DEPNOD(NODE,5)

C     Fill node
      DEPNOD(NODE,1) = SCOL
      DEPNOD(NODE,2) = SROW
      DEPNOD(NODE,3) = DCOL
      DEPNOD(NODE,4) = DROW

C     Insert at head of hash bucket
      DEPNOD(NODE,5) = DEPHT(HASH)
      DEPHT(HASH) = NODE

      RETURN
      END

C======================================================================
C     DEPSDEL - Delete dependency
C======================================================================
      SUBROUTINE DEPSDEL(SCOL, SROW, DCOL, DROW)
      INTEGER SCOL, SROW, DCOL, DROW

C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ
      PARAMETER (MAXDEP=50, DEPHSZ=32)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE

      INTEGER HASH, DEPSHSH, NODE, PREV, CURR

C     Get hash for source cell
      HASH = DEPSHSH(SCOL, SROW)

C     Search for node to delete
      PREV = 0
      CURR = DEPHT(HASH)

100   IF (CURR .EQ. 0) RETURN

C     Check if this is the node to delete
      IF (DEPNOD(CURR,1) .NE. SCOL) GO TO 110
      IF (DEPNOD(CURR,2) .NE. SROW) GO TO 110
      IF (DEPNOD(CURR,3) .NE. DCOL) GO TO 110
      IF (DEPNOD(CURR,4) .NE. DROW) GO TO 110

C     Found it - remove from list
      IF (PREV .EQ. 0) GO TO 120
      DEPNOD(PREV,5) = DEPNOD(CURR,5)
      GO TO 130

120   DEPHT(HASH) = DEPNOD(CURR,5)

C     Return node to free list
130   DEPNOD(CURR,5) = DEPFRE
      DEPFRE = CURR
      RETURN

C     Not this node - continue search
110   PREV = CURR
      CURR = DEPNOD(CURR,5)
      GO TO 100
      END

C======================================================================
C     DEPSGET - Get all dependents of a cell
C======================================================================
      SUBROUTINE DEPSGET(SCOL, SROW, DEPS, NDEPS)
      INTEGER SCOL, SROW
C     Configuration parameters
      INTEGER MAXDPS
      PARAMETER (MAXDPS=25)
C
      INTEGER DEPS(MAXDPS, 2)
      INTEGER NDEPS

C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ
      PARAMETER (MAXDEP=50, DEPHSZ=32)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE

      INTEGER HASH, DEPSHSH, NODE

      NDEPS = 0
      HASH = DEPSHSH(SCOL, SROW)
      NODE = DEPHT(HASH)

C     Scan hash bucket
100   IF (NODE .EQ. 0) RETURN

C     Check if source matches
      IF (DEPNOD(NODE,1) .NE. SCOL) GO TO 110
      IF (DEPNOD(NODE,2) .NE. SROW) GO TO 110

C     Match - add to result
      NDEPS = NDEPS + 1
      DEPS(NDEPS,1) = DEPNOD(NODE,3)
      DEPS(NDEPS,2) = DEPNOD(NODE,4)

C     Continue to next node
110   NODE = DEPNOD(NODE,5)
      GO TO 100
      END

C======================================================================
C     DEPSCIR - Check for circular reference (iterative)
C======================================================================
      SUBROUTINE DEPSCIR(COL, ROW, CIRC)
      INTEGER COL, ROW, CIRC

C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ, MAXQUE
      PARAMETER (MAXDEP=50, DEPHSZ=32, MAXQUE=25)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE

C     Work queue for breadth-first search
      INTEGER QUEUE(MAXQUE, 2)
      INTEGER QHEAD, QTAIL

C     Visited set
      INTEGER VISIT(MAXQUE)
      INTEGER NVISIT

C     Working variables
      INTEGER CCOL, CROW, HASH, DEPSHSH, NODE
      INTEGER DCOL, DROW, I, FOUND

      CIRC = 0
      QHEAD = 1
      QTAIL = 1
      NVISIT = 0

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

C     Check if already visited
      DO 110 I = 1, NVISIT
        IF (VISIT(I) .EQ. CCOL * 1000 + CROW) GO TO 100
110   CONTINUE

C     Mark as visited
      NVISIT = NVISIT + 1
      VISIT(NVISIT) = CCOL * 1000 + CROW

C     Get all dependents of this cell
      HASH = DEPSHSH(CCOL, CROW)
      NODE = DEPHT(HASH)

C     Check each dependent
200   IF (NODE .EQ. 0) GO TO 100

C     Check if source matches current cell
      IF (DEPNOD(NODE,1) .NE. CCOL) GO TO 210
      IF (DEPNOD(NODE,2) .NE. CROW) GO TO 210

C     Get dependent cell
      DCOL = DEPNOD(NODE,3)
      DROW = DEPNOD(NODE,4)

C     Check if dependent is original target (circular!)
      IF (DCOL .NE. COL) GO TO 220
      IF (DROW .NE. ROW) GO TO 220
      CIRC = 1
      RETURN

C     Add dependent to queue
220   QUEUE(QTAIL,1) = DCOL
      QUEUE(QTAIL,2) = DROW
      QTAIL = QTAIL + 1

C     Continue to next node
210   NODE = DEPNOD(NODE,5)
      GO TO 200
      END

C======================================================================
C     DEPSHSH - Hash function for dependency lookup
C======================================================================
      INTEGER FUNCTION DEPSHSH(COL, ROW)
      INTEGER COL, ROW

C     Configuration parameters
      INTEGER DEPHSZ
      PARAMETER (DEPHSZ=32)
C
C     Hash function: (COL * 257 + ROW) MOD DEPHSZ
      DEPSHSH = MOD(COL * 257 + ROW, DEPHSZ) + 1

      RETURN
      END

C======================================================================
C     DEPSFND - Find dependency node (internal)
C======================================================================
      SUBROUTINE DEPSFND(SCOL, SROW, DCOL, DROW, NODE)
      INTEGER SCOL, SROW, DCOL, DROW, NODE

C     Configuration parameters
      INTEGER MAXDEP, DEPHSZ
      PARAMETER (MAXDEP=50, DEPHSZ=32)
C
      COMMON /DEPDAT/ DEPNOD, DEPHT, DEPFRE
      INTEGER DEPNOD(MAXDEP, 5)
      INTEGER DEPHT(DEPHSZ)
      INTEGER DEPFRE

      INTEGER HASH, DEPSHSH

      HASH = DEPSHSH(SCOL, SROW)
      NODE = DEPHT(HASH)

C     Search hash bucket
100   IF (NODE .EQ. 0) RETURN

C     Check if matches
      IF (DEPNOD(NODE,1) .NE. SCOL) GO TO 110
      IF (DEPNOD(NODE,2) .NE. SROW) GO TO 110
      IF (DEPNOD(NODE,3) .NE. DCOL) GO TO 110
      IF (DEPNOD(NODE,4) .NE. DROW) GO TO 110
      RETURN

C     Continue search
110   NODE = DEPNOD(NODE,5)
      GO TO 100
      END
