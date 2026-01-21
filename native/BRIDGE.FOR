C     BRIDGE.FOR - Bridge routines for native compilation
C
C     Purpose: Provide wrapper routines to bridge naming/interface
C              differences between XLMAIN and the layer1 routines
C
C     These wrappers allow the original source to remain untouched
C     while enabling compilation with gfortran
C
C======================================================================

C======================================================================
C     DEPINI - Initialize dependencies (wrapper for DEPSINI)
C======================================================================
      SUBROUTINE DEPINI
      CALL DEPSINI
      RETURN
      END

C======================================================================
C     DEPSAD - Add dependencies from array (wrapper for DEPSADD)
C======================================================================
      SUBROUTINE DEPSAD(SCOL, SROW, DEPS, NDEPS)
      INTEGER SCOL, SROW
      INTEGER DEPS(100, 2)
      INTEGER NDEPS
C     Add each dependency from the DEPS array
C     DEPS(i,1) = column, DEPS(i,2) = row of dependent cell

      INTEGER I

      DO 10 I = 1, NDEPS
        CALL DEPSADD(DEPS(I,1), DEPS(I,2), SCOL, SROW)
10    CONTINUE

      RETURN
      END

C======================================================================
C     RECALC - Recalculate from cell (wrapper for RECCEL)
C======================================================================
      SUBROUTINE RECALC(COL, ROW)
      INTEGER COL, ROW

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
C     TMPREP - Print character repeated N times
C======================================================================
      SUBROUTINE TMPREP(CH, N)
      INTEGER CH
      INTEGER N
C     Output character CH repeated N times
C     Used for spaces, dashes, etc.

      INTEGER I

      DO 10 I = 1, N
        CALL TMPUTC(CH)
10    CONTINUE

      RETURN
      END
