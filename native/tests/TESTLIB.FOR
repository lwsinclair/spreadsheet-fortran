C     TESTLIB.FOR - Test Framework Library
C
C     Purpose: Simple test harness for FORTRAN IV compatible tests
C
C     Usage:
C       CALL TSTINI - Initialize test suite
C       CALL TSTEQ(NAME, EXPECTED, ACTUAL) - Test integer equality
C       CALL TSTREQ(NAME, EXPECTED, ACTUAL) - Test real equality
C       CALL TSTEND - Print test results
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

C======================================================================
C     Test state
C======================================================================
      BLOCK DATA TSTDAT
      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL
      DATA TPASS /0/
      DATA TFAIL /0/
      DATA TTOTAL /0/
      END

C======================================================================
C     TSTINI - Initialize test suite
C======================================================================
      SUBROUTINE TSTINI
      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL

      TPASS = 0
      TFAIL = 0
      TTOTAL = 0

      WRITE(*,*) '=== Test Suite Started ==='
      WRITE(*,*) ''

      RETURN
      END

C======================================================================
C     TSTEQ - Test integer equality
C======================================================================
      SUBROUTINE TSTEQ(NAME, EXPECT, ACTUAL)
      CHARACTER*(*) NAME
      INTEGER EXPECT, ACTUAL

      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL

      TTOTAL = TTOTAL + 1

      IF (EXPECT .EQ. ACTUAL) THEN
        TPASS = TPASS + 1
        WRITE(*,100) NAME
100     FORMAT('  PASS: ', A)
      ELSE
        TFAIL = TFAIL + 1
        WRITE(*,200) NAME, EXPECT, ACTUAL
200     FORMAT('  FAIL: ', A, ' expected=', I6, ' actual=', I6)
      END IF

      RETURN
      END

C======================================================================
C     TSTREQ - Test real equality (within tolerance)
C======================================================================
      SUBROUTINE TSTREQ(NAME, EXPECT, ACTUAL)
      CHARACTER*(*) NAME
      REAL EXPECT, ACTUAL

      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL

      REAL DIFF, TOL
      TOL = 0.0001

      TTOTAL = TTOTAL + 1
      DIFF = ABS(EXPECT - ACTUAL)

      IF (DIFF .LT. TOL) THEN
        TPASS = TPASS + 1
        WRITE(*,100) NAME
100     FORMAT('  PASS: ', A)
      ELSE
        TFAIL = TFAIL + 1
        WRITE(*,200) NAME, EXPECT, ACTUAL
200     FORMAT('  FAIL: ', A, ' expected=', F10.4, ' actual=', F10.4)
      END IF

      RETURN
      END

C======================================================================
C     TSTSTR - Test string equality
C======================================================================
      SUBROUTINE TSTSTR(NAME, EXPARR, EXPLEN, ACTARR, ACTLEN)
      CHARACTER*(*) NAME
      INTEGER EXPARR(*), EXPLEN
      INTEGER ACTARR(*), ACTLEN

      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL

      INTEGER I, MATCH

      TTOTAL = TTOTAL + 1

      MATCH = 1
      IF (EXPLEN .NE. ACTLEN) THEN
        MATCH = 0
      ELSE
        DO 10 I = 1, EXPLEN
          IF (EXPARR(I) .NE. ACTARR(I)) MATCH = 0
10      CONTINUE
      END IF

      IF (MATCH .EQ. 1) THEN
        TPASS = TPASS + 1
        WRITE(*,100) NAME
100     FORMAT('  PASS: ', A)
      ELSE
        TFAIL = TFAIL + 1
        WRITE(*,200) NAME
200     FORMAT('  FAIL: ', A, ' (strings differ)')
      END IF

      RETURN
      END

C======================================================================
C     TSTGRP - Start a test group (for organization)
C======================================================================
      SUBROUTINE TSTGRP(NAME)
      CHARACTER*(*) NAME

      WRITE(*,*) ''
      WRITE(*,100) NAME
100   FORMAT('--- ', A, ' ---')

      RETURN
      END

C======================================================================
C     TSTEND - Print test results summary
C======================================================================
      SUBROUTINE TSTEND
      INTEGER TPASS, TFAIL, TTOTAL
      COMMON /TSTST/ TPASS, TFAIL, TTOTAL

      WRITE(*,*) ''
      WRITE(*,*) '=== Test Results ==='
      WRITE(*,100) TPASS, TTOTAL
100   FORMAT('  Passed: ', I4, ' / ', I4)

      IF (TFAIL .GT. 0) THEN
        WRITE(*,200) TFAIL
200     FORMAT('  FAILED: ', I4, ' tests')
      ELSE
        WRITE(*,*) '  All tests passed!'
      END IF

      RETURN
      END
