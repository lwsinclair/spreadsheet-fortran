C     TESTMAIN.FOR - Test Suite Main Program
C
C     Purpose: Run all unit tests
C
C     Build: Compile with application source files
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

      PROGRAM TESTS

C     Initialize test framework
      CALL TSTINI

C     Run core canary tests (most critical)
      CALL TCORE

C     Run row/column tests
      CALL TROWCL

C     Print results
      CALL TSTEND

      STOP
      END

C======================================================================
C     Mock display functions for testing (no actual display needed)
C======================================================================

C======================================================================
C     DSPCLC - Mock display clear (no-op for tests)
C======================================================================
      SUBROUTINE DSPCLC(MSGNUM)
      INTEGER MSGNUM
      RETURN
      END

C======================================================================
C     DSPFUL - Mock display full refresh (no-op for tests)
C======================================================================
      SUBROUTINE DSPFUL
      RETURN
      END

C======================================================================
C     RDHLP - Mock render help (no-op for tests)
C======================================================================
      SUBROUTINE RDHLP
      RETURN
      END
