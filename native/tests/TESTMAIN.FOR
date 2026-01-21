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
