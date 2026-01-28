C     TTYMAIN.FOR - TTY Renderer Test Main
C
C     Runs TTY-specific renderer tests
C
C======================================================================

      PROGRAM TTYTEST

C     Initialize test framework
      CALL TSTINI

C     Run TTY renderer tests
      CALL TTTY

C     Print results
      CALL TSTEND

      STOP
      END
