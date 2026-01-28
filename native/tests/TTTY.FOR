C     TTTY.FOR - Teletype Renderer Tests
C
C     Tests for RENDTTY.FOR (TTY35 line-oriented display)
C
C     Tests:
C       1. RDCAPS returns correct capabilities
C       2. TTYUPC converts to uppercase
C       3. RDSEP outputs 72 dashes
C       4. Output buffer handling
C
C     Author: Claude Code
C     Date: 2026-01-27
C
C======================================================================

C======================================================================
C     Output capture buffer for testing
C======================================================================
      BLOCK DATA CAPDAT
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX
      DATA CAPLEN /0/
      DATA CAPMAX /500/
      END

C======================================================================
C     CAPINI - Initialize capture buffer
C======================================================================
      SUBROUTINE CAPINI
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX

      CAPLEN = 0

      RETURN
      END

C======================================================================
C     CAPGET - Get character from buffer at position
C======================================================================
      INTEGER FUNCTION CAPGET(POS)
      INTEGER POS
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX

      IF (POS .LT. 1 .OR. POS .GT. CAPLEN) THEN
        CAPGET = 0
      ELSE
        CAPGET = CAPBUF(POS)
      END IF

      RETURN
      END

C======================================================================
C     CAPGLN - Get capture buffer length
C======================================================================
      INTEGER FUNCTION CAPGLN()
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX

      CAPGLN = CAPLEN

      RETURN
      END

C======================================================================
C     Mock TERMINAL.FOR functions for testing
C======================================================================

C======================================================================
C     TMINIT - Mock init (no-op)
C======================================================================
      SUBROUTINE TMINIT
      RETURN
      END

C======================================================================
C     TMREST - Mock restore (no-op)
C======================================================================
      SUBROUTINE TMREST
      RETURN
      END

C======================================================================
C     TMPUTC - Mock put char (capture to buffer)
C======================================================================
      SUBROUTINE TMPUTC(CH)
      INTEGER CH
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX

      IF (CAPLEN .LT. CAPMAX) THEN
        CAPLEN = CAPLEN + 1
        CAPBUF(CAPLEN) = CH
      END IF

      RETURN
      END

C======================================================================
C     TMFLSH - Mock flush (no-op)
C======================================================================
      SUBROUTINE TMFLSH
      RETURN
      END

C======================================================================
C     TMBELL - Mock bell (no-op)
C======================================================================
      SUBROUTINE TMBELL
      RETURN
      END

C======================================================================
C     TMWAIT - Mock wait (no-op, return space)
C======================================================================
      SUBROUTINE TMWAIT(KEY)
      INTEGER KEY
      KEY = 32
      RETURN
      END

C======================================================================
C     IOPUTRW - Mock formatted real output
C======================================================================
      SUBROUTINE IOPUTRW(VALUE, DECPL, WIDTH)
      REAL VALUE
      INTEGER DECPL, WIDTH
      INTEGER CAPBUF(500), CAPLEN, CAPMAX
      COMMON /CAPTURE/ CAPBUF, CAPLEN, CAPMAX
      INTEGER I
      CHARACTER*20 FMTSTR
      CHARACTER*20 NUMSTR

C     Simple mock: output spaces for width
C     (Real formatting is complex, just placeholder for test)
      DO 10 I = 1, WIDTH
        IF (CAPLEN .LT. CAPMAX) THEN
          CAPLEN = CAPLEN + 1
          CAPBUF(CAPLEN) = 32
        END IF
10    CONTINUE

      RETURN
      END

C======================================================================
C     Mock column width function
C======================================================================
      INTEGER FUNCTION CWGET(COL)
      INTEGER COL
C     Return fixed width of 8 for all columns
      CWGET = 8
      RETURN
      END

C======================================================================
C     TTTY - Run all TTY renderer tests
C======================================================================
      SUBROUTINE TTTY

      CALL TSTGRP('TTY Capabilities')
      CALL TCAPS

      CALL TSTGRP('TTY Uppercase Conversion')
      CALL TUPC

      CALL TSTGRP('TTY Separator Line')
      CALL TSEP

      CALL TSTGRP('TTY Newline Output')
      CALL TNEWL

      CALL TSTGRP('TTY Cell Prompt')
      CALL TPROMPT

      RETURN
      END

C======================================================================
C     TCAPS - Test RDCAPS capabilities
C======================================================================
      SUBROUTINE TCAPS
      INTEGER CURSOR, CLEAR, RVIDEO, ROWS, COLS

C     Get TTY capabilities
      CALL RDCAPS(CURSOR, CLEAR, RVIDEO, ROWS, COLS)

C     TTY has no cursor positioning
      CALL TSTEQ('No cursor', 0, CURSOR)

C     TTY has no clear screen
      CALL TSTEQ('No clear', 0, CLEAR)

C     TTY has no reverse video
      CALL TSTEQ('No reverse', 0, RVIDEO)

C     Display 10 rows
      CALL TSTEQ('Rows = 10', 10, ROWS)

C     Display 5 columns
      CALL TSTEQ('Cols = 5', 5, COLS)

      RETURN
      END

C======================================================================
C     TUPC - Test TTYUPC uppercase conversion
C======================================================================
      SUBROUTINE TUPC
      INTEGER CAPGET, CAPGLN

C     Test lowercase 'a' (97) -> 'A' (65)
      CALL CAPINI
      CALL TTYUPC(97)
      CALL TSTEQ('a -> A', 65, CAPGET(1))

C     Test lowercase 'z' (122) -> 'Z' (90)
      CALL CAPINI
      CALL TTYUPC(122)
      CALL TSTEQ('z -> Z', 90, CAPGET(1))

C     Test uppercase 'A' stays 'A'
      CALL CAPINI
      CALL TTYUPC(65)
      CALL TSTEQ('A stays A', 65, CAPGET(1))

C     Test number '5' (53) stays '5'
      CALL CAPINI
      CALL TTYUPC(53)
      CALL TSTEQ('5 stays 5', 53, CAPGET(1))

C     Test space (32) stays space
      CALL CAPINI
      CALL TTYUPC(32)
      CALL TSTEQ('space stays space', 32, CAPGET(1))

      RETURN
      END

C======================================================================
C     TSEP - Test RDSEP separator line
C======================================================================
      SUBROUTINE TSEP
      INTEGER CAPGET, CAPGLN
      INTEGER I, ALLDASH

C     Clear buffer and call RDSEP
      CALL CAPINI
      CALL RDSEP

C     Should have 72 dashes + CR + LF = 74 chars
      CALL TSTEQ('Sep length', 74, CAPGLN())

C     First 72 chars should be dash (45)
      ALLDASH = 1
      DO 10 I = 1, 72
        IF (CAPGET(I) .NE. 45) ALLDASH = 0
10    CONTINUE
      CALL TSTEQ('72 dashes', 1, ALLDASH)

C     Char 73 should be CR (13)
      CALL TSTEQ('Ends with CR', 13, CAPGET(73))

C     Char 74 should be LF (10)
      CALL TSTEQ('Ends with LF', 10, CAPGET(74))

      RETURN
      END

C======================================================================
C     TNEWL - Test TTYNEW newline output
C======================================================================
      SUBROUTINE TNEWL
      INTEGER CAPGET, CAPGLN

C     Clear buffer and call TTYNEW
      CALL CAPINI
      CALL TTYNEW

C     Should output CR + LF = 2 chars
      CALL TSTEQ('Newline length', 2, CAPGLN())

C     First char CR (13)
      CALL TSTEQ('CR first', 13, CAPGET(1))

C     Second char LF (10)
      CALL TSTEQ('LF second', 10, CAPGET(2))

      RETURN
      END

C======================================================================
C     TPROMPT - Test RDSTS cell prompt
C======================================================================
      SUBROUTINE TPROMPT
      INTEGER CAPGET, CAPGLN
      INTEGER I

C     Initialize cells (needed for FMTCEL)
      CALL CELINI

C     Clear buffer and render prompt for A1
      CALL CAPINI
      CALL RDSTS(1, 1, 0)

C     Should start with CR+LF (newline)
      CALL TSTEQ('Prompt starts CR', 13, CAPGET(1))
      CALL TSTEQ('Prompt starts LF', 10, CAPGET(2))

C     Then 'A' (65), '1' (49), ':' (58), ' ' (32)
      CALL TSTEQ('Cell ref A', 65, CAPGET(3))
      CALL TSTEQ('Cell ref 1', 49, CAPGET(4))
      CALL TSTEQ('Colon', 58, CAPGET(5))
      CALL TSTEQ('Space', 32, CAPGET(6))

C     Test prompt for B10
      CALL CAPINI
      CALL RDSTS(2, 10, 0)

C     After CR+LF: 'B' (66), '1' (49), '0' (48), ':' (58), ' ' (32)
      CALL TSTEQ('B10 ref B', 66, CAPGET(3))
      CALL TSTEQ('B10 ref 1', 49, CAPGET(4))
      CALL TSTEQ('B10 ref 0', 48, CAPGET(5))

      RETURN
      END
