C     IOUNIX.FOR - Unix/macOS I/O Primitives
C
C     Purpose: Platform-specific I/O for Unix-like systems
C
C     This module provides the low-level I/O primitives that the
C     terminal protocol modules use. On Unix, we delegate to C
C     functions in termio.c for proper raw terminal handling.
C
C     I/O Primitive API:
C       IOINIT       - Initialize I/O subsystem
C       IOREST       - Restore I/O to normal state
C       IOPUTC(CH)   - Output single byte (0-255)
C       IOPUTS(S,L)  - Output array of bytes
C       IOGETC(C,V)  - Get single byte (non-blocking)
C       IOFLSH       - Flush output buffer
C
C     C Functions (in termio.c):
C       tminitc_()            - Initialize raw mode
C       tmrstc_()             - Restore normal mode
C       tmputcc_(ch)          - Output single byte
C       tmputsc_(str, len)    - Output byte array
C       tmreadc_(ch, valid)   - Read byte non-blocking
C       tmflshc_()            - Flush output
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     IOINIT - Initialize I/O subsystem
C======================================================================
      SUBROUTINE IOINIT
C     Initialize raw terminal mode via C

      CALL TMINITC

      RETURN
      END

C======================================================================
C     IOREST - Restore I/O to normal state
C======================================================================
      SUBROUTINE IOREST
C     Restore terminal to cooked mode via C

      CALL TMRSTC

      RETURN
      END

C======================================================================
C     IOPUTC - Output single byte
C======================================================================
      SUBROUTINE IOPUTC(CH)
      INTEGER CH
C     Output single character/byte via C

      CALL TMPUTCC(CH)

      RETURN
      END

C======================================================================
C     IOPUTS - Output array of bytes
C======================================================================
      SUBROUTINE IOPUTS(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
C     Output string of bytes via C

      CALL TMPUTSC(STR, LEN)

      RETURN
      END

C======================================================================
C     IOGETC - Get single byte (non-blocking)
C======================================================================
      SUBROUTINE IOGETC(CH, VALID)
      INTEGER CH, VALID
C     Read single character non-blocking via C
C     VALID = 1 if character available, 0 if not

      CALL TMREADC(CH, VALID)

      RETURN
      END

C======================================================================
C     IOFLSH - Flush output buffer
C======================================================================
      SUBROUTINE IOFLSH
C     Ensure all output sent via C

      CALL TMFLSHC

      RETURN
      END

C======================================================================
C     IOPUTN - Output integer as formatted string (6 chars)
C======================================================================
      SUBROUTINE IOPUTN(NUM)
      INTEGER NUM
C     Output integer via C printf

      CALL TMPUTNC(NUM)

      RETURN
      END

C======================================================================
C     IOPUTN3 - Output integer in 3 chars (for row numbers)
C======================================================================
      SUBROUTINE IOPUTN3(NUM)
      INTEGER NUM
C     Output integer via C printf, 3-char width

      CALL TMPUTN3C(NUM)

      RETURN
      END

C======================================================================
C     IOPUTN5 - Output integer in 5 chars (for 5-digit row numbers)
C======================================================================
      SUBROUTINE IOPUTN5(NUM)
      INTEGER NUM
C     Output integer via C printf, 5-char width

      CALL TMPUTN5C(NUM)

      RETURN
      END

C======================================================================
C     IOPUTR - Output real as formatted string (10 chars)
C======================================================================
      SUBROUTINE IOPUTR(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Output real with precision via C printf

      CALL TMPUTRC(NUM, PREC)

      RETURN
      END

C======================================================================
C     IOPUTR8 - Output real in 8 chars (for cell values)
C======================================================================
      SUBROUTINE IOPUTR8(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Output real with precision via C printf, 8-char width

      CALL TMPUTR8C(NUM, PREC)

      RETURN
      END

C======================================================================
C     IOPUTRW - Output real in variable width (for variable cell widths)
C======================================================================
      SUBROUTINE IOPUTRW(NUM, PREC, WIDTH)
      REAL NUM
      INTEGER PREC, WIDTH
C     Output real with precision via C printf, variable width

      CALL TMPUTRWC(NUM, PREC, WIDTH)

      RETURN
      END
