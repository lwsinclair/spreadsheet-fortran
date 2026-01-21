C     IORSX.FOR - RSX-11M I/O Primitives
C
C     Purpose: Platform-specific I/O for PDP-11 RSX-11M
C
C     This module provides the low-level I/O primitives that the
C     terminal protocol modules use. On RSX-11M, terminal I/O uses
C     FORTRAN WRITE/READ statements with appropriate formatting.
C
C     RSX-11M Terminal I/O:
C       Unit 5 = TI: (terminal input)
C       Unit 6 = TI: (terminal output)
C       OPEN statements may be needed to set characteristics
C
C     Note: For true single-character input, QIO$ calls would be
C     needed, but basic operation works with FORTRAN I/O.
C
C     I/O Primitive API:
C       IOINIT       - Initialize I/O subsystem
C       IOREST       - Restore I/O to normal state
C       IOPUTC(CH)   - Output single byte (0-255)
C       IOPUTS(S,L)  - Output array of bytes
C       IOGETC(C,V)  - Get single byte (non-blocking)
C       IOFLSH       - Flush output buffer
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     IOINIT - Initialize I/O subsystem
C======================================================================
      SUBROUTINE IOINIT
C     RSX-11M: Open terminal with correct characteristics
C     For line mode operation, no special setup needed
C     For single-char input, would need QIO$ setup

      RETURN
      END

C======================================================================
C     IOREST - Restore I/O to normal state
C======================================================================
      SUBROUTINE IOREST
C     RSX-11M: Restore terminal to normal mode

      RETURN
      END

C======================================================================
C     IOPUTC - Output single byte
C======================================================================
      SUBROUTINE IOPUTC(CH)
      INTEGER CH
C     Output single character using FORTRAN WRITE
C     The A1 format with INTEGER works on PDP-11 FORTRAN

      WRITE(6,100) CH
100   FORMAT(A1,$)

      RETURN
      END

C======================================================================
C     IOPUTS - Output array of bytes
C======================================================================
      SUBROUTINE IOPUTS(STR, LEN)
      INTEGER STR(*)
      INTEGER LEN
C     Output string of characters

      INTEGER I

      DO 10 I = 1, LEN
        WRITE(6,100) STR(I)
10    CONTINUE

100   FORMAT(A1,$)

      RETURN
      END

C======================================================================
C     IOGETC - Get single byte (non-blocking)
C======================================================================
      SUBROUTINE IOGETC(CH, VALID)
      INTEGER CH, VALID
C     Read single character from terminal
C
C     Note: True non-blocking requires QIO$ with IO.RNE
C     This basic version uses READ which waits for RETURN
C
C     For real single-char input on RSX-11M:
C       CALL QIO$(IO.RNE, ...) or similar

      INTEGER INCHAR

      READ(5,100,END=200,ERR=200) INCHAR
      CH = INCHAR
      VALID = 1
      RETURN

200   CH = 0
      VALID = 0
      RETURN

100   FORMAT(A1)

      END

C======================================================================
C     IOFLSH - Flush output buffer
C======================================================================
      SUBROUTINE IOFLSH
C     RSX-11M: Force output flush
C     FORTRAN output is typically line-buffered

      RETURN
      END

C======================================================================
C     IOPUTN - Output integer as formatted string
C======================================================================
      SUBROUTINE IOPUTN(NUM)
      INTEGER NUM
C     Output integer using FORTRAN format

      WRITE(6,100) NUM
100   FORMAT(I6,$)

      RETURN
      END

C======================================================================
C     IOPUTR - Output real as formatted string
C======================================================================
      SUBROUTINE IOPUTR(NUM, PREC)
      REAL NUM
      INTEGER PREC
C     Output real with variable precision
C     FORTRAN 66 doesn't have variable formats, so we use fixed

      IF (PREC .LE. 0) THEN
        WRITE(6,100) NUM
      ELSE IF (PREC .EQ. 1) THEN
        WRITE(6,101) NUM
      ELSE IF (PREC .EQ. 2) THEN
        WRITE(6,102) NUM
      ELSE
        WRITE(6,103) NUM
      END IF

100   FORMAT(F10.0,$)
101   FORMAT(F10.1,$)
102   FORMAT(F10.2,$)
103   FORMAT(F10.3,$)

      RETURN
      END
