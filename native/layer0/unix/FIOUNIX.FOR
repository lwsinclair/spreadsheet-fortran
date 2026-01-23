C     FIOUNIX.FOR - Unix/POSIX File I/O Primitives
C
C     Purpose: Platform-specific file operations for Unix/macOS
C
C     This module provides the low-level file I/O that the portable
C     Layer 2 file routines (FILESAV, FILELOAD) call.
C
C     Primitives:
C       FIOINI            - Initialize file subsystem
C       FIOOPN(U,F,L,M,E) - Open file (M: 1=read, 2=write)
C       FIOCLS(U)         - Close file
C       FIOWTL(U,L,N)     - Write line
C       FIORDL(U,L,M,N,E) - Read line
C
C     Author: Claude Code
C     Date: 2026-01-21
C
C======================================================================

C======================================================================
C     FIOINI - Initialize file subsystem
C======================================================================
      SUBROUTINE FIOINI
C     Nothing to initialize for Unix
      RETURN
      END

C======================================================================
C     FIOOPN - Open file
C======================================================================
      SUBROUTINE FIOOPN(UNIT, FNAME, FLEN, MODE, ERR)
      INTEGER UNIT
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER MODE
      INTEGER ERR

C     Convert integer array to character string for FORTRAN OPEN
      CHARACTER*80 CNAME
      INTEGER I, IERR

C     Build character filename
      DO 10 I = 1, FLEN
        CNAME(I:I) = CHAR(FNAME(I))
10    CONTINUE
      DO 20 I = FLEN+1, 80
        CNAME(I:I) = ' '
20    CONTINUE

      ERR = 0

      IF (MODE .EQ. 1) THEN
C       Open for reading
        OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='OLD',
     &       IOSTAT=IERR)
        IF (IERR .NE. 0) ERR = 1
      ELSE IF (MODE .EQ. 2) THEN
C       Open for writing (create/replace)
        OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='REPLACE',
     &       IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
C         Try with STATUS='NEW' if REPLACE fails
          OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='NEW',
     &         IOSTAT=IERR)
          IF (IERR .NE. 0) ERR = 1
        END IF
      ELSE
        ERR = 1
      END IF

      RETURN
      END

C======================================================================
C     FIOCLS - Close file
C======================================================================
      SUBROUTINE FIOCLS(UNIT)
      INTEGER UNIT

      CLOSE(UNIT)

      RETURN
      END

C======================================================================
C     FIOWTL - Write line to file
C======================================================================
      SUBROUTINE FIOWTL(UNIT, LINE, LEN)
      INTEGER UNIT
      INTEGER LINE(*)
      INTEGER LEN

C     Convert integer array to character string
      CHARACTER*256 CLINE
      INTEGER I

      IF (LEN .GT. 256) LEN = 256

      DO 10 I = 1, LEN
        CLINE(I:I) = CHAR(LINE(I))
10    CONTINUE

      WRITE(UNIT, '(A)') CLINE(1:LEN)

      RETURN
      END

C======================================================================
C     FIORDL - Read line from file
C======================================================================
      SUBROUTINE FIORDL(UNIT, LINE, MAXLEN, LEN, EOF)
      INTEGER UNIT
      INTEGER LINE(*)
      INTEGER MAXLEN, LEN
      INTEGER EOF

C     Read into character string then convert
      CHARACTER*256 CLINE
      INTEGER I, IERR, ACTLEN

      EOF = 0
      LEN = 0

      READ(UNIT, '(A)', IOSTAT=IERR) CLINE

      IF (IERR .LT. 0) THEN
C       End of file
        EOF = 1
        RETURN
      ELSE IF (IERR .GT. 0) THEN
C       Read error
        EOF = -1
        RETURN
      END IF

C     Find actual length (trim trailing spaces)
      ACTLEN = LEN_TRIM(CLINE)
      IF (ACTLEN .GT. MAXLEN) ACTLEN = MAXLEN

C     Convert to integer array
      DO 10 I = 1, ACTLEN
        LINE(I) = ICHAR(CLINE(I:I))
10    CONTINUE

      LEN = ACTLEN

      RETURN
      END

C======================================================================
C     FIOEXT - Add extension to filename if missing
C======================================================================
      SUBROUTINE FIOEXT(FNAME, FLEN, EXT, ELEN, MAXLEN)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER EXT(*)
      INTEGER ELEN
      INTEGER MAXLEN

      INTEGER I, HASDOT

C     Check if filename already has a dot (extension)
      HASDOT = 0
      DO 10 I = 1, FLEN
        IF (FNAME(I) .EQ. 46) HASDOT = 1
10    CONTINUE

C     If no extension, add one
      IF (HASDOT .EQ. 0 .AND. FLEN + ELEN + 1 .LE. MAXLEN) THEN
        FNAME(FLEN + 1) = 46
        DO 20 I = 1, ELEN
          FNAME(FLEN + 1 + I) = EXT(I)
20      CONTINUE
        FLEN = FLEN + 1 + ELEN
      END IF

      RETURN
      END

C======================================================================
C     FIOOPB - Open file for binary I/O
C======================================================================
      SUBROUTINE FIOOPB(UNIT, FNAME, FLEN, MODE, ERR)
      INTEGER UNIT
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER MODE
      INTEGER ERR

C     Convert integer array to character string for FORTRAN OPEN
      CHARACTER*80 CNAME
      INTEGER I, IERR

C     Build character filename
      DO 10 I = 1, FLEN
        CNAME(I:I) = CHAR(FNAME(I))
10    CONTINUE
      DO 20 I = FLEN+1, 80
        CNAME(I:I) = ' '
20    CONTINUE

      ERR = 0

      IF (MODE .EQ. 1) THEN
C       Open for reading (binary/unformatted stream)
        OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='OLD',
     &       ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IERR)
        IF (IERR .NE. 0) ERR = 1
      ELSE IF (MODE .EQ. 2) THEN
C       Open for writing (binary/unformatted stream)
        OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='REPLACE',
     &       ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
C         Try with STATUS='NEW' if REPLACE fails
          OPEN(UNIT=UNIT, FILE=CNAME(1:FLEN), STATUS='NEW',
     &         ACCESS='STREAM', FORM='UNFORMATTED', IOSTAT=IERR)
          IF (IERR .NE. 0) ERR = 1
        END IF
      ELSE
        ERR = 1
      END IF

      RETURN
      END

C======================================================================
C     FIOWRB - Write single byte to binary file
C======================================================================
      SUBROUTINE FIOWRB(UNIT, BYTE)
      INTEGER UNIT
      INTEGER BYTE

      CHARACTER*1 CBYTE

      CBYTE = CHAR(MOD(BYTE, 256))
      WRITE(UNIT) CBYTE

      RETURN
      END

C======================================================================
C     FIORDB - Read single byte from binary file
C======================================================================
      SUBROUTINE FIORDB(UNIT, BYTE, EOF)
      INTEGER UNIT
      INTEGER BYTE
      INTEGER EOF

      CHARACTER*1 CBYTE
      INTEGER IERR

      EOF = 0
      READ(UNIT, IOSTAT=IERR) CBYTE

      IF (IERR .LT. 0) THEN
        EOF = 1
        BYTE = 0
      ELSE IF (IERR .GT. 0) THEN
        EOF = -1
        BYTE = 0
      ELSE
        BYTE = ICHAR(CBYTE)
      END IF

      RETURN
      END

C======================================================================
C     FIOWR2 - Write 16-bit integer little-endian
C======================================================================
      SUBROUTINE FIOWR2(UNIT, IVAL)
      INTEGER UNIT
      INTEGER IVAL

      INTEGER LO, HI

      LO = MOD(IVAL, 256)
      HI = IVAL / 256
      IF (LO .LT. 0) LO = LO + 256
      IF (HI .LT. 0) HI = HI + 256

      CALL FIOWRB(UNIT, LO)
      CALL FIOWRB(UNIT, HI)

      RETURN
      END

C======================================================================
C     FIORD2 - Read 16-bit integer little-endian
C======================================================================
      SUBROUTINE FIORD2(UNIT, IVAL, EOF)
      INTEGER UNIT
      INTEGER IVAL
      INTEGER EOF

      INTEGER LO, HI

      CALL FIORDB(UNIT, LO, EOF)
      IF (EOF .NE. 0) RETURN
      CALL FIORDB(UNIT, HI, EOF)
      IF (EOF .NE. 0) RETURN

      IVAL = HI * 256 + LO

      RETURN
      END

C======================================================================
C     FIOWRR - Write REAL as 4 bytes (native format)
C======================================================================
      SUBROUTINE FIOWRR(UNIT, RVAL)
      INTEGER UNIT
      REAL RVAL

C     Write REAL directly - native binary format
      WRITE(UNIT) RVAL

      RETURN
      END

C======================================================================
C     FIORDR - Read REAL as 4 bytes (native format)
C======================================================================
      SUBROUTINE FIORDR(UNIT, RVAL, EOF)
      INTEGER UNIT
      REAL RVAL
      INTEGER EOF

      INTEGER IERR

      EOF = 0
      READ(UNIT, IOSTAT=IERR) RVAL

      IF (IERR .LT. 0) THEN
        EOF = 1
        RVAL = 0.0
      ELSE IF (IERR .GT. 0) THEN
        EOF = -1
        RVAL = 0.0
      END IF

      RETURN
      END
