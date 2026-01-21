C     FILEBINS.FOR - Binary-Only File I/O Wrappers (Small Builds)
C
C     Purpose: Wrapper functions for small builds (binary format only)
C
C     This file provides FLXSAV and FLXLOD that only use binary format.
C     Use this instead of the full FILEBIN.FOR wrappers for small builds
C     to avoid linking FILESAV.FOR and FILELOAD.FOR (JSON support).
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

C======================================================================
C     FLXSAV - Save wrapper (binary only for small builds)
C======================================================================
      SUBROUTINE FLXSAV(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     Always use binary format - default extension is .xlb
      CALL FLBSAV(FNAME, FLEN, ERR)

      RETURN
      END

C======================================================================
C     FLXLOD - Load wrapper (binary only for small builds)
C======================================================================
      SUBROUTINE FLXLOD(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

C     Always use binary format
      CALL FLBLOD(FNAME, FLEN, ERR)

      RETURN
      END
