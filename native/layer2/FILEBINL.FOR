C     FILEBINL.FOR - Smart File I/O Wrappers (Large Builds)
C
C     Purpose: Wrapper functions for large builds (JSON + Binary support)
C
C     This file provides FLXSAV and FLXLOD that choose format based on
C     file extension (.xlb = binary, .xl = JSON).
C
C     Author: Claude Code
C     Date: 2026-01-22
C
C======================================================================

C======================================================================
C     FLXSAV - Smart save: choose format by extension
C     .xlb = binary format, .xl or other = JSON format
C======================================================================
      SUBROUTINE FLXSAV(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

      INTEGER ISXLB

C     Check extension
      CALL FLXEXT(FNAME, FLEN, ISXLB)

      IF (ISXLB .EQ. 1) THEN
C       Binary format
        CALL FLBSAV(FNAME, FLEN, ERR)
      ELSE
C       JSON format
        CALL FLSAVE(FNAME, FLEN, ERR)
      END IF

      RETURN
      END

C======================================================================
C     FLXLOD - Smart load: detect format by magic number or extension
C     Tries to auto-detect format, falls back to JSON
C======================================================================
      SUBROUTINE FLXLOD(FNAME, FLEN, ERR)
      INTEGER FNAME(*)
      INTEGER FLEN
      INTEGER ERR

      INTEGER ISXLB, ISBIN

C     First check extension
      CALL FLXEXT(FNAME, FLEN, ISXLB)

      IF (ISXLB .EQ. 1) THEN
C       .xlb extension - definitely binary
        CALL FLBLOD(FNAME, FLEN, ERR)
        RETURN
      END IF

C     Try to detect by magic number (for files without extension)
C     This also handles .xl files that might be binary
      CALL FLBCHK(FNAME, FLEN, ISBIN, ERR)

      IF (ISBIN .EQ. 1) THEN
C       Has binary magic - load as binary
        CALL FLBLOD(FNAME, FLEN, ERR)
      ELSE
C       Assume JSON
        CALL FLLOAD(FNAME, FLEN, ERR)
      END IF

      RETURN
      END
