C     CELLS.FOR - Cell Storage Module
C
C     Purpose: Store and retrieve spreadsheet cells using hash table
C
C     Features:
C       - Hash table with open chaining for collisions
C       - Sparse storage (only non-empty cells stored)
C       - Support for numeric and formula cells
C       - String pool for formula storage
C
C     Configuration:
C       Array sizes defined by PARAMETER statements below.
C       To change configuration, modify these parameters and recompile.
C       See src/config/CONFIG_*.FOR for suggested configurations.
C
C     Dependencies: STRUTIL.FOR
C
C     Author: Claude Code
C     Date: 2026-01-18
C     Updated: 2026-01-19 (Added configurable array sizes)
C
C======================================================================
C
C     Configuration Note:
C       Array sizes are controlled by PARAMETER statements in each subroutine.
C       To change configuration, modify all PARAMETER statements in this file.
C       Suggested configurations (see src/config/ for details):
C         - Full (default): MAXCEL=300, HASHSZ=256, MAXSTR=2000
C         - CP/M (48KB):    MAXCEL=300,  HASHSZ=256,  MAXSTR=2000
C         - Minimal:        MAXCEL=100,  HASHSZ=64,   MAXSTR=500
C
C======================================================================

C======================================================================
C     CELINI - Initialize cell storage
C======================================================================
      SUBROUTINE CELINI
C     Cell array - stores all cells in hash table
C     CELLA(i,1) = COL    (column number 1-63)
C     CELLA(i,2) = ROW    (row number 1-254)
C     CELLA(i,3) = TYPE   (0=empty, 1=number, 2=formula, 3=text)
C     CELLA(i,4) = FMLIDX (formula/text index into FMLPOL string pool)
C     CELLA(i,5) = NEXT   (next cell index for collision chain)
C     CELLA(i,6) = FLAGS  (recalc needed, etc)
C     CELLA(i,7) = ALIGN  (text alignment: 1=left, 2=right, 3=center)
C     CELLV(i)   = VALUE  (REAL value for TYPE=1 numeric cells)
C     CELLR(i)   = RESULT (REAL result for TYPE=2 formulas)
C
C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER I

C     Clear hash table
      DO 10 I = 1, HASHSZ
        HTABLE(I) = 0
10    CONTINUE

C     Clear cell array (mark all as empty)
      DO 20 I = 1, MAXCEL
        CELLA(I,1) = 0
        CELLA(I,2) = 0
        CELLA(I,3) = 0        ! TYPE=0 (empty)
        CELLA(I,4) = 0
        CELLA(I,5) = 0
        CELLA(I,6) = 0
        CELLA(I,7) = 0
        CELLV(I) = 0.0
        CELLR(I) = 0.0
        FMLLEN(I) = 0
20    CONTINUE

C     Initialize free list (not used initially)
      FRLIST = 0
      CELCNT = 0

C     Initialize formula pool
      FMLPTR = 1

      RETURN
      END

C======================================================================
C     CELHSH - Compute hash value for cell
C======================================================================
      INTEGER FUNCTION CELHSH(COL, ROW)
      INTEGER COL, ROW

C     Configuration parameters
      INTEGER HASHSZ
      PARAMETER (HASHSZ=256)
C
C     Hash function: (COL * 257 + ROW) MOD HASHSZ
C     257 is prime, provides good distribution
      CELHSH = MOD(COL * 257 + ROW, HASHSZ)

C     Ensure positive and 1-indexed
      IF (CELHSH .LE. 0) CELHSH = CELHSH + HASHSZ
      IF (CELHSH .LT. 1) CELHSH = 1
      IF (CELHSH .GT. HASHSZ) CELHSH = HASHSZ

      RETURN
      END

C======================================================================
C     CELFND - Find cell index in hash table
C======================================================================
      INTEGER FUNCTION CELFND(COL, ROW)
      INTEGER COL, ROW

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER HASH, IDX
      INTEGER CELHSH

C     Compute hash
      HASH = CELHSH(COL, ROW)

C     Get head of chain
      IDX = HTABLE(HASH)

C     Search chain
100   IF (IDX .EQ. 0) GO TO 900

C     Check if this is the cell
      IF (CELLA(IDX,1) .EQ. COL .AND. CELLA(IDX,2) .EQ. ROW) GO TO 800

C     Move to next in chain
      IDX = CELLA(IDX,5)
      GO TO 100

C     Found
800   CELFND = IDX
      RETURN

C     Not found
900   CELFND = 0
      RETURN
      END

C======================================================================
C     CELNEW - Allocate new cell slot
C======================================================================
      INTEGER FUNCTION CELNEW()
C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER I

C     Check if free list has slots
      IF (FRLIST .NE. 0) GO TO 100

C     No free list, allocate from end
      CELCNT = CELCNT + 1
      IF (CELCNT .GT. MAXCEL) GO TO 900

      CELNEW = CELCNT
      RETURN

C     Use slot from free list
100   I = FRLIST
      FRLIST = CELLA(I,5)
      CELNEW = I
      RETURN

C     Out of space
900   CELNEW = 0
      RETURN
      END

C======================================================================
C     CELPUT - Store cell value
C======================================================================
      SUBROUTINE CELPUT(COL, ROW, TYPE, VALUE)
      INTEGER COL, ROW, TYPE
      REAL VALUE

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER HASH, IDX, NEWIDX
      INTEGER CELHSH, CELFND, CELNEW

C     Check if cell already exists
      IDX = CELFND(COL, ROW)

C     If exists, update it
      IF (IDX .NE. 0) GO TO 200

C     Cell doesn't exist, create new
      NEWIDX = CELNEW()
      IF (NEWIDX .EQ. 0) RETURN

C     Initialize new cell
      CELLA(NEWIDX,1) = COL
      CELLA(NEWIDX,2) = ROW
      CELLA(NEWIDX,3) = TYPE
      CELLA(NEWIDX,4) = 0
      CELLA(NEWIDX,5) = 0
      CELLA(NEWIDX,6) = 0

C     Store REAL value based on TYPE
      IF (TYPE .EQ. 1) THEN
C       Direct number - store in CELLV
        CELLV(NEWIDX) = VALUE
      ELSE IF (TYPE .EQ. 2) THEN
C       Formula result - store in CELLR
        CELLR(NEWIDX) = VALUE
      END IF

C     Add to hash table chain
      HASH = CELHSH(COL, ROW)
      CELLA(NEWIDX,5) = HTABLE(HASH)
      HTABLE(HASH) = NEWIDX

      RETURN

C     Update existing cell
200   CELLA(IDX,3) = TYPE
      IF (TYPE .EQ. 1) THEN
        CELLV(IDX) = VALUE
      ELSE IF (TYPE .EQ. 2) THEN
        CELLR(IDX) = VALUE
      END IF

      RETURN
      END

C======================================================================
C     CELGET - Retrieve cell value
C======================================================================
      SUBROUTINE CELGET(COL, ROW, TYPE, VALUE)
      INTEGER COL, ROW, TYPE
      REAL VALUE

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX
      INTEGER CELFND

C     Find cell
      IDX = CELFND(COL, ROW)

C     If not found, return empty
      IF (IDX .EQ. 0) GO TO 900

C     Return cell data
      TYPE = CELLA(IDX,3)

C     For formulas, return calculated result from CELLR
      IF (TYPE .EQ. 2) GO TO 910

C     For numbers, return value from CELLV (fixes precision loss!)
      VALUE = CELLV(IDX)
      RETURN

C     Formula - return result from CELLR
910   VALUE = CELLR(IDX)
      RETURN

C     Cell not found
900   TYPE = 0
      VALUE = 0.0
      RETURN
      END

C======================================================================
C     CELDEL - Delete cell
C======================================================================
      SUBROUTINE CELDEL(COL, ROW)
      INTEGER COL, ROW

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER HASH, IDX, PREV
      INTEGER CELHSH

C     Compute hash
      HASH = CELHSH(COL, ROW)

C     Get head of chain
      IDX = HTABLE(HASH)
      PREV = 0

C     Search chain
100   IF (IDX .EQ. 0) RETURN

C     Check if this is the cell
      IF (CELLA(IDX,1) .EQ. COL .AND. CELLA(IDX,2) .EQ. ROW) GO TO 200

C     Move to next
      PREV = IDX
      IDX = CELLA(IDX,5)
      GO TO 100

C     Found cell to delete
200   CONTINUE

C     Remove from chain
      IF (PREV .EQ. 0) GO TO 210

C     Not head of chain
      CELLA(PREV,5) = CELLA(IDX,5)
      GO TO 220

C     Head of chain
210   HTABLE(HASH) = CELLA(IDX,5)

C     Mark cell as empty
220   CELLA(IDX,1) = 0
      CELLA(IDX,2) = 0
      CELLA(IDX,3) = 0
      CELLA(IDX,4) = 0
      CELLA(IDX,6) = 0

C     Add to free list
      CELLA(IDX,5) = FRLIST
      FRLIST = IDX

      RETURN
      END

C======================================================================
C     FMLADD - Add formula to string pool (STUB for future)
C======================================================================
      SUBROUTINE FMLADD(FMLSTR, FMLLN, FMLIDX)
      INTEGER FMLSTR(*), FMLLN, FMLIDX

C     TODO: Implement formula string pool
C     For now, just return index 1
      FMLIDX = 1

      RETURN
      END

C======================================================================
C     CELRES - Store formula result
C======================================================================
      SUBROUTINE CELRES(COL, ROW, RESULT)
      INTEGER COL, ROW
      REAL RESULT

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND

C     Find cell
      IDX = CELFND(COL, ROW)
      IF (IDX .EQ. 0) RETURN

C     Store result in CELLR (fixes precision loss!)
      CELLR(IDX) = RESULT

      RETURN
      END

C======================================================================
C     CELPTK - Put formula tokens into cell
C======================================================================
      SUBROUTINE CELPTK(COL, ROW, TOKENS, NTOK)
      INTEGER COL, ROW
      INTEGER TOKENS(100, 4)
      INTEGER NTOK

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, CELNEW, I, J, BASE, HASH, CELHSH

C     Find or create cell
      IDX = CELFND(COL, ROW)

C     If cell doesn't exist, create it
      IF (IDX .EQ. 0) GO TO 10
      GO TO 20

C     Create new cell
10    IDX = CELNEW()
      IF (IDX .EQ. 0) RETURN

C     Initialize new cell
      CELLA(IDX,1) = COL
      CELLA(IDX,2) = ROW
      CELLA(IDX,3) = 2      ! TYPE=formula
      CELLA(IDX,4) = 0
      CELLA(IDX,5) = 0
      CELLA(IDX,6) = 0
      CELLA(IDX,7) = 0

C     Add to hash table
      HASH = CELHSH(COL, ROW)
      CELLA(IDX,5) = HTABLE(HASH)
      HTABLE(HASH) = IDX

C     Store token count
20    FMLLEN(IDX) = NTOK

C     Allocate space in formula pool
      BASE = FMLPTR

C     Copy tokens to pool (4 integers per token)
      DO 100 I = 1, NTOK
        DO 110 J = 1, 4
          FMLPOL(FMLPTR) = TOKENS(I,J)
          FMLPTR = FMLPTR + 1
110     CONTINUE
100   CONTINUE

C     Mark cell as formula type
      CELLA(IDX,3) = 2
      CELLA(IDX,4) = BASE

      RETURN
      END

C======================================================================
C     CELGTK - Get formula tokens from cell
C======================================================================
      SUBROUTINE CELGTK(COL, ROW, TOKENS, NTOK)
      INTEGER COL, ROW
      INTEGER TOKENS(100, 4)
      INTEGER NTOK

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, I, J, BASE, POS

C     Find cell
      IDX = CELFND(COL, ROW)
      IF (IDX .EQ. 0) GO TO 900

C     Check if formula
      IF (CELLA(IDX,3) .NE. 2) GO TO 900

C     Get token count
      NTOK = FMLLEN(IDX)

C     Get base pointer
      BASE = CELLA(IDX,4)
      POS = BASE

C     Copy tokens from pool
      DO 100 I = 1, NTOK
        DO 110 J = 1, 4
          TOKENS(I,J) = FMLPOL(POS)
          POS = POS + 1
110     CONTINUE
100   CONTINUE

      RETURN

C     Not a formula
900   NTOK = 0
      RETURN
      END

C======================================================================
C     CELTXT - Store text cell with alignment
C======================================================================
      SUBROUTINE CELTXT(COL, ROW, TEXT, TLEN, ALIGN)
      INTEGER COL, ROW
      INTEGER TEXT(*)
      INTEGER TLEN
      INTEGER ALIGN

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, CELNEW, I, BASE, HASH, CELHSH

C     Find or create cell
      IDX = CELFND(COL, ROW)

C     If cell doesn't exist, create it
      IF (IDX .EQ. 0) GO TO 10
      GO TO 20

C     Create new cell
10    IDX = CELNEW()
      IF (IDX .EQ. 0) RETURN

C     Initialize new cell
      CELLA(IDX,1) = COL
      CELLA(IDX,2) = ROW
      CELLA(IDX,3) = 3
      CELLA(IDX,4) = 0
      CELLA(IDX,5) = 0
      CELLA(IDX,6) = 0
      CELLA(IDX,7) = ALIGN

C     Add to hash table
      HASH = CELHSH(COL, ROW)
      CELLA(IDX,5) = HTABLE(HASH)
      HTABLE(HASH) = IDX

C     Store text
20    FMLLEN(IDX) = TLEN

C     Allocate space in string pool
      BASE = FMLPTR

C     Copy text to pool
      DO 100 I = 1, TLEN
        FMLPOL(FMLPTR) = TEXT(I)
        FMLPTR = FMLPTR + 1
100   CONTINUE

C     Mark cell as text type
      CELLA(IDX,3) = 3
      CELLA(IDX,4) = BASE
      CELLA(IDX,7) = ALIGN

      RETURN
      END

C======================================================================
C     CELGTX - Get text from cell
C======================================================================
      SUBROUTINE CELGTX(COL, ROW, TEXT, MAXLEN, TLEN, ALIGN)
      INTEGER COL, ROW
      INTEGER TEXT(*)
      INTEGER MAXLEN, TLEN
      INTEGER ALIGN

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, I, BASE, POS, COPYLEN

C     Find cell
      IDX = CELFND(COL, ROW)
      IF (IDX .EQ. 0) GO TO 900

C     Check if text cell
      IF (CELLA(IDX,3) .NE. 3) GO TO 900

C     Get text length
      TLEN = FMLLEN(IDX)

C     Get alignment
      ALIGN = CELLA(IDX,7)

C     Limit to buffer size
      COPYLEN = TLEN
      IF (COPYLEN .GT. MAXLEN) COPYLEN = MAXLEN

C     Get base pointer
      BASE = CELLA(IDX,4)
      POS = BASE

C     Copy text from pool
      DO 100 I = 1, COPYLEN
        TEXT(I) = FMLPOL(POS)
        POS = POS + 1
100   CONTINUE

      TLEN = COPYLEN
      RETURN

C     Not a text cell
900   TLEN = 0
      ALIGN = 0
      RETURN
      END

C======================================================================
C     CELPFS - Store formula string for cell
C======================================================================
      SUBROUTINE CELPFS(COL, ROW, FSTR, FLEN)
      INTEGER COL, ROW
      INTEGER FSTR(*)
      INTEGER FLEN

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, CELNEW, I, BASE, HASH, CELHSH

C     Find or create cell
      IDX = CELFND(COL, ROW)

C     If cell doesn't exist, create it
      IF (IDX .EQ. 0) GO TO 10
      GO TO 20

C     Create new cell
10    IDX = CELNEW()
      IF (IDX .EQ. 0) RETURN

C     Initialize new cell
      CELLA(IDX,1) = COL
      CELLA(IDX,2) = ROW
      CELLA(IDX,3) = 2
      CELLA(IDX,4) = 0
      CELLA(IDX,5) = 0
      CELLA(IDX,6) = 0
      CELLA(IDX,7) = 0

C     Add to hash table
      HASH = CELHSH(COL, ROW)
      CELLA(IDX,5) = HTABLE(HASH)
      HTABLE(HASH) = IDX

C     Store formula string
20    FMLLEN(IDX) = FLEN

C     Allocate space in string pool
      BASE = FMLPTR

C     Copy formula string to pool
      DO 100 I = 1, FLEN
        FMLPOL(FMLPTR) = FSTR(I)
        FMLPTR = FMLPTR + 1
100   CONTINUE

C     Mark cell as formula type, store base pointer
      CELLA(IDX,3) = 2
      CELLA(IDX,4) = BASE

      RETURN
      END

C======================================================================
C     CELGFS - Get formula string from cell
C======================================================================
      SUBROUTINE CELGFS(COL, ROW, FSTR, MAXLEN, FLEN)
      INTEGER COL, ROW
      INTEGER FSTR(*)
      INTEGER MAXLEN, FLEN

C     Configuration parameters
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300, HASHSZ=256, MAXSTR=2000)
C
      INTEGER CELLA(MAXCEL, 7)
      INTEGER HTABLE(HASHSZ)
      INTEGER FRLIST, CELCNT
      INTEGER FMLPOL(MAXSTR)
      INTEGER FMLLEN(MAXCEL)
      INTEGER FMLPTR
      REAL CELLV(MAXCEL)
      REAL CELLR(MAXCEL)

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
      COMMON /CELVDAT/ CELLV, CELLR

      INTEGER IDX, CELFND, I, BASE, POS, COPYLEN

C     Find cell
      IDX = CELFND(COL, ROW)
      IF (IDX .EQ. 0) GO TO 900

C     Check if formula cell
      IF (CELLA(IDX,3) .NE. 2) GO TO 900

C     Get formula length
      FLEN = FMLLEN(IDX)

C     Limit to buffer size
      COPYLEN = FLEN
      IF (COPYLEN .GT. MAXLEN) COPYLEN = MAXLEN

C     Get base pointer
      BASE = CELLA(IDX,4)
      POS = BASE

C     Copy formula from pool
      DO 100 I = 1, COPYLEN
        FSTR(I) = FMLPOL(POS)
        POS = POS + 1
100   CONTINUE

      FLEN = COPYLEN
      RETURN

C     Not a formula cell
900   FLEN = 0
      RETURN
      END
