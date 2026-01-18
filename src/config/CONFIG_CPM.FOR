C     CONFIG_CPM.FOR - Compact Configuration for CP/M
C
C     Target: CP/M (8080/Z80) with 48KB RAM limit
C     Memory: ~33 KB total (fits in 48KB limit!)
C
C     Features: Reduced capacity, fully functional spreadsheet
C
C     Capacity:
C       - 300 cells (e.g., 12 columns x 25 rows)
C       - 150 formulas with dependencies
C       - 2000 character formula pool
C
C     Last Updated: 2026-01-19
C
C======================================================================
C     Array Size Parameters
C======================================================================

C     Cell storage (CELLS.FOR)
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=300)     ! Maximum cells (reduced from 2000)
      PARAMETER (HASHSZ=256)     ! Hash table size (reduced from 1024)
      PARAMETER (MAXSTR=2000)    ! Formula pool size (reduced from 10000)

C     Dependency graph (DEPS.FOR)
      INTEGER MAXDEP, DEPHSZ, MAXQUE
      PARAMETER (MAXDEP=150)     ! Maximum dependency nodes (reduced from 1000)
      PARAMETER (DEPHSZ=64)      ! Dependency hash size (reduced from 256)
      PARAMETER (MAXQUE=75)      ! Recalc queue size (reduced from 500)

C     Parser and evaluator (PARSE.FOR, EVAL.FOR, RECALC.FOR)
      INTEGER MAXTOK, MAXSTK, MAXDPS
      PARAMETER (MAXTOK=50)      ! Maximum tokens per formula (reduced from 100)
      PARAMETER (MAXSTK=25)      ! Operator stack size (reduced from 50)
      PARAMETER (MAXDPS=50)      ! Maximum dependencies per cell (reduced from 100)

C======================================================================
C     Memory Calculation (16-bit integers, 2 bytes each)
C======================================================================
C
C     CELLA(300, 7)      =  4,200 bytes ( 4 KB)  - Cell storage
C     CELLV(300)         =  1,200 bytes ( 1 KB)  - REAL values
C     CELLR(300)         =  1,200 bytes ( 1 KB)  - REAL results
C     FMLPOL(2000)       =  4,000 bytes ( 4 KB)  - Formula pool
C     DEPNOD(150, 5)     =  1,500 bytes ( 1 KB)  - Dependency graph
C     HTABLE(256)        =    512 bytes ( 0.5 KB) - Hash table
C     QUEUE(75, 2)       =    300 bytes ( 0.3 KB) - Recalc queue
C     Other arrays       =  ~3,000 bytes ( 3 KB) - Misc buffers
C     ─────────────────────────────────────────
C     Total Data:        = ~16 KB
C     Estimated Code:    = ~20 KB
C     ─────────────────────────────────────────
C     Total Program:     = ~36 KB  ✓ Fits in 48KB!
C
C     Provides working spreadsheet with reasonable capacity for CP/M
C
C======================================================================
