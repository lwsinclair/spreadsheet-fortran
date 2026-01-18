C     CONFIG_FULL.FOR - Full Configuration for CP-V / Large PDP-11
C
C     Target: CP-V (32-bit) or large PDP-11 with 512KB+ RAM
C     Memory: ~70 KB (after REAL fix)
C
C     Features: Maximum capacity, suitable for production use
C
C     Last Updated: 2026-01-19
C
C======================================================================
C     Array Size Parameters
C======================================================================

C     Cell storage
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=2000)    ! Maximum cells
      PARAMETER (HASHSZ=1024)    ! Hash table size
      PARAMETER (MAXSTR=10000)   ! Formula pool size

C     Dependency graph
      INTEGER MAXDEP, DEPHSZ, MAXQUE
      PARAMETER (MAXDEP=1000)    ! Maximum dependency nodes
      PARAMETER (DEPHSZ=256)     ! Dependency hash size
      PARAMETER (MAXQUE=500)     ! Recalc queue size

C     Parser and evaluator
      INTEGER MAXTOK, MAXSTK, MAXDPS
      PARAMETER (MAXTOK=100)     ! Maximum tokens per formula
      PARAMETER (MAXSTK=50)      ! Operator stack size
      PARAMETER (MAXDPS=100)     ! Maximum dependencies per cell

C======================================================================
C     Memory Calculation (16-bit integers, 2 bytes each)
C======================================================================
C
C     CELLA(2000, 7)     = 28,000 bytes (28 KB)  - Cell storage
C     CELLV(2000)        =  8,000 bytes ( 8 KB)  - REAL values
C     CELLR(2000)        =  8,000 bytes ( 8 KB)  - REAL results
C     FMLPOL(10000)      = 20,000 bytes (20 KB)  - Formula pool
C     DEPNOD(1000, 5)    = 10,000 bytes (10 KB)  - Dependency graph
C     HTABLE(1024)       =  2,048 bytes ( 2 KB)  - Hash table
C     QUEUE(500, 2)      =  2,000 bytes ( 2 KB)  - Recalc queue
C     Other arrays       =  ~5,000 bytes ( 5 KB) - Misc buffers
C     ─────────────────────────────────────────
C     Total Data:        = ~83 KB
C     Estimated Code:    = ~20 KB
C     ─────────────────────────────────────────
C     Total Program:     = ~103 KB
C
C     Too large for CP/M (48KB) or small PDP-11 (64KB)
C     Suitable for large systems only
C
C======================================================================
