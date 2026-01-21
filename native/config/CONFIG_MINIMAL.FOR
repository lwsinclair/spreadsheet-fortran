C     CONFIG_MINIMAL.FOR - Minimal Configuration for Tiny Systems
C
C     Target: Educational systems, embedded systems, memory-constrained environments
C     Memory: ~12 KB total
C
C     Features: Minimal capacity, demonstration/learning purposes
C
C     Capacity:
C       - 100 cells (e.g., 10 columns x 10 rows)
C       - 50 formulas with dependencies
C       - 500 character formula pool
C
C     Last Updated: 2026-01-19
C
C======================================================================
C     Array Size Parameters
C======================================================================

C     Cell storage
      INTEGER MAXCEL, HASHSZ, MAXSTR
      PARAMETER (MAXCEL=100)     ! Maximum cells (minimal)
      PARAMETER (HASHSZ=64)      ! Hash table size (minimal)
      PARAMETER (MAXSTR=500)     ! Formula pool size (minimal)

C     Dependency graph
      INTEGER MAXDEP, DEPHSZ, MAXQUE
      PARAMETER (MAXDEP=50)      ! Maximum dependency nodes (minimal)
      PARAMETER (DEPHSZ=32)      ! Dependency hash size (minimal)
      PARAMETER (MAXQUE=25)      ! Recalc queue size (minimal)

C     Parser and evaluator
      INTEGER MAXTOK, MAXSTK, MAXDPS
      PARAMETER (MAXTOK=25)      ! Maximum tokens per formula (minimal)
      PARAMETER (MAXSTK=15)      ! Operator stack size (minimal)
      PARAMETER (MAXDPS=25)      ! Maximum dependencies per cell (minimal)

C======================================================================
C     Memory Calculation (16-bit integers, 2 bytes each)
C======================================================================
C
C     CELLA(100, 7)      =  1,400 bytes ( 1 KB)  - Cell storage
C     CELLV(100)         =    400 bytes ( 0.4 KB) - REAL values
C     CELLR(100)         =    400 bytes ( 0.4 KB) - REAL results
C     FMLPOL(500)        =  1,000 bytes ( 1 KB)  - Formula pool
C     DEPNOD(50, 5)      =    500 bytes ( 0.5 KB) - Dependency graph
C     HTABLE(64)         =    128 bytes ( 0.1 KB) - Hash table
C     QUEUE(25, 2)       =    100 bytes ( 0.1 KB) - Recalc queue
C     Other arrays       =  ~1,000 bytes ( 1 KB) - Misc buffers
C     ─────────────────────────────────────────
C     Total Data:        = ~5 KB
C     Estimated Code:    = ~20 KB
C     ─────────────────────────────────────────
C     Total Program:     = ~25 KB
C
C     Extremely compact, suitable for very constrained systems
C     Good for learning, demonstrations, embedded applications
C
C======================================================================
