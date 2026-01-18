C     CONFIG.FOR - Array Size Configuration Parameters
C
C     This file defines array size constants used throughout the application.
C     To build for different targets, copy the appropriate CONFIG_*.FOR file
C     to CONFIG.FOR before compiling.
C
C     Build targets:
C       - CONFIG_FULL.FOR    -> Large systems (CP-V, big PDP-11): ~103 KB
C       - CONFIG_CPM.FOR     -> CP/M systems: ~36 KB
C       - CONFIG_MINIMAL.FOR -> Tiny systems: ~25 KB
C
C     DEFAULT: Using FULL configuration
C
C     Last Updated: 2026-01-19
C
C======================================================================
C     Array Size Parameters (FULL Configuration)
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
C     How to Change Configuration
C======================================================================
C
C     To build for CP/M:
C       cp src/config/CONFIG_CPM.FOR src/config/CONFIG.FOR
C       make clean
C       make
C
C     To build for minimal systems:
C       cp src/config/CONFIG_MINIMAL.FOR src/config/CONFIG.FOR
C       make clean
C       make
C
C     To restore full configuration:
C       cp src/config/CONFIG_FULL.FOR src/config/CONFIG.FOR
C       make clean
C       make
C
C======================================================================
