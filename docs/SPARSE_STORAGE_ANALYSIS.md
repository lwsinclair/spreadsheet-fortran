# Sparse Storage Analysis for XL Spreadsheet

**Date**: 2026-01-19
**Author**: Analysis for CP-V and CP/M Port Memory Optimization
**Current Status**: CP-V implementation complete, CP/M port planning phase

---

## Executive Summary

**Key Finding**: The current hash table implementation is ALREADY a sparse storage approach and is near-optimal for the target platforms. Alternative sparse storage methods (linked lists, sorted arrays) would provide minimal or negative memory benefits while significantly complicating assembly language conversion.

**Recommendation**: **Keep the current hash table implementation**. For CP/M port memory reduction, focus on reducing array sizes (MAXCEL, MAXSTR, HASHMD) rather than changing the storage algorithm.

**Memory Impact Summary**:

- Current implementation: 56 KB for cell storage (2000 cells)
- Linked list approach: 52-56 KB (minimal savings, significant complexity)
- Sorted array approach: 48-56 KB (moderate savings, severe performance penalty)
- Hash table with reduced limits: 14 KB (500 cells) - **Best for CP/M**

---

## 1. Current Implementation Analysis

### 1.1 Hash Table with Chaining

**Data Structure** (from `/Volumes/SECURE8/git/spreadsheet-fortran/src/layer1/CELLS.FOR`):

```fortran
INTEGER CELLA(2000, 7)    ! Cell array - 56 KB
INTEGER HTABLE(1024)      ! Hash table - 4 KB
INTEGER FMLPOL(10000)     ! Formula pool - 40 KB
INTEGER FMLLEN(2000)      ! Formula lengths - 8 KB
REAL CELLV(2000)          ! Cell values - 8 KB
REAL CELLR(2000)          ! Formula results - 8 KB
```

**Total Cell Storage**: 124 KB (56 KB for CELLA alone)

**Storage Details**:

- Each cell slot: 7 integers + 2 reals = 36 bytes
- Maximum cells: 2000
- Hash buckets: 1024 (average chain length ~2 when full)
- Only non-empty cells are stored (sparse)
- Free list for deleted cells (memory reuse)

**Key Characteristics**:

1. **Already Sparse**: Empty cells cost nothing (type field = 0)
2. **Efficient Lookup**: O(1) average case, O(n) worst case
3. **Memory Overhead**: 4 bytes per hash bucket + 4 bytes per cell (NEXT pointer)
4. **Insertion/Deletion**: O(1) average case

### 1.2 Current Memory Breakdown

From PROJECT_STATUS.md:

```
Layer 1 (Calc Engine):     ~135 KB
  - Cell storage (CELLA):     56 KB
  - Cell values (CELLV):       8 KB
  - Cell results (CELLR):      8 KB
  - Hash table (HTABLE):       4 KB
  - Formula pool (FMLPOL):    40 KB
  - Formula lengths (FMLLEN):  8 KB
  - Dependency graph:          10 KB
  - Other buffers:              1 KB

Total Application:         ~142 KB
```

**Platform Fit**:

- CP-V (512 KB available): Comfortable (28% utilization)
- CP/M (40 KB available): **Does not fit** (355% over budget)

---

## 2. Alternative Sparse Storage Approaches

### 2.1 Linked List Storage

**Concept**: Store cells as a single linked list, no hash table.

**Data Structure**:

```fortran
INTEGER CELLST(2000, 8)   ! Cell storage with NEXT pointer
! Column layout:
! (1) COL
! (2) ROW
! (3) TYPE
! (4) FMLIDX
! (5) FLAGS
! (6) UNUSED (was RESULT, now in CELLR)
! (7) UNUSED (was NEXT for hash, now for global list)
! (8) PREV (for doubly-linked list - optional)

REAL CELLV(2000)          ! Cell values
REAL CELLR(2000)          ! Formula results
INTEGER LSTHD             ! List head pointer
INTEGER LSTTL             ! List tail pointer
```

**Memory Analysis**:

*Single-Linked List*:

- CELLST: 2000 × 8 × 4 = 64 KB (vs 56 KB current)
- No hash table: -4 KB
- CELLV: 8 KB (unchanged)
- CELLR: 8 KB (unchanged)
- **Total: 80 KB vs 76 KB current = +4 KB (WORSE!)**

*Doubly-Linked List (for faster deletion)*:

- CELLST: 2000 × 9 × 4 = 72 KB
- **Total: 88 KB vs 76 KB current = +12 KB (MUCH WORSE!)**

**Performance**:

- Lookup: O(n) - **Must traverse list** (vs O(1) hash)
- Insert: O(1) at head/tail, O(n) if sorted
- Delete: O(n) to find, O(1) to remove
- Average lookup with 100 cells: 50 comparisons vs 1-2 for hash

**Assembly Conversion**:

- Simpler: No hash function
- More complex: Pointer manipulation, list traversal
- **Overall**: Roughly equal complexity

**Verdict**: **REJECTED** - Worse memory, much worse performance

### 2.2 Sorted Array Storage

**Concept**: Store cells in a sorted array by (row, col), use binary search.

**Data Structure**:

```fortran
INTEGER CELLST(2000, 6)   ! Sorted cell array (no NEXT pointer needed)
! Sorted by: ROW * 256 + COL
REAL CELLV(2000)
REAL CELLR(2000)
INTEGER CELCNT            ! Current cell count
```

**Memory Analysis**:

- CELLST: 2000 × 6 × 4 = 48 KB (vs 56 KB current) - **SAVES 8 KB**
- No hash table: -4 KB
- CELLV: 8 KB (unchanged)
- CELLR: 8 KB (unchanged)
- **Total: 64 KB vs 76 KB current = -12 KB (BETTER!)**

**Performance**:

- Lookup: O(log n) - Binary search (vs O(1) hash)
- Insert: O(n) - Must shift array (vs O(1) hash)
- Delete: O(n) - Must shift array (vs O(1) hash)
- Average with 100 cells:
  - Lookup: 7 comparisons vs 1-2 for hash
  - Insert: 50 moves (200 bytes shifted) vs 0 for hash

**Assembly Conversion**:

- Simpler: No hash function, no linked lists
- More complex: Binary search, array shifting
- **Overall**: Slightly simpler than hash table

**Use Case Fit**:

- Good for: Read-heavy workloads, infrequent updates
- Bad for: Frequent cell updates (spreadsheet editing!)
- Typical spreadsheet: Many updates during entry, then mostly reads

**Verdict**: **CONDITIONALLY ACCEPTABLE** - Memory savings, but performance penalty during editing. Only viable for CP/M where memory is critical.

### 2.3 Hybrid: Small Hash Table + Overflow List

**Concept**: Use smaller hash table with longer chains.

**Data Structure**:

```fortran
INTEGER CELLA(2000, 7)    ! Same as current
INTEGER HTABLE(256)       ! 1/4 size hash table
REAL CELLV(2000)
REAL CELLR(2000)
```

**Memory Analysis**:

- CELLA: 56 KB (unchanged)
- HTABLE: 256 × 4 = 1 KB (vs 4 KB current) - **SAVES 3 KB**
- CELLV/CELLR: 16 KB (unchanged)
- **Total: 73 KB vs 76 KB current = -3 KB (MARGINAL)**

**Performance**:

- Average chain length: ~8 when full (vs ~2 current)
- Lookup: Still O(1) average, but 4× slower
- Insert/Delete: O(1) average, 4× slower

**Assembly Conversion**:

- Identical complexity to current approach

**Verdict**: **ACCEPTABLE FOR CP/M** - Small memory savings, acceptable performance degradation

### 2.4 Pure Array (No Sparse Storage)

**Concept**: Allocate full grid, store every cell.

**Data Structure**:

```fortran
INTEGER CELLTP(64, 256)   ! Type array: 64 KB
REAL CELLV(64, 256)       ! Value array: 64 KB
INTEGER FMLIDX(64, 256)   ! Formula indices: 64 KB
! Total: 192 KB for 16,384 possible cells
```

**Verdict**: **REJECTED IMMEDIATELY** - 192 KB vs 76 KB current. Not sparse at all.

---

## 3. CP/M Port Strategy

### 3.1 Memory Budget

**CP/M Available Memory**: ~40 KB (from xl-spec.md)

**Current Implementation**: 142 KB total

**Required Reduction**: 102 KB (72% reduction needed)

### 3.2 Recommended Approach: Scale Down Current Implementation

**Strategy**: Reduce array dimensions, keep hash table algorithm.

**Proposed Constants**:

```fortran
C     CP/M Configuration
      MAXCEL = 500      ! Max cells (vs 2000)
      MAXSTR = 2500     ! Formula pool (vs 10000)
      HASHMD = 256      ! Hash buckets (vs 1024)
      MAXDEP = 250      ! Dependency nodes (vs 1000)
      MAXQUE = 100      ! Recalc queue (vs 500)
```

**Memory Calculation**:

```
CELLS.FOR (CP/M):
  CELLA:     500 cells × 7 columns × 4 bytes = 14 KB  (was 56 KB)
  HTABLE:    256 buckets × 4 bytes          =  1 KB  (was 4 KB)
  CELLV:     500 × 4 bytes                  =  2 KB  (was 8 KB)
  CELLR:     500 × 4 bytes                  =  2 KB  (was 8 KB)
  FMLPOL:    2500 integers × 4 bytes        = 10 KB  (was 40 KB)
  FMLLEN:    500 integers × 4 bytes         =  2 KB  (was 8 KB)
  Subtotal:                                   31 KB  (was 124 KB)

DEPS.FOR (CP/M):
  DEPNOD:    250 × 5 × 4 bytes              =  5 KB  (was 20 KB)
  DEPHT:     128 × 4 bytes                  =  0.5 KB (was 1 KB)
  Subtotal:                                   5.5 KB (was 21 KB)

RECALC.FOR (CP/M):
  QUEUE:     100 × 2 × 4 bytes              =  0.8 KB (was 4 KB)

Other modules:                                2 KB  (unchanged)

TOTAL (CP/M):                                ~39 KB (was 142 KB)
```

**Result**: Fits in 40 KB budget with minimal headroom (97.5% utilization)

**Grid Capacity**:

- 500 cells in a 26 × 64 grid (1664 possible cells)
- 30% fill capacity (reasonable for CP/M era)
- Matches xl-spec.md recommendation: 26 cols × 64 rows for CP/M

### 3.3 Alternative: Sorted Array for CP/M

If 39 KB is still too tight (OS overhead, stack, etc.), consider sorted array:

**Memory**:

```
CELLST:    500 × 6 × 4 bytes              = 12 KB  (vs 14 KB hash)
CELLV:     500 × 4 bytes                  =  2 KB
CELLR:     500 × 4 bytes                  =  2 KB
FMLPOL:    2500 integers × 4 bytes        = 10 KB
FMLLEN:    500 integers × 4 bytes         =  2 KB
DEPS/RECALC (scaled):                      6 KB
Other:                                     2 KB

TOTAL:                                    ~36 KB (saves 3 KB vs hash)
```

**Trade-off**: 3 KB memory savings for slower cell updates. Acceptable for CP/M where memory is more critical than speed.

---

## 4. Assembly Language Conversion Implications

### 4.1 Hash Table (Current Approach)

**Complexity**:

- Hash function: Moderate (multiply, modulo, bounds check)
- Linked list traversal: Simple (pointer following)
- Insert/delete: Moderate (pointer manipulation)

**Assembly Code Estimate**: ~150 lines for core operations

**Benefits**:

- Well-understood algorithm
- Predictable performance
- Minimal pointer manipulation

**Challenges**:

- Hash function requires multiplication and division (slow on 8080)
- Modulo operation can be optimized with power-of-2 hash table size

### 4.2 Linked List

**Complexity**:

- Traversal: Simple (pointer following)
- Insert/delete: Simple (pointer manipulation)
- No hash function: Simpler

**Assembly Code Estimate**: ~100 lines for core operations

**Benefits**:

- Simpler than hash table (no hash function)
- Straightforward pointer operations

**Challenges**:

- O(n) lookup requires loop implementation
- More memory accesses (cache unfriendly on modern ports)

### 4.3 Sorted Array

**Complexity**:

- Binary search: Moderate (loop with arithmetic)
- Array shifting: Simple but verbose (byte-copy loops)
- No pointers: Simpler

**Assembly Code Estimate**: ~120 lines for core operations

**Benefits**:

- No pointers (simpler memory model)
- Binary search is well-understood
- Better cache locality (on modern ports)

**Challenges**:

- Array shifting is slow (many memory moves)
- Binary search requires careful index arithmetic

### 4.4 Recommendation for Assembly Conversion

**For CP-V (512 KB)**: Hash table (current) - best performance
**For CP/M (40 KB)**: Hash table with reduced arrays - good balance
**For extreme memory pressure**: Sorted array - if every KB counts

**Rationale**: Hash table complexity is acceptable for the performance benefits. The hash function can be optimized in assembly (use bit shifts for power-of-2 modulo). The code structure is already well-tested in FORTRAN, making conversion lower risk.

---

## 5. Performance Comparison

### 5.1 Lookup Performance

Assume 100 cells stored:

| Method              | Best Case    | Average Case    | Worst Case      | Notes               |
| ------------------- | ------------ | --------------- | --------------- | ------------------- |
| Hash (1024 buckets) | 1 comparison | 1-2 comparisons | ~10 comparisons | Hash collision rare |
| Hash (256 buckets)  | 1 comparison | 2-4 comparisons | ~15 comparisons | More collisions     |
| Linked List         | 1 comparison | 50 comparisons  | 100 comparisons | Linear search       |
| Sorted Array        | 1 comparison | 7 comparisons   | 7 comparisons   | Binary search       |

**Winner**: Hash table (current)

### 5.2 Insert Performance

| Method                 | Best Case | Average Case | Worst Case        | Notes              |
| ---------------------- | --------- | ------------ | ----------------- | ------------------ |
| Hash (any size)        | O(1)      | O(1)         | O(n) on collision | Prepend to chain   |
| Linked List (unsorted) | O(1)      | O(1)         | O(1)              | Insert at head     |
| Linked List (sorted)   | O(1)      | O(n)         | O(n)              | Must find position |
| Sorted Array           | O(1) move | O(n) moves   | O(n) moves        | Shift array        |

With 100 cells, sorted array insert averages 50 cell moves (1800 bytes shifted).

**Winner**: Hash table (current)

### 5.3 Delete Performance

| Method       | Best Case | Average Case | Worst Case | Notes            |
| ------------ | --------- | ------------ | ---------- | ---------------- |
| Hash         | O(1)      | O(1)         | O(n)       | Find + unlink    |
| Linked List  | O(1) find | O(n) find    | O(n) find  | Then O(1) unlink |
| Sorted Array | O(1) move | O(n) moves   | O(n) moves | Shift array      |

**Winner**: Hash table (current)

### 5.4 Memory Efficiency (with 100 active cells)

| Method                          | Memory Used | Overhead       | Utilization |
| ------------------------------- | ----------- | -------------- | ----------- |
| Hash (1024 buckets, 2000 slots) | 76 KB       | 73.7 KB unused | 3%          |
| Hash (256 buckets, 500 slots)   | 19 KB       | 15.4 KB unused | 19%         |
| Linked List (2000 slots)        | 80 KB       | 76.8 KB unused | 4%          |
| Sorted Array (500 slots, tight) | 16 KB       | 12.4 KB unused | 22%         |

For sparse usage (100 cells), all methods have low utilization. The memory difference matters only for the total array size allocation.

**Winner for CP/M**: Sorted array (if memory critical)
**Winner for CP-V**: Hash table (abundant memory, best performance)

---

## 6. Recommendations by Platform

### 6.1 CP-V (Xerox Sigma, 512 KB)

**Recommendation**: **Keep current hash table implementation**

**Rationale**:

- Memory is abundant (142 KB / 512 KB = 28% utilization)
- Performance is critical for interactive feel
- Hash table provides best user experience
- Code is already written and tested

**Configuration**:

```fortran
MAXCEL = 2000
HASHMD = 1024
MAXSTR = 10000
```

### 6.2 CP/M (Intel 8080, 40 KB)

**Primary Recommendation**: **Hash table with reduced arrays**

**Configuration**:

```fortran
MAXCEL = 500
HASHMD = 256
MAXSTR = 2500
MAXDEP = 250
MAXQUE = 100
```

**Memory**: 39 KB (fits in 40 KB budget)
**Grid**: 26 columns × 64 rows (1664 possible cells, 30% fillable)
**Performance**: Acceptable (2-4× slower lookup than CP-V, but still interactive)

**Fallback Recommendation**: **Sorted array if memory headroom needed**

**Configuration**:

```fortran
MAXCEL = 500
MAXSTR = 2500
! No hash table
```

**Memory**: 36 KB (3 KB savings)
**Performance**: Slower editing (array shifting), but acceptable for CP/M era expectations

### 6.3 PDP-11 (if targeted)

The user mentioned PDP-11, but I don't see it in the specification. If targeting:

**RT-11 or RSX-11**: Similar to CP-V approach (ample memory)
**Tiny systems**: Similar to CP/M approach (constrained memory)

---

## 7. Implementation Roadmap

### 7.1 Phase 1: CP-V Release (Current)

**Action**: No changes needed
**Status**: Implementation complete (97 tests passing)
**Memory**: 142 KB / 512 KB (28% utilization)

### 7.2 Phase 2: CP/M Port Planning

**Actions**:

1. Create CELLS_CPM.FOR with reduced array sizes
2. Test with 500-cell limit
3. Benchmark insert/lookup performance
4. If performance acceptable, ship it
5. If memory still tight, implement sorted array variant

**Estimated Effort**: 2-3 days for hash table variant, 5-7 days for sorted array

### 7.3 Phase 3: Assembly Conversion (Future)

**Recommended Order**:

1. Convert CP-V version first (hash table, generous memory)
2. Validate performance and correctness
3. Port to CP/M with reduced arrays (same algorithm)
4. Consider sorted array only if memory absolutely critical

**Rationale**: Minimize algorithm variants. Test one approach thoroughly before introducing alternatives.

---

## 8. Technical Deep Dive

### 8.1 Hash Function Optimization for Assembly

Current hash function (from CELLS.FOR):

```fortran
CELHSH = MOD(COL * 257 + ROW, 1024)
```

**Assembly Optimization** (for power-of-2 modulo):

```assembly
; Assume COL in register A, ROW in register B
; Result in HL register pair

; COL * 257 = COL * (256 + 1) = (COL << 8) + COL
MOV H, A        ; H = COL (high byte)
MOV L, A        ; L = COL (low byte)
; HL now = (COL << 8) + COL = COL * 257

; Add ROW
MOV A, L
ADD B           ; A = L + ROW
MOV L, A        ; HL = COL * 257 + ROW (with overflow into H)

; Modulo 1024 = keep low 10 bits = AND with 0x03FF
MOV A, H
ANI 03H         ; Keep bits 8-9 (high 2 bits of 10-bit value)
MOV H, A        ; HL = (COL * 257 + ROW) & 0x3FF
```

**Performance**: ~10 instructions vs division (30-50 instructions on 8080)

### 8.2 Binary Search Implementation Notes

For sorted array approach:

```fortran
C     Binary search for cell
      FUNCTION CELSRCH(COL, ROW)
      INTEGER COL, ROW
      INTEGER CELLST(500, 6)
      COMMON /CELDAT/ CELLST, CELCNT

      INTEGER LOW, HIGH, MID, KEY, MIDKEY

C     Compute search key: ROW * 256 + COL
      KEY = ROW * 256 + COL

      LOW = 1
      HIGH = CELCNT

100   IF (LOW .GT. HIGH) GO TO 900

      MID = (LOW + HIGH) / 2
      MIDKEY = CELLST(MID, 2) * 256 + CELLST(MID, 1)

      IF (MIDKEY .EQ. KEY) GO TO 800
      IF (MIDKEY .LT. KEY) GO TO 200

C     Search lower half
      HIGH = MID - 1
      GO TO 100

C     Search upper half
200   LOW = MID + 1
      GO TO 100

C     Found
800   CELSRCH = MID
      RETURN

C     Not found
900   CELSRCH = 0
      RETURN
      END
```

**Assembly**: Straightforward translation, ~40 instructions per iteration.

### 8.3 Memory Layout Comparison

**Hash Table** (current):

```
+----------------+
| CELLA(2000,7)  |  56 KB - Cell data (sparse, indexed by slot)
+----------------+
| HTABLE(1024)   |   4 KB - Hash table (pointers to cell slots)
+----------------+
| CELLV(2000)    |   8 KB - Cell values (parallel array)
+----------------+
| CELLR(2000)    |   8 KB - Formula results (parallel array)
+----------------+
| FMLPOL(10000)  |  40 KB - Formula token pool
+----------------+
| FMLLEN(2000)   |   8 KB - Formula lengths
+----------------+

Total: 124 KB
Fragmentation: Low (all static arrays)
Access pattern: Random (hash-based)
```

**Sorted Array**:

```
+----------------+
| CELLST(500,6)  |  12 KB - Sorted cell data (ROW*256+COL order)
+----------------+
| CELLV(500)     |   2 KB - Cell values (parallel, sorted)
+----------------+
| CELLR(500)     |   2 KB - Formula results (parallel, sorted)
+----------------+
| FMLPOL(2500)   |  10 KB - Formula token pool
+----------------+
| FMLLEN(500)    |   2 KB - Formula lengths (parallel, sorted)
+----------------+

Total: 28 KB
Fragmentation: Low (all static arrays)
Access pattern: Sequential (sorted order)
Cache efficiency: Higher (sequential access during search)
```

---

## 9. Risk Analysis

### 9.1 Hash Table Risks

**Risk**: Hash collisions with small table (256 buckets for CP/M)
**Likelihood**: Medium
**Impact**: Moderate (longer chains, slower lookup)
**Mitigation**: Test with realistic data, tune hash function if needed

**Risk**: Multiplication/modulo slow on 8080
**Likelihood**: High
**Impact**: Low (still faster than linear search)
**Mitigation**: Optimize in assembly with bit shifts

### 9.2 Sorted Array Risks

**Risk**: Slow array shifting during editing
**Likelihood**: High
**Impact**: High (noticeable lag when entering cells)
**Mitigation**: Accept as trade-off for memory savings, or add "dirty" flag and defer compaction

**Risk**: Complex insertion logic
**Likelihood**: Medium
**Impact**: Medium (bugs during development)
**Mitigation**: Extensive testing, incremental development

### 9.3 CP/M Port Risks

**Risk**: 39 KB still too large (OS overhead)
**Likelihood**: Medium
**Impact**: High (doesn't fit)
**Mitigation**: Implement sorted array variant, reduce limits further

**Risk**: Performance unacceptable on slow 8080
**Likelihood**: Low (calculations are simple)
**Impact**: Medium (poor user experience)
**Mitigation**: Profile on emulator, optimize hot paths

---

## 10. Conclusion

### 10.1 Summary of Findings

1. **Current implementation is already sparse**: Only non-empty cells consume memory
2. **Hash table is near-optimal**: Best performance, acceptable memory for CP-V
3. **Linked lists are worse**: More memory, worse performance
4. **Sorted arrays save memory**: 12 KB savings, but performance penalty
5. **For CP/M**: Reduce array sizes, keep hash table algorithm
6. **Assembly conversion**: Hash table is acceptable complexity

### 10.2 Final Recommendation

**For CP-V (Current)**: ✅ **KEEP CURRENT HASH TABLE IMPLEMENTATION**

**For CP/M Port**: ✅ **USE HASH TABLE WITH REDUCED ARRAYS**

- MAXCEL = 500, HASHMD = 256
- Memory: 39 KB (fits in 40 KB)
- Performance: Acceptable degradation
- Fallback: Sorted array if 3 KB savings critical

**For Assembly Conversion**: ✅ **CONVERT HASH TABLE APPROACH**

- Optimize hash function with bit shifts
- Keep same algorithm across platforms
- Lower risk, proven design

### 10.3 Memory Savings Potential

| Approach                   | CP-V Memory | CP/M Memory | Savings vs Current | Performance |
| -------------------------- | ----------- | ----------- | ------------------ | ----------- |
| Current (2000 cells, hash) | 142 KB      | N/A         | Baseline           | Excellent   |
| Reduced hash (500 cells)   | N/A         | 39 KB       | 103 KB             | Good        |
| Sorted array (500 cells)   | N/A         | 36 KB       | 106 KB             | Fair        |
| Linked list (500 cells)    | N/A         | 41 KB       | 101 KB             | Poor        |

### 10.4 Action Items

**Immediate** (for current CP-V development):

- ✅ No changes needed
- ✅ Current implementation is optimal

**For CP/M Port** (future):

1. Create CELLS_CPM.FOR with reduced constants
2. Test with 500-cell limit
3. Benchmark on CP/M emulator
4. Ship hash table variant if acceptable
5. Implement sorted array only if necessary

**For Assembly Conversion** (future):

1. Convert hash table version first
2. Validate on CP-V (ample memory for debugging)
3. Port to CP/M with same algorithm
4. Optimize hash function in assembly

---

## Appendix A: Memory Size Reference

**CP-V (Xerox Sigma 7)**:

- Available: 512 KB
- Current usage: 142 KB (28%)
- Headroom: 370 KB (72%)

**CP/M (Intel 8080)**:

- Available: ~40 KB (from xl-spec.md)
- Proposed usage: 39 KB (97.5%)
- Headroom: ~1 KB (2.5%)

**Why CP/M is tight**:

- TPA (Transient Program Area): ~48 KB typical
- BDOS/CCP overhead: ~8 KB
- Stack space needed: ~1-2 KB
- Net available: ~40 KB for application

---

## Appendix B: Code References

**Current Implementation**:

- `/Volumes/SECURE8/git/spreadsheet-fortran/src/layer1/CELLS.FOR` (525 lines)
- Hash function: Line 84-97 (CELHSH)
- Cell lookup: Line 102-144 (CELFND)
- Cell storage: Line 189-242 (CELPUT)

**Documentation**:

- `/Volumes/SECURE8/git/spreadsheet-fortran/docs/calc-engine-docs.md` (Section 6.2)
- `/Volumes/SECURE8/git/spreadsheet-fortran/docs/PROJECT_STATUS.md` (Memory Usage section)
- `/Volumes/SECURE8/git/spreadsheet-fortran/xl-spec.md` (Section 3.2, CP/M platform)

---

**End of Analysis**
