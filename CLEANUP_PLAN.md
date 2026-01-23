# Code Cleanup Plan: Identifying Zombie Code and Unnecessary Complexity

## Objective

Systematically identify and remove:
- Dead code (never executed)
- Unused routines (defined but never called)
- Redundant complexity (achieves little, costs many lines)
- Duplicate logic (same code in multiple places)

## Phase 1: Static Call Analysis

### 1.1 Find Uncalled Subroutines

```bash
# List all SUBROUTINE definitions
grep -h "SUBROUTINE" native/layer*/*.FOR | sed 's/.*SUBROUTINE //' | cut -d'(' -f1 | sort -u > /tmp/defined_subs.txt

# List all CALL statements
grep -h "CALL " native/layer*/*.FOR | sed 's/.*CALL //' | cut -d'(' -f1 | sort -u > /tmp/called_subs.txt

# Find defined but never called
comm -23 /tmp/defined_subs.txt /tmp/called_subs.txt
```

### 1.2 Find Uncalled Functions

```bash
# List all FUNCTION definitions
grep -h "FUNCTION" native/layer*/*.FOR | grep -v "^C" | sed 's/.*FUNCTION //' | cut -d'(' -f1 | sort -u > /tmp/defined_funcs.txt

# Search for each function being used (harder - need to check usage context)
```

### 1.3 Find Unused COMMON Blocks

```bash
# List all COMMON block definitions
grep -h "COMMON /" native/layer*/*.FOR | sed 's/.*COMMON //' | cut -d'/' -f2 | sort | uniq -c | sort -n
```

## Phase 2: Complexity Analysis

### 2.1 Large Routines (Candidates for Splitting or Simplification)

```bash
# Find routines > 100 lines (rough heuristic)
awk '/SUBROUTINE|FUNCTION/{name=$2; count=0} {count++} /^      END/{if(count>100) print name, count}' native/layer*/*.FOR
```

### 2.2 Deeply Nested Code

Look for:
- Multiple levels of IF/THEN/ELSE
- GO TO spaghetti patterns
- Complex conditional chains

### 2.3 Duplicate Code Patterns

```bash
# Find similar 5-line blocks (manual inspection needed)
# Look for copy-paste patterns in:
# - Error handling sequences
# - Buffer manipulation
# - Output formatting
```

## Phase 3: Semantic Analysis

### 3.1 Unreachable Code

Look for:
- Code after unconditional RETURN or GO TO
- Conditions that are always true/false
- Dead branches in IF statements

### 3.2 Defensive Code That Can't Fail

Look for:
- Array bounds checks on fixed-size operations
- Null checks on values that are always set
- Error handling for errors that can't occur

### 3.3 Over-Parameterized Routines

Find routines where:
- Parameters are passed but never used
- Parameters are always the same value
- Return values are never checked

## Phase 4: Architectural Review

### 4.1 Layer Violations

Check for:
- Layer 2 code that belongs in Layer 1
- Layer 1 code that duplicates Layer 0 functionality
- Routines in wrong modules

### 4.2 Abstraction Issues

Look for:
- Wrapper routines that add no value
- Indirection that serves no purpose
- Interfaces that are never used polymorphically

### 4.3 Feature Bloat

Review each feature:
- Is it actually used?
- Does it justify its line count?
- Could it be simpler?

## Phase 5: Execution

### Priority Order

1. **Quick wins**: Uncalled routines (Phase 1) - automated detection
2. **Medium effort**: Duplicate code (Phase 2.3) - manual review
3. **Higher effort**: Complexity reduction (Phase 2.1, 2.2) - refactoring

### Validation

After each removal:
1. Rebuild all configurations (small/medium/large)
2. Run test suite
3. Manual smoke test of core features

### Documentation

Track all removals:
- What was removed
- Why it was unnecessary
- Lines saved

## Specific Areas to Investigate

Based on the codebase structure, investigate:

| Area | File(s) | Concern |
|------|---------|---------|
| Multiple terminal drivers | layer3/TERM*.FOR | Are all variants needed? |
| Multiple file formats | FILEBIN, FILESAV, FILELOAD | Do we need both JSON and binary? |
| String utilities | STRUTIL.FOR | Any unused helpers? |
| Display routines | DISPLAY.FOR | Redundant rendering paths? |
| Parse utilities | PARSE.FOR | Unused parsing helpers? |

## Success Metrics

| Metric | Current | Target |
|--------|---------|--------|
| Total lines | 10,355 | < 9,000 |
| Active lines | 5,893 | < 5,000 |
| Uncalled routines | ? | 0 |
| Duplicate code blocks | ? | 0 |

## Analysis Results (2026-01-23)

### Phase 1 Results: No Uncalled Routines Found

All 192 defined subroutines and 13 functions are referenced somewhere.
The stub cleanup already removed the dead code (FILES.FOR, UI stubs, MSG stub).

### Phase 2 Results: Complexity Candidates

**Large Routines (>100 lines):**

| Routine | Lines | File | Notes |
|---------|------:|------|-------|
| CMDKEY | 487 | COMMANDS.FOR | Command dispatcher - could be split |
| ENTKEY | 227 | UI.FOR | Entry mode handler |
| EVAL | 187 | EVAL.FOR | Expression evaluator - complex by nature |
| FLBSAV | 139 | FILEBIN.FOR | Binary save |
| FLSAVE | 118 | FILESAV.FOR | JSON save |
| RDHLP | 115 | RENDVT.FOR | Help screen renderer |
| MSGGET | 114 | MSG.FOR | Message lookup - linear search |
| PARSE | 108 | PARSE.FOR | Formula parser - complex by nature |

**Recommendation:** CMDKEY (487 lines) is the top candidate for decomposition.

### Phase 3 Results: Potential Optimizations

**Repeated Escape Sequence Pattern:**
```fortran
CALL IOPUTC(27)    ! ESC - appears 22 times
CALL IOPUTC(91)    ! [   - appears 11 times
```

Could create helper: `CALL IOESC()` to output ESC[ in one call, saving ~20 lines.

**Duplicate Initialization Calls:**
Several `xxxINI` routines are very short (3-6 lines) but serve the abstraction.
These are acceptable architectural overhead.

### Recommendations Priority

1. **CMDKEY decomposition** - Split 487-line routine into:
   - Command parser (get command name)
   - Individual command handlers (QUIT, SAVE, LOAD, COPY, etc.)
   - Estimated savings: Better maintainability, possibly +50 lines but much cleaner

2. **Escape sequence helper** - Create IOESC() routine
   - Estimated savings: ~20 lines
   - Reduced call overhead

3. **Consider removing JSON format** - Keep only binary format
   - FILESAV.FOR: 599 lines
   - FILELOAD.FOR: 673 lines
   - Potential savings: ~1,200 lines if JSON removed
   - Trade-off: Binary files not human-readable

## Next Steps

1. ~~Run Phase 1 scripts to identify uncalled code~~ Done - no dead code found
2. ~~Review results and categorize~~ Done - see above
3. Decide on CMDKEY refactoring approach
4. Decide on JSON vs binary-only file format
5. Update development analysis with new metrics
