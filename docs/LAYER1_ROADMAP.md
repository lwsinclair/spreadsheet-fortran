# Layer 1: Calculation Engine - Implementation Roadmap

**Timeline**: Weeks 3-5 (3 weeks)
**Status**: Starting
**Dependencies**: Layer 0 (STRUTIL.FOR) ✅ Complete

---

## Overview

Layer 1 is the core calculation engine that makes XL a spreadsheet. It handles:
- Cell storage and retrieval
- Formula parsing
- Expression evaluation
- Dependency tracking
- Automatic recalculation

---

## Components

### 1. CELLS.FOR - Cell Storage (Week 3)
**Purpose**: Store and retrieve cell values efficiently

**Key Features:**
- Hash table with open chaining
- Sparse storage (only non-empty cells stored)
- String pool for formula text
- Support for 2000 cells (MAXCEL)

**Hash Function:**
```fortran
HASH = MOD((COL * 257 + ROW), 1024)
```

**Data Structures:**
```fortran
C     Cell entry
      INTEGER CELLA(MAXCEL, 6)  ! Cell array
C     CELLA(i,1) = COL
C     CELLA(i,2) = ROW
C     CELLA(i,3) = TYPE (1=number, 2=formula)
C     CELLA(i,4) = VALUE (number) or FMLIDX (formula index)
C     CELLA(i,5) = NEXT (linked list for collisions)
C     CELLA(i,6) = FLAGS (recalc needed, etc)

C     String pool for formulas
      INTEGER FMLPOL(MAXSTR)    ! Formula pool
      INTEGER FMLLEN(MAXCEL)    ! Formula lengths
```

**Functions:**
- `CELPUT(COL, ROW, TYPE, VALUE)` - Store cell
- `CELGET(COL, ROW, TYPE, VALUE)` - Retrieve cell
- `CELDEL(COL, ROW)` - Delete cell
- `CELFND(COL, ROW)` - Find cell index
- `CELNEW()` - Allocate new cell
- `FMLADD(FMLSTR, FMLLEN)` - Add formula to string pool

**Tests** (15 tests):
- Basic storage/retrieval
- Hash collision handling
- Delete and reuse
- String pool management
- Capacity limits
- Empty cell handling

### 2. PARSE.FOR - Formula Parser (Week 3-4)
**Purpose**: Convert infix formulas to postfix (RPN)

**Algorithm**: Shunting-yard (Dijkstra)
- No recursion (FORTRAN IV limitation)
- Explicit operator stack
- Operator precedence: ^ > */ > +- > comparisons

**Input**: `+A1*2+B2`
**Output**: `A1 2 * B2 +` (postfix tokens)

**Data Structures:**
```fortran
C     Token types
      INTEGER TOKTYP(MAXTOK)   ! 1=number, 2=cell, 3=op, 4=func
      INTEGER TOKVAL(MAXTOK)   ! Value or operator code
      INTEGER TOKCOL(MAXTOK)   ! Cell column (if type=2)
      INTEGER TOKROW(MAXTOK)   ! Cell row (if type=2)

C     Operator stack
      INTEGER OPSTAK(100)      ! Operator stack
      INTEGER OPSTKP           ! Stack pointer
```

**Functions:**
- `PARSE(FMLSTR, FMLLEN, TOKENS, NTOK)` - Main parser
- `TOKNXT(STR, POS, TOK)` - Get next token
- `OPPUSH(OP)` - Push operator to stack
- `OPPOP()` - Pop operator from stack
- `OPPREC(OP)` - Get operator precedence

**Operators** (precedence):
1. `^` - Exponentiation (highest)
2. `*` `/` - Multiplication, Division
3. `+` `-` - Addition, Subtraction
4. `=` `<` `>` `<=` `>=` `<>` - Comparisons (lowest)

**Functions** (@-functions):
- `@SUM(range)`, `@AVG(range)`, `@MIN(range)`, `@MAX(range)`
- `@COUNT(range)`, `@ABS(x)`, `@INT(x)`, `@SQRT(x)`
- `@IF(cond,true,false)`, `@NA`, `@ERR`, `@ROUND(x,n)`

**Tests** (20 tests):
- Simple expressions
- Operator precedence
- Parentheses grouping
- Cell references
- Functions
- Range references
- Error handling (syntax errors)

### 3. EVAL.FOR - Expression Evaluator (Week 4)
**Purpose**: Evaluate postfix expressions

**Algorithm**: Stack-based evaluation
- Process postfix tokens left to right
- Push operands to stack
- Pop operands for operators
- Push result back to stack

**Data Structures:**
```fortran
C     Evaluation stack
      REAL EVSTK(100)          ! Operand stack
      INTEGER EVSTKP           ! Stack pointer
```

**Functions:**
- `EVAL(TOKENS, NTOK, RESULT, ERROR)` - Evaluate expression
- `EVPUSH(VALUE)` - Push value to stack
- `EVPOP()` - Pop value from stack
- `EVALOP(OP, RESULT)` - Evaluate operator
- `EVALFN(FN, RESULT)` - Evaluate function

**Error Handling:**
- Division by zero → ERR
- Invalid cell reference → NA
- Stack underflow → ERR
- Unknown function → ERR

**Tests** (18 tests):
- Arithmetic operations
- Operator precedence verification
- Function evaluation
- Range functions
- Error conditions
- Complex expressions

### 4. DEPS.FOR - Dependency Tracking (Week 5)
**Purpose**: Track which cells depend on which

**Key Concept**: When A1 changes, need to know which cells have formulas referencing A1

**Data Structures:**
```fortran
C     Dependency graph (reverse lookup)
C     For each cell, list of cells that depend on it
      INTEGER DEPGRF(MAXDEP, 3)
C     DEPGRF(i,1) = Source COL
C     DEPGRF(i,2) = Source ROW
C     DEPGRF(i,3) = Dependent cell index

C     Dependency count per cell
      INTEGER DEPCNT(MAXCEL)   ! Number of dependencies
```

**Functions:**
- `DEPADD(SCOL, SROW, DCOL, DROW)` - Add dependency
- `DEPDEL(COL, ROW)` - Delete all dependencies for cell
- `DEPGET(COL, ROW, DEPLIST, NDEPS)` - Get dependents
- `DEPCIR(COL, ROW)` - Check for circular reference

**Circular Detection**:
- Depth-first search (iterative, no recursion)
- Mark visited cells
- If cell visits itself → circular

**Tests** (12 tests):
- Add dependencies
- Delete dependencies
- Get dependent list
- Circular detection (simple)
- Circular detection (complex chains)
- Max dependency limits

### 5. RECALC.FOR - Recalculation Engine (Week 5)
**Purpose**: Recalculate cells in correct order

**Algorithm**: Topological sort
- Start with changed cell
- Find all dependents
- Calculate in dependency order
- Propagate changes through chain

**Data Structures:**
```fortran
C     Recalculation queue
      INTEGER RECALQ(MAXCEL, 2) ! Queue of cells to recalc
C     RECALQ(i,1) = COL
C     RECALQ(i,2) = ROW
      INTEGER RECALP            ! Queue pointer

C     Visited flags (prevent recalc loops)
      INTEGER RECVIS(MAXCEL)
```

**Functions:**
- `RECALC(COL, ROW)` - Recalculate cell and dependents
- `RECADD(COL, ROW)` - Add cell to recalc queue
- `RECPROC()` - Process recalc queue
- `RECONE(COL, ROW)` - Recalculate single cell

**Modes:**
- Automatic: Recalc on every change (default)
- Manual: User triggers with /! command

**Tests** (10 tests):
- Simple chain (A1→A2→A3)
- Wide fan (A1→B1,C1,D1...)
- Deep chain (26+ levels)
- Circular handling
- Manual vs automatic mode

---

## Implementation Order

### Week 3: Foundation
1. **Day 1-2**: CELLS.FOR
   - Hash table structure
   - Basic CELPUT/CELGET
   - Collision handling

2. **Day 3-4**: CELLS.FOR completion
   - String pool for formulas
   - Delete and reuse
   - Tests (15 tests)

3. **Day 5**: Start PARSE.FOR
   - Tokenizer
   - Basic parsing

### Week 4: Parsing and Evaluation
1. **Day 1-2**: PARSE.FOR
   - Shunting-yard implementation
   - Operator precedence
   - Parentheses

2. **Day 3**: PARSE.FOR completion
   - Cell references
   - Function parsing
   - Tests (20 tests)

3. **Day 4-5**: EVAL.FOR
   - Stack-based evaluation
   - Operator evaluation
   - Function evaluation
   - Tests (18 tests)

### Week 5: Dependencies and Recalculation
1. **Day 1-2**: DEPS.FOR
   - Dependency graph structure
   - Add/delete dependencies
   - Circular detection
   - Tests (12 tests)

2. **Day 3-4**: RECALC.FOR
   - Topological sort
   - Recalculation queue
   - Propagation
   - Tests (10 tests)

3. **Day 5**: Integration
   - Test all components together
   - Run acceptance tests
   - Some should start PASSING!

---

## Success Criteria

### Week 3 Complete
- ✅ CELLS.FOR: 15/15 tests passing
- ✅ PARSE.FOR: 20/20 tests passing
- ✅ Can store cells and parse formulas

### Week 4 Complete
- ✅ EVAL.FOR: 18/18 tests passing
- ✅ Can evaluate simple formulas
- ✅ Acceptance tests: Basic entry tests PASS
- ✅ Acceptance tests: Simple formula tests PASS

### Week 5 Complete (Layer 1 Done)
- ✅ DEPS.FOR: 12/12 tests passing
- ✅ RECALC.FOR: 10/10 tests passing
- ✅ Total: 75/75 unit tests passing
- ✅ Acceptance tests: ~8/15 PASS (formulas and recalc)
- ✅ Full calculation engine working

---

## Test-Driven Development Workflow

For each function:

1. **Write test first** (Python harness)
   ```python
   def test_celput_basic():
       test_program = """
           PROGRAM TEST
           CALL CELPUT(1, 1, 1, 100)
           ...
       """
       result = run_fortran(test_program)
       assert result == expected
   ```

2. **Run test** → FAIL (function not implemented)

3. **Implement function** (FORTRAN IV)
   ```fortran
   SUBROUTINE CELPUT(COL, ROW, TYPE, VALUE)
   ...
   ```

4. **Run test** → PASS

5. **Refactor** if needed

6. **Commit** when test passes

---

## Key Challenges

### 1. Hash Table Collisions
- Test with pathological cases
- Benchmark distribution
- May need to tune HASHMOD constant

### 2. No Recursion (FORTRAN IV)
- Shunting-yard is iterative (good!)
- Circular detection needs explicit stack
- Topological sort iterative with queue

### 3. String Pool Management
- Fixed-size pool (MAXSTR)
- Fragmentation possible
- May need compaction strategy

### 4. Limited Memory
- CP-V: 512KB total
- Need sparse storage
- Hash table sized for MAXCEL=2000

---

## Files to Create

```
src/layer1/
├── CELLS.FOR      # Cell storage (400 lines)
├── PARSE.FOR      # Formula parser (500 lines)
├── EVAL.FOR       # Expression evaluator (400 lines)
├── DEPS.FOR       # Dependency tracking (300 lines)
└── RECALC.FOR     # Recalculation engine (300 lines)

test/unit/
├── test_cells.py      # 15 tests
├── test_parse.py      # 20 tests
├── test_eval.py       # 18 tests
├── test_deps.py       # 12 tests
└── test_recalc.py     # 10 tests
```

---

## Next Step

Starting with CELLS.FOR:

1. Define data structures
2. Write first test (test_celput_basic)
3. Implement CELPUT
4. Continue TDD cycle

---

**Created**: 2026-01-18
**Status**: Ready to begin
**Next**: Implement CELLS.FOR using TDD
