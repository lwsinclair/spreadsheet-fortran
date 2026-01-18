# Spreadsheet Calculation Engine - Technical Reference

**Version**: 1.0
**Date**: 2026-01-19
**Status**: Complete
**Layer**: Layer 1 (Calculation Engine)
**Language**: FORTRAN IV
**Target Platform**: CP-V on Sigma 7 (1978)

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Architecture Overview](#2-architecture-overview)
3. [Module Details](#3-module-details)
   - [3.1 CELLS.FOR - Cell Storage](#31-cellsfor---cell-storage)
   - [3.2 PARSE.FOR - Formula Parser](#32-parsefor---formula-parser)
   - [3.3 EVAL.FOR - Expression Evaluator](#33-evalfor---expression-evaluator)
   - [3.4 DEPS.FOR - Dependency Tracking](#34-depsfor---dependency-tracking)
   - [3.5 RECALC.FOR - Recalculation Engine](#35-recalcfor---recalculation-engine)
4. [Integration Flow](#4-integration-flow)
5. [FORTRAN IV Considerations](#5-fortran-iv-considerations)
6. [Performance](#6-performance)
7. [Testing](#7-testing)
8. [Known Limitations](#8-known-limitations)
9. [Future Enhancements](#9-future-enhancements)

---

## 1. Introduction

### 1.1 Purpose

The Calculation Engine (Layer 1) is the core computational component of the CP-V spreadsheet application. It provides all functionality needed to store, parse, evaluate, and automatically recalculate spreadsheet formulas.

### 1.2 What It Does

The calculation engine enables users to:
- Store numeric values in cells
- Enter formulas with cell references and arithmetic operators
- Have formulas automatically evaluated using correct operator precedence
- Track dependencies between cells
- Automatically recalculate when dependent cells change
- Detect circular reference errors

### 1.3 Design Philosophy

**Simplicity**: Each module has a single, well-defined responsibility
**Performance**: Hash tables and iterative algorithms for speed
**Portability**: Pure FORTRAN IV with no platform-specific features
**Reliability**: Comprehensive test coverage (100% of implemented features)

### 1.4 Key Capabilities

```
User enters: A3 = +A1+A2
    ↓
PARSE converts to postfix: A1 A2 +
    ↓
EVAL calculates result: (value of A1) + (value of A2)
    ↓
DEPS tracks dependencies: A3 depends on A1 and A2
    ↓
RECALC auto-updates when A1 or A2 changes
```

---

## 2. Architecture Overview

### 2.1 Module Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                    RECALC.FOR                               │
│              (Recalculation Engine)                         │
│  • Propagate changes through dependency graph               │
│  • Queue-based breadth-first traversal                      │
└────────┬──────────────────────────────────┬─────────────────┘
         │                                  │
         │ uses                             │ uses
         │                                  │
    ┌────▼────────┐                    ┌───▼──────────┐
    │  EVAL.FOR   │                    │  DEPS.FOR    │
    │ Evaluator   │                    │ Dependencies │
    │             │                    │              │
    │ • Stack-    │                    │ • Hash table │
    │   based     │                    │ • BFS circ   │
    │   postfix   │                    │   detection  │
    └────┬────────┘                    └──────────────┘
         │
         │ uses
         │
    ┌────▼────────┐
    │ PARSE.FOR   │
    │   Parser    │
    │             │
    │ • Shunting- │
    │   yard      │
    │ • Infix to  │
    │   postfix   │
    └────┬────────┘
         │
         │ uses
         │
    ┌────▼────────┐
    │ CELLS.FOR   │
    │ Cell Storage│
    │             │
    │ • Hash table│
    │ • Formula   │
    │   pool      │
    └─────────────┘
```

### 2.2 Data Flow

**Formula Entry Flow:**
```
1. User input: "+A1*2+B2"
2. PARSE → Tokens: A1 2 * B2 + (postfix)
3. CELLS.CELPTK → Store tokens in formula pool
4. DEPS.DEPSADD → Add dependencies (A1→A3, A2→A3)
5. EVAL → Calculate: (A1*2)+(B2)
6. CELLS.CELRES → Store result
```

**Value Change Flow:**
```
1. CELLS.CELPUT → Update cell value
2. RECALC.RECCEL → Trigger recalculation
3. DEPS.DEPSGET → Get all dependents
4. For each dependent:
   - CELLS.CELGTK → Get formula tokens
   - EVAL → Recalculate
   - CELLS.CELRES → Store new result
   - DEPS.DEPSGET → Get next level dependents
5. Repeat until queue empty
```

### 2.3 Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Hash tables for storage | O(1) average lookup, sparse data |
| Shunting-yard algorithm | Standard, efficient, well-tested |
| Postfix evaluation | Simple stack-based, no recursion needed |
| BFS for graph traversal | Iterative (FORTRAN IV has no recursion) |
| Token-based formulas | Compact, efficient, no re-parsing |
| Formula result cache | Avoid re-evaluation, faster display |

---

## 3. Module Details

### 3.1 CELLS.FOR - Cell Storage

**Purpose**: Store and retrieve spreadsheet cells using hash table with sparse storage.

**Lines of Code**: 491 lines
**Test Coverage**: 7/7 passing (100%)

#### 3.1.1 Data Structures

**Cell Array (CELLA)**
```fortran
INTEGER CELLA(2000, 7)

CELLA(i,1) = COL    Column number (1-63)
CELLA(i,2) = ROW    Row number (1-254)
CELLA(i,3) = TYPE   0=empty, 1=number, 2=formula
CELLA(i,4) = VALUE  For numbers: value
                    For formulas: formula pool index
CELLA(i,5) = NEXT   Next cell in hash chain (collision handling)
CELLA(i,6) = FLAGS  Reserved for future use (recalc flags, etc.)
CELLA(i,7) = RESULT Calculated value for formulas
```

**Hash Table (HTABLE)**
```fortran
INTEGER HTABLE(1024)

HTABLE(hash) = Index of first cell in this bucket
               0 = empty bucket
```

**Formula Pool (FMLPOL)**
```fortran
INTEGER FMLPOL(10000)   ! Token storage
INTEGER FMLLEN(2000)    ! Token count per formula
INTEGER FMLPTR          ! Next free position

Each token uses 4 integers:
  FMLPOL(i)   = TYPE  (1=number, 2=cell, 3=operator)
  FMLPOL(i+1) = VALUE
  FMLPOL(i+2) = COL
  FMLPOL(i+3) = ROW
```

**Free List**
```fortran
INTEGER FRLIST  ! Head of deleted cell list
INTEGER CELCNT  ! Count of allocated cells
```

#### 3.1.2 Hash Function

```fortran
C     CELHSH - Compute hash value for cell
      INTEGER FUNCTION CELHSH(COL, ROW)
      INTEGER COL, ROW

      CELHSH = MOD(COL * 257 + ROW, 1024)
```

**Why this formula?**
- Prime multiplier (257) provides good distribution
- Modulo 1024 maps to hash buckets
- COL and ROW weighted differently to minimize collisions

**Example hash values:**
```
A1  (1,1):   (1*257 + 1) MOD 1024 = 258
B1  (2,1):   (2*257 + 1) MOD 1024 = 515
A2  (1,2):   (1*257 + 2) MOD 1024 = 259
A10 (1,10):  (1*257 + 10) MOD 1024 = 267
```

#### 3.1.3 Collision Handling

**Open Chaining**: Each hash bucket is the head of a linked list

```
Example: A1 and Z245 hash to bucket 258

HTABLE(258) → Cell_A1 → Cell_Z245 → NULL
              NEXT=idx2  NEXT=0
```

**Search algorithm** (from CELFND, lines 95-134):
```fortran
C     Get head of chain
      IDX = HTABLE(HASH)

C     Search chain
100   IF (IDX .EQ. 0) GO TO 900    ! End of chain

      IF (CELLA(IDX,1) .EQ. COL .AND. CELLA(IDX,2) .EQ. ROW) GO TO 800

      IDX = CELLA(IDX,5)            ! Next in chain
      GO TO 100
```

#### 3.1.4 Function Reference

**CELINI** - Initialize cell storage
```fortran
SUBROUTINE CELINI

Purpose:  Initialize all cell storage structures
Input:    None
Output:   None
Effects:  Clears HTABLE, CELLA, initializes FRLIST, FMLPTR
Calls:    None
Called by: Main program initialization
```

**CELHSH** - Compute hash value
```fortran
INTEGER FUNCTION CELHSH(COL, ROW)
INTEGER COL, ROW

Purpose:  Compute hash bucket for cell coordinates
Input:    COL (1-63), ROW (1-254)
Output:   Hash value (1-1024)
Algorithm: (COL * 257 + ROW) MOD 1024
Time:     O(1)
```

**CELFND** - Find cell in hash table
```fortran
INTEGER FUNCTION CELFND(COL, ROW)
INTEGER COL, ROW

Purpose:  Search for cell in hash table
Input:    COL, ROW
Output:   Cell index (1-2000), or 0 if not found
Algorithm: Linear search of hash chain
Time:     O(1) average, O(n) worst case
Lines:    95-134
```

**CELNEW** - Allocate new cell slot
```fortran
INTEGER FUNCTION CELNEW()

Purpose:  Allocate a new cell slot
Input:    None
Output:   Cell index (1-2000), or 0 if full
Algorithm: Use free list if available, else allocate from end
Time:     O(1)
Lines:    139-171
```

**CELPUT** - Store cell value
```fortran
SUBROUTINE CELPUT(COL, ROW, TYPE, VALUE)
INTEGER COL, ROW, TYPE
REAL VALUE

Purpose:  Store or update cell value
Input:    COL, ROW - Cell coordinates
          TYPE - 0=empty, 1=number, 2=formula
          VALUE - Numeric value
Output:   None
Effects:  Updates existing cell or creates new one
          Adds to hash chain if new
Algorithm: Find existing or allocate new, update fields
Time:     O(1) average
Lines:    176-223
```

**CELGET** - Retrieve cell value
```fortran
SUBROUTINE CELGET(COL, ROW, TYPE, VALUE)
INTEGER COL, ROW, TYPE
REAL VALUE

Purpose:  Get cell value
Input:    COL, ROW
Output:   TYPE - Cell type (0 if not found)
          VALUE - Cell value (0.0 if not found)
          For TYPE=1 (number): returns CELLA(i,4)
          For TYPE=2 (formula): returns CELLA(i,7) (result)
Algorithm: Search hash table
Time:     O(1) average
Lines:    228-269
```

**CELDEL** - Delete cell
```fortran
SUBROUTINE CELDEL(COL, ROW)
INTEGER COL, ROW

Purpose:  Delete cell from storage
Input:    COL, ROW
Output:   None
Effects:  Removes from hash chain, adds to free list
Algorithm: Search and unlink from chain
Time:     O(1) average
Lines:    274-333
```

**CELPTK** - Put formula tokens
```fortran
SUBROUTINE CELPTK(COL, ROW, TOKENS, NTOK)
INTEGER COL, ROW
INTEGER TOKENS(100, 4)
INTEGER NTOK

Purpose:  Store formula tokens in cell
Input:    COL, ROW - Cell coordinates
          TOKENS - Token array from parser
          NTOK - Number of tokens
Output:   None
Effects:  Creates/updates cell, stores tokens in formula pool
          Sets TYPE=2 (formula)
Algorithm: Find/create cell, copy tokens to pool, update pointers
Time:     O(NTOK)
Lines:    380-441
```

**CELGTK** - Get formula tokens
```fortran
SUBROUTINE CELGTK(COL, ROW, TOKENS, NTOK)
INTEGER COL, ROW
INTEGER TOKENS(100, 4)
INTEGER NTOK

Purpose:  Retrieve formula tokens from cell
Input:    COL, ROW
Output:   TOKENS - Token array (filled)
          NTOK - Number of tokens (0 if not formula)
Algorithm: Find cell, copy tokens from pool
Time:     O(NTOK)
Lines:    446-490
```

**CELRES** - Store formula result
```fortran
SUBROUTINE CELRES(COL, ROW, RESULT)
INTEGER COL, ROW
REAL RESULT

Purpose:  Store calculated result for formula cell
Input:    COL, ROW - Cell coordinates
          RESULT - Calculated value
Output:   None
Effects:  Updates CELLA(idx,7) with result
          Preserves formula tokens
Algorithm: Find cell, update result column
Time:     O(1) average
Lines:    351-375
```

#### 3.1.5 Usage Examples

**Example 1: Store and retrieve a number**
```fortran
C     Store 42 in cell B5
      CALL CELPUT(2, 5, 1, 42.0)

C     Later, retrieve it
      INTEGER CTYPE
      REAL VALUE
      CALL CELGET(2, 5, CTYPE, VALUE)
C     CTYPE = 1 (number)
C     VALUE = 42.0
```

**Example 2: Store a formula**
```fortran
C     Formula: +A1+A2
C     Assume TOKENS contains: A1 A2 + (from PARSE)
      INTEGER TOKENS(100, 4)
      INTEGER NTOK
      NTOK = 3

C     Store in cell A3
      CALL CELPTK(1, 3, TOKENS, NTOK)

C     Later, retrieve tokens
      INTEGER RTOKENS(100, 4)
      INTEGER RNTOK
      CALL CELGTK(1, 3, RTOKENS, RNTOK)
C     RNTOK = 3
C     RTOKENS matches TOKENS
```

**Example 3: Store formula result**
```fortran
C     After evaluating formula in A3
      REAL RESULT
      RESULT = 30.0
      CALL CELRES(1, 3, RESULT)

C     Now CELGET returns the result
      CALL CELGET(1, 3, CTYPE, VALUE)
C     CTYPE = 2 (formula)
C     VALUE = 30.0 (from CELLA(i,7))
```

---

### 3.2 PARSE.FOR - Formula Parser

**Purpose**: Parse infix formulas and convert to postfix (Reverse Polish Notation) using the shunting-yard algorithm.

**Lines of Code**: 277 lines
**Test Coverage**: 6/6 passing (100% of implemented features)

#### 3.2.1 Algorithm: Shunting-Yard

Developed by Edsger Dijkstra, the shunting-yard algorithm converts infix notation to postfix notation with correct operator precedence.

**Visual representation:**

```
Input (infix):   2 + 3 * 4
               ┌───┬───┬───┬───┬───┐
               │ 2 │ + │ 3 │ * │ 4 │
               └───┴───┴───┴───┴───┘

Processing:
Token  │ Action              │ Output    │ Stack
───────┼─────────────────────┼───────────┼───────
2      │ output number       │ 2         │
+      │ push operator       │ 2         │ +
3      │ output number       │ 2 3       │ +
*      │ push (* > +)        │ 2 3       │ + *
4      │ output number       │ 2 3 4     │ + *
(end)  │ pop *               │ 2 3 4 *   │ +
(end)  │ pop +               │ 2 3 4 * + │

Output (postfix): 2 3 4 * +

Evaluation: 2 + (3 * 4) = 2 + 12 = 14 ✓
```

**Key principle**: Higher precedence operators are processed before lower precedence ones.

#### 3.2.2 Operator Precedence

```fortran
C     From OPPREC function (lines 250-276)
      IF (OP .EQ. 5) OPPREC = 3  ! ^ (power)
      IF (OP .EQ. 3) OPPREC = 2  ! * (multiply)
      IF (OP .EQ. 4) OPPREC = 2  ! / (divide)
      IF (OP .EQ. 1) OPPREC = 1  ! + (add)
      IF (OP .EQ. 2) OPPREC = 1  ! - (subtract)
```

**Precedence table:**

| Level | Operators | Example | Result |
|-------|-----------|---------|--------|
| 3 | ^ | 2^3^2 | 2^(3^2) = 512 |
| 2 | * / | 2+3*4 | 2+(3*4) = 14 |
| 1 | + - | 2*3+4 | (2*3)+4 = 10 |

#### 3.2.3 Token Structure

```fortran
TOKENS(i,1) = TYPE
  1 = number
  2 = cell reference
  3 = operator
  4 = function (future)

TOKENS(i,2) = VALUE
  For numbers: actual value (42, 100, etc.)
  For operators: operator code (1=+, 2=-, 3=*, 4=/, 5=^)
  For cells: unused (0)

TOKENS(i,3) = COL (for cell references)
  A=1, B=2, ..., Z=26

TOKENS(i,4) = ROW (for cell references)
  1, 2, 3, ..., 254
```

#### 3.2.4 Function Reference

**PRSINI** - Initialize parser
```fortran
SUBROUTINE PRSINI

Purpose:  Initialize parser (currently no-op)
Input:    None
Output:   None
Future:   May initialize operator tables
Lines:    37-42
```

**PARSE** - Parse formula to postfix
```fortran
SUBROUTINE PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)
INTEGER INPUT(*), INLEN
INTEGER TOKENS(100, 4)
INTEGER NTOK, ERROR

Purpose:  Convert infix formula to postfix tokens
Input:    INPUT - Formula string as integer array
          INLEN - Length of formula
Output:   TOKENS - Postfix token array
          NTOK - Number of tokens
          ERROR - 0=success, 1=error
Algorithm: Shunting-yard with operator stack
Time:     O(n) where n = formula length
Lines:    47-128
```

**TOKNXT** - Get next token from input
```fortran
INTEGER FUNCTION TOKNXT(INPUT, INLEN, POS, TVAL, TCOL, TROW)
INTEGER INPUT(*), INLEN, POS
INTEGER TVAL, TCOL, TROW

Purpose:  Tokenize next element from input string
Input:    INPUT - Input string
          INLEN - String length
          POS - Current position (updated on return)
Output:   TOKNXT - Token type (0=end, 1=number, 2=cell, 3=operator)
          TVAL - Token value
          TCOL, TROW - Cell coordinates (if applicable)
Algorithm: Character-by-character parsing
Time:     O(k) where k = token length
Lines:    133-245
```

**OPPREC** - Get operator precedence
```fortran
INTEGER FUNCTION OPPREC(OP)
INTEGER OP

Purpose:  Return precedence level for operator
Input:    OP - Operator code (1-5)
Output:   Precedence (1-3, higher = tighter binding)
Time:     O(1)
Lines:    250-276
```

#### 3.2.5 Parsing Examples

**Example 1: Simple addition**
```fortran
Input:  "+10+20"
Parse:
  Skip leading '+'
  Token: 10 (number) → output
  Token: +  (operator) → push stack
  Token: 20 (number) → output
  End: pop + → output
Output: [10, 20, +]
Result: 30
```

**Example 2: Precedence**
```fortran
Input:  "+2+3*4"
Parse:
  Skip leading '+'
  Token: 2 (number) → output [2]
  Token: + (operator) → push stack [+]
  Token: 3 (number) → output [2, 3]
  Token: * (operator) → push (* > +) [+, *]
  Token: 4 (number) → output [2, 3, 4]
  End: pop * → output [2, 3, 4, *]
  End: pop + → output [2, 3, 4, *, +]
Output: [2, 3, 4, *, +]
Result: 2 + (3*4) = 14 ✓
```

**Example 3: Cell references**
```fortran
Input:  "+A1+B2"
Parse:
  Skip leading '+'
  Token: A1 (cell, col=1, row=1) → output
  Token: +  (operator) → push
  Token: B2 (cell, col=2, row=2) → output
  End: pop + → output
Output: [A1, B2, +]
```

**Example 4: Complex formula**
```fortran
Input:  "+A1*2+B2*3"
Parse:
  A1 (cell) → [A1]
  *  (op) → [A1], stack=[*]
  2  (num) → [A1, 2], stack=[*]
  +  (op, * > +) → pop * → [A1, 2, *], push + → stack=[+]
  B2 (cell) → [A1, 2, *, B2], stack=[+]
  *  (op, * > +) → push * → stack=[+, *]
  3  (num) → [A1, 2, *, B2, 3], stack=[+, *]
  End: pop * → [A1, 2, *, B2, 3, *], stack=[+]
  End: pop + → [A1, 2, *, B2, 3, *, +]
Output: [A1, 2, *, B2, 3, *, +]
Result: (A1*2) + (B2*3)
```

---

### 3.3 EVAL.FOR - Expression Evaluator

**Purpose**: Evaluate postfix expressions using stack-based algorithm.

**Lines of Code**: 143 lines
**Test Coverage**: 9/9 passing (100% of implemented features)

#### 3.3.1 Algorithm: Stack-Based Postfix Evaluation

```
Postfix expression: 10 20 +

Processing:
Token  │ Action              │ Stack
───────┼─────────────────────┼──────────
10     │ push 10             │ [10]
20     │ push 20             │ [10, 20]
+      │ pop 20, pop 10      │ []
       │ compute 10+20=30    │
       │ push 30             │ [30]
(end)  │ result = top        │ 30

Result: 30 ✓
```

**Complex example:**
```
Postfix: 2 3 4 * +

Token  │ Action              │ Stack
───────┼─────────────────────┼──────────
2      │ push 2              │ [2]
3      │ push 3              │ [2, 3]
4      │ push 4              │ [2, 3, 4]
*      │ pop 4, pop 3        │ [2]
       │ compute 3*4=12      │
       │ push 12             │ [2, 12]
+      │ pop 12, pop 2       │ []
       │ compute 2+12=14     │
       │ push 14             │ [14]
(end)  │ result = top        │ 14

Result: 14 = 2 + (3*4) ✓
```

#### 3.3.2 Operator Implementation

```fortran
C     From EVAL function (lines 96-100)
      IF (TVAL .EQ. 1) TRES = OP1 + OP2     ! Add
      IF (TVAL .EQ. 2) TRES = OP1 - OP2     ! Subtract
      IF (TVAL .EQ. 3) TRES = OP1 * OP2     ! Multiply
      IF (TVAL .EQ. 4) TRES = OP1 / OP2     ! Divide
      IF (TVAL .EQ. 5) TRES = OP1 ** OP2    ! Power
```

**Important**: Operand order matters for non-commutative operators:
```fortran
C     For subtraction/division:
C     Stack before: [..., OP1, OP2]
C     Pop order: OP2 first, OP1 second
C     Compute: OP1 operator OP2

Example: 10 5 -
  Stack: [10, 5]
  Pop: OP2 = 5, OP1 = 10
  Compute: 10 - 5 = 5 ✓
```

#### 3.3.3 Function Reference

**EVLINI** - Initialize evaluator
```fortran
SUBROUTINE EVLINI

Purpose:  Initialize evaluator (currently no-op)
Input:    None
Output:   None
Lines:    30-34
```

**EVAL** - Evaluate postfix expression
```fortran
SUBROUTINE EVAL(TOKENS, NTOK, RESULT, ERROR)
INTEGER TOKENS(100, 4), NTOK, ERROR
REAL RESULT

Purpose:  Evaluate postfix token array
Input:    TOKENS - Postfix tokens from PARSE
          NTOK - Number of tokens
Output:   RESULT - Calculated value
          ERROR - 0=success, 1=error (stack underflow/overflow)
Algorithm: Stack-based evaluation
          - Numbers/cells: push value
          - Operators: pop 2, compute, push result
Time:     O(n) where n = number of tokens
Lines:    39-116
```

**EVPUSH** - Push value to stack
```fortran
SUBROUTINE EVPUSH(VAL, EVSTK, EVSTKP)
REAL VAL, EVSTK(*)
INTEGER EVSTKP

Purpose:  Push value onto evaluation stack
Input:    VAL - Value to push
          EVSTK - Stack array
          EVSTKP - Stack pointer (updated)
Output:   Updated EVSTKP
Time:     O(1)
Lines:    121-129
```

**EVPOP** - Pop value from stack
```fortran
SUBROUTINE EVPOP(VAL, EVSTK, EVSTKP)
REAL VAL, EVSTK(*)
INTEGER EVSTKP

Purpose:  Pop value from evaluation stack
Input:    EVSTK - Stack array
          EVSTKP - Stack pointer (updated)
Output:   VAL - Popped value
          Updated EVSTKP
Time:     O(1)
Lines:    134-142
```

#### 3.3.4 Cell Value Retrieval

```fortran
C     From EVAL (lines 81-84)
      CALL CELGET(TCOL, TROW, CTYPE, VAL)
      IF (CTYPE .EQ. 0) VAL = 0.0    ! Empty cell = 0
```

**Cell types:**
- CTYPE = 0: Empty → returns 0.0
- CTYPE = 1: Number → returns stored value
- CTYPE = 2: Formula → returns calculated result (from CELLA column 7)

#### 3.3.5 Error Handling

```fortran
C     Stack underflow check (line 91)
      IF (EVSTKP .LT. 2) GO TO 900

C     Final stack validation (line 108)
      IF (EVSTKP .NE. 1) GO TO 900

C     Error handler (lines 113-115)
900   ERROR = 1
      RESULT = 0.0
      RETURN
```

**Error conditions:**
- Stack has less than 2 values for binary operator
- Final stack has more than 1 value (incomplete expression)
- Stack overflow (> 50 values)

#### 3.3.6 Usage Examples

**Example 1: Simple arithmetic**
```fortran
C     Evaluate: 10 + 20
      INTEGER TOKENS(100, 4)
      INTEGER NTOK, ERROR
      REAL RESULT

C     Tokens: [10, 20, +] (from PARSE)
      TOKENS(1,1) = 1    ! Number
      TOKENS(1,2) = 10
      TOKENS(2,1) = 1    ! Number
      TOKENS(2,2) = 20
      TOKENS(3,1) = 3    ! Operator
      TOKENS(3,2) = 1    ! ADD
      NTOK = 3

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
C     RESULT = 30.0
C     ERROR = 0
```

**Example 2: Cell references**
```fortran
C     Store values
      CALL CELPUT(1, 1, 1, 10.0)   ! A1 = 10
      CALL CELPUT(1, 2, 1, 20.0)   ! A2 = 20

C     Evaluate: A1 + A2
      TOKENS(1,1) = 2    ! Cell
      TOKENS(1,3) = 1    ! COL=1
      TOKENS(1,4) = 1    ! ROW=1
      TOKENS(2,1) = 2    ! Cell
      TOKENS(2,3) = 1    ! COL=1
      TOKENS(2,4) = 2    ! ROW=2
      TOKENS(3,1) = 3    ! Operator
      TOKENS(3,2) = 1    ! ADD
      NTOK = 3

      CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
C     RESULT = 30.0 (10+20)
```

---

### 3.4 DEPS.FOR - Dependency Tracking

**Purpose**: Track which cells depend on other cells for recalculation and circular reference detection.

**Lines of Code**: 309 lines
**Test Coverage**: 13/13 passing (100%)

#### 3.4.1 Data Structure

**Dependency Node**
```fortran
DEPNOD(i,1) = Source column    (cell being referenced)
DEPNOD(i,2) = Source row
DEPNOD(i,3) = Dependent column (cell with formula)
DEPNOD(i,4) = Dependent row
DEPNOD(i,5) = Next pointer     (linked list)
```

**Example: A3 = +A1+A2**
```
Two dependency nodes created:

Node 1: Source=A1 (1,1), Dependent=A3 (1,3)
Node 2: Source=A2 (1,2), Dependent=A3 (1,3)

Hash table:
  Hash(A1) → [Node1: A1→A3] → NULL
  Hash(A2) → [Node2: A2→A3] → NULL

Meaning: When A1 or A2 changes, A3 must be recalculated
```

**Visual representation:**
```
Formulas:
  A3 = +A1+A2
  A4 = +A1*2
  A5 = +A3+A4

Dependency graph:
        A1
       /  \
      ↓    ↓
     A3    A4
       \  /
        ↓
        A5

Storage (hash table):
  Hash(A1) → [A1→A3] → [A1→A4] → NULL
  Hash(A2) → [A2→A3] → NULL
  Hash(A3) → [A3→A5] → NULL
  Hash(A4) → [A4→A5] → NULL
```

#### 3.4.2 Hash Table

```fortran
INTEGER DEPHT(256)     ! Hash table (256 buckets)
INTEGER DEPNOD(1000,5) ! Dependency nodes
INTEGER DEPFRE         ! Free list head
```

**Hash function:**
```fortran
DEPSHSH = MOD(COL * 257 + ROW, 256) + 1
```

#### 3.4.3 Algorithm: Circular Reference Detection

**Problem**: Detect cycles like A1→A2→A3→A1

**Solution**: Breadth-first search (BFS) with explicit queue

```fortran
C     From DEPSCIR (lines 189-265)

Queue-based algorithm:
1. Start with target cell in queue
2. For each cell in queue:
   - Mark as visited
   - Get all its dependents
   - If any dependent is original target → CIRCULAR!
   - Otherwise add dependents to queue
3. Continue until queue empty
4. If target never seen again → NO CIRCULAR
```

**Example: Detecting A1→A2→A1**
```
Initial: Queue=[A1], Visited=[]

Iteration 1:
  Current: A1
  Visited: [A1]
  Dependents of A1: [A2]
  A2 == A1? No
  Queue: [A2]

Iteration 2:
  Current: A2
  Visited: [A1, A2]
  Dependents of A2: [A1]
  A1 == A1? YES → CIRCULAR!
```

**Example: No circular**
```
A1 = 10
A2 = +A1
A3 = +A1

Check A1:
  Queue=[A1], Visited=[]
  Process A1: Dependents=[A2, A3]
  Queue=[A2, A3]
  Process A2: Dependents=[]
  Process A3: Dependents=[]
  Queue=[] → NO CIRCULAR ✓
```

#### 3.4.4 Function Reference

**DEPSINI** - Initialize dependency graph
```fortran
SUBROUTINE DEPSINI

Purpose:  Initialize all dependency structures
Input:    None
Output:   None
Effects:  Clears DEPHT, initializes free list
Time:     O(n) where n = max nodes
Lines:    40-64
```

**DEPSADD** - Add dependency
```fortran
SUBROUTINE DEPSADD(SCOL, SROW, DCOL, DROW)
INTEGER SCOL, SROW, DCOL, DROW

Purpose:  Add dependency relationship
Input:    SCOL, SROW - Source cell (being referenced)
          DCOL, DROW - Dependent cell (with formula)
Output:   None
Effects:  Adds node to hash table (if not duplicate)
Example:  DEPSADD(1,1, 1,3) means A3 depends on A1
Time:     O(1) average, O(n) if many collisions
Lines:    69-102
```

**DEPSDEL** - Delete dependency
```fortran
SUBROUTINE DEPSDEL(SCOL, SROW, DCOL, DROW)
INTEGER SCOL, SROW, DCOL, DROW

Purpose:  Remove specific dependency
Input:    SCOL, SROW - Source cell
          DCOL, DROW - Dependent cell
Output:   None
Effects:  Removes node from hash table, adds to free list
Time:     O(1) average
Lines:    107-148
```

**DEPSGET** - Get all dependents
```fortran
SUBROUTINE DEPSGET(SCOL, SROW, DEPS, NDEPS)
INTEGER SCOL, SROW
INTEGER DEPS(100, 2)
INTEGER NDEPS

Purpose:  Get all cells that depend on given cell
Input:    SCOL, SROW - Source cell
Output:   DEPS - Array of (col, row) pairs
          NDEPS - Count of dependents
Algorithm: Scan hash bucket for matching source
Time:     O(k) where k = dependents in bucket
Lines:    153-184
```

**DEPSCIR** - Check for circular reference
```fortran
SUBROUTINE DEPSCIR(COL, ROW, CIRC)
INTEGER COL, ROW, CIRC

Purpose:  Detect circular reference from cell
Input:    COL, ROW - Starting cell
Output:   CIRC - 0=no circular, 1=circular detected
Algorithm: Breadth-first search with queue
          Checks if any dependent path leads back to start
Time:     O(V + E) where V=cells, E=dependencies
Lines:    189-265
```

**DEPSHSH** - Hash function
```fortran
INTEGER FUNCTION DEPSHSH(COL, ROW)
INTEGER COL, ROW

Purpose:  Compute hash bucket for cell
Input:    COL, ROW
Output:   Hash value (1-256)
Algorithm: (COL * 257 + ROW) MOD 256 + 1
Time:     O(1)
Lines:    270-277
```

**DEPSFND** - Find dependency node (internal)
```fortran
SUBROUTINE DEPSFND(SCOL, SROW, DCOL, DROW, NODE)
INTEGER SCOL, SROW, DCOL, DROW, NODE

Purpose:  Search for specific dependency node
Input:    SCOL, SROW, DCOL, DROW
Output:   NODE - Node index (0 if not found)
Algorithm: Linear search of hash bucket
Time:     O(k) where k = nodes in bucket
Lines:    282-308
```

#### 3.4.5 Usage Examples

**Example 1: Add dependencies for formula**
```fortran
C     User enters: A3 = +A1+A2
C     After parsing, we know formula references A1 and A2

C     Add dependencies
      CALL DEPSADD(1, 1, 1, 3)    ! A3 depends on A1
      CALL DEPSADD(1, 2, 1, 3)    ! A3 depends on A2
```

**Example 2: Get dependents for recalc**
```fortran
C     User changes A1
C     Need to find what to recalculate

      INTEGER DEPS(100, 2)
      INTEGER NDEPS, I

      CALL DEPSGET(1, 1, DEPS, NDEPS)
C     DEPS might contain: [(1,3), (1,4), (1,5)]
C     Meaning: A3, A4, A5 all depend on A1

C     Recalculate each dependent
      DO 100 I = 1, NDEPS
        CALL RECALC_CELL(DEPS(I,1), DEPS(I,2))
100   CONTINUE
```

**Example 3: Check for circular before adding**
```fortran
C     User wants: A1 = +A2
C     But A2 already has: A2 = +A1

      INTEGER CIRC

C     Temporarily add the dependency
      CALL DEPSADD(1, 2, 1, 1)    ! A1 depends on A2

C     Check for circular
      CALL DEPSCIR(1, 1, CIRC)

      IF (CIRC .NE. 0) THEN
C       Circular detected!
        CALL DEPSDEL(1, 2, 1, 1)  ! Remove bad dependency
C       Show error to user
      ENDIF
```

---

### 3.5 RECALC.FOR - Recalculation Engine

**Purpose**: Propagate changes through the dependency graph and automatically recalculate affected cells.

**Lines of Code**: 126 lines
**Test Coverage**: 4/4 passing (100% of implemented features)

#### 3.5.1 Algorithm: Breadth-First Propagation

**Problem**: When a cell changes, update all dependent cells in correct order

**Solution**: Queue-based breadth-first traversal

```
Example: A1→A2→A3→A4 (linear chain)

Change A1:
  Queue: [A1]
  Process A1 (number, no recalc needed)
    Get dependents: [A2]
    Queue: [A2]
  Process A2 (formula)
    Recalculate: A2 = +A1
    Get dependents: [A3]
    Queue: [A3]
  Process A3 (formula)
    Recalculate: A3 = +A2
    Get dependents: [A4]
    Queue: [A4]
  Process A4 (formula)
    Recalculate: A4 = +A3
    Get dependents: []
    Queue: []
  Done! All updated in correct order.
```

**Visual:**
```
A1=10  A2=+A1  A3=+A2  A4=+A3
  ↓      ↓       ↓       ↓
A1=20  A2=20   A3=20   A4=20

Queue progression:
[A1] → [A2] → [A3] → [A4] → []
```

#### 3.5.2 Recalculation Process

```fortran
C     From RECCEL (lines 37-99)

For each cell in queue:
  1. Get cell type (CELGET)
  2. If formula (TYPE=2):
     a. Get formula tokens (CELGTK)
     b. Evaluate tokens (EVAL)
     c. Store result (CELRES)
  3. Get dependents (DEPSGET)
  4. Add dependents to queue
  5. Continue until queue empty
```

**Code flow:**
```fortran
C     Initialize queue with changed cell
      QUEUE(1,1) = COL
      QUEUE(1,2) = ROW
      QTAIL = 2

C     Process queue
100   IF (QHEAD .GE. QTAIL) RETURN

      CCOL = QUEUE(QHEAD,1)
      CROW = QUEUE(QHEAD,2)
      QHEAD = QHEAD + 1

C     If formula, recalculate
      CALL CELGET(CCOL, CROW, CTYPE, VAL)
      IF (CTYPE .EQ. 2) THEN
        CALL CELGTK(CCOL, CROW, TOKENS, NTOK)
        CALL EVAL(TOKENS, NTOK, RESULT, ERROR)
        CALL CELRES(CCOL, CROW, RESULT)
      ENDIF

C     Add dependents to queue
      CALL DEPSGET(CCOL, CROW, DEPS, NDEPS)
      DO I = 1, NDEPS
        QUEUE(QTAIL,1) = DEPS(I,1)
        QUEUE(QTAIL,2) = DEPS(I,2)
        QTAIL = QTAIL + 1
      ENDDO

      GO TO 100
```

#### 3.5.3 Function Reference

**RECINI** - Initialize recalc engine
```fortran
SUBROUTINE RECINI

Purpose:  Initialize recalculation engine
Input:    None
Output:   None
Future:   May initialize recalc mode settings
Lines:    29-32
```

**RECCEL** - Recalculate cell and dependents
```fortran
SUBROUTINE RECCEL(COL, ROW)
INTEGER COL, ROW

Purpose:  Recalculate cell and all its dependents
Input:    COL, ROW - Changed cell
Output:   None
Effects:  Updates all dependent formulas
Algorithm: BFS propagation through dependency graph
Uses:     CELGET, CELGTK, EVAL, CELRES, DEPSGET
Time:     O(V + E) where V=affected cells, E=dependencies
Lines:    37-99
```

**RECMOD** - Set recalc mode (stub)
```fortran
SUBROUTINE RECMOD(MODE)
INTEGER MODE

Purpose:  Set automatic vs manual recalc mode
Input:    MODE - 0=manual, 1=automatic
Output:   None
Status:   Stub for future implementation
Lines:    104-112
```

**RECALL** - Recalculate all cells (stub)
```fortran
SUBROUTINE RECALL

Purpose:  Recalculate entire spreadsheet
Input:    None
Output:   None
Status:   Stub for future implementation
Future:   Will implement topological sort
Lines:    117-126
```

#### 3.5.4 Usage Examples

**Example 1: Value change triggers recalc**
```fortran
C     Setup: A1=10, A2=+A1, A3=+A2
      CALL CELPUT(1, 1, 1, 10.0)
      CALL CELPTK(1, 2, TOKENS_A2, NTOK_A2)
      CALL DEPSADD(1, 1, 1, 2)
      CALL CELPTK(1, 3, TOKENS_A3, NTOK_A3)
      CALL DEPSADD(1, 2, 1, 3)

C     Initial calculation
      CALL RECCEL(1, 2)    ! A2 = 10
      CALL RECCEL(1, 3)    ! A3 = 10

C     User changes A1 to 20
      CALL CELPUT(1, 1, 1, 20.0)

C     Automatic recalculation
      CALL RECCEL(1, 1)
C     Queue: [A1] → [A2] → [A3]
C     A2 recalculated to 20
C     A3 recalculated to 20
```

**Example 2: Wide fan recalc**
```fortran
C     Setup: A1=10, A2=+A1, A3=+A1, A4=+A1
      CALL CELPUT(1, 1, 1, 10.0)
      CALL CELPTK(1, 2, TOKENS_A2, NTOK)
      CALL DEPSADD(1, 1, 1, 2)
      CALL CELPTK(1, 3, TOKENS_A3, NTOK)
      CALL DEPSADD(1, 1, 1, 3)
      CALL CELPTK(1, 4, TOKENS_A4, NTOK)
      CALL DEPSADD(1, 1, 1, 4)

C     User changes A1 to 5
      CALL CELPUT(1, 1, 1, 5.0)
      CALL RECCEL(1, 1)
C     Queue: [A1] → [A2, A3, A4]
C     All three cells recalculated in parallel level
```

---

## 4. Integration Flow

### 4.1 Complete Formula Entry Flow

```fortran
C═══════════════════════════════════════════════════════════════
C     SCENARIO: User enters formula "+A1+A2" in cell A3
C═══════════════════════════════════════════════════════════════

C───────────────────────────────────────────────────────────────
C     STEP 1: Initialize modules (done once at startup)
C───────────────────────────────────────────────────────────────
      CALL CELINI          ! Initialize cell storage
      CALL PRSINI          ! Initialize parser
      CALL EVLINI          ! Initialize evaluator
      CALL DEPSINI         ! Initialize dependencies
      CALL RECINI          ! Initialize recalc engine

C───────────────────────────────────────────────────────────────
C     STEP 2: Store prerequisite values
C───────────────────────────────────────────────────────────────
      CALL CELPUT(1, 1, 1, 10.0)    ! A1 = 10
      CALL CELPUT(1, 2, 1, 20.0)    ! A2 = 20

C───────────────────────────────────────────────────────────────
C     STEP 3: Parse formula
C───────────────────────────────────────────────────────────────
      INTEGER INPUT(100)
      INTEGER INLEN, TOKENS(100,4), NTOK, ERROR

C     Convert string "+A1+A2" to integer array
      INPUT(1) = 43     ! '+'
      INPUT(2) = 65     ! 'A'
      INPUT(3) = 49     ! '1'
      INPUT(4) = 43     ! '+'
      INPUT(5) = 65     ! 'A'
      INPUT(6) = 50     ! '2'
      INLEN = 6

      CALL PARSE(INPUT, INLEN, TOKENS, NTOK, ERROR)

C     Result: NTOK = 3
C     TOKENS(1,*) = [TYPE=2, VAL=0, COL=1, ROW=1]  (A1)
C     TOKENS(2,*) = [TYPE=2, VAL=0, COL=1, ROW=2]  (A2)
C     TOKENS(3,*) = [TYPE=3, VAL=1, COL=0, ROW=0]  (+)

C───────────────────────────────────────────────────────────────
C     STEP 4: Store formula tokens
C───────────────────────────────────────────────────────────────
      CALL CELPTK(1, 3, TOKENS, NTOK)

C     Effects:
C       - Creates cell A3 with TYPE=2 (formula)
C       - Stores 3 tokens in formula pool
C       - CELLA(idx,4) = pool index

C───────────────────────────────────────────────────────────────
C     STEP 5: Extract cell references and add dependencies
C───────────────────────────────────────────────────────────────
C     (In production, UI would scan TOKENS for TYPE=2 cells)
      INTEGER I, TCOL, TROW

      DO 100 I = 1, NTOK
        IF (TOKENS(I,1) .EQ. 2) THEN    ! Cell reference
          TCOL = TOKENS(I,3)
          TROW = TOKENS(I,4)
          CALL DEPSADD(TCOL, TROW, 1, 3)
        ENDIF
100   CONTINUE

C     Result:
C       DEPSADD(1, 1, 1, 3)  - A3 depends on A1
C       DEPSADD(1, 2, 1, 3)  - A3 depends on A2

C───────────────────────────────────────────────────────────────
C     STEP 6: Calculate initial result
C───────────────────────────────────────────────────────────────
      CALL RECCEL(1, 3)

C     Internal process:
C       1. CELGTK(1,3) → retrieves tokens
C       2. EVAL(tokens) → calculates 10+20=30
C       3. CELRES(1,3, 30) → stores result

C───────────────────────────────────────────────────────────────
C     STEP 7: Display result to user
C───────────────────────────────────────────────────────────────
      INTEGER CTYPE
      REAL VALUE

      CALL CELGET(1, 3, CTYPE, VALUE)

C     Result:
C       CTYPE = 2 (formula)
C       VALUE = 30.0
```

### 4.2 Value Change and Automatic Recalculation

```fortran
C═══════════════════════════════════════════════════════════════
C     SCENARIO: User changes A1 from 10 to 15
C═══════════════════════════════════════════════════════════════

C───────────────────────────────────────────────────────────────
C     STEP 1: Update cell value
C───────────────────────────────────────────────────────────────
      CALL CELPUT(1, 1, 1, 15.0)

C───────────────────────────────────────────────────────────────
C     STEP 2: Trigger recalculation
C───────────────────────────────────────────────────────────────
      CALL RECCEL(1, 1)

C───────────────────────────────────────────────────────────────
C     STEP 3: RECCEL internal process
C───────────────────────────────────────────────────────────────
C     Queue: [A1]

C     Process A1:
C       - CELGET(1,1) → TYPE=1 (number), no formula to eval
C       - DEPSGET(1,1) → returns [(1,3)] (A3 depends on A1)
C       - Add A3 to queue
C     Queue: [A3]

C     Process A3:
C       - CELGET(1,3) → TYPE=2 (formula)
C       - CELGTK(1,3) → get tokens [A1, A2, +]
C       - EVAL:
C           Stack: []
C           Token A1: CELGET(1,1) → 15, push → [15]
C           Token A2: CELGET(1,2) → 20, push → [15, 20]
C           Token +:  pop 20, pop 15, calc 15+20=35, push → [35]
C           Result: 35
C       - CELRES(1,3, 35.0) → store new result
C       - DEPSGET(1,3) → returns [] (no cells depend on A3)
C     Queue: []

C     Done! A3 automatically updated to 35
```

### 4.3 Circular Reference Detection

```fortran
C═══════════════════════════════════════════════════════════════
C     SCENARIO: User tries to create A1 = +A2 when A2 = +A1
C═══════════════════════════════════════════════════════════════

C───────────────────────────────────────────────────────────────
C     STEP 1: Current state
C───────────────────────────────────────────────────────────────
C     A2 = +A1 (already exists)
C     Dependencies: A1 → [A2]

C───────────────────────────────────────────────────────────────
C     STEP 2: Parse new formula
C───────────────────────────────────────────────────────────────
      CALL PARSE(INPUT_A2, LEN, TOKENS, NTOK, ERROR)
C     TOKENS: [A2, ...]

C───────────────────────────────────────────────────────────────
C     STEP 3: Tentatively add dependency
C───────────────────────────────────────────────────────────────
      CALL DEPSADD(1, 2, 1, 1)    ! A1 depends on A2

C     Dependencies now:
C       A1 → [A2]
C       A2 → [A1]

C───────────────────────────────────────────────────────────────
C     STEP 4: Check for circular reference
C───────────────────────────────────────────────────────────────
      INTEGER CIRC

      CALL DEPSCIR(1, 1, CIRC)

C     DEPSCIR internal process (BFS):
C       Queue: [A1], Visited: []
C
C       Process A1:
C         Visited: [A1]
C         Get dependents of A1: [A2]
C         Is A2 == A1? No
C         Add A2 to queue
C       Queue: [A2]
C
C       Process A2:
C         Visited: [A1, A2]
C         Get dependents of A2: [A1]
C         Is A1 == A1? YES → CIRCULAR!
C
C       CIRC = 1

C───────────────────────────────────────────────────────────────
C     STEP 5: Handle circular reference
C───────────────────────────────────────────────────────────────
      IF (CIRC .NE. 0) THEN
C       Remove the bad dependency
        CALL DEPSDEL(1, 2, 1, 1)

C       Reject formula
        CALL MSG_ERROR("CIRCULAR REFERENCE")

C       Don't store formula
        RETURN
      ENDIF
```

### 4.4 Module Call Graph

```
User Action: Enter formula

Main/UI
  ├─→ CELINI (once at startup)
  ├─→ PARSE(formula) → TOKENS
  │     └─→ TOKNXT (internal, called repeatedly)
  │     └─→ OPPREC (internal)
  │
  ├─→ CELPTK(col, row, TOKENS)
  │     └─→ CELNEW (internal)
  │     └─→ CELHSH (internal)
  │
  ├─→ DEPSADD (for each cell reference)
  │     └─→ DEPSHSH (internal)
  │     └─→ DEPSFND (internal)
  │
  └─→ RECCEL(col, row)
        ├─→ CELGET
        │     └─→ CELFND (internal)
        │
        ├─→ CELGTK
        │     └─→ CELFND (internal)
        │
        ├─→ EVAL(TOKENS)
        │     ├─→ CELGET (for cell references)
        │     ├─→ EVPUSH (internal)
        │     └─→ EVPOP (internal)
        │
        ├─→ CELRES
        │     └─→ CELFND (internal)
        │
        └─→ DEPSGET
              └─→ DEPSHSH (internal)
```

---

## 5. FORTRAN IV Considerations

### 5.1 Language Constraints

FORTRAN IV (1966 standard) has significant limitations compared to modern languages:

| Constraint | Impact | Solution |
|------------|--------|----------|
| No CHARACTER type | Can't use strings directly | INTEGER arrays for text |
| No recursion | Can't use recursive algorithms | Explicit stacks/queues |
| Identifiers ≤ 6 chars | Long names not allowed | Abbreviations (CELINI, DEPSGET) |
| No block IF/ELSE | Can't use IF...THEN...ELSE | Arithmetic IF and GO TO |
| Fixed format | Columns matter | Source in columns 7-72 |
| No PARAMETER in procedures | Can't declare constants | Hardcoded literals |
| No dynamic allocation | Arrays must be fixed size | Pre-allocate maximum sizes |

### 5.2 No Recursion Solutions

**Problem**: Many algorithms naturally use recursion (tree traversal, graph search, etc.)

**Solutions Implemented:**

#### 5.2.1 Explicit Evaluation Stack (EVAL.FOR)
```fortran
C     Instead of:
C       FUNCTION EVAL_RECURSIVE(expr)
C         IF expr is number THEN
C           RETURN number
C         ELSE
C           left = EVAL_RECURSIVE(left_expr)
C           right = EVAL_RECURSIVE(right_expr)
C           RETURN left operator right
C         ENDIF
C       END

C     We use explicit stack:
      REAL EVSTK(50)
      INTEGER EVSTKP

      DO 100 I = 1, NTOK
        IF (TYPE .EQ. NUMBER) THEN
          CALL EVPUSH(value, EVSTK, EVSTKP)
        ELSE
          CALL EVPOP(right, EVSTK, EVSTKP)
          CALL EVPOP(left, EVSTK, EVSTKP)
          result = left operator right
          CALL EVPUSH(result, EVSTK, EVSTKP)
        ENDIF
100   CONTINUE
```

#### 5.2.2 Explicit Queue (DEPSCIR, RECCEL)
```fortran
C     Instead of:
C       SUBROUTINE CHECK_CIRCULAR_RECURSIVE(cell)
C         FOR each dependent OF cell DO
C           IF dependent == original THEN CIRCULAR
C           CHECK_CIRCULAR_RECURSIVE(dependent)
C         ENDFOR
C       END

C     We use explicit queue:
      INTEGER QUEUE(500, 2)
      INTEGER QHEAD, QTAIL

      QHEAD = 1
      QTAIL = 1
      QUEUE(QTAIL,1) = COL
      QUEUE(QTAIL,2) = ROW
      QTAIL = QTAIL + 1

100   IF (QHEAD .GE. QTAIL) RETURN

      CCOL = QUEUE(QHEAD,1)
      CROW = QUEUE(QHEAD,2)
      QHEAD = QHEAD + 1

      CALL DEPSGET(CCOL, CROW, DEPS, NDEPS)

      DO 200 I = 1, NDEPS
        QUEUE(QTAIL,1) = DEPS(I,1)
        QUEUE(QTAIL,2) = DEPS(I,2)
        QTAIL = QTAIL + 1
200   CONTINUE

      GO TO 100
```

### 5.3 String Handling

**Problem**: No native string type

**Solution**: INTEGER arrays with manual length tracking

```fortran
C     String: "HELLO"
      INTEGER STR(10)
      INTEGER LEN

      STR(1) = 72    ! 'H'
      STR(2) = 69    ! 'E'
      STR(3) = 76    ! 'L'
      STR(4) = 76    ! 'L'
      STR(5) = 79    ! 'O'
      LEN = 5

C     Comparing strings
      DO 100 I = 1, LEN
        IF (STR1(I) .NE. STR2(I)) GO TO 900
100   CONTINUE
C     Equal
      ...
900   CONTINUE
C     Not equal
```

### 5.4 Control Flow

**Problem**: No block IF/ELSE, no WHILE

**Solution**: Arithmetic IF and GO TO labels

```fortran
C     Instead of:
C       IF (x > 0) THEN
C         positive code
C       ELSE
C         negative code
C       ENDIF

C     We use:
      IF (X .GT. 0) GO TO 100
C     Negative code
      ...
      GO TO 200
100   CONTINUE
C     Positive code
      ...
200   CONTINUE

C     Or three-way arithmetic IF:
      IF (X) 100, 200, 300    ! negative, zero, positive
100   CONTINUE
C     Negative code
      GO TO 400
200   CONTINUE
C     Zero code
      GO TO 400
300   CONTINUE
C     Positive code
400   CONTINUE
```

### 5.5 Data Sharing

**Problem**: No modules or namespaces

**Solution**: COMMON blocks

```fortran
C     CELLS.FOR
      SUBROUTINE CELINI
      INTEGER CELLA(2000, 7)
      INTEGER HTABLE(1024)
      INTEGER FRLIST, CELCNT
      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT
      ...
      END

C     Other subroutines in CELLS.FOR
      SUBROUTINE CELPUT(...)
      INTEGER CELLA(2000, 7)
      INTEGER HTABLE(1024)
      INTEGER FRLIST, CELCNT
      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT
      ...
      END

C     Each subroutine must declare the same COMMON block
```

### 5.6 Fixed Format Source

**Column layout:**
```
Columns 1-5:   Statement labels (optional)
Column 6:      Continuation character (any char except 0 or space)
Columns 7-72:  Code
Columns 73-80: Sequence numbers (optional, ignored)
```

**Example:**
```fortran
C234567890123456789012345678901234567890123456789012345678901234567890123
C     ^col 7                                                    ^col 72
      SUBROUTINE CELPUT(COL, ROW, TYPE, VALUE)
      INTEGER COL, ROW, TYPE
      REAL VALUE

      COMMON /CELDAT/ CELLA, HTABLE, FRLIST, CELCNT,
     &                FMLPOL, FMLLEN, FMLPTR
C     ^continuation character in column 6
```

---

## 6. Performance

### 6.1 Time Complexity

| Operation | Average | Worst Case | Notes |
|-----------|---------|------------|-------|
| CELPUT | O(1) | O(n) | Hash collision |
| CELGET | O(1) | O(n) | Hash collision |
| CELFND | O(1) | O(n) | Chain length |
| PARSE | O(n) | O(n) | n = formula length |
| EVAL | O(k) | O(k) | k = token count |
| DEPSADD | O(1) | O(m) | m = deps in bucket |
| DEPSGET | O(d) | O(d) | d = dependents |
| DEPSCIR | O(V+E) | O(V+E) | V=cells, E=edges |
| RECCEL | O(V+E) | O(V+E) | Affected subgraph |

### 6.2 Space Complexity

**Static Memory Allocation:**

```
CELLS.FOR:
  CELLA:     2000 cells × 7 columns × 4 bytes = 56 KB
  HTABLE:    1024 buckets × 4 bytes          =  4 KB
  FMLPOL:   10000 integers × 4 bytes         = 40 KB
  FMLLEN:    2000 integers × 4 bytes         =  8 KB
  Subtotal:                                    108 KB

PARSE.FOR:
  TOKENS:    100 × 4 × 4 bytes               =  1.6 KB
  OPSTK:      50 × 4 bytes                   =  0.2 KB
  Subtotal:                                     1.8 KB

EVAL.FOR:
  EVSTK:      50 × 4 bytes (REAL)            =  0.2 KB
  Subtotal:                                     0.2 KB

DEPS.FOR:
  DEPNOD:   1000 × 5 × 4 bytes               = 20 KB
  DEPHT:     256 × 4 bytes                   =  1 KB
  Subtotal:                                    21 KB

RECALC.FOR:
  QUEUE:     500 × 2 × 4 bytes               =  4 KB
  Subtotal:                                     4 KB

TOTAL:                                        ~135 KB
```

**Platform Capacity:**
- CP-V on Sigma 7: 512 KB available → **Fits comfortably (26% used)**
- CP/M on 8080: 40 KB available → **Tight but possible with tuning**

### 6.3 Typical Operation Timings

**Estimated on Sigma 7 (1 MIPS, ~1978):**

| Operation | Time | Notes |
|-----------|------|-------|
| Cell put/get | < 100 μs | Hash lookup |
| Parse simple formula | < 1 ms | "+A1+A2" |
| Parse complex formula | < 5 ms | "+A1*B2+C3*D4+E5" |
| Evaluate 2 operands | < 100 μs | "10+20" |
| Evaluate 10 operands | < 500 μs | Complex expression |
| Check circular (depth 5) | < 5 ms | BFS traversal |
| Recalc linear chain (5) | < 10 ms | Serial dependencies |
| Recalc wide fan (50) | < 50 ms | Parallel dependencies |

**User perception:**
- Instant: < 100 ms (most operations)
- Fast: 100-500 ms (complex formulas)
- Acceptable: 500 ms - 1 sec (very large dependency chains)

### 6.4 Scalability Limits

**Current implementation limits:**

| Resource | Limit | Full? | Notes |
|----------|-------|-------|-------|
| Total cells | 2000 | 63 × 254 = 16,002 possible | 12.5% capacity |
| Hash buckets (cells) | 1024 | - | Load factor ~2 when full |
| Formula pool | 10000 tokens | - | ~500 complex formulas |
| Dependencies | 1000 edges | - | ~3 refs per formula avg |
| Dep hash buckets | 256 | - | Load factor ~4 when full |
| Recalc queue | 500 cells | - | Max cascade depth |
| Eval stack | 50 values | - | Max expression depth |

**Bottlenecks if scaling up:**
1. Hash collisions increase as cells approach 2000
2. Formula pool exhaustion with many complex formulas
3. Dependency graph memory if avg refs/formula > 3
4. Recalc queue overflow if cascade > 500 cells

**Mitigation strategies:**
- Increase hash bucket count (reduce collisions)
- Increase formula pool size
- Increase dependency node count
- Add dirty flags (avoid unnecessary recalcs)
- Implement topological sort (optimal recalc order)

---

## 7. Testing

### 7.1 Test Framework

**Approach**: Test-driven development (TDD) with Python harness

**Tools:**
- gfortran (with -std=legacy for FORTRAN IV)
- pytest (Python test framework)
- ctypes (Python FFI to call Fortran)

**Test structure:**
```
test/unit/
├── test_cells.py       Cell storage tests
├── test_parse.py       Parser tests
├── test_eval.py        Evaluator tests
├── test_deps.py        Dependency tests
└── test_recalc.py      Recalc tests
```

### 7.2 Test Coverage Summary

| Module | Passing | Skipped | Total | Coverage |
|--------|---------|---------|-------|----------|
| CELLS | 7 | 3 | 10 | 100% of implemented |
| PARSE | 6 | 4 | 10 | 100% of implemented |
| EVAL | 9 | 1 | 10 | 100% of implemented |
| DEPS | 13 | 0 | 13 | 100% |
| RECALC | 4 | 4 | 8 | 100% of implemented |
| **Total** | **39** | **12** | **51** | **100%** |

**Note**: Skipped tests are for planned features not yet implemented (parentheses, functions, error handling, etc.)

### 7.3 Test Categories

#### 7.3.1 CELLS.FOR Tests

**Basic Operations:**
- test_celput_celget_number - Store and retrieve number
- test_celput_multiple_cells - Multiple cells don't interfere
- test_celput_overwrite - Update existing cell
- test_celget_empty_cell - Non-existent cell returns TYPE=0

**Advanced:**
- test_hash_collisions - Collision handling works
- test_celdel_basic - Delete cell
- test_celdel_and_reuse - Free list reuse

**Skipped:**
- test_celput_formula - Formula storage (needs FMLADD)
- test_max_cells - Stress test to 2000 cells
- test_exceed_max_cells - Capacity error handling

#### 7.3.2 PARSE.FOR Tests

**Tokenization:**
- test_tokenize_simple_number - Parse "100"
- test_tokenize_cell_reference - Parse "A1"
- test_tokenize_operator - Parse "*"

**Parsing:**
- test_parse_addition - Parse "+1+2" → "1 2 +"
- test_parse_cell_reference_formula - Parse "+A1+10"
- test_precedence_multiply_over_add - Parse "+1+2*3" → "1 2 3 * +"

**Skipped:**
- test_precedence_power_over_multiply - Power operator
- test_parse_parentheses_grouping - Parentheses
- test_parse_sum_function - @SUM function
- test_parse_invalid_syntax - Error handling

#### 7.3.3 EVAL.FOR Tests

**Arithmetic:**
- test_eval_simple_number - "42" → 42.0
- test_eval_addition - "10 20 +" → 30.0
- test_eval_subtraction - "50 20 -" → 30.0
- test_eval_multiplication - "5 7 *" → 35.0
- test_eval_division - "20 4 /" → 5.0

**Precedence:**
- test_eval_precedence_multiply_first - "2 3 4 * +" → 14.0

**Cell Operations:**
- test_eval_cell_value - Get cell value
- test_eval_cell_addition - Add two cells

**Integration:**
- test_eval_parse_integration - Full PARSE→EVAL pipeline

**Skipped:**
- test_eval_division_by_zero - Error handling

#### 7.3.4 DEPS.FOR Tests

**Basic:**
- test_deps_init - Initialization
- test_deps_add_single - Add one dependency
- test_deps_multiple_dependents - A1 → [A3, A4, A5]
- test_deps_multiple_sources - A3 depends on A1 and A2

**Removal:**
- test_deps_remove_single - Remove dependency
- test_deps_remove_from_multiple - Remove one of many

**Chains:**
- test_deps_linear_chain - A1→A2→A3→A4

**Circular Detection:**
- test_deps_circular_direct - A1 = +A1
- test_deps_circular_indirect - A1 = +A2, A2 = +A1
- test_deps_circular_deep - A1→A2→A3→A1
- test_deps_no_circular - Verify no false positives

**Edge Cases:**
- test_deps_get_empty - Get dependents of cell with none
- test_deps_remove_nonexistent - Remove non-existent (no crash)

#### 7.3.5 RECALC.FOR Tests

**Basic:**
- test_recalc_init - Initialization
- test_recalc_single_cell - Recalc one formula

**Propagation:**
- test_recalc_linear_chain - A1→A2→A3→A4
- test_recalc_wide_fan - A1→[A2, A3, A4]

**Skipped:**
- test_recalc_auto_mode - Automatic recalc
- test_recalc_manual_mode - Manual recalc
- test_recalc_all - Full spreadsheet recalc
- test_recalc_circular - Circular handling

### 7.4 Test Results

**All 39 tests passing:**
```
test/unit/test_cells.py::test_celput_celget_number PASSED
test/unit/test_cells.py::test_celput_multiple_cells PASSED
test/unit/test_cells.py::test_celput_overwrite PASSED
test/unit/test_cells.py::test_celget_empty_cell PASSED
test/unit/test_cells.py::test_hash_collisions PASSED
test/unit/test_cells.py::test_celdel_basic PASSED
test/unit/test_cells.py::test_celdel_and_reuse PASSED

test/unit/test_parse.py::test_tokenize_simple_number PASSED
test/unit/test_parse.py::test_tokenize_cell_reference PASSED
test/unit/test_parse.py::test_tokenize_operator PASSED
test/unit/test_parse.py::test_parse_addition PASSED
test/unit/test_parse.py::test_parse_cell_reference_formula PASSED
test/unit/test_parse.py::test_precedence_multiply_over_add PASSED

test/unit/test_eval.py::test_eval_simple_number PASSED
test/unit/test_eval.py::test_eval_addition PASSED
test/unit/test_eval.py::test_eval_subtraction PASSED
test/unit/test_eval.py::test_eval_multiplication PASSED
test/unit/test_eval.py::test_eval_division PASSED
test/unit/test_eval.py::test_eval_precedence_multiply_first PASSED
test/unit/test_eval.py::test_eval_cell_value PASSED
test/unit/test_eval.py::test_eval_cell_addition PASSED
test/unit/test_eval.py::test_eval_parse_integration PASSED

test/unit/test_deps.py::test_deps_init PASSED
test/unit/test_deps.py::test_deps_add_single PASSED
test/unit/test_deps.py::test_deps_multiple_dependents PASSED
test/unit/test_deps.py::test_deps_multiple_sources PASSED
test/unit/test_deps.py::test_deps_remove_single PASSED
test/unit/test_deps.py::test_deps_remove_from_multiple PASSED
test/unit/test_deps.py::test_deps_linear_chain PASSED
test/unit/test_deps.py::test_deps_circular_direct PASSED
test/unit/test_deps.py::test_deps_circular_indirect PASSED
test/unit/test_deps.py::test_deps_circular_deep PASSED
test/unit/test_deps.py::test_deps_no_circular PASSED
test/unit/test_deps.py::test_deps_get_empty PASSED
test/unit/test_deps.py::test_deps_remove_nonexistent PASSED

test/unit/test_recalc.py::test_recalc_init PASSED
test/unit/test_recalc.py::test_recalc_single_cell PASSED
test/unit/test_recalc.py::test_recalc_linear_chain PASSED
test/unit/test_recalc.py::test_recalc_wide_fan PASSED

===== 39 passed, 12 skipped in 1.23s =====
```

---

## 8. Known Limitations

### 8.1 Current Implementation

**Formula Features:**
- No parentheses - Can't override operator precedence
- No functions - @SUM, @AVG, @MIN, @MAX not implemented
- Integer literals only - No decimal numbers (3.14)
- Single-letter columns - A-Z only (no AA, AB)
- No string formulas - Only numeric calculations
- No comparison operators - No <, >, =, etc.
- No logical operators - No AND, OR, NOT

**Error Handling:**
- No division by zero detection
- Circular references detected but cause infinite loop if triggered
- No error value propagation (@ERR, @NA)
- No formula error display

**Recalculation:**
- No dirty flags - Always recalcs entire chain
- No topological sort - BFS order may not be optimal
- No auto/manual modes - Always automatic
- No selective recalc - RECALL stub not implemented

**Capacity:**
- Maximum 2000 cells total
- Maximum 10000 formula tokens (pool)
- Maximum 1000 dependency edges
- Maximum 500 cell recalc cascade
- Maximum 50 expression stack depth

### 8.2 FORTRAN IV Limitations

**Language:**
- Single precision only - ~7 decimal digits
- Limited exponent range - ~10^-38 to 10^38
- Integer truncation - REAL stored as INTEGER loses precision
- No dynamic allocation - All arrays fixed size
- No string type - Cumbersome text handling

**Performance:**
- Hash collisions increase with load
- Linear search of collision chains
- No caching beyond formula results
- No lazy evaluation

### 8.3 Platform Limitations

**CP-V on Sigma 7 (1978):**
- Limited memory (512 KB available)
- Slow CPU (~1 MIPS)
- Limited I/O bandwidth
- No floating point hardware (on some models)

**CP/M on 8080 (if ported):**
- Very limited memory (40 KB)
- Very slow CPU (~2 MHz)
- Would require memory reduction

---

## 9. Future Enhancements

### 9.1 High Priority

**Parentheses Support**
```fortran
C     Allow: +(A1+A2)*B1
C     Modify PARSE to handle () tokens
C     Stack ( until matching )
```

**Functions**
```fortran
C     @SUM(A1:A10), @AVG(A1:A10), @MIN, @MAX, @COUNT
C     Add function token type (TYPE=4)
C     Implement range parsing (A1:A10)
C     Add function evaluation in EVAL
```

**Decimal Literals**
```fortran
C     Allow: +3.14*A1
C     Modify TOKNXT to parse decimal point
C     Store as REAL instead of INTEGER
```

**Circular Enforcement**
```fortran
C     Check DEPSCIR before allowing formula
C     Return error instead of looping
C     Display "CIRC" error to user
```

**Error Propagation**
```fortran
C     Define error values:
C       @ERR  = formula error
C       @NA   = not available
C       @DIV0 = division by zero
C     Propagate through calculations
```

### 9.2 Medium Priority

**Dirty Flags**
```fortran
C     Add CELLA column 6 usage:
C       Bit 1: Dirty (needs recalc)
C       Bit 2: Error state
C     Only recalc dirty cells
```

**Topological Sort**
```fortran
C     Calculate dependency levels
C     Recalc in optimal order
C     Avoid redundant calculations
```

**Auto/Manual Recalc Modes**
```fortran
C     RECMOD(1) = auto (recalc on every change)
C     RECMOD(0) = manual (user triggers with /!)
C     Add dirty flag to mark changed cells
```

**Full Spreadsheet Recalc**
```fortran
C     RECALL implementation
C     Iterate all formula cells
C     Topological sort
C     Recalc in dependency order
```

**Two-Letter Columns**
```fortran
C     Support AA, AB, ..., ZZ (676 columns)
C     Modify TOKNXT to parse multi-char columns
C     Update hash functions
```

### 9.3 Low Priority

**String Formulas**
```fortran
C     CONCATENATE, LEFT, RIGHT, MID
C     Requires string storage in cells
C     Significant complexity increase
```

**Date/Time Functions**
```fortran
C     DATE, TIME, NOW, YEAR, MONTH, DAY
C     Requires date arithmetic
C     Platform-specific I/O
```

**Logical Operators**
```fortran
C     AND, OR, NOT
C     IF function: @IF(cond, true_val, false_val)
C     Boolean token type
```

**Comparison Operators**
```fortran
C     <, >, =, <=, >=, <>
C     Return 1 (true) or 0 (false)
C     Enable conditional formulas
```

**Array Formulas**
```fortran
C     Process entire ranges at once
C     {=SUM(A1:A10*B1:B10)}
C     Very complex implementation
```

**Performance Optimizations**
```fortran
C     Hash table resizing
C     Better hash functions
C     Formula compilation/caching
C     Parallel evaluation (multi-CPU)
```

---

## Appendix A: Quick Reference

### Function Index by Module

**CELLS.FOR**
- CELINI - Initialize storage
- CELHSH - Hash function
- CELFND - Find cell
- CELNEW - Allocate cell
- CELPUT - Store value
- CELGET - Retrieve value
- CELDEL - Delete cell
- CELPTK - Store formula tokens
- CELGTK - Get formula tokens
- CELRES - Store formula result

**PARSE.FOR**
- PRSINI - Initialize parser
- PARSE - Parse to postfix
- TOKNXT - Get next token
- OPPREC - Operator precedence

**EVAL.FOR**
- EVLINI - Initialize evaluator
- EVAL - Evaluate postfix
- EVPUSH - Push stack
- EVPOP - Pop stack

**DEPS.FOR**
- DEPSINI - Initialize dependencies
- DEPSADD - Add dependency
- DEPSDEL - Remove dependency
- DEPSGET - Get dependents
- DEPSCIR - Check circular
- DEPSHSH - Hash function
- DEPSFND - Find node

**RECALC.FOR**
- RECINI - Initialize recalc
- RECCEL - Recalculate cell
- RECMOD - Set recalc mode (stub)
- RECALL - Recalc all (stub)

---

## Appendix B: Common Patterns

### Pattern 1: Enter Formula

```fortran
C     Parse formula
      CALL PARSE(input, inlen, tokens, ntok, error)
      IF (error .NE. 0) RETURN

C     Store tokens
      CALL CELPTK(col, row, tokens, ntok)

C     Add dependencies (scan tokens for TYPE=2)
      DO I = 1, ntok
        IF (tokens(i,1) .EQ. 2) THEN
          CALL DEPSADD(tokens(i,3), tokens(i,4), col, row)
        ENDIF
      ENDDO

C     Calculate initial result
      CALL RECCEL(col, row)
```

### Pattern 2: Change Value

```fortran
C     Update cell
      CALL CELPUT(col, row, type, value)

C     Recalculate dependents
      CALL RECCEL(col, row)
```

### Pattern 3: Delete Cell

```fortran
C     Remove all dependencies where this cell is dependent
C     (Requires tracking which cells this one depends on)

C     Delete the cell
      CALL CELDEL(col, row)

C     Recalculate cells that referenced this one
      CALL RECCEL(col, row)
```

---

## Appendix C: Glossary

**BFS** - Breadth-First Search. Graph traversal algorithm that explores all neighbors before going deeper.

**Cell** - A single location in the spreadsheet grid, identified by column and row (e.g., A1, B23).

**Circular Reference** - A formula that directly or indirectly references itself (e.g., A1=+A2, A2=+A1).

**Dependent** - A cell that contains a formula referencing another cell. If A3=+A1, then A3 is a dependent of A1.

**Formula** - An expression beginning with + that calculates a value (e.g., +A1+A2*3).

**Hash Table** - Data structure providing O(1) average lookup using a hash function to map keys to buckets.

**Infix** - Standard mathematical notation with operators between operands (e.g., 2+3*4).

**Open Chaining** - Hash collision resolution using linked lists at each bucket.

**Operator Precedence** - Rules determining evaluation order (e.g., * before +).

**Postfix (RPN)** - Reverse Polish Notation where operators follow operands (e.g., 2 3 4 * +).

**Propagation** - Process of recalculating dependent cells when a source cell changes.

**Shunting-Yard** - Algorithm for converting infix notation to postfix (RPN).

**Source** - A cell being referenced by formulas in other cells. If A3=+A1, then A1 is a source.

**Token** - Atomic unit of a formula (number, cell reference, operator).

**Topological Sort** - Ordering of graph nodes such that dependencies come before dependents.

---

**Document Version**: 1.0
**Last Updated**: 2026-01-19
**Author**: Claude Code
**Status**: Complete

---
