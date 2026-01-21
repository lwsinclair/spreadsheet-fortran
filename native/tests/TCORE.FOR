C     TCORE.FOR - Core Canary Tests
C
C     Minimal tests for silent-failure detection:
C       1. Operator precedence (parser)
C       2. Multi-column cell refs (AA1, ZZ1)
C       3. Hash collision chain integrity
C       4. Dependency chain propagation
C       5. Formula reference adjustment
C
C======================================================================

C======================================================================
C     TCORE - Run all core canary tests
C======================================================================
      SUBROUTINE TCORE

      CALL TSTGRP('Operator Precedence')
      CALL TPREC

      CALL TSTGRP('Cell Reference Parsing')
      CALL TCREF

      CALL TSTGRP('Hash Chain Integrity')
      CALL THASH

      CALL TSTGRP('Dependency Propagation')
      CALL TDEPS

      CALL TSTGRP('Formula Ref Adjustment')
      CALL TFADJ

      RETURN
      END

C======================================================================
C     TPREC - Test operator precedence (catches parser bugs)
C======================================================================
      SUBROUTINE TPREC
C     2+3*4 must be 14, not 20. If wrong, parser is broken.

      INTEGER TOKENS(100, 4), NTOK, ERR
      REAL RESULT
      INTEGER FSTR(20), FLEN

C     Initialize
      CALL CELINI

C     Formula: 2+3*4 (ASCII: 50,43,51,42,52)
      FSTR(1) = 50
      FSTR(2) = 43
      FSTR(3) = 51
      FSTR(4) = 42
      FSTR(5) = 52
      FLEN = 5

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL TSTEQ('2+3*4 parses', 0, ERR)

      CALL EVAL(TOKENS, NTOK, RESULT, ERR)
      CALL TSTEQ('2+3*4 evals', 0, ERR)
      CALL TSTREQ('2+3*4 = 14', 14.0, RESULT)

C     Formula: (2+3)*4 = 20 (ASCII: 40,50,43,51,41,42,52)
      FSTR(1) = 40
      FSTR(2) = 50
      FSTR(3) = 43
      FSTR(4) = 51
      FSTR(5) = 41
      FSTR(6) = 42
      FSTR(7) = 52
      FLEN = 7

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL EVAL(TOKENS, NTOK, RESULT, ERR)
      CALL TSTREQ('(2+3)*4 = 20', 20.0, RESULT)

C     Formula: 10-2-3 = 5 (left assoc) not 11
      FSTR(1) = 49
      FSTR(2) = 48
      FSTR(3) = 45
      FSTR(4) = 50
      FSTR(5) = 45
      FSTR(6) = 51
      FLEN = 6

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL EVAL(TOKENS, NTOK, RESULT, ERR)
      CALL TSTREQ('10-2-3 = 5', 5.0, RESULT)

C     Formula: 8/4/2 = 1 (left assoc) not 4
      FSTR(1) = 56
      FSTR(2) = 47
      FSTR(3) = 52
      FSTR(4) = 47
      FSTR(5) = 50
      FLEN = 5

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL EVAL(TOKENS, NTOK, RESULT, ERR)
      CALL TSTREQ('8/4/2 = 1', 1.0, RESULT)

      RETURN
      END

C======================================================================
C     TCREF - Test multi-column cell references
C======================================================================
      SUBROUTINE TCREF
C     Verify AA1=col 27, AZ1=col 52

      INTEGER TOKENS(100, 4), NTOK, ERR
      INTEGER FSTR(20), FLEN
      INTEGER CTYPE
      REAL VALUE

C     Initialize
      CALL CELINI

C     Put values in multi-column cells
      CALL CELPUT(27, 1, 1, 27.0)
      CALL CELPUT(52, 1, 1, 52.0)

C     Verify direct retrieval
      CALL CELGET(27, 1, CTYPE, VALUE)
      CALL TSTREQ('Col 27 (AA) value', 27.0, VALUE)

      CALL CELGET(52, 1, CTYPE, VALUE)
      CALL TSTREQ('Col 52 (AZ) value', 52.0, VALUE)

C     Formula: AA1 (ASCII: 65,65,49)
      FSTR(1) = 65
      FSTR(2) = 65
      FSTR(3) = 49
      FLEN = 3

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL TSTEQ('AA1 parses', 0, ERR)

C     Check token decodes to col=27, row=1
C     Token format: (1)=type, (2)=value, (3)=col, (4)=row
      CALL TSTEQ('AA1 col=27', 27, TOKENS(1,3))
      CALL TSTEQ('AA1 row=1', 1, TOKENS(1,4))

C     Formula: AZ1 (ASCII: 65,90,49)
      FSTR(1) = 65
      FSTR(2) = 90
      FSTR(3) = 49
      FLEN = 3

      CALL PARSE(FSTR, FLEN, TOKENS, NTOK, ERR)
      CALL TSTEQ('AZ1 col=52', 52, TOKENS(1,3))

C     Eval formula referencing multi-col cell
      CALL EVAL(TOKENS, NTOK, VALUE, ERR)
      CALL TSTREQ('AZ1 eval=52', 52.0, VALUE)

      RETURN
      END

C======================================================================
C     THASH - Test hash chain integrity after delete
C======================================================================
      SUBROUTINE THASH
C     Create collision chain, delete middle, verify rest accessible

      INTEGER CTYPE, I
      REAL VALUE

C     Initialize
      CALL CELINI

C     Create cells that likely hash-collide (same col, sequential rows)
      DO 10 I = 1, 10
        CALL CELPUT(1, I, 1, REAL(I * 10))
10    CONTINUE

C     Verify all exist
      CALL CELGET(1, 5, CTYPE, VALUE)
      CALL TSTEQ('A5 exists before del', 1, CTYPE)
      CALL TSTREQ('A5 = 50', 50.0, VALUE)

C     Delete middle of chain
      CALL CELDEL(1, 5)

C     Verify deleted cell is gone
      CALL CELGET(1, 5, CTYPE, VALUE)
      CALL TSTEQ('A5 gone after del', 0, CTYPE)

C     Verify cells after deletion point still accessible
      CALL CELGET(1, 6, CTYPE, VALUE)
      CALL TSTEQ('A6 exists after del', 1, CTYPE)
      CALL TSTREQ('A6 = 60', 60.0, VALUE)

      CALL CELGET(1, 10, CTYPE, VALUE)
      CALL TSTEQ('A10 exists after del', 1, CTYPE)
      CALL TSTREQ('A10 = 100', 100.0, VALUE)

C     Verify cells before deletion point ok
      CALL CELGET(1, 1, CTYPE, VALUE)
      CALL TSTREQ('A1 = 10', 10.0, VALUE)

      RETURN
      END

C======================================================================
C     TDEPS - Test dependency chain propagation
C======================================================================
      SUBROUTINE TDEPS
C     A1=10, B1=A1*2. Recalc B1, verify it gets 20.

      INTEGER TOKENS(100, 4), NTOK, ERR
      INTEGER CTYPE
      REAL VALUE
      INTEGER FSTR(20), FLEN

C     Initialize
      CALL CELINI
      CALL DEPSINI

C     A1 = 10
      CALL CELPUT(1, 1, 1, 10.0)

C     B1 formula = A1*2 (ASCII: A=65,1=49,*=42,2=50)
      FSTR(1) = 65
      FSTR(2) = 49
      FSTR(3) = 42
      FSTR(4) = 50
      FLEN = 4
      CALL CELPFS(2, 1, FSTR, FLEN)
      CALL CELPUT(2, 1, 2, 0.0)

C     Add dependency: B1 depends on A1
      CALL DEPSADD(1, 1, 2, 1)

C     Recalculate B1
      CALL RECCEL(2, 1)

C     Verify B1 = 20
      CALL CELGET(2, 1, CTYPE, VALUE)
      CALL TSTEQ('B1 is formula', 2, CTYPE)
      CALL TSTREQ('B1 = A1*2 = 20', 20.0, VALUE)

C     Change A1 to 15
      CALL CELPUT(1, 1, 1, 15.0)

C     Recalculate B1 again
      CALL RECCEL(2, 1)

C     Verify B1 = 30
      CALL CELGET(2, 1, CTYPE, VALUE)
      CALL TSTREQ('B1 = A1*2 = 30', 30.0, VALUE)

      RETURN
      END

C======================================================================
C     TFADJ - Test formula reference adjustment
C======================================================================
      SUBROUTINE TFADJ
C     Test ADJROW/ADJCOL for row/col insert/delete

      INTEGER FSTR(80), FLEN
      INTEGER NFSTR(80), NFLEN

C     Test: B2 -> B3 after row insert at 2
C     Input: B2 (ASCII: B=66, 2=50)
      FSTR(1) = 66
      FSTR(2) = 50
      FLEN = 2

      CALL ADJROW(FSTR, FLEN, NFSTR, NFLEN, 2, 1)

C     Should become B3 (66, 51)
      CALL TSTEQ('B2->B3 len', 2, NFLEN)
      CALL TSTEQ('B2->B3 col', 66, NFSTR(1))
      CALL TSTEQ('B2->B3 row', 51, NFSTR(2))

C     Test: B$2 should NOT change (absolute row)
C     Input: B$2 (ASCII: B=66, $=36, 2=50)
      FSTR(1) = 66
      FSTR(2) = 36
      FSTR(3) = 50
      FLEN = 3

      CALL ADJROW(FSTR, FLEN, NFSTR, NFLEN, 2, 1)

C     Should stay B$2
      CALL TSTEQ('B$2 len', 3, NFLEN)
      CALL TSTEQ('B$2 $', 36, NFSTR(2))
      CALL TSTEQ('B$2 row', 50, NFSTR(3))

C     Test: C1 -> B1 after col delete at B
C     Input: C1 (ASCII: C=67, 1=49)
      FSTR(1) = 67
      FSTR(2) = 49
      FLEN = 2

      CALL ADJCOL(FSTR, FLEN, NFSTR, NFLEN, 2, -1)

C     Should become B1 (66, 49)
      CALL TSTEQ('C1->B1 len', 2, NFLEN)
      CALL TSTEQ('C1->B1 col', 66, NFSTR(1))
      CALL TSTEQ('C1->B1 row', 49, NFSTR(2))

C     Test: $C1 should NOT change (absolute col)
C     Input: $C1 (ASCII: $=36, C=67, 1=49)
      FSTR(1) = 36
      FSTR(2) = 67
      FSTR(3) = 49
      FLEN = 3

      CALL ADJCOL(FSTR, FLEN, NFSTR, NFLEN, 2, -1)

C     Should stay $C1
      CALL TSTEQ('$C1 len', 3, NFLEN)
      CALL TSTEQ('$C1 $', 36, NFSTR(1))
      CALL TSTEQ('$C1 col', 67, NFSTR(2))

      RETURN
      END

