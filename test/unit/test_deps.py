"""
Unit Tests: DEPS.FOR - Dependency Tracking

Tests the dependency tracking system that maintains relationships between cells.

Purpose: Track which cells depend on other cells for recalculation.

Data Structure: Hash table with linked lists
  - Hash by source cell (the cell being referenced)
  - Each node stores one dependency relationship
  - Supports multiple dependents per source cell

Example:
  A3 = +A1+A2

  Dependencies stored:
    A1 -> [A3]  (A3 depends on A1)
    A2 -> [A3]  (A3 depends on A2)

Test Coverage:
    - Initialize dependency graph
    - Add dependencies
    - Get dependents of a cell
    - Remove dependencies
    - Circular reference detection
    - Multiple dependents
    - Deep chains
"""

import sys
from pathlib import Path
import pytest

sys.path.insert(0, str(Path(__file__).parent.parent / 'framework'))

from fortran_tester import FortranTester


class TestDepsBasic:
    """Basic dependency operations"""

    def test_deps_init(self):
        """
        Initialize dependency graph.

        Should initialize internal structures successfully.
        """
        test_program = """
      PROGRAM TEST

C     Initialize dependency tracking
      CALL DEPSINI

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_add_single(self):
        """
        Add single dependency: A3 depends on A1.

        Setup: A3 = +A1
        Result: A1 -> [A3]
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS

      CALL DEPSINI

C     Add dependency: A3 depends on A1
C     Source: A1 (col=1, row=1)
C     Dependent: A3 (col=1, row=3)
      CALL DEPSADD(1, 1, 1, 3)

C     Get dependents of A1
      CALL DEPSGET(1, 1, DEPS, NDEPS)

C     Should have 1 dependent
      IF (NDEPS .NE. 1) STOP 1

C     Dependent should be A3 (col=1, row=3)
      IF (DEPS(1,1) .NE. 1) STOP 2
      IF (DEPS(1,2) .NE. 3) STOP 3

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_multiple_dependents(self):
        """
        Multiple cells depend on one cell.

        Setup:
          A3 = +A1
          A4 = +A1*2
          A5 = +A1+10

        Result: A1 -> [A3, A4, A5]
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS
      INTEGER I, FOUND3, FOUND4, FOUND5

      CALL DEPSINI

C     A3 depends on A1
      CALL DEPSADD(1, 1, 1, 3)

C     A4 depends on A1
      CALL DEPSADD(1, 1, 1, 4)

C     A5 depends on A1
      CALL DEPSADD(1, 1, 1, 5)

C     Get dependents of A1
      CALL DEPSGET(1, 1, DEPS, NDEPS)

C     Should have 3 dependents
      IF (NDEPS .NE. 3) STOP 1

C     Check all three are present
      FOUND3 = 0
      FOUND4 = 0
      FOUND5 = 0

      DO 100 I = 1, NDEPS
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 3) FOUND3 = 1
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 4) FOUND4 = 1
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 5) FOUND5 = 1
100   CONTINUE

      IF (FOUND3 .NE. 1) STOP 2
      IF (FOUND4 .NE. 1) STOP 3
      IF (FOUND5 .NE. 1) STOP 4

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_multiple_sources(self):
        """
        One cell depends on multiple cells.

        Setup: A3 = +A1+A2

        Result:
          A1 -> [A3]
          A2 -> [A3]
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS

      CALL DEPSINI

C     A3 depends on A1
      CALL DEPSADD(1, 1, 1, 3)

C     A3 depends on A2
      CALL DEPSADD(1, 2, 1, 3)

C     A1 should have A3 as dependent
      CALL DEPSGET(1, 1, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 1
      IF (DEPS(1,1) .NE. 1) STOP 2
      IF (DEPS(1,2) .NE. 3) STOP 3

C     A2 should have A3 as dependent
      CALL DEPSGET(1, 2, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 4
      IF (DEPS(1,1) .NE. 1) STOP 5
      IF (DEPS(1,2) .NE. 3) STOP 6

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestDepsRemove:
    """Dependency removal tests"""

    def test_deps_remove_single(self):
        """
        Remove a dependency.

        Setup:
          Add: A3 depends on A1
          Remove: A3 depends on A1

        Result: A1 -> []
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS

      CALL DEPSINI

C     Add dependency
      CALL DEPSADD(1, 1, 1, 3)

C     Verify it exists
      CALL DEPSGET(1, 1, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 1

C     Remove dependency
      CALL DEPSDEL(1, 1, 1, 3)

C     Should now have 0 dependents
      CALL DEPSGET(1, 1, DEPS, NDEPS)
      IF (NDEPS .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_remove_from_multiple(self):
        """
        Remove one dependency, others remain.

        Setup:
          Add: A1 -> [A3, A4, A5]
          Remove: A4 from A1

        Result: A1 -> [A3, A5]
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS
      INTEGER I, FOUND3, FOUND4, FOUND5

      CALL DEPSINI

C     Add three dependencies
      CALL DEPSADD(1, 1, 1, 3)
      CALL DEPSADD(1, 1, 1, 4)
      CALL DEPSADD(1, 1, 1, 5)

C     Remove A4
      CALL DEPSDEL(1, 1, 1, 4)

C     Get remaining dependents
      CALL DEPSGET(1, 1, DEPS, NDEPS)

C     Should have 2 dependents
      IF (NDEPS .NE. 2) STOP 1

C     Check A3 and A5 present, A4 absent
      FOUND3 = 0
      FOUND4 = 0
      FOUND5 = 0

      DO 100 I = 1, NDEPS
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 3) FOUND3 = 1
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 4) FOUND4 = 1
        IF (DEPS(I,1) .EQ. 1 .AND. DEPS(I,2) .EQ. 5) FOUND5 = 1
100   CONTINUE

      IF (FOUND3 .NE. 1) STOP 2
      IF (FOUND4 .NE. 0) STOP 3
      IF (FOUND5 .NE. 1) STOP 4

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestDepsChains:
    """Dependency chain tests"""

    def test_deps_linear_chain(self):
        """
        Linear dependency chain.

        Setup:
          A1 = 10
          A2 = +A1
          A3 = +A2
          A4 = +A3

        Result:
          A1 -> [A2]
          A2 -> [A3]
          A3 -> [A4]
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS

      CALL DEPSINI

C     Build chain
      CALL DEPSADD(1, 1, 1, 2)  ! A2 depends on A1
      CALL DEPSADD(1, 2, 1, 3)  ! A3 depends on A2
      CALL DEPSADD(1, 3, 1, 4)  ! A4 depends on A3

C     Verify A1 -> A2
      CALL DEPSGET(1, 1, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 1
      IF (DEPS(1,2) .NE. 2) STOP 2

C     Verify A2 -> A3
      CALL DEPSGET(1, 2, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 3
      IF (DEPS(1,2) .NE. 3) STOP 4

C     Verify A3 -> A4
      CALL DEPSGET(1, 3, DEPS, NDEPS)
      IF (NDEPS .NE. 1) STOP 5
      IF (DEPS(1,2) .NE. 4) STOP 6

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestDepsCircular:
    """Circular reference detection tests"""

    def test_deps_circular_direct(self):
        """
        Detect direct circular reference.

        Setup: A1 = +A1 (direct self-reference)

        Result: DEPSCIR(A1) = true
        """
        test_program = """
      PROGRAM TEST
      INTEGER CIRC

      CALL DEPSINI

C     A1 depends on A1 (direct circular)
      CALL DEPSADD(1, 1, 1, 1)

C     Check for circular reference
      CALL DEPSCIR(1, 1, CIRC)

C     Should detect circular reference
      IF (CIRC .NE. 1) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_circular_indirect(self):
        """
        Detect indirect circular reference.

        Setup:
          A1 = +A2
          A2 = +A1

        Result: DEPSCIR(A1) = true, DEPSCIR(A2) = true
        """
        test_program = """
      PROGRAM TEST
      INTEGER CIRC

      CALL DEPSINI

C     A1 depends on A2
      CALL DEPSADD(1, 2, 1, 1)

C     A2 depends on A1
      CALL DEPSADD(1, 1, 1, 2)

C     Check A1 for circular reference
      CALL DEPSCIR(1, 1, CIRC)
      IF (CIRC .NE. 1) STOP 1

C     Check A2 for circular reference
      CALL DEPSCIR(1, 2, CIRC)
      IF (CIRC .NE. 1) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_circular_deep(self):
        """
        Detect deep circular reference.

        Setup:
          A1 = +A2
          A2 = +A3
          A3 = +A1

        Result: DEPSCIR(A1) = true
        """
        test_program = """
      PROGRAM TEST
      INTEGER CIRC

      CALL DEPSINI

C     Build circular chain
      CALL DEPSADD(1, 2, 1, 1)  ! A1 depends on A2
      CALL DEPSADD(1, 3, 1, 2)  ! A2 depends on A3
      CALL DEPSADD(1, 1, 1, 3)  ! A3 depends on A1 (circle!)

C     Check for circular reference
      CALL DEPSCIR(1, 1, CIRC)
      IF (CIRC .NE. 1) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_no_circular(self):
        """
        Verify no false positives for circular detection.

        Setup:
          A1 = 10
          A2 = +A1
          A3 = +A1
          A4 = +A2+A3

        Result: DEPSCIR(A1) = false (no circular reference)
        """
        test_program = """
      PROGRAM TEST
      INTEGER CIRC

      CALL DEPSINI

C     Build non-circular dependencies
      CALL DEPSADD(1, 1, 1, 2)  ! A2 depends on A1
      CALL DEPSADD(1, 1, 1, 3)  ! A3 depends on A1
      CALL DEPSADD(1, 2, 1, 4)  ! A4 depends on A2
      CALL DEPSADD(1, 3, 1, 4)  ! A4 depends on A3

C     Check A1 - should be no circular reference
      CALL DEPSCIR(1, 1, CIRC)
      IF (CIRC .NE. 0) STOP 1

C     Check A2 - should be no circular reference
      CALL DEPSCIR(1, 2, CIRC)
      IF (CIRC .NE. 0) STOP 2

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


class TestDepsEdgeCases:
    """Edge case tests"""

    def test_deps_get_empty(self):
        """
        Get dependents of cell with no dependents.

        Setup: (empty graph)
        Query: DEPSGET(A1)
        Result: NDEPS = 0
        """
        test_program = """
      PROGRAM TEST
      INTEGER DEPS(100, 2), NDEPS

      CALL DEPSINI

C     Get dependents of A1 (none added)
      CALL DEPSGET(1, 1, DEPS, NDEPS)

C     Should have 0 dependents
      IF (NDEPS .NE. 0) STOP 1

      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"

    def test_deps_remove_nonexistent(self):
        """
        Remove a dependency that doesn't exist.

        Should not crash, just do nothing.
        """
        test_program = """
      PROGRAM TEST

      CALL DEPSINI

C     Try to remove nonexistent dependency
      CALL DEPSDEL(1, 1, 1, 3)

C     Should not crash
      WRITE(*,*) 1
      STOP
      END
"""
        tester = FortranTester()
        result, _ = tester.compile_and_run(
            ['layer0/STRUTIL.FOR', 'layer1/CELLS.FOR',
             'layer1/PARSE.FOR', 'layer1/EVAL.FOR', 'layer1/DEPS.FOR'],
            test_program
        )
        assert result.strip() == "1", f"Expected success, got: {result}"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
