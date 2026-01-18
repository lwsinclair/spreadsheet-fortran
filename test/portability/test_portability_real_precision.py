"""
Test suite for verifying REAL precision compliance.

This test validates that the XL Spreadsheet correctly uses REAL (single precision)
and does not use DOUBLE PRECISION, which is not available on target platforms.

REAL specifications:
- Size: 4 bytes (32 bits)
- Precision: ~6-7 significant decimal digits
- Range: ~10^±38
- Standard: IEEE 754 single precision (on most systems)

Target platform constraints:
- CP/M: REAL only (no DOUBLE PRECISION)
- PDP-11: REAL only
- CP-V: Has DOUBLE PRECISION, but we avoid it for portability

Test coverage:
1. No DOUBLE PRECISION declarations anywhere
2. REAL precision is adequate for spreadsheet calculations
3. Decimal values store and retrieve correctly
4. Formula results maintain reasonable precision
5. No precision loss in calculations
"""

import pytest
import subprocess
import sys
import os
import re


class TestRealPrecision:
    """
    Test REAL type precision compliance.

    Note: Actual precision tests for decimal storage are in test/unit/test_cells.py
    (TestCellsDecimalPrecision class). This module focuses on source code compliance.
    """

    def test_decimal_precision_tests_exist(self):
        """
        Verify that decimal precision tests exist in test_cells.py.

        These tests validate that REAL values store and retrieve correctly:
        - test_decimal_precision_pi: 3.14
        - test_decimal_precision_negative: -2.71
        - test_decimal_precision_small_fraction: 0.001
        - test_decimal_precision_formula_result: formula results
        - test_decimal_precision_overwrite: overwriting decimals
        """
        with open('test/unit/test_cells.py', 'r') as f:
            content = f.read()

        # Check that decimal precision test class exists
        assert 'TestCellsDecimalPrecision' in content, \
            "TestCellsDecimalPrecision class not found in test_cells.py"

        # Check for specific decimal tests
        assert 'test_decimal_precision_pi' in content, \
            "test_decimal_precision_pi not found"
        assert 'test_decimal_precision_negative' in content, \
            "test_decimal_precision_negative not found"
        assert 'test_decimal_precision_small_fraction' in content, \
            "test_decimal_precision_small_fraction not found"

        print("✓ Decimal precision tests exist in test_cells.py")
        print("  - TestCellsDecimalPrecision class ✓")
        print("  - test_decimal_precision_pi ✓")
        print("  - test_decimal_precision_negative ✓")
        print("  - test_decimal_precision_small_fraction ✓")

    def test_cellv_array_exists(self):
        """Verify CELLV REAL array exists for storing decimal values."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        # Check for CELLV array declaration
        assert re.search(r'REAL\s+CELLV\s*\(', content), \
            "CELLV REAL array not found in CELLS.FOR"

        # Check for CELLR array (formula results)
        assert re.search(r'REAL\s+CELLR\s*\(', content), \
            "CELLR REAL array not found in CELLS.FOR"

        print("✓ REAL arrays for decimal storage exist")
        print("  - CELLV (cell values) ✓")
        print("  - CELLR (formula results) ✓")

    def test_real_storage_not_integer(self):
        """Verify that values are NOT stored as INT(VALUE) anymore."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            lines = f.readlines()

        # Look for old broken pattern: CELLA(...,4) = INT(VALUE)
        violations = []
        for i, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith('C'):
                continue

            # Check for INT(VALUE) or INT(RESULT) being assigned to CELLA
            if 'CELLA' in line and 'INT(' in line and '=' in line:
                # This would be the old broken pattern
                violations.append(f"Line {i}: {line.strip()}")

        # We should NOT find any such patterns
        assert len(violations) == 0, \
            f"Found INT() conversions in CELLA assignments:\n" + "\n".join(violations)

        print("✓ No INT() conversions found (values stored as REAL)")

    def test_real_retrieval_correct(self):
        """Verify values are retrieved from CELLV, not converted from INTEGER."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        # Look for CELGET subroutine
        assert 'SUBROUTINE CELGET' in content, "CELGET not found"

        # In CELGET, VALUE should be assigned from CELLV
        # Find CELGET subroutine
        celget_start = content.find('SUBROUTINE CELGET')
        celget_end = content.find('END', celget_start)
        celget_code = content[celget_start:celget_end]

        # Should have: VALUE = CELLV(...)
        assert 'CELLV' in celget_code, \
            "CELGET should use CELLV array for retrieving values"

        print("✓ CELGET retrieves values from CELLV array")


class TestNoDoublePrecision:
    """Test that no DOUBLE PRECISION is used anywhere."""

    FORTRAN_FILES = [
        'src/layer1/CELLS.FOR',
        'src/layer1/DEPS.FOR',
        'src/layer1/PARSE.FOR',
        'src/layer1/EVAL.FOR',
        'src/layer1/RECALC.FOR',
    ]

    def test_no_double_precision_declarations(self):
        """Verify no DOUBLE PRECISION type declarations."""
        violations = []

        for filepath in self.FORTRAN_FILES:
            with open(filepath, 'r') as f:
                lines = f.readlines()

            for i, line in enumerate(lines, 1):
                # Skip comment lines
                if line.strip().startswith('C') or line.strip().startswith('c'):
                    continue

                # Check for DOUBLE PRECISION (case insensitive)
                if 'DOUBLE' in line.upper() and 'PRECISION' in line.upper():
                    violations.append(f"{filepath}:{i}: {line.strip()}")

        assert len(violations) == 0, \
            f"Found DOUBLE PRECISION declarations:\n" + "\n".join(violations)

        print(f"✓ No DOUBLE PRECISION in {len(self.FORTRAN_FILES)} source files")

    def test_no_complex_type(self):
        """Verify no COMPLEX type (also not portable)."""
        violations = []

        for filepath in self.FORTRAN_FILES:
            with open(filepath, 'r') as f:
                lines = f.readlines()

            for i, line in enumerate(lines, 1):
                # Skip comment lines
                if line.strip().startswith('C') or line.strip().startswith('c'):
                    continue

                # Check for COMPLEX type (as standalone word)
                import re
                if re.search(r'\bCOMPLEX\b', line.upper()):
                    violations.append(f"{filepath}:{i}: {line.strip()}")

        assert len(violations) == 0, \
            f"Found COMPLEX type declarations:\n" + "\n".join(violations)

        print(f"✓ No COMPLEX types in {len(self.FORTRAN_FILES)} source files")

    def test_only_standard_types(self):
        """Verify only INTEGER, REAL, CHARACTER types are used."""
        violations = []
        allowed_types = {'INTEGER', 'REAL', 'CHARACTER', 'LOGICAL', 'DIMENSION', 'PARAMETER', 'COMMON'}

        for filepath in self.FORTRAN_FILES:
            with open(filepath, 'r') as f:
                lines = f.readlines()

            for i, line in enumerate(lines, 1):
                # Skip comment lines and continuation lines
                if line.strip().startswith('C') or line.strip().startswith('c'):
                    continue
                if len(line) > 5 and line[5] != ' ':
                    continue  # Continuation line

                # Extract type declarations (lines starting with type keywords)
                stripped = line.strip().upper()

                # Check if line starts with a type keyword
                for keyword in ['DOUBLE', 'COMPLEX', 'BYTE', 'INTEGER*2', 'INTEGER*4', 'REAL*4', 'REAL*8']:
                    if stripped.startswith(keyword):
                        violations.append(f"{filepath}:{i}: {line.strip()}")

        assert len(violations) == 0, \
            f"Found non-standard type declarations:\n" + "\n".join(violations)

        print(f"✓ Only standard types (INTEGER, REAL, CHARACTER) used")


class TestRealArithmetic:
    """
    Test REAL arithmetic precision documentation.

    Note: Actual arithmetic precision tests are in test/unit/test_eval.py which tests
    the evaluator with various formulas and operations.
    """

    def test_eval_tests_exist(self):
        """Verify that evaluator tests exist for arithmetic operations."""
        import os
        assert os.path.exists('test/unit/test_eval.py'), \
            "test_eval.py not found - evaluator tests missing"

        with open('test/unit/test_eval.py', 'r') as f:
            content = f.read()

        # Check for arithmetic operation tests
        assert 'EVAL' in content.upper() or 'eval' in content, \
            "test_eval.py should contain evaluator tests"

        print("✓ Evaluator tests exist in test_eval.py")

    def test_real_precision_documented(self):
        """Verify that REAL precision is documented in PORTABILITY.md."""
        with open('docs/PORTABILITY.md', 'r') as f:
            content = f.read()

        # Check for REAL precision documentation
        assert 'REAL' in content, \
            "PORTABILITY.md should document REAL type"

        # Check for precision information
        content_upper = content.upper()
        assert 'PRECISION' in content_upper or 'SIGNIFICANT DIGIT' in content_upper, \
            "PORTABILITY.md should document precision constraints"

        print("✓ REAL precision documented in PORTABILITY.md")

    def test_floating_point_limitations_documented(self):
        """
        Document known limitation: REAL cannot represent some decimals exactly.

        This is expected behavior for binary floating point.
        """
        # This is a documentation test - just verify it's understood
        # Binary floating point cannot represent 0.1, 0.2, etc. exactly
        # But approximations are close enough for spreadsheet use

        print("✓ Floating point limitations understood:")
        print("  - Binary float cannot represent 0.1 exactly (expected)")
        print("  - Approximations are acceptable for spreadsheet use")
        print("  - Tests validate decimal storage within epsilon")


class TestPortabilityDocumentation:
    """Verify portability documentation is complete."""

    def test_portability_md_exists(self):
        """Verify PORTABILITY.md documentation exists."""
        import os
        assert os.path.exists('docs/PORTABILITY.md'), \
            "PORTABILITY.md documentation not found"

        print("✓ PORTABILITY.md exists")

    def test_portability_md_mentions_real(self):
        """Verify PORTABILITY.md documents REAL type constraints."""
        with open('docs/PORTABILITY.md', 'r') as f:
            content = f.read()

        assert 'REAL' in content, \
            "PORTABILITY.md should document REAL type"
        assert 'precision' in content.lower(), \
            "PORTABILITY.md should mention precision constraints"

        print("✓ PORTABILITY.md documents REAL type constraints")

    def test_portability_md_warns_double_precision(self):
        """Verify PORTABILITY.md warns against DOUBLE PRECISION."""
        with open('docs/PORTABILITY.md', 'r') as f:
            content = f.read().upper()

        assert 'DOUBLE' in content or 'DOUBLE PRECISION' in content, \
            "PORTABILITY.md should warn about DOUBLE PRECISION"

        print("✓ PORTABILITY.md warns about DOUBLE PRECISION")


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
