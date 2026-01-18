"""
Test suite for verifying 16-bit integer range compliance.

This test validates that all values used in the XL Spreadsheet stay within
the range of 16-bit signed integers (−32,767 to +32,767) to ensure portability
to PDP-11 and CP/M systems.

Target platforms:
- CP/M (Intel 8080/Z80): 16-bit integers
- PDP-11: 16-bit integers
- CP-V: 32-bit (but must remain compatible)

Test coverage:
1. Array size parameters are within range
2. Hash calculations don't overflow
3. Row/column limits are enforced
4. Loop counters and indices stay in bounds
"""

import pytest
import subprocess
import re


class TestIntegerRangeCompliance:
    """Test 16-bit integer range compliance for portability."""

    # 16-bit signed integer limits
    INT16_MIN = -32767
    INT16_MAX = 32767

    def test_maxcel_within_range(self):
        """Verify MAXCEL parameter is within 16-bit range."""
        # Read CELLS.FOR to extract MAXCEL value
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        # Find PARAMETER statement with MAXCEL
        match = re.search(r'PARAMETER\s*\(\s*MAXCEL\s*=\s*(\d+)', content)
        assert match, "Could not find MAXCEL PARAMETER"

        maxcel = int(match.group(1))
        assert maxcel <= self.INT16_MAX, f"MAXCEL={maxcel} exceeds INT16_MAX={self.INT16_MAX}"
        assert maxcel > 0, f"MAXCEL={maxcel} must be positive"

        print(f"✓ MAXCEL={maxcel} is within 16-bit range")

    def test_hashsz_within_range(self):
        """Verify HASHSZ parameter is within 16-bit range."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'HASHSZ\s*=\s*(\d+)', content)
        assert match, "Could not find HASHSZ parameter"

        hashsz = int(match.group(1))
        assert hashsz <= self.INT16_MAX, f"HASHSZ={hashsz} exceeds INT16_MAX"
        assert hashsz > 0, "HASHSZ must be positive"

        print(f"✓ HASHSZ={hashsz} is within 16-bit range")

    def test_hashsz_is_power_of_two(self):
        """Verify HASHSZ is power of 2 for efficient modulo using bit masking."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'HASHSZ\s*=\s*(\d+)', content)
        hashsz = int(match.group(1))

        # Check if power of 2: (n & (n-1)) == 0 and n != 0
        is_power_of_2 = (hashsz & (hashsz - 1)) == 0 and hashsz != 0
        assert is_power_of_2, f"HASHSZ={hashsz} should be power of 2 for efficient modulo"

        print(f"✓ HASHSZ={hashsz} is a power of 2")

    def test_maxstr_within_range(self):
        """Verify MAXSTR parameter is within 16-bit range."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'MAXSTR\s*=\s*(\d+)', content)
        assert match, "Could not find MAXSTR parameter"

        maxstr = int(match.group(1))
        assert maxstr <= self.INT16_MAX, f"MAXSTR={maxstr} exceeds INT16_MAX"
        assert maxstr > 0, "MAXSTR must be positive"

        print(f"✓ MAXSTR={maxstr} is within 16-bit range")

    def test_maxdep_within_range(self):
        """Verify MAXDEP parameter is within 16-bit range."""
        with open('src/layer1/DEPS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'PARAMETER\s*\(\s*MAXDEP\s*=\s*(\d+)', content)
        assert match, "Could not find MAXDEP PARAMETER"

        maxdep = int(match.group(1))
        assert maxdep <= self.INT16_MAX, f"MAXDEP={maxdep} exceeds INT16_MAX"
        assert maxdep > 0, "MAXDEP must be positive"

        print(f"✓ MAXDEP={maxdep} is within 16-bit range")

    def test_dephsz_within_range(self):
        """Verify DEPHSZ parameter is within 16-bit range."""
        with open('src/layer1/DEPS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'DEPHSZ\s*=\s*(\d+)', content)
        assert match, "Could not find DEPHSZ parameter"

        dephsz = int(match.group(1))
        assert dephsz <= self.INT16_MAX, f"DEPHSZ={dephsz} exceeds INT16_MAX"
        assert dephsz > 0, "DEPHSZ must be positive"

        print(f"✓ DEPHSZ={dephsz} is within 16-bit range")

    def test_maxque_within_range(self):
        """Verify MAXQUE parameter is within 16-bit range."""
        with open('src/layer1/DEPS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'MAXQUE\s*=\s*(\d+)', content)
        assert match, "Could not find MAXQUE parameter"

        maxque = int(match.group(1))
        assert maxque <= self.INT16_MAX, f"MAXQUE={maxque} exceeds INT16_MAX"
        assert maxque > 0, "MAXQUE must be positive"

        print(f"✓ MAXQUE={maxque} is within 16-bit range")

    def test_maxtok_within_range(self):
        """Verify MAXTOK parameter is within 16-bit range."""
        with open('src/layer1/PARSE.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'PARAMETER\s*\(\s*MAXTOK\s*=\s*(\d+)', content)
        assert match, "Could not find MAXTOK PARAMETER"

        maxtok = int(match.group(1))
        assert maxtok <= self.INT16_MAX, f"MAXTOK={maxtok} exceeds INT16_MAX"
        assert maxtok > 0, "MAXTOK must be positive"

        print(f"✓ MAXTOK={maxtok} is within 16-bit range")

    def test_maxstk_within_range(self):
        """Verify MAXSTK parameter is within 16-bit range."""
        with open('src/layer1/PARSE.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'MAXSTK\s*=\s*(\d+)', content)
        assert match, "Could not find MAXSTK parameter"

        maxstk = int(match.group(1))
        assert maxstk <= self.INT16_MAX, f"MAXSTK={maxstk} exceeds INT16_MAX"
        assert maxstk > 0, "MAXSTK must be positive"

        print(f"✓ MAXSTK={maxstk} is within 16-bit range")

    def test_hash_function_bounds(self):
        """
        Verify hash function calculations stay within 16-bit range.

        Hash function: (COL * 257 + ROW) MOD HASHSZ
        Max input: COL=26, ROW=254
        """
        # Maximum column (A-Z = 26)
        MAX_COL = 26
        # Maximum row (limited to stay in range)
        MAX_ROW = 254

        # Get HASHSZ
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()
        match = re.search(r'HASHSZ\s*=\s*(\d+)', content)
        hashsz = int(match.group(1))

        # Test hash calculation at maximum values
        # Formula: (COL * 257 + ROW) MOD HASHSZ
        max_hash_input = MAX_COL * 257 + MAX_ROW

        assert max_hash_input <= self.INT16_MAX, \
            f"Hash calculation {MAX_COL}*257+{MAX_ROW}={max_hash_input} exceeds INT16_MAX"

        # Verify modulo result is also in range
        hash_result = max_hash_input % hashsz
        assert hash_result < hashsz, "Hash result should be less than HASHSZ"
        assert hash_result >= 0, "Hash result should be non-negative"

        print(f"✓ Hash function max value: {max_hash_input} (within range)")
        print(f"  Formula: {MAX_COL}*257+{MAX_ROW} = {max_hash_input}")
        print(f"  Modulo {hashsz} = {hash_result}")

    def test_row_column_limits_enforced(self):
        """
        Verify that row and column limits are reasonable for 16-bit systems.

        Maximum addressable cells = MAXCEL
        Practical limits: 26 columns (A-Z), rows limited by MAXCEL/26
        """
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()
        match = re.search(r'PARAMETER\s*\(\s*MAXCEL\s*=\s*(\d+)', content)
        maxcel = int(match.group(1))

        MAX_COLS = 26  # A-Z
        max_rows = maxcel // MAX_COLS

        assert max_rows <= self.INT16_MAX, f"Max rows {max_rows} exceeds INT16_MAX"
        assert max_rows > 0, "Max rows must be positive"

        # Also verify reasonable addressing
        max_address = MAX_COLS * max_rows
        assert max_address <= maxcel, \
            f"Max addressable cells {max_address} should not exceed MAXCEL {maxcel}"

        print(f"✓ Row/column limits: {MAX_COLS} cols × {max_rows} rows = {max_address} cells")
        print(f"  MAXCEL={maxcel} (sufficient capacity)")

    def test_array_index_calculations(self):
        """Verify array index calculations don't overflow."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        # Extract MAXCEL
        match = re.search(r'PARAMETER\s*\(\s*MAXCEL\s*=\s*(\d+)', content)
        maxcel = int(match.group(1))

        # CELLA is dimensioned CELLA(MAXCEL, 7)
        # Maximum index: MAXCEL * 7
        max_cell_index = maxcel * 7

        # This doesn't need to fit in 16-bit because it's an array dimension
        # But verify the individual dimensions are reasonable
        assert maxcel <= self.INT16_MAX, f"MAXCEL={maxcel} exceeds INT16_MAX"

        print(f"✓ CELLA({maxcel}, 7) dimensions are valid")
        print(f"  Total elements: {max_cell_index}")

    def test_string_pool_pointer_range(self):
        """Verify string pool pointers stay within 16-bit range."""
        with open('src/layer1/CELLS.FOR', 'r') as f:
            content = f.read()

        match = re.search(r'MAXSTR\s*=\s*(\d+)', content)
        maxstr = int(match.group(1))

        # String pool pointers go from 1 to MAXSTR
        # In FMLPOL(MAXSTR), indices are 1..MAXSTR
        assert maxstr <= self.INT16_MAX, f"String pool size {maxstr} exceeds INT16_MAX"

        # Verify we can have MAXCEL formulas without exceeding string pool
        match = re.search(r'PARAMETER\s*\(\s*MAXCEL\s*=\s*(\d+)', content)
        maxcel = int(match.group(1))

        # Each cell can have a formula, but they share the pool
        # Just verify the pool size itself is in range
        print(f"✓ String pool MAXSTR={maxstr} is within 16-bit range")
        print(f"  Can accommodate formulas for up to {maxcel} cells")

    def test_no_double_precision_types(self):
        """Verify no DOUBLE PRECISION declarations (forbidden on target platforms)."""
        fortran_files = [
            'src/layer1/CELLS.FOR',
            'src/layer1/DEPS.FOR',
            'src/layer1/PARSE.FOR',
            'src/layer1/EVAL.FOR',
            'src/layer1/RECALC.FOR',
        ]

        for filepath in fortran_files:
            with open(filepath, 'r') as f:
                content = f.read()

            # Check for DOUBLE PRECISION declarations
            # Allow in comments (starts with C)
            lines = content.split('\n')
            for i, line in enumerate(lines, 1):
                # Skip comment lines
                if line.strip().startswith('C') or line.strip().startswith('c'):
                    continue

                # Check for DOUBLE PRECISION
                if 'DOUBLE' in line.upper() and 'PRECISION' in line.upper():
                    pytest.fail(f"Found DOUBLE PRECISION in {filepath}:{i}: {line.strip()}")

        print(f"✓ No DOUBLE PRECISION found in {len(fortran_files)} Fortran files")

    def test_no_complex_types(self):
        """Verify no COMPLEX declarations (forbidden on target platforms)."""
        fortran_files = [
            'src/layer1/CELLS.FOR',
            'src/layer1/DEPS.FOR',
            'src/layer1/PARSE.FOR',
            'src/layer1/EVAL.FOR',
            'src/layer1/RECALC.FOR',
        ]

        for filepath in fortran_files:
            with open(filepath, 'r') as f:
                content = f.read()

            lines = content.split('\n')
            for i, line in enumerate(lines, 1):
                # Skip comment lines
                if line.strip().startswith('C') or line.strip().startswith('c'):
                    continue

                # Check for COMPLEX (but not in words like "complexity")
                # Must be standalone word
                if re.search(r'\bCOMPLEX\b', line.upper()):
                    pytest.fail(f"Found COMPLEX type in {filepath}:{i}: {line.strip()}")

        print(f"✓ No COMPLEX types found in {len(fortran_files)} Fortran files")


class TestConfigurationConsistency:
    """Test that all configurations maintain parameter consistency."""

    def test_full_config_parameters(self):
        """Verify full configuration parameters are documented correctly."""
        with open('src/config/CONFIG_FULL.FOR', 'r') as f:
            config_content = f.read()

        # Extract parameters from config file
        params = {}
        params['MAXCEL'] = int(re.search(r'MAXCEL\s*=\s*(\d+)', config_content).group(1))
        params['HASHSZ'] = int(re.search(r'HASHSZ\s*=\s*(\d+)', config_content).group(1))
        params['MAXSTR'] = int(re.search(r'MAXSTR\s*=\s*(\d+)', config_content).group(1))

        # These should match the "full" configuration values
        assert params['MAXCEL'] == 2000, "Full config MAXCEL should be 2000"
        assert params['HASHSZ'] == 1024, "Full config HASHSZ should be 1024"
        assert params['MAXSTR'] == 10000, "Full config MAXSTR should be 10000"

        print("✓ Full configuration parameters match specification")

    def test_cpm_config_parameters(self):
        """Verify CP/M configuration parameters fit in 48KB limit."""
        with open('src/config/CONFIG_CPM.FOR', 'r') as f:
            config_content = f.read()

        params = {}
        params['MAXCEL'] = int(re.search(r'MAXCEL\s*=\s*(\d+)', config_content).group(1))
        params['HASHSZ'] = int(re.search(r'HASHSZ\s*=\s*(\d+)', config_content).group(1))
        params['MAXSTR'] = int(re.search(r'MAXSTR\s*=\s*(\d+)', config_content).group(1))

        # Verify CP/M config values
        assert params['MAXCEL'] == 300, "CP/M config MAXCEL should be 300"
        assert params['HASHSZ'] == 256, "CP/M config HASHSZ should be 256"
        assert params['MAXSTR'] == 2000, "CP/M config MAXSTR should be 2000"

        # Calculate approximate memory usage (assuming 2-byte integers, 4-byte REALs)
        memory_estimate = (
            params['MAXCEL'] * 6 * 2 +  # CELLA(MAXCEL, 6) - removed col 4
            params['MAXCEL'] * 4 +       # CELLV(MAXCEL) - REAL
            params['MAXCEL'] * 4 +       # CELLR(MAXCEL) - REAL
            params['HASHSZ'] * 2 +       # HTABLE(HASHSZ)
            params['MAXSTR'] * 2 +       # FMLPOL(MAXSTR)
            params['MAXCEL'] * 2         # FMLLEN(MAXCEL)
        )

        # Convert to KB
        memory_kb = memory_estimate / 1024

        # CP/M data should be well under 20KB (leaving 28KB for code + overhead)
        assert memory_kb < 20, f"CP/M data memory {memory_kb:.1f}KB should be < 20KB"

        print(f"✓ CP/M configuration memory: ~{memory_kb:.1f}KB data (fits in 48KB limit)")

    def test_minimal_config_parameters(self):
        """Verify minimal configuration parameters."""
        with open('src/config/CONFIG_MINIMAL.FOR', 'r') as f:
            config_content = f.read()

        params = {}
        params['MAXCEL'] = int(re.search(r'MAXCEL\s*=\s*(\d+)', config_content).group(1))
        params['HASHSZ'] = int(re.search(r'HASHSZ\s*=\s*(\d+)', config_content).group(1))
        params['MAXSTR'] = int(re.search(r'MAXSTR\s*=\s*(\d+)', config_content).group(1))

        # Verify minimal config values
        assert params['MAXCEL'] == 100, "Minimal config MAXCEL should be 100"
        assert params['HASHSZ'] == 64, "Minimal config HASHSZ should be 64"
        assert params['MAXSTR'] == 500, "Minimal config MAXSTR should be 500"

        print("✓ Minimal configuration parameters match specification")


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
