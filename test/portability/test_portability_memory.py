"""
Test suite for verifying memory usage compliance with platform limits.

This test validates that memory usage stays within platform constraints:
- CP/M: ~48 KB total (40-48KB TPA available)
- PDP-11 small: ~64 KB total
- CP-V: 512 KB+ (no constraint)

Memory breakdown for Fortran:
- INTEGER: 2 bytes (16-bit systems) or 4 bytes (32-bit)
- REAL: 4 bytes (single precision)
- CHARACTER: 1 byte

Target validation:
1. Full configuration < 120 KB (comfortable on CP-V)
2. Compact configuration < 40 KB (fits in CP/M TPA)
3. Minimal configuration < 30 KB (minimal systems)
"""

import pytest
import re
from pathlib import Path


class MemoryCalculator:
    """Calculate memory usage based on Fortran array declarations."""

    def __init__(self, integer_size=2, real_size=4):
        """
        Initialize memory calculator.

        Args:
            integer_size: Size of INTEGER in bytes (2 for 16-bit, 4 for 32-bit)
            real_size: Size of REAL in bytes (typically 4)
        """
        self.integer_size = integer_size
        self.real_size = real_size

    def extract_parameters(self, filepath):
        """Extract PARAMETER values from a Fortran source file."""
        with open(filepath, 'r') as f:
            content = f.read()

        params = {}

        # Extract various parameters
        patterns = {
            'MAXCEL': r'PARAMETER\s*\(\s*MAXCEL\s*=\s*(\d+)',
            'HASHSZ': r'HASHSZ\s*=\s*(\d+)',
            'MAXSTR': r'MAXSTR\s*=\s*(\d+)',
            'MAXDEP': r'PARAMETER\s*\(\s*MAXDEP\s*=\s*(\d+)',
            'DEPHSZ': r'DEPHSZ\s*=\s*(\d+)',
            'MAXQUE': r'MAXQUE\s*=\s*(\d+)',
            'MAXTOK': r'PARAMETER\s*\(\s*MAXTOK\s*=\s*(\d+)',
            'MAXSTK': r'MAXSTK\s*=\s*(\d+)',
            'MAXDPS': r'MAXDPS\s*=\s*(\d+)',
        }

        for param, pattern in patterns.items():
            match = re.search(pattern, content)
            if match:
                params[param] = int(match.group(1))

        return params

    def calculate_cells_memory(self, maxcel, hashsz, maxstr):
        """
        Calculate memory usage for CELLS.FOR module.

        Arrays:
        - CELLA(MAXCEL, 7) - INTEGER - Cell data (after removing col 4)
        - CELLV(MAXCEL) - REAL - Cell values
        - CELLR(MAXCEL) - REAL - Formula results
        - HTABLE(HASHSZ) - INTEGER - Hash table
        - FMLPOL(MAXSTR) - INTEGER - Formula pool
        - FMLLEN(MAXCEL) - INTEGER - Formula lengths
        - FRLIST - INTEGER - Free list head (single value)
        - CELCNT - INTEGER - Cell count (single value)
        """
        memory = 0

        # CELLA: Note we removed column 4, so it's now (MAXCEL, 7) not (MAXCEL, 8)
        # But actually checking the code, it was always (MAXCEL, 7)
        memory += maxcel * 7 * self.integer_size  # CELLA

        # REAL arrays for values and results
        memory += maxcel * self.real_size  # CELLV
        memory += maxcel * self.real_size  # CELLR

        # Hash table
        memory += hashsz * self.integer_size  # HTABLE

        # Formula storage
        memory += maxstr * self.integer_size  # FMLPOL
        memory += maxcel * self.integer_size  # FMLLEN

        # Scalars (FRLIST, CELCNT)
        memory += 2 * self.integer_size

        return memory

    def calculate_deps_memory(self, maxdep, dephsz, maxque):
        """
        Calculate memory usage for DEPS.FOR module.

        Arrays:
        - DEPNOD(MAXDEP, 5) - INTEGER - Dependency nodes
        - DEPHT(DEPHSZ) - INTEGER - Dependency hash table
        - QUEUE(MAXQUE, 2) - INTEGER - BFS queue
        - VISIT(MAXQUE) - INTEGER - Visited flags
        """
        memory = 0

        memory += maxdep * 5 * self.integer_size  # DEPNOD
        memory += dephsz * self.integer_size      # DEPHT
        memory += maxque * 2 * self.integer_size  # QUEUE
        memory += maxque * self.integer_size      # VISIT

        return memory

    def calculate_parse_memory(self, maxtok, maxstk):
        """
        Calculate memory usage for PARSE.FOR module.

        Arrays:
        - TOKENS(MAXTOK, 4) - INTEGER - Token array
        - OPSTK(MAXSTK) - INTEGER - Operator stack
        """
        memory = 0

        memory += maxtok * 4 * self.integer_size  # TOKENS
        memory += maxstk * self.integer_size      # OPSTK

        return memory

    def calculate_eval_memory(self, maxtok, maxstk):
        """
        Calculate memory usage for EVAL.FOR module.

        Arrays:
        - TOKENS(MAXTOK, 4) - INTEGER - Token array
        - EVSTK(MAXSTK) - REAL - Evaluation stack
        """
        memory = 0

        memory += maxtok * 4 * self.integer_size  # TOKENS
        memory += maxstk * self.real_size         # EVSTK (REAL)

        return memory

    def calculate_recalc_memory(self, maxque, maxtok, maxdps):
        """
        Calculate memory usage for RECALC.FOR module.

        Arrays:
        - QUEUE(MAXQUE, 2) - INTEGER - Recalc queue
        - TOKENS(MAXTOK, 4) - INTEGER - Token array
        - DEPS(MAXDPS, 2) - INTEGER - Dependencies array
        - VISIT(MAXQUE) - INTEGER - Visited flags
        """
        memory = 0

        memory += maxque * 2 * self.integer_size  # QUEUE
        memory += maxtok * 4 * self.integer_size  # TOKENS
        memory += maxdps * 2 * self.integer_size  # DEPS
        memory += maxque * self.integer_size      # VISIT

        return memory

    def calculate_total_memory(self, params):
        """
        Calculate total program memory usage.

        Returns:
            dict with breakdown: {'data': bytes, 'code_estimate': bytes, 'total': bytes}
        """
        data_memory = 0

        # CELLS.FOR
        cells_mem = self.calculate_cells_memory(
            params.get('MAXCEL', 0),
            params.get('HASHSZ', 0),
            params.get('MAXSTR', 0)
        )
        data_memory += cells_mem

        # DEPS.FOR
        deps_mem = self.calculate_deps_memory(
            params.get('MAXDEP', 0),
            params.get('DEPHSZ', 0),
            params.get('MAXQUE', 0)
        )
        data_memory += deps_mem

        # PARSE.FOR
        parse_mem = self.calculate_parse_memory(
            params.get('MAXTOK', 0),
            params.get('MAXSTK', 0)
        )
        data_memory += parse_mem

        # EVAL.FOR
        eval_mem = self.calculate_eval_memory(
            params.get('MAXTOK', 0),
            params.get('MAXSTK', 0)
        )
        data_memory += eval_mem

        # RECALC.FOR
        recalc_mem = self.calculate_recalc_memory(
            params.get('MAXQUE', 0),
            params.get('MAXTOK', 0),
            params.get('MAXDPS', 0)
        )
        data_memory += recalc_mem

        # Estimate for other modules (UI, I/O, utilities)
        # Conservative estimate: ~3-5 KB
        other_estimate = 4096  # 4 KB

        data_memory += other_estimate

        # Code size estimate
        # Fortran code typically compiles to ~2-3x source size
        # Estimate: ~20 KB for all modules
        code_estimate = 20 * 1024

        total = data_memory + code_estimate

        return {
            'data': data_memory,
            'code_estimate': code_estimate,
            'total': total,
            'breakdown': {
                'cells': cells_mem,
                'deps': deps_mem,
                'parse': parse_mem,
                'eval': eval_mem,
                'recalc': recalc_mem,
                'other': other_estimate,
            }
        }


class TestMemoryUsage:
    """Test memory usage compliance with platform limits."""

    def test_full_config_memory(self):
        """Test that full configuration fits comfortably in CP-V (512 KB)."""
        calc = MemoryCalculator(integer_size=2, real_size=4)

        # Extract parameters from CELLS.FOR (full config)
        params = {}
        params.update(calc.extract_parameters('src/layer1/CELLS.FOR'))
        params.update(calc.extract_parameters('src/layer1/DEPS.FOR'))
        params.update(calc.extract_parameters('src/layer1/PARSE.FOR'))
        params.update(calc.extract_parameters('src/layer1/EVAL.FOR'))
        params.update(calc.extract_parameters('src/layer1/RECALC.FOR'))

        # Calculate memory
        memory = calc.calculate_total_memory(params)

        # Full config should be under 120 KB total
        assert memory['total'] < 120 * 1024, \
            f"Full config {memory['total']/1024:.1f}KB exceeds 120KB limit"

        # Should fit comfortably in CP-V (512 KB available)
        utilization = (memory['total'] / (512 * 1024)) * 100
        assert utilization < 50, \
            f"Full config uses {utilization:.1f}% of CP-V memory (should be < 50%)"

        print(f"\n✓ Full Configuration Memory Usage:")
        print(f"  Data:  {memory['data']/1024:>6.1f} KB")
        print(f"  Code:  {memory['code_estimate']/1024:>6.1f} KB (estimate)")
        print(f"  Total: {memory['total']/1024:>6.1f} KB")
        print(f"  CP-V utilization: {utilization:.1f}% of 512 KB")
        print(f"\n  Breakdown:")
        for module, size in memory['breakdown'].items():
            print(f"    {module:8s}: {size/1024:>6.1f} KB")

    def test_cpm_config_memory(self):
        """Test that CP/M configuration fits in 48 KB limit."""
        calc = MemoryCalculator(integer_size=2, real_size=4)

        # Load CP/M config parameters
        with open('src/config/CONFIG_CPM.FOR', 'r') as f:
            content = f.read()

        # Parse parameters from config file
        params = {
            'MAXCEL': int(re.search(r'MAXCEL\s*=\s*(\d+)', content).group(1)),
            'HASHSZ': int(re.search(r'HASHSZ\s*=\s*(\d+)', content).group(1)),
            'MAXSTR': int(re.search(r'MAXSTR\s*=\s*(\d+)', content).group(1)),
            'MAXDEP': int(re.search(r'MAXDEP\s*=\s*(\d+)', content).group(1)),
            'DEPHSZ': int(re.search(r'DEPHSZ\s*=\s*(\d+)', content).group(1)),
            'MAXQUE': int(re.search(r'MAXQUE\s*=\s*(\d+)', content).group(1)),
            'MAXTOK': int(re.search(r'MAXTOK\s*=\s*(\d+)', content).group(1)),
            'MAXSTK': int(re.search(r'MAXSTK\s*=\s*(\d+)', content).group(1)),
            'MAXDPS': int(re.search(r'MAXDPS\s*=\s*(\d+)', content).group(1)),
        }

        # Calculate memory
        memory = calc.calculate_total_memory(params)

        # CP/M limit: 48 KB total
        # TPA is typically 40-48 KB, we need to fit in ~40 KB to be safe
        CP_M_LIMIT = 40 * 1024

        assert memory['total'] < CP_M_LIMIT, \
            f"CP/M config {memory['total']/1024:.1f}KB exceeds {CP_M_LIMIT/1024}KB limit"

        # Should use < 100% of available TPA (must fit in limit)
        utilization = (memory['total'] / CP_M_LIMIT) * 100
        # Allow up to 100% - the important thing is it fits
        assert utilization <= 100, \
            f"CP/M config uses {utilization:.1f}% of TPA (exceeds 100%)"

        print(f"\n✓ CP/M Configuration Memory Usage:")
        print(f"  Data:  {memory['data']/1024:>6.1f} KB")
        print(f"  Code:  {memory['code_estimate']/1024:>6.1f} KB (estimate)")
        print(f"  Total: {memory['total']/1024:>6.1f} KB")
        print(f"  TPA utilization: {utilization:.1f}% of {CP_M_LIMIT/1024:.0f} KB")
        print(f"  Headroom: {(CP_M_LIMIT - memory['total'])/1024:.1f} KB")
        print(f"\n  Breakdown:")
        for module, size in memory['breakdown'].items():
            print(f"    {module:8s}: {size/1024:>6.1f} KB")

    def test_minimal_config_memory(self):
        """Test that minimal configuration is truly minimal."""
        calc = MemoryCalculator(integer_size=2, real_size=4)

        # Load minimal config parameters
        with open('src/config/CONFIG_MINIMAL.FOR', 'r') as f:
            content = f.read()

        params = {
            'MAXCEL': int(re.search(r'MAXCEL\s*=\s*(\d+)', content).group(1)),
            'HASHSZ': int(re.search(r'HASHSZ\s*=\s*(\d+)', content).group(1)),
            'MAXSTR': int(re.search(r'MAXSTR\s*=\s*(\d+)', content).group(1)),
            'MAXDEP': int(re.search(r'MAXDEP\s*=\s*(\d+)', content).group(1)),
            'DEPHSZ': int(re.search(r'DEPHSZ\s*=\s*(\d+)', content).group(1)),
            'MAXQUE': int(re.search(r'MAXQUE\s*=\s*(\d+)', content).group(1)),
            'MAXTOK': int(re.search(r'MAXTOK\s*=\s*(\d+)', content).group(1)),
            'MAXSTK': int(re.search(r'MAXSTK\s*=\s*(\d+)', content).group(1)),
            'MAXDPS': int(re.search(r'MAXDPS\s*=\s*(\d+)', content).group(1)),
        }

        # Calculate memory
        memory = calc.calculate_total_memory(params)

        # Minimal should be under 30 KB
        assert memory['total'] < 30 * 1024, \
            f"Minimal config {memory['total']/1024:.1f}KB exceeds 30KB limit"

        print(f"\n✓ Minimal Configuration Memory Usage:")
        print(f"  Data:  {memory['data']/1024:>6.1f} KB")
        print(f"  Code:  {memory['code_estimate']/1024:>6.1f} KB (estimate)")
        print(f"  Total: {memory['total']/1024:>6.1f} KB")
        print(f"\n  Breakdown:")
        for module, size in memory['breakdown'].items():
            print(f"    {module:8s}: {size/1024:>6.1f} KB")

    def test_data_vs_code_ratio(self):
        """Test that data doesn't dominate memory usage excessively."""
        calc = MemoryCalculator(integer_size=2, real_size=4)

        # Check full config
        params = {}
        params.update(calc.extract_parameters('src/layer1/CELLS.FOR'))
        params.update(calc.extract_parameters('src/layer1/DEPS.FOR'))
        params.update(calc.extract_parameters('src/layer1/PARSE.FOR'))
        params.update(calc.extract_parameters('src/layer1/EVAL.FOR'))
        params.update(calc.extract_parameters('src/layer1/RECALC.FOR'))

        memory = calc.calculate_total_memory(params)

        data_ratio = memory['data'] / memory['total']

        # Data should be < 85% of total (leaving room for code)
        assert data_ratio < 0.85, \
            f"Data uses {data_ratio*100:.1f}% of total memory (should be < 85%)"

        print(f"\n✓ Data/Code Ratio:")
        print(f"  Data: {data_ratio*100:.1f}%")
        print(f"  Code: {(1-data_ratio)*100:.1f}%")

    def test_32bit_system_memory(self):
        """Test memory usage if compiled for 32-bit system (INTEGER=4 bytes)."""
        # On 32-bit systems like VAX, INTEGER is 4 bytes
        calc = MemoryCalculator(integer_size=4, real_size=4)

        params = {}
        params.update(calc.extract_parameters('src/layer1/CELLS.FOR'))
        params.update(calc.extract_parameters('src/layer1/DEPS.FOR'))
        params.update(calc.extract_parameters('src/layer1/PARSE.FOR'))
        params.update(calc.extract_parameters('src/layer1/EVAL.FOR'))
        params.update(calc.extract_parameters('src/layer1/RECALC.FOR'))

        memory = calc.calculate_total_memory(params)

        # 32-bit full config should still fit in 512 KB
        assert memory['total'] < 200 * 1024, \
            f"32-bit full config {memory['total']/1024:.1f}KB exceeds 200KB limit"

        print(f"\n✓ 32-bit System Memory (INTEGER=4):")
        print(f"  Total: {memory['total']/1024:.1f} KB")
        print(f"  (Full config with 4-byte integers)")


class TestMemoryEfficiency:
    """Test memory efficiency and optimization opportunities."""

    def test_hash_table_load_factor(self):
        """Test that hash table size is appropriate for max cells."""
        calc = MemoryCalculator()

        # Full config
        params = calc.extract_parameters('src/layer1/CELLS.FOR')
        maxcel = params.get('MAXCEL', 0)
        hashsz = params.get('HASHSZ', 0)

        # Load factor if all cells filled
        load_factor = maxcel / hashsz

        # Should be < 3.0 for reasonable performance
        # (Each bucket averages < 3 entries when full)
        assert load_factor < 3.0, \
            f"Load factor {load_factor:.2f} is too high (should be < 3.0)"

        print(f"\n✓ Hash Table Efficiency (Full Config):")
        print(f"  Max cells: {maxcel}")
        print(f"  Hash buckets: {hashsz}")
        print(f"  Load factor: {load_factor:.2f} (avg chain length if full)")

        # CP/M config
        with open('src/config/CONFIG_CPM.FOR', 'r') as f:
            content = f.read()

        maxcel_cpm = int(re.search(r'MAXCEL\s*=\s*(\d+)', content).group(1))
        hashsz_cpm = int(re.search(r'HASHSZ\s*=\s*(\d+)', content).group(1))

        load_factor_cpm = maxcel_cpm / hashsz_cpm

        assert load_factor_cpm < 3.0, \
            f"CP/M load factor {load_factor_cpm:.2f} too high"

        print(f"\n  CP/M Config:")
        print(f"  Max cells: {maxcel_cpm}")
        print(f"  Hash buckets: {hashsz_cpm}")
        print(f"  Load factor: {load_factor_cpm:.2f}")

    def test_string_pool_capacity(self):
        """Test that string pool can accommodate reasonable formulas."""
        calc = MemoryCalculator()

        params = calc.extract_parameters('src/layer1/CELLS.FOR')
        maxcel = params.get('MAXCEL', 0)
        maxstr = params.get('MAXSTR', 0)

        # Average formula length if all cells have formulas
        avg_formula_len = maxstr / maxcel

        # Should support at least 5 characters per cell on average
        assert avg_formula_len >= 5, \
            f"Average formula length {avg_formula_len:.1f} chars is too small"

        print(f"\n✓ String Pool Capacity (Full Config):")
        print(f"  Pool size: {maxstr} characters")
        print(f"  Max cells: {maxcel}")
        print(f"  Avg formula length: {avg_formula_len:.1f} chars/cell")

        # CP/M
        with open('src/config/CONFIG_CPM.FOR', 'r') as f:
            content = f.read()

        maxcel_cpm = int(re.search(r'MAXCEL\s*=\s*(\d+)', content).group(1))
        maxstr_cpm = int(re.search(r'MAXSTR\s*=\s*(\d+)', content).group(1))

        avg_formula_len_cpm = maxstr_cpm / maxcel_cpm

        assert avg_formula_len_cpm >= 5, \
            f"CP/M avg formula length {avg_formula_len_cpm:.1f} too small"

        print(f"\n  CP/M Config:")
        print(f"  Pool size: {maxstr_cpm} characters")
        print(f"  Avg formula length: {avg_formula_len_cpm:.1f} chars/cell")


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
