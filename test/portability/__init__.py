"""
Portability test suite for XL Spreadsheet.

This package contains tests to verify portability constraints for:
- PDP-11 (16-bit, ~64KB memory)
- CP/M (8-bit CPU, ~48KB memory)
- CP-V (32-bit, reference platform)

Test modules:
- test_portability_integer_range: Verify 16-bit integer compliance
- test_portability_memory: Verify memory usage fits platform limits
- test_portability_real_precision: Verify REAL type usage and precision
"""

__all__ = [
    'test_portability_integer_range',
    'test_portability_memory',
    'test_portability_real_precision',
]
