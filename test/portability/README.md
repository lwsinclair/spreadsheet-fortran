# Portability Test Suite

This directory contains automated tests to validate that the XL Spreadsheet codebase meets portability constraints for target platforms:

- **CP/M** (Intel 8080/Z80, 48KB RAM limit)
- **PDP-11** (16-bit, 64KB+ RAM)
- **CP-V** (Xerox Sigma 7, 512KB+ RAM)

## Test Modules

### 1. `test_portability_integer_range.py` (18 tests)

Validates 16-bit integer compliance:
- All array size parameters within ±32,767 range
- Hash function calculations don't overflow
- Row/column limits enforced
- No DOUBLE PRECISION or COMPLEX types used
- Configuration consistency across templates

**Key Validations:**
- MAXCEL, HASHSZ, MAXSTR, MAXDEP, etc. all < 32,767 ✓
- Hash function max: (26 * 257 + 254) = 6,936 ✓
- HASHSZ is power of 2 for efficient modulo ✓

### 2. `test_portability_memory.py` (7 tests)

Validates memory usage for each configuration:
- Full config: < 120 KB (comfortable on CP-V)
- CP/M config: < 40 KB (fits in TPA!)
- Minimal config: < 30 KB
- Hash table load factors reasonable
- String pool capacity adequate

**Memory Results:**
```
Full:    111.2 KB (21.7% of CP-V 512KB)
CP/M:     39.4 KB (98.6% of 40KB TPA - fits!)
Minimal:  29.1 KB (minimal footprint)
```

### 3. `test_portability_real_precision.py` (13 tests)

Validates REAL type compliance:
- CELLV and CELLR REAL arrays exist
- No INT() conversion (bug is fixed)
- No DOUBLE PRECISION anywhere in code
- No COMPLEX types
- Documentation completeness
- References to existing decimal precision tests

**Key Validations:**
- CELLV/CELLR arrays properly declared ✓
- Values retrieved from REAL arrays, not INT ✓
- No forbidden types found ✓

## Running the Tests

### Run all portability tests:
```bash
python -m pytest test/portability/ -v
```

### Run specific test module:
```bash
python -m pytest test/portability/test_portability_integer_range.py -v
python -m pytest test/portability/test_portability_memory.py -v
python -m pytest test/portability/test_portability_real_precision.py -v
```

### Run with detailed output:
```bash
python -m pytest test/portability/ -v -s
```

## Expected Results

All 38 tests should pass:
```
test_portability_integer_range.py:  18 passed
test_portability_memory.py:          7 passed
test_portability_real_precision.py: 13 passed
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Total:                               38 passed ✓
```

## Test Coverage

### Integer Range Compliance
- ✓ All parameters within 16-bit signed range (-32,767 to +32,767)
- ✓ Hash calculations don't overflow
- ✓ Array indices safe
- ✓ No forbidden type declarations

### Memory Compliance
- ✓ Full config fits comfortably in CP-V (512 KB)
- ✓ CP/M config fits in TPA (~40 KB available)
- ✓ Minimal config ultra-compact (~25 KB)
- ✓ Data/code ratio reasonable
- ✓ Hash table efficiency validated

### REAL Precision Compliance
- ✓ REAL arrays exist for decimal storage
- ✓ No INT() conversion bug
- ✓ No DOUBLE PRECISION used
- ✓ No COMPLEX types used
- ✓ Standard types only (INTEGER, REAL, CHARACTER)

## Integration with Unit Tests

The portability tests complement the unit tests:

- **Unit tests** (`test/unit/`) validate functional correctness
- **Portability tests** (`test/portability/`) validate platform compliance

Both test suites should pass before deploying to target platforms.

## Configuration Testing

Tests validate all three configurations:

1. **Full** (default) - `CONFIG_FULL.FOR`
   - MAXCEL=2000, HASHSZ=1024, MAXSTR=10000
   - Target: CP-V, large PDP-11

2. **Compact (CP/M)** - `CONFIG_CPM.FOR`
   - MAXCEL=300, HASHSZ=256, MAXSTR=2000
   - Target: CP/M with 48KB limit

3. **Minimal** - `CONFIG_MINIMAL.FOR`
   - MAXCEL=100, HASHSZ=64, MAXSTR=500
   - Target: Educational/embedded systems

## Continuous Validation

Run these tests after:
- Modifying array sizes or parameters
- Changing configuration templates
- Adding new data structures
- Porting to new platforms
- Before production deployment

## Documentation

For detailed portability information, see:
- `docs/PORTABILITY.md` - Comprehensive portability guide
- `docs/BUILD_CONFIGS.md` - Configuration details
- `docs/PORTABILITY_PROGRESS.md` - Implementation status

## Success Criteria

All 38 tests must pass to ensure:
1. Code will compile and run on 16-bit systems
2. Memory usage fits within platform limits
3. No non-portable constructs are used
4. Configurations are consistent and valid

---

**Status:** All 38 tests passing ✓

**Last Updated:** 2026-01-19
