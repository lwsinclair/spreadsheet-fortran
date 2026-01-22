# Makefile for XL Spreadsheet - Native Build (3 Configurations)
#
# Compiles the FORTRAN IV/66 source with gfortran
# Builds SMALL, MEDIUM, and LARGE variants
#
# Usage:
#   make all      - Build all 3 variants
#   make small    - Build small variant (~25KB, 100 cells)
#   make medium   - Build medium variant (~36KB, 300 cells)
#   make large    - Build large variant (~103KB, 2000 cells)
#   make clean    - Remove all build artifacts
#   make test     - Quick compile test
#

FC = gfortran
CC = cc
# -fno-automatic: static arrays (like FORTRAN 66)
# -std=legacy: accept old syntax
# -ffixed-form: column-based source format
# -O2: optimize for speed and size
FFLAGS = -std=legacy -ffixed-form -fno-automatic -w -O2
CFLAGS = -c -O2

# Source directories (relative to build dir)
LAYER0 = layer0
LAYER1 = layer1
LAYER2 = layer2
LAYER3 = layer3

# Core source files (portable FORTRAN IV, no platform dependencies)
# Base sources - used by all builds
SRCS_BASE = $(LAYER0)/STRUTIL.FOR \
            $(LAYER1)/CELLS.FOR \
            $(LAYER1)/DEPS.FOR \
            $(LAYER1)/PARSE.FOR \
            $(LAYER1)/EVAL.FOR \
            $(LAYER1)/RECALC.FOR \
            $(LAYER2)/UI.FOR \
            $(LAYER2)/DISPLAY.FOR \
            $(LAYER2)/MSG.FOR \
            $(LAYER2)/COMMANDS.FOR \
            $(LAYER2)/FILES.FOR \
            XLMAIN.FOR \
            BRIDGE.FOR

# Large builds: JSON + Binary file I/O (smart wrappers)
SRCS_FILE_LARGE = $(LAYER2)/FILESAV.FOR \
                  $(LAYER2)/FILELOAD.FOR \
                  $(LAYER2)/FILEBIN.FOR \
                  $(LAYER2)/FILEBINL.FOR

# Small/Medium builds: Binary-only file I/O
SRCS_FILE_SMALL = $(LAYER2)/FILEBIN.FOR \
                  $(LAYER2)/FILEBINS.FOR

# Legacy compatibility
SRCS_CORE = $(SRCS_BASE) $(SRCS_FILE_LARGE)

# Terminal layer (protocol + I/O + driver)
# Select ONE protocol and ONE I/O module per build
TERM_PROTOCOL = $(LAYER3)/PROTVT100.FOR
TERM_IO = $(LAYER3)/IOUNIX.FOR
TERM_DRIVER = $(LAYER3)/TERMINAL.FOR

# File I/O layer (platform-specific file operations)
FILE_IO = $(LAYER3)/FIOUNIX.FOR

# Combined source list for native Unix/macOS build
# Large: includes JSON support
SRCS_LARGE = $(SRCS_BASE) $(SRCS_FILE_LARGE) $(TERM_DRIVER) $(TERM_PROTOCOL) $(TERM_IO) $(FILE_IO)
# Small/Medium: binary-only file I/O
SRCS_SMALL = $(SRCS_BASE) $(SRCS_FILE_SMALL) $(TERM_DRIVER) $(TERM_PROTOCOL) $(TERM_IO) $(FILE_IO)

# Legacy compatibility (uses large)
SRCS = $(SRCS_LARGE)

# Build directories for each variant
BUILD_SMALL = build_small
BUILD_MEDIUM = build_medium
BUILD_LARGE = build_large

# Parameter values for SMALL configuration (~25KB, 100 cells)
SMALL_MAXCEL = 100
SMALL_HASHSZ = 64
SMALL_MAXSTR = 500
SMALL_MAXDEP = 50
SMALL_DEPHSZ = 32
SMALL_MAXQUE = 25
SMALL_MAXTOK = 25
SMALL_MAXSTK = 15
SMALL_MAXDPS = 25

# Parameter values for MEDIUM configuration (~36KB, 300 cells)
MEDIUM_MAXCEL = 300
MEDIUM_HASHSZ = 256
MEDIUM_MAXSTR = 2000
MEDIUM_MAXDEP = 150
MEDIUM_DEPHSZ = 64
MEDIUM_MAXQUE = 75
MEDIUM_MAXTOK = 50
MEDIUM_MAXSTK = 25
MEDIUM_MAXDPS = 50

# Parameter values for LARGE configuration (~103KB, 2000 cells)
LARGE_MAXCEL = 2000
LARGE_HASHSZ = 1024
LARGE_MAXSTR = 10000
LARGE_MAXDEP = 1000
LARGE_DEPHSZ = 256
LARGE_MAXQUE = 500
LARGE_MAXTOK = 100
LARGE_MAXSTK = 50
LARGE_MAXDPS = 100

# Default target: build all variants
all: small medium large
	@echo ""
	@echo "=== All variants built successfully ==="
	@echo "  xl_small  - 100 cells, ~25KB"
	@echo "  xl_medium - 300 cells, ~36KB"
	@echo "  xl_large  - 2000 cells, ~103KB"
	@echo ""

# Build SMALL variant
small: xl_small
	@echo "Built: xl_small (100 cells, ~25KB)"

xl_small: $(BUILD_SMALL)
	cd $(BUILD_SMALL) && $(CC) $(CFLAGS) termio.c && $(FC) $(FFLAGS) -o ../xl_small *.FOR termio.o

$(BUILD_SMALL): $(SRCS_SMALL) termio.c
	@mkdir -p $(BUILD_SMALL)
	@cp termio.c $(BUILD_SMALL)/
	@for f in $(SRCS_SMALL); do \
		sed -e 's/MAXCEL=2000/MAXCEL=$(SMALL_MAXCEL)/g' \
		    -e 's/MAXCEL=300/MAXCEL=$(SMALL_MAXCEL)/g' \
		    -e 's/HASHSZ=1024/HASHSZ=$(SMALL_HASHSZ)/g' \
		    -e 's/HASHSZ=256/HASHSZ=$(SMALL_HASHSZ)/g' \
		    -e 's/MAXSTR=10000/MAXSTR=$(SMALL_MAXSTR)/g' \
		    -e 's/MAXSTR=2000/MAXSTR=$(SMALL_MAXSTR)/g' \
		    -e 's/MAXDEP=1000/MAXDEP=$(SMALL_MAXDEP)/g' \
		    -e 's/MAXDEP=150/MAXDEP=$(SMALL_MAXDEP)/g' \
		    -e 's/DEPHSZ=256/DEPHSZ=$(SMALL_DEPHSZ)/g' \
		    -e 's/DEPHSZ=64/DEPHSZ=$(SMALL_DEPHSZ)/g' \
		    -e 's/MAXQUE=500/MAXQUE=$(SMALL_MAXQUE)/g' \
		    -e 's/MAXQUE=75/MAXQUE=$(SMALL_MAXQUE)/g' \
		    -e 's/MAXTOK=100/MAXTOK=$(SMALL_MAXTOK)/g' \
		    -e 's/MAXTOK=50/MAXTOK=$(SMALL_MAXTOK)/g' \
		    -e 's/MAXSTK=50/MAXSTK=$(SMALL_MAXSTK)/g' \
		    -e 's/MAXSTK=25/MAXSTK=$(SMALL_MAXSTK)/g' \
		    -e 's/MAXDPS=100/MAXDPS=$(SMALL_MAXDPS)/g' \
		    -e 's/MAXDPS=50/MAXDPS=$(SMALL_MAXDPS)/g' \
		    $$f > $(BUILD_SMALL)/$$(basename $$f); \
	done
	@touch $(BUILD_SMALL)

# Build MEDIUM variant
medium: xl_medium
	@echo "Built: xl_medium (300 cells, ~36KB)"

xl_medium: $(BUILD_MEDIUM)
	cd $(BUILD_MEDIUM) && $(CC) $(CFLAGS) termio.c && $(FC) $(FFLAGS) -o ../xl_medium *.FOR termio.o

$(BUILD_MEDIUM): $(SRCS_SMALL) termio.c
	@mkdir -p $(BUILD_MEDIUM)
	@cp termio.c $(BUILD_MEDIUM)/
	@for f in $(SRCS_SMALL); do \
		sed -e 's/MAXCEL=2000/MAXCEL=$(MEDIUM_MAXCEL)/g' \
		    -e 's/HASHSZ=1024/HASHSZ=$(MEDIUM_HASHSZ)/g' \
		    -e 's/MAXSTR=10000/MAXSTR=$(MEDIUM_MAXSTR)/g' \
		    -e 's/MAXDEP=1000/MAXDEP=$(MEDIUM_MAXDEP)/g' \
		    -e 's/DEPHSZ=256/DEPHSZ=$(MEDIUM_DEPHSZ)/g' \
		    -e 's/MAXQUE=500/MAXQUE=$(MEDIUM_MAXQUE)/g' \
		    -e 's/MAXTOK=100/MAXTOK=$(MEDIUM_MAXTOK)/g' \
		    -e 's/MAXSTK=50/MAXSTK=$(MEDIUM_MAXSTK)/g' \
		    -e 's/MAXDPS=100/MAXDPS=$(MEDIUM_MAXDPS)/g' \
		    $$f > $(BUILD_MEDIUM)/$$(basename $$f); \
	done
	@touch $(BUILD_MEDIUM)

# Build LARGE variant
large: xl_large
	@echo "Built: xl_large (2000 cells, ~103KB)"

xl_large: $(BUILD_LARGE)
	cd $(BUILD_LARGE) && $(CC) $(CFLAGS) termio.c && $(FC) $(FFLAGS) -o ../xl_large *.FOR termio.o

$(BUILD_LARGE): $(SRCS_LARGE) termio.c
	@mkdir -p $(BUILD_LARGE)
	@cp termio.c $(BUILD_LARGE)/
	@for f in $(SRCS_LARGE); do \
		cp $$f $(BUILD_LARGE)/$$(basename $$f); \
	done
	@touch $(BUILD_LARGE)

# Quick test compile (no linking)
test:
	@echo "Testing compilation..."
	$(FC) $(FFLAGS) -c $(LAYER3)/TERMCPV.FOR -o /tmp/test.o
	@rm -f /tmp/test.o
	@echo "Compilation test passed (no recursion errors)"

# Clean all build artifacts
clean:
	rm -rf $(BUILD_SMALL) $(BUILD_MEDIUM) $(BUILD_LARGE)
	rm -f xl_small xl_medium xl_large
	rm -f *.o

# Show configuration info
info:
	@echo "XL Spreadsheet Build Configurations"
	@echo "===================================="
	@echo ""
	@echo "SMALL (~25KB, for tiny/embedded systems):"
	@echo "  Cells: $(SMALL_MAXCEL), Deps: $(SMALL_MAXDEP), Formula pool: $(SMALL_MAXSTR)"
	@echo "  File format: Binary only (.xlb)"
	@echo ""
	@echo "MEDIUM (~36KB, for CP/M 48KB limit):"
	@echo "  Cells: $(MEDIUM_MAXCEL), Deps: $(MEDIUM_MAXDEP), Formula pool: $(MEDIUM_MAXSTR)"
	@echo "  File format: Binary only (.xlb)"
	@echo ""
	@echo "LARGE (~103KB, for CP-V/large PDP-11):"
	@echo "  Cells: $(LARGE_MAXCEL), Deps: $(LARGE_MAXDEP), Formula pool: $(LARGE_MAXSTR)"
	@echo "  File format: JSON (.xl) and Binary (.xlb)"

.PHONY: all small medium large test clean info
