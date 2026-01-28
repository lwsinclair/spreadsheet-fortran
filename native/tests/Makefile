# Makefile for XL Spreadsheet Tests
#
# Usage:
#   make        - Build and run all tests
#   make core   - Build and run core tests
#   make tty    - Build and run TTY renderer tests
#   make all    - Run all test suites
#   make clean  - Remove build artifacts

FC = gfortran
FFLAGS = -std=legacy -ffixed-form -fno-automatic -w

# Source directories
SRCDIR = ..
L0DIR = $(SRCDIR)/layer0
L1DIR = $(SRCDIR)/layer1
L2DIR = $(SRCDIR)/layer2
L3DIR = $(SRCDIR)/layer3

# Application source files (using large config)
APP_SRC = \
	$(L1DIR)/STRUTIL.FOR \
	$(L1DIR)/CELLS.FOR \
	$(L1DIR)/DEPS.FOR \
	$(L1DIR)/PARSE.FOR \
	$(L1DIR)/EVAL.FOR \
	$(L1DIR)/RECALC.FOR \
	$(L2DIR)/UI.FOR

# Core test source files
CORE_TEST_SRC = \
	TESTLIB.FOR \
	TCORE.FOR \
	TROWCOL.FOR \
	TESTMAIN.FOR

# TTY test source files (includes mock terminal and renderer)
TTY_TEST_SRC = \
	TESTLIB.FOR \
	TTTY.FOR \
	$(L3DIR)/tty35/RENDTTY.FOR \
	TTYMAIN.FOR

# Minimal source for TTY tests (just what renderer needs)
TTY_APP_SRC = \
	$(L1DIR)/STRUTIL.FOR \
	$(L1DIR)/CELLS.FOR \
	$(L1DIR)/DEPS.FOR

# Output
CORE_TARGET = run_tests
TTY_TARGET = run_tty_tests

.PHONY: all core tty build run clean

all: core tty

core: $(CORE_TARGET)
	@echo ""
	@echo "=== Running Core Tests ==="
	./$(CORE_TARGET)

tty: $(TTY_TARGET)
	@echo ""
	@echo "=== Running TTY Renderer Tests ==="
	./$(TTY_TARGET)

$(CORE_TARGET): $(APP_SRC) $(CORE_TEST_SRC)
	$(FC) $(FFLAGS) -o $@ $(APP_SRC) $(CORE_TEST_SRC)

$(TTY_TARGET): $(TTY_APP_SRC) $(TTY_TEST_SRC)
	$(FC) $(FFLAGS) -o $@ $(TTY_APP_SRC) $(TTY_TEST_SRC)

# Legacy targets
build: $(CORE_TARGET) $(TTY_TARGET)

run: all

clean:
	rm -f $(CORE_TARGET) $(TTY_TARGET)
