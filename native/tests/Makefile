# Makefile for XL Spreadsheet Tests
#
# Usage:
#   make        - Build and run tests
#   make build  - Build tests only
#   make run    - Run tests
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
	$(L0DIR)/STRUTIL.FOR \
	$(L1DIR)/CELLS.FOR \
	$(L1DIR)/DEPS.FOR \
	$(L1DIR)/PARSE.FOR \
	$(L1DIR)/EVAL.FOR \
	$(L1DIR)/RECALC.FOR \
	$(L2DIR)/UI.FOR

# Test source files
TEST_SRC = \
	TESTLIB.FOR \
	TCORE.FOR \
	TROWCOL.FOR \
	TESTMAIN.FOR

# Output
TARGET = run_tests

.PHONY: all build run clean

all: build run

build: $(TARGET)

$(TARGET): $(APP_SRC) $(TEST_SRC)
	$(FC) $(FFLAGS) -o $@ $(APP_SRC) $(TEST_SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)
