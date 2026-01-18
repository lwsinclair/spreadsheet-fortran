# Makefile for XL - FORTRAN IV Spreadsheet
#
# Targets:
#   all          - Compile all sources
#   test         - Run unit tests
#   test-integration - Run integration tests
#   lint         - Check FORTRAN IV compliance
#   clean        - Remove build artifacts
#   help         - Show this help

# FORTRAN compiler
FC = gfortran

# FORTRAN IV compatibility flags
FFLAGS = -std=legacy \
         -fno-automatic \
         -ffixed-form \
         -ffixed-line-length-72 \
         -Wno-unused-variable \
         -Wno-unused-dummy-argument

# Directories
SRC_DIR = src
TEST_DIR = test
BUILD_DIR = build
BIN_DIR = bin

# Source files by layer
LAYER0_SRC = $(SRC_DIR)/layer0/STRUTIL.FOR
LAYER1_SRC = $(wildcard $(SRC_DIR)/layer1/*.FOR)
LAYER2_SRC = $(wildcard $(SRC_DIR)/layer2/*.FOR)
LAYER3_SRC = $(wildcard $(SRC_DIR)/layer3/*.FOR)
MAIN_SRC = $(SRC_DIR)/CALCSH.FOR

ALL_SRC = $(LAYER0_SRC) $(LAYER1_SRC) $(LAYER2_SRC) $(LAYER3_SRC) $(MAIN_SRC)

# Object files
OBJS = $(patsubst $(SRC_DIR)/%.FOR,$(BUILD_DIR)/%.o,$(ALL_SRC))

# Main executable
EXECUTABLE = $(BIN_DIR)/xl

# Python test runner
PYTEST = pytest
PYTEST_FLAGS = -v --tb=short

# Linter
LINTER = python3 $(TEST_DIR)/framework/fortran_iv_lint.py

.PHONY: all test test-unit test-integration lint clean help dirs

# Default target
all: dirs $(EXECUTABLE)

# Create necessary directories
dirs:
	@mkdir -p $(BUILD_DIR)/layer0 $(BUILD_DIR)/layer1 $(BUILD_DIR)/layer2 $(BUILD_DIR)/layer3
	@mkdir -p $(BIN_DIR)

# Link executable
$(EXECUTABLE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^

# Compile object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.FOR
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c -o $@ $<

# Run all tests
test: test-unit

# Run unit tests
test-unit:
	@echo "Running unit tests..."
	@cd $(TEST_DIR) && $(PYTEST) $(PYTEST_FLAGS) unit/

# Run integration tests
test-integration:
	@echo "Running integration tests..."
	@cd $(TEST_DIR) && $(PYTEST) $(PYTEST_FLAGS) integration/

# Check FORTRAN IV compliance
lint:
	@echo "Checking FORTRAN IV compliance..."
	@python3 -c "from test.framework.fortran_iv_lint import lint_directory; lint_directory('$(SRC_DIR)')"
	@echo "All files pass FORTRAN IV compliance checks."

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR) $(BIN_DIR)
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -delete
	@echo "Clean complete."

# Show help
help:
	@echo "XL Spreadsheet Makefile"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all              - Compile all sources (default)"
	@echo "  test             - Run unit tests"
	@echo "  test-integration - Run integration tests"
	@echo "  lint             - Check FORTRAN IV compliance"
	@echo "  clean            - Remove build artifacts"
	@echo "  help             - Show this help"
	@echo ""
	@echo "FORTRAN IV Compiler Flags:"
	@echo "  -std=legacy                - FORTRAN 66 compatibility"
	@echo "  -fno-automatic             - Static storage"
	@echo "  -ffixed-form               - Fixed format source"
	@echo "  -ffixed-line-length-72     - 72-column limit"

# Dependencies (simplified - assumes layer dependencies)
$(BUILD_DIR)/layer1/%.o: $(LAYER0_SRC)
$(BUILD_DIR)/layer2/%.o: $(LAYER0_SRC) $(LAYER1_SRC)
$(BUILD_DIR)/layer3/%.o: $(LAYER0_SRC)
$(BUILD_DIR)/CALCSH.o: $(ALL_SRC)
