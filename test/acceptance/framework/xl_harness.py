"""
XL Spreadsheet Test Harness

Provides high-level interface for testing XL spreadsheet functionality.
Wraps terminal simulator and provides spreadsheet-specific operations.
"""

import subprocess
import os
import time
from pathlib import Path
from typing import Optional, Tuple, Any

from .terminal_simulator import TerminalSimulator


class XLSpreadsheet:
    """
    Test harness for XL spreadsheet.

    Manages FORTRAN compilation, process execution, and provides
    high-level spreadsheet operations for testing.
    """

    def __init__(self, terminal: Optional[TerminalSimulator] = None):
        """
        Initialize XL test harness.

        Args:
            terminal: Terminal simulator instance (creates new if None)
        """
        self.terminal = terminal or TerminalSimulator()
        self.process: Optional[subprocess.Popen] = None
        self.src_dir = Path(__file__).parent.parent.parent.parent / 'src'
        self.build_dir = Path(__file__).parent.parent.parent.parent / 'build'
        self.is_running = False

    def compile(self, test_mode: bool = True) -> bool:
        """
        Compile XL spreadsheet.

        Args:
            test_mode: If True, use TERMTEST.FOR (headless), else TERMCPV.FOR

        Returns:
            True if compilation successful

        Raises:
            RuntimeError: If compilation fails
        """
        # Ensure build directory exists
        self.build_dir.mkdir(exist_ok=True)

        # Determine which terminal driver to use
        term_driver = 'layer3/TERMTEST.FOR' if test_mode else 'layer3/TERMCPV.FOR'

        # Source files in dependency order
        sources = [
            'layer0/STRUTIL.FOR',
            'layer1/CELLS.FOR',
            'layer1/PARSE.FOR',
            'layer1/EVAL.FOR',
            'layer1/DEPS.FOR',
            'layer1/RECALC.FOR',
            'layer2/MSG.FOR',
            'layer2/UI.FOR',
            'layer2/DISPLAY.FOR',
            'layer2/COMMANDS.FOR',
            'layer2/FILES.FOR',
            term_driver,
            'CALCSH.FOR',
        ]

        # Build source file paths
        source_paths = []
        for src in sources:
            src_path = self.src_dir / src
            if src_path.exists():
                source_paths.append(str(src_path))

        if not source_paths:
            raise RuntimeError("No source files found - layers not implemented yet")

        # Compile command
        output_exe = self.build_dir / 'xl_test'
        compile_cmd = [
            'gfortran',
            '-std=legacy',
            '-fno-automatic',
            '-ffixed-form',
            '-ffixed-line-length-72',
        ] + source_paths + ['-o', str(output_exe)]

        # Run compilation
        result = subprocess.run(
            compile_cmd,
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            raise RuntimeError(f"Compilation failed:\n{result.stderr}")

        return True

    def start(self, timeout: float = 5.0):
        """
        Start XL spreadsheet.

        Args:
            timeout: Maximum time to wait for startup

        Raises:
            RuntimeError: If startup fails
            NotImplementedError: If executable doesn't exist yet
        """
        exe_path = self.build_dir / 'xl_test'

        if not exe_path.exists():
            raise NotImplementedError(
                "XL executable not built yet. "
                "Need to implement Layers 1-3 first. "
                "This test will pass once implementation is complete."
            )

        # Start process
        self.process = subprocess.Popen(
            [str(exe_path)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        self.is_running = True

        # Wait for initial prompt/screen
        time.sleep(0.1)

    def stop(self):
        """Stop XL spreadsheet"""
        if self.process and self.is_running:
            self.process.terminate()
            self.process.wait(timeout=5)
            self.is_running = False

    def navigate_to(self, col: int, row: int):
        """
        Navigate to specific cell.

        Args:
            col: Column number (1-based)
            row: Row number (1-based)
        """
        # Send /G command (goto)
        self.terminal.send_keys('/G')
        # Send cell reference
        from test.framework.fortran_tester import FortranTester
        # Format as cell reference (e.g., "A1")
        col_letter = chr(64 + col) if col <= 26 else f"A{chr(64 + col - 26)}"
        cell_ref = f"{col_letter}{row}"
        self.terminal.send_keys(f"{cell_ref}\n")

    def enter_value(self, value: str):
        """
        Enter value in current cell.

        Args:
            value: Value to enter (number or formula)
        """
        self.terminal.send_keys(f"{value}\n")

    def enter_cell(self, col: int, row: int, value: str):
        """
        Navigate to cell and enter value.

        Args:
            col: Column number (1-based)
            row: Row number (1-based)
            value: Value to enter
        """
        self.navigate_to(col, row)
        self.enter_value(value)

    def get_cell_value(self, col: int, row: int) -> Any:
        """
        Get current value of cell.

        Args:
            col: Column number (1-based)
            row: Row number (1-based)

        Returns:
            Cell value (parsed as number if possible, else string)
        """
        raise NotImplementedError(
            "get_cell_value not implemented yet - need CELLS.FOR"
        )

    def get_cell_display(self, col: int, row: int) -> str:
        """
        Get displayed value of cell from screen.

        Args:
            col: Column number (1-based)
            row: Row number (1-based)

        Returns:
            Cell display string
        """
        # Calculate screen position based on grid layout
        # (This depends on DISPLAY.FOR implementation)
        screen_col = (col - 1) * 10 + 1  # Assuming 10 char cell width
        screen_row = row + 2  # Assuming 2 header rows

        return self.terminal.get_cell_text(screen_row, screen_col, 10).strip()

    def send_command(self, command: str):
        """
        Send slash command.

        Args:
            command: Command string (e.g., '/S', '/L', '/BR')
        """
        self.terminal.send_keys(command)

    def screen_shows(self, text: str) -> bool:
        """
        Check if text appears on screen.

        Args:
            text: Text to find

        Returns:
            True if text found on screen
        """
        return text in self.terminal.get_screen_text()

    def assert_screen_shows(self, text: str, message: str = ""):
        """
        Assert text appears on screen.

        Args:
            text: Text to find
            message: Optional assertion message

        Raises:
            AssertionError: If text not found
        """
        self.terminal.assert_screen_contains(text, message)

    def assert_cell_shows(self, col: int, row: int, expected: str, message: str = ""):
        """
        Assert cell displays expected value.

        Args:
            col: Column number (1-based)
            row: Row number (1-based)
            expected: Expected display value
            message: Optional assertion message

        Raises:
            AssertionError: If cell doesn't show expected value
        """
        actual = self.get_cell_display(col, row)
        if actual != expected:
            msg = f"Cell ({col},{row}) shows '{actual}', expected '{expected}'"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)

    def wait_for_recalc(self, timeout: float = 1.0):
        """
        Wait for recalculation to complete.

        Args:
            timeout: Maximum time to wait
        """
        time.sleep(0.1)  # Simple delay for now

    def __enter__(self):
        """Context manager entry"""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.stop()
        return False


class XLWorksheet:
    """Helper class for managing worksheet operations"""

    def __init__(self, xl: XLSpreadsheet):
        self.xl = xl

    def set_cells(self, cells: dict):
        """
        Set multiple cells at once.

        Args:
            cells: Dict of {(col, row): value}

        Example:
            worksheet.set_cells({
                (1, 1): "100",
                (1, 2): "200",
                (1, 3): "+A1+A2"
            })
        """
        for (col, row), value in cells.items():
            self.xl.enter_cell(col, row, str(value))

    def verify_cells(self, cells: dict):
        """
        Verify multiple cell values.

        Args:
            cells: Dict of {(col, row): expected_value}

        Raises:
            AssertionError: If any cell doesn't match
        """
        for (col, row), expected in cells.items():
            self.xl.assert_cell_shows(col, row, str(expected))
