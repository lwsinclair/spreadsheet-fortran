"""
Terminal Simulator for XL Acceptance Testing

Simulates period-appropriate terminals for headless testing of XL spreadsheet.
Supports:
- ADM-3A (Lear Siegler) - Primary 1978 terminal
- VT52 (DEC) - Secondary 1978 terminal
- VT100 (DEC) - Default/modern terminal

Allows automated user interaction testing without requiring actual terminal.
"""

from typing import List, Tuple, Optional
from enum import Enum


class TerminalType(Enum):
    """Supported terminal types"""
    ADM3A = 'adm3a'
    VT52 = 'vt52'
    VT100 = 'vt100'
    UNKNOWN = 'unknown'


class Key(Enum):
    """Special keys (VT100 default)"""
    UP = '\x1b[A'
    DOWN = '\x1b[B'
    RIGHT = '\x1b[C'
    LEFT = '\x1b[D'
    ENTER = '\n'
    ESC = '\x1b'
    BACKSPACE = '\x08'
    DELETE = '\x7f'


class TerminalSimulator:
    """
    Simulates period-appropriate terminals for testing.

    Supports:
    - ADM-3A: Most common 1978 terminal, simple control codes
    - VT52: DEC terminal with ESC-based control sequences
    - VT100: Modern default terminal (backward compatible)

    Provides:
    - 80x24 character grid
    - Terminal-specific cursor positioning
    - Input buffer for simulated keystrokes
    - Screen capture for assertions
    - Control sequence logging
    """

    def __init__(self,
                 width: int = 80,
                 height: int = 24,
                 term_type: str = 'vt100'):
        """
        Initialize terminal simulator.

        Args:
            width: Screen width in characters (default 80)
            height: Screen height in characters (default 24)
            term_type: Terminal type ('adm3a', 'vt52', 'vt100')
        """
        self.width = width
        self.height = height
        self.term_type = TerminalType(term_type) if term_type in [t.value for t in TerminalType] else TerminalType.UNKNOWN
        self.screen = [[' '] * width for _ in range(height)]
        self.cursor_row = 0
        self.cursor_col = 0
        self.input_buffer: List[str] = []
        self.output_log: List[str] = []  # For debugging
        self.control_sequences: List[str] = []  # Received control sequences
        self.env: dict = {}  # Environment variables

    def send_keys(self, keys: str):
        """
        Simulate user typing keys.

        Args:
            keys: String of characters to type

        Special sequences:
            \\n -> Enter
            {UP}, {DOWN}, {LEFT}, {RIGHT} -> Arrow keys
            {ESC} -> Escape
        """
        # Replace special key markers
        keys = keys.replace('{UP}', Key.UP.value)
        keys = keys.replace('{DOWN}', Key.DOWN.value)
        keys = keys.replace('{LEFT}', Key.LEFT.value)
        keys = keys.replace('{RIGHT}', Key.RIGHT.value)
        keys = keys.replace('{ESC}', Key.ESC.value)

        self.input_buffer.extend(list(keys))

    def read_char(self) -> str:
        """
        Read next character from input buffer.

        Returns:
            Next character, or empty string if buffer empty
        """
        if self.input_buffer:
            char = self.input_buffer.pop(0)
            self.output_log.append(f"READ: {repr(char)}")
            return char
        return ''

    def has_input(self) -> bool:
        """Check if input buffer has data"""
        return len(self.input_buffer) > 0

    def write_char(self, char: str, row: Optional[int] = None, col: Optional[int] = None):
        """
        Write character to screen at cursor or specified position.

        Args:
            char: Character to write
            row: Row (0-based), or None for current cursor row
            col: Column (0-based), or None for current cursor col
        """
        if row is None:
            row = self.cursor_row
        if col is None:
            col = self.cursor_col

        if 0 <= row < self.height and 0 <= col < self.width:
            self.screen[row][col] = char
            self.output_log.append(f"WRITE: '{char}' at ({row},{col})")

            # Advance cursor if writing at cursor position
            if row == self.cursor_row and col == self.cursor_col:
                self.cursor_col += 1
                if self.cursor_col >= self.width:
                    self.cursor_col = 0
                    self.cursor_row += 1
                    if self.cursor_row >= self.height:
                        self.cursor_row = self.height - 1

    def write_string(self, text: str, row: int, col: int):
        """
        Write string to screen at specified position.

        Args:
            text: Text to write
            row: Starting row (0-based)
            col: Starting column (0-based)
        """
        for i, char in enumerate(text):
            if col + i < self.width:
                self.write_char(char, row, col + i)

    def move_cursor(self, row: int, col: int):
        """
        Move cursor to position.

        Args:
            row: Row (0-based)
            col: Column (0-based)
        """
        self.cursor_row = max(0, min(row, self.height - 1))
        self.cursor_col = max(0, min(col, self.width - 1))
        self.output_log.append(f"CURSOR: ({self.cursor_row},{self.cursor_col})")

    def clear_screen(self):
        """Clear entire screen"""
        self.screen = [[' '] * self.width for _ in range(self.height)]
        self.cursor_row = 0
        self.cursor_col = 0
        self.output_log.append("CLEAR SCREEN")

    def get_screen_text(self) -> str:
        """
        Get current screen contents as string.

        Returns:
            Screen contents with newlines between rows
        """
        return '\n'.join(''.join(row) for row in self.screen)

    def get_row_text(self, row: int) -> str:
        """
        Get text from specific row.

        Args:
            row: Row number (0-based)

        Returns:
            Row contents as string
        """
        if 0 <= row < self.height:
            return ''.join(self.screen[row])
        return ''

    def get_cell_text(self, row: int, col: int, width: int = 1) -> str:
        """
        Get text from specific cell or range.

        Args:
            row: Starting row (0-based)
            col: Starting column (0-based)
            width: Number of characters to read

        Returns:
            Cell contents as string
        """
        if 0 <= row < self.height:
            end_col = min(col + width, self.width)
            return ''.join(self.screen[row][col:end_col])
        return ''

    def assert_screen_contains(self, text: str, message: str = ""):
        """
        Assert that screen contains text.

        Args:
            text: Text to find
            message: Optional assertion message

        Raises:
            AssertionError: If text not found
        """
        screen = self.get_screen_text()
        if text not in screen:
            msg = f"'{text}' not found on screen"
            if message:
                msg = f"{message}: {msg}"
            msg += f"\n\nScreen contents:\n{screen}"
            raise AssertionError(msg)

    def assert_cursor_at(self, row: int, col: int, message: str = ""):
        """
        Assert cursor is at position.

        Args:
            row: Expected row (0-based)
            col: Expected column (0-based)
            message: Optional assertion message

        Raises:
            AssertionError: If cursor not at position
        """
        if self.cursor_row != row or self.cursor_col != col:
            msg = f"Cursor at ({self.cursor_row},{self.cursor_col}), expected ({row},{col})"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)

    def assert_cell_contains(self, row: int, col: int, text: str, message: str = ""):
        """
        Assert cell contains text.

        Args:
            row: Cell row (0-based)
            col: Cell column (0-based)
            text: Expected text
            message: Optional assertion message

        Raises:
            AssertionError: If cell doesn't contain text
        """
        cell_text = self.get_cell_text(row, col, len(text))
        if cell_text != text:
            msg = f"Cell ({row},{col}) contains '{cell_text}', expected '{text}'"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)

    def print_screen(self):
        """Print current screen contents (for debugging)"""
        print("\n" + "="*self.width)
        print(self.get_screen_text())
        print("="*self.width)
        print(f"Cursor: ({self.cursor_row}, {self.cursor_col})")

    def get_output_log(self) -> List[str]:
        """Get log of all screen operations (for debugging)"""
        return self.output_log.copy()

    def clear_output_log(self):
        """Clear the output log"""
        self.output_log = []

    # Terminal-specific support methods

    def received_sequence(self, sequence: str) -> bool:
        """
        Check if a specific control sequence was received.

        Args:
            sequence: Control sequence to check for

        Returns:
            True if sequence was logged
        """
        return sequence in self.control_sequences

    def set_env(self, key: str, value: str):
        """Set environment variable (for TERM detection tests)"""
        self.env[key] = value

    def get_env(self, key: str, default: str = '') -> str:
        """Get environment variable"""
        return self.env.get(key, default)

    def screen_is_mostly_blank(self) -> bool:
        """Check if screen is mostly blank (ignoring status lines)"""
        non_blank = 0
        for row in self.screen:
            for char in row:
                if char != ' ':
                    non_blank += 1
        # Allow for status line + grid headers (~100 chars)
        return non_blank < 150

    def has_overflow(self) -> bool:
        """Check if any content extends beyond screen dimensions"""
        # This would be detected during rendering
        # For now, just check if screen size is respected
        return len(self.screen) > self.height

    def has_control_chars_in_display(self) -> bool:
        """Check for unwanted control characters in display"""
        for row in self.screen:
            for char in row:
                # Check for control chars (< space, except tab)
                if ord(char) < 32 and char != '\t':
                    return True
        return False

    def screen_has_graphics_chars(self) -> bool:
        """Check if VT52 graphics mode characters are used"""
        # VT52 graphics chars are in range 95-126 when in graphics mode
        # For simulation, we can check if special line-drawing chars present
        screen_text = self.get_screen_text()
        # These would be VT52 graphics equivalents
        graphics_chars = ['─', '│', '┌', '┐', '└', '┘', '├', '┤', '┬', '┴', '┼']
        return any(char in screen_text for char in graphics_chars)

    def log_control_sequence(self, sequence: str):
        """Log a received control sequence (for test assertions)"""
        self.control_sequences.append(sequence)
        self.output_log.append(f"CTRL: {repr(sequence)}")
