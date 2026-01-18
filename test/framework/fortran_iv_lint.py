"""
FORTRAN IV Compliance Linter

Validates that FORTRAN source code complies with FORTRAN IV (FORTRAN 66)
constraints and does not use features from FORTRAN 77 or later.
"""

import re
from pathlib import Path
from typing import List, Tuple


class FortranIVError(Exception):
    """Raised when FORTRAN IV compliance violation is detected"""
    pass


class FortranIVLinter:
    """
    Checks FORTRAN source code for FORTRAN IV compliance.

    Forbidden constructs:
    - CHARACTER type declarations
    - Block IF/ELSE statements
    - PARAMETER statements
    - Variable array dimensions
    - Identifiers longer than 6 characters
    - Recursion
    - DOUBLE PRECISION (for CP/M target)
    - Code beyond column 72
    """

    def __init__(self, strict: bool = True):
        """
        Initialize linter.

        Args:
            strict: If True, enforce all rules including identifier length
        """
        self.strict = strict
        self.errors = []

    def check_file(self, file_path: str) -> List[Tuple[int, str]]:
        """
        Check a FORTRAN source file for FORTRAN IV compliance.

        Args:
            file_path: Path to .FOR file

        Returns:
            List of (line_number, error_message) tuples

        Raises:
            FortranIVError: If compliance violations found
        """
        self.errors = []
        path = Path(file_path)

        if not path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")

        with open(path, 'r') as f:
            lines = f.readlines()

        for line_num, line in enumerate(lines, start=1):
            self._check_line(line_num, line)

        if self.errors:
            error_report = '\n'.join(
                f"Line {num}: {msg}" for num, msg in self.errors
            )
            raise FortranIVError(
                f"FORTRAN IV compliance errors in {file_path}:\n{error_report}"
            )

        return self.errors

    def _check_line(self, line_num: int, line: str):
        """Check a single line for violations"""

        # Skip comment lines and continuation lines
        if len(line) > 0 and line[0] in 'Cc*':
            return

        # Check line length (column 73+ should be empty)
        if len(line) > 72 and line[72:].strip():
            self.errors.append(
                (line_num, "Code beyond column 72 (fixed-format violation)")
            )

        # Get the statement part (columns 7-72)
        if len(line) >= 7:
            statement = line[6:72] if len(line) > 72 else line[6:]
        else:
            statement = line

        # Skip blank lines
        if not statement.strip():
            return

        # Check for CHARACTER type
        if re.search(r'\bCHARACTER\s*\*', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "CHARACTER type not allowed (use INTEGER arrays)")
            )

        # Check for block IF
        if re.search(r'\bIF\s*\([^)]+\)\s+THEN\b', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "Block IF/THEN not allowed (use arithmetic IF or GO TO)")
            )

        # Check for ELSE statement
        if re.match(r'\s*ELSE\b', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "ELSE statement not allowed (use GO TO)")
            )

        # Check for ELSEIF
        if re.search(r'\bELSE\s*IF\b', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "ELSEIF not allowed (use GO TO)")
            )

        # Check for END IF
        if re.search(r'\bEND\s*IF\b', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "END IF not allowed (use arithmetic IF or GO TO)")
            )

        # Check for PARAMETER statement
        if re.match(r'\s*PARAMETER\s*\(', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "PARAMETER statement not allowed (use COMMON initialization)")
            )

        # Check for DOUBLE PRECISION (CP/M limitation)
        if re.search(r'\bDOUBLE\s+PRECISION\b', statement, re.IGNORECASE):
            self.errors.append(
                (line_num, "DOUBLE PRECISION not allowed (CP/M limitation)")
            )

        # Check for variable array dimensions
        # Look for array declarations with non-constant dimensions
        array_decl = re.search(
            r'\b(INTEGER|REAL|LOGICAL)\s+\w+\s*\(([^)]+)\)',
            statement,
            re.IGNORECASE
        )
        if array_decl:
            dimensions = array_decl.group(2)
            # Check if dimension contains variables (not just numbers)
            if re.search(r'[A-Z]', dimensions, re.IGNORECASE):
                # Allow if it's in a COMMON block or parameter
                if not re.search(r'\bCOMMON\b', statement, re.IGNORECASE):
                    self.errors.append(
                        (line_num, "Variable array dimensions not allowed")
                    )

        # Check for identifier length (strict mode only)
        if self.strict:
            # Extract identifiers (simplified - may have false positives)
            identifiers = re.findall(r'\b[A-Z][A-Z0-9]*\b', statement)
            for ident in identifiers:
                # Skip FORTRAN keywords
                keywords = {
                    'INTEGER', 'REAL', 'LOGICAL', 'DIMENSION', 'COMMON',
                    'EQUIVALENCE', 'EXTERNAL', 'IMPLICIT', 'CALL', 'RETURN',
                    'STOP', 'END', 'PROGRAM', 'FUNCTION', 'SUBROUTINE',
                    'CONTINUE', 'GO', 'TO', 'IF', 'THEN', 'ELSE', 'DO',
                    'FORMAT', 'READ', 'WRITE', 'OPEN', 'CLOSE', 'DATA'
                }
                if ident not in keywords and len(ident) > 6:
                    self.errors.append(
                        (line_num,
                         f"Identifier '{ident}' exceeds 6 characters (may be truncated)")
                    )

    def check_directory(self, directory: str, pattern: str = '*.FOR') -> bool:
        """
        Check all FORTRAN files in a directory.

        Args:
            directory: Directory to scan
            pattern: File pattern to match

        Returns:
            True if all files pass, False if any violations found

        Raises:
            FortranIVError: If compliance violations found
        """
        dir_path = Path(directory)
        files = list(dir_path.rglob(pattern))

        all_errors = {}
        for file_path in files:
            try:
                self.check_file(str(file_path))
            except FortranIVError as e:
                all_errors[str(file_path)] = str(e)

        if all_errors:
            error_report = '\n\n'.join(
                f"{path}:\n{msg}" for path, msg in all_errors.items()
            )
            raise FortranIVError(
                f"FORTRAN IV compliance errors found:\n{error_report}"
            )

        return True


def lint_file(file_path: str, strict: bool = True) -> bool:
    """
    Convenience function to lint a single file.

    Args:
        file_path: Path to .FOR file
        strict: If True, enforce strict rules

    Returns:
        True if file passes linting

    Raises:
        FortranIVError: If compliance violations found
    """
    linter = FortranIVLinter(strict=strict)
    linter.check_file(file_path)
    return True


def lint_directory(directory: str, pattern: str = '*.FOR', strict: bool = True) -> bool:
    """
    Convenience function to lint all files in directory.

    Args:
        directory: Directory to scan
        pattern: File pattern to match
        strict: If True, enforce strict rules

    Returns:
        True if all files pass linting

    Raises:
        FortranIVError: If compliance violations found
    """
    linter = FortranIVLinter(strict=strict)
    linter.check_directory(directory, pattern)
    return True
