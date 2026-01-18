"""
FORTRAN IV Test Harness for XL Spreadsheet

Compiles and executes FORTRAN IV code for unit testing.
Uses gfortran with strict FORTRAN IV compatibility flags.
"""

import os
import subprocess
import tempfile
import shutil
from pathlib import Path
from typing import List, Any, Optional, Tuple


class FortranCompileError(Exception):
    """Raised when FORTRAN compilation fails"""
    pass


class FortranRuntimeError(Exception):
    """Raised when FORTRAN program execution fails"""
    pass


class FortranTester:
    """
    Test harness for FORTRAN IV code.

    Compiles and runs FORTRAN programs with strict FORTRAN IV compatibility.
    """

    # FORTRAN IV compatibility flags
    FFLAGS = [
        '-std=legacy',              # FORTRAN 66 compatibility
        '-fno-automatic',           # Static storage
        '-ffixed-form',             # Fixed format
        '-ffixed-line-length-72',   # 72-column limit
        '-Wno-unused-variable',     # Suppress unused variable warnings
        '-Wno-unused-dummy-argument',  # Suppress unused argument warnings
    ]

    def __init__(self, src_dir: str = None):
        """
        Initialize the test harness.

        Args:
            src_dir: Root directory containing FORTRAN source files
        """
        if src_dir is None:
            # Default to project src directory
            self.src_dir = Path(__file__).parent.parent.parent / 'src'
        else:
            self.src_dir = Path(src_dir)

        self.temp_dir = None

    def compile_and_run(self,
                       source_files: List[str],
                       test_program: str,
                       timeout: int = 5) -> Tuple[str, str]:
        """
        Compile FORTRAN sources with test program and run.

        Args:
            source_files: List of source file paths (relative to src_dir)
            test_program: FORTRAN test program code
            timeout: Maximum execution time in seconds

        Returns:
            Tuple of (stdout, stderr)

        Raises:
            FortranCompileError: If compilation fails
            FortranRuntimeError: If execution fails
        """
        self.temp_dir = tempfile.mkdtemp(prefix='fortran_test_')

        try:
            # Write test program
            test_file = os.path.join(self.temp_dir, 'test_main.for')
            with open(test_file, 'w') as f:
                f.write(test_program)

            # Copy source files to temp directory
            source_paths = [test_file]
            for src_file in source_files:
                src_path = self.src_dir / src_file
                if not src_path.exists():
                    raise FileNotFoundError(f"Source file not found: {src_path}")

                dest_path = os.path.join(self.temp_dir, os.path.basename(src_file))
                shutil.copy2(src_path, dest_path)
                source_paths.append(dest_path)

            # Compile
            output_exe = os.path.join(self.temp_dir, 'test_program')
            compile_cmd = ['gfortran'] + self.FFLAGS + source_paths + ['-o', output_exe]

            result = subprocess.run(
                compile_cmd,
                capture_output=True,
                text=True,
                timeout=timeout
            )

            if result.returncode != 0:
                raise FortranCompileError(
                    f"Compilation failed:\n{result.stderr}"
                )

            # Run
            result = subprocess.run(
                [output_exe],
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=self.temp_dir
            )

            if result.returncode != 0:
                raise FortranRuntimeError(
                    f"Runtime error (exit code {result.returncode}):\n{result.stderr}"
                )

            return result.stdout, result.stderr

        finally:
            # Cleanup
            if self.temp_dir and os.path.exists(self.temp_dir):
                shutil.rmtree(self.temp_dir)

    def run_function_test(self,
                         source_files: List[str],
                         function_name: str,
                         args: List[Any],
                         return_type: str = 'INTEGER') -> Any:
        """
        Test a single FORTRAN function with given arguments.

        Args:
            source_files: List of source file paths
            function_name: Name of function to test
            args: List of arguments (converted to FORTRAN literals)
            return_type: Expected return type (INTEGER, REAL, LOGICAL)

        Returns:
            Function return value
        """
        # Build test program
        arg_list = ', '.join(self._to_fortran_literal(arg) for arg in args)

        test_program = f"""
      PROGRAM TEST
      {return_type} {function_name}, RESULT
      RESULT = {function_name}({arg_list})
      WRITE(*,*) RESULT
      STOP
      END
"""

        stdout, _ = self.compile_and_run(source_files, test_program)

        # Parse result
        return self._parse_output(stdout.strip(), return_type)

    def run_subroutine_test(self,
                           source_files: List[str],
                           subroutine_name: str,
                           args: List[Tuple[str, Any]],
                           output_vars: List[str]) -> List[Any]:
        """
        Test a FORTRAN subroutine with given arguments.

        Args:
            source_files: List of source file paths
            subroutine_name: Name of subroutine to test
            args: List of (type, value) tuples for arguments
            output_vars: List of variable names to print after call

        Returns:
            List of output variable values
        """
        # Build declarations and initializations
        declarations = []
        initializations = []
        arg_names = []

        for i, (arg_type, arg_value) in enumerate(args):
            var_name = f'ARG{i+1}'
            arg_names.append(var_name)

            # Handle arrays
            if isinstance(arg_value, list):
                array_size = len(arg_value)
                declarations.append(f'      {arg_type} {var_name}({array_size})')
                for j, val in enumerate(arg_value):
                    initializations.append(
                        f'      {var_name}({j+1}) = {self._to_fortran_literal(val)}'
                    )
            else:
                declarations.append(f'      {arg_type} {var_name}')
                initializations.append(
                    f'      {var_name} = {self._to_fortran_literal(arg_value)}'
                )

        # Build output statements
        output_stmts = []
        for var in output_vars:
            output_stmts.append(f'      WRITE(*,*) {var}')

        # Build complete test program
        test_program = f"""
      PROGRAM TEST
{chr(10).join(declarations)}
{chr(10).join(initializations)}
      CALL {subroutine_name}({', '.join(arg_names)})
{chr(10).join(output_stmts)}
      STOP
      END
"""

        stdout, _ = self.compile_and_run(source_files, test_program)

        # Parse results (one per line)
        lines = stdout.strip().split('\n')
        results = []
        for line in lines:
            # Try to parse as integer first, then real
            try:
                results.append(int(line.strip()))
            except ValueError:
                try:
                    results.append(float(line.strip()))
                except ValueError:
                    results.append(line.strip())

        return results

    def _to_fortran_literal(self, value: Any) -> str:
        """Convert Python value to FORTRAN literal"""
        if isinstance(value, bool):
            return '.TRUE.' if value else '.FALSE.'
        elif isinstance(value, int):
            return str(value)
        elif isinstance(value, float):
            return str(value)
        elif isinstance(value, str):
            # FORTRAN strings in Hollerith notation
            return f"'{value}'"
        else:
            raise ValueError(f"Unsupported type for FORTRAN literal: {type(value)}")

    def _parse_output(self, output: str, output_type: str) -> Any:
        """Parse FORTRAN output based on expected type"""
        output = output.strip()

        if output_type == 'INTEGER':
            return int(output)
        elif output_type == 'REAL':
            return float(output)
        elif output_type == 'LOGICAL':
            if 'T' in output.upper():
                return True
            elif 'F' in output.upper():
                return False
            else:
                raise ValueError(f"Could not parse LOGICAL output: {output}")
        else:
            return output


# Convenience functions for common test patterns
def test_integer_function(source_files: List[str],
                         function_name: str,
                         *args) -> int:
    """Test an INTEGER function"""
    tester = FortranTester()
    return tester.run_function_test(source_files, function_name, list(args), 'INTEGER')


def test_real_function(source_files: List[str],
                      function_name: str,
                      *args) -> float:
    """Test a REAL function"""
    tester = FortranTester()
    return tester.run_function_test(source_files, function_name, list(args), 'REAL')


def test_logical_function(source_files: List[str],
                         function_name: str,
                         *args) -> bool:
    """Test a LOGICAL function"""
    tester = FortranTester()
    return tester.run_function_test(source_files, function_name, list(args), 'LOGICAL')
