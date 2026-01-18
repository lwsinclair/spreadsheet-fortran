"""
Custom assertions for FORTRAN IV testing

Provides specialized assertion functions for common test patterns.
"""

from typing import Any, List


def assert_fortran_equal(actual: Any, expected: Any, message: str = None):
    """
    Assert that FORTRAN output equals expected value.

    Handles floating-point comparison with tolerance.
    """
    if isinstance(expected, float):
        tolerance = 1e-6
        if abs(actual - expected) > tolerance:
            msg = f"Expected {expected}, got {actual}"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)
    else:
        if actual != expected:
            msg = f"Expected {expected}, got {actual}"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)


def assert_fortran_arrays_equal(actual: List[Any],
                                expected: List[Any],
                                message: str = None):
    """
    Assert that two arrays are equal element-wise.
    """
    if len(actual) != len(expected):
        msg = f"Array lengths differ: expected {len(expected)}, got {len(actual)}"
        if message:
            msg = f"{message}: {msg}"
        raise AssertionError(msg)

    for i, (a, e) in enumerate(zip(actual, expected)):
        try:
            assert_fortran_equal(a, e)
        except AssertionError as ex:
            msg = f"Array element {i} differs: {ex}"
            if message:
                msg = f"{message}: {msg}"
            raise AssertionError(msg)


def assert_fortran_string_equal(actual: str,
                                expected: str,
                                ignore_trailing_spaces: bool = True,
                                message: str = None):
    """
    Assert that FORTRAN strings are equal.

    Args:
        actual: Actual string value
        expected: Expected string value
        ignore_trailing_spaces: If True, ignore trailing spaces (FORTRAN default)
        message: Optional message prefix
    """
    if ignore_trailing_spaces:
        actual = actual.rstrip()
        expected = expected.rstrip()

    if actual != expected:
        msg = f"Expected '{expected}', got '{actual}'"
        if message:
            msg = f"{message}: {msg}"
        raise AssertionError(msg)


def assert_in_range(actual: float,
                   min_val: float,
                   max_val: float,
                   message: str = None):
    """
    Assert that value is within range [min_val, max_val].
    """
    if not (min_val <= actual <= max_val):
        msg = f"Expected value in range [{min_val}, {max_val}], got {actual}"
        if message:
            msg = f"{message}: {msg}"
        raise AssertionError(msg)
