$ ! BUILD.CMD - Compile all XL Spreadsheet modules for RSX-11M
$ !
$ ! Purpose: Compile all FORTRAN IV source files
$ !
$ ! Usage: @BUILD
$ !
$ ! Prerequisites:
$ !   - PDP-11 FORTRAN IV compiler (F4 v2.1 or later)
$ !   - RSX-11M v3.2 or later
$ !   - Current directory contains src/ subdirectory
$ !
$ ! Output: .OBJ files for each module
$ !
$ ! Author: Claude Code
$ ! Date: 2026-01-19
$ !
$ !======================================================================
$
$ WRITE SYS$OUTPUT "XL Spreadsheet - RSX-11M Build"
$ WRITE SYS$OUTPUT "=============================="
$ WRITE SYS$OUTPUT ""
$
$ ! Set up error handling
$ ON ERROR THEN GOTO ERROR_HANDLER
$
$ ! Layer 0: Utilities (no dependencies)
$ WRITE SYS$OUTPUT "Building Layer 0: Utilities..."
$ FOR SRC:STRUTIL=SRC:STRUTIL.FOR
$
$ ! Layer 1: Calculation Engine
$ WRITE SYS$OUTPUT "Building Layer 1: Calculation Engine..."
$ FOR SRC:CELLS=SRC:CELLS.FOR
$ FOR SRC:DEPS=SRC:DEPS.FOR
$ FOR SRC:PARSE=SRC:PARSE.FOR
$ FOR SRC:EVAL=SRC:EVAL.FOR
$ FOR SRC:RECALC=SRC:RECALC.FOR
$
$ ! Layer 2: Application Logic
$ WRITE SYS$OUTPUT "Building Layer 2: Application Logic..."
$ FOR SRC:UI=SRC:UI.FOR
$ FOR SRC:DISPLAY=SRC:DISPLAY.FOR
$ FOR SRC:MSG=SRC:MSG.FOR
$
$ ! Layer 3: Platform-Specific (RSX-11M)
$ WRITE SYS$OUTPUT "Building Layer 3: Terminal Driver..."
$ FOR SRC:TERMRSX=SRC:TERMRSX.FOR
$
$ ! Main Program
$ WRITE SYS$OUTPUT "Building Main Program..."
$ FOR SRC:XLMAIN=SRC:XLMAIN.FOR
$
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Build complete!"
$ WRITE SYS$OUTPUT "Object files ready for linking."
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Next step: @XLBUILD to link executable"
$ EXIT
$
$ ERROR_HANDLER:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** Build failed! ***"
$ WRITE SYS$OUTPUT "Check compiler errors above."
$ EXIT
