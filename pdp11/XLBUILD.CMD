$ ! XLBUILD.CMD - Link XL Spreadsheet executable for RSX-11M
$ !
$ ! Purpose: Link all object files into XL.TSK executable task
$ !
$ ! Usage: @XLBUILD
$ !
$ ! Prerequisites:
$ !   - All .OBJ files from BUILD.CMD
$ !   - TKB (Task Builder) available
$ !   - F77FCS library in [1,1] (FORTRAN runtime)
$ !
$ ! Output: XL.TSK (executable task, ~100KB)
$ !
$ ! Author: Claude Code
$ ! Date: 2026-01-19
$ !
$ !======================================================================
$
$ WRITE SYS$OUTPUT "XL Spreadsheet - RSX-11M Linker"
$ WRITE SYS$OUTPUT "================================"
$ WRITE SYS$OUTPUT ""
$
$ ! Set up error handling
$ ON ERROR THEN GOTO LINK_ERROR
$
$ WRITE SYS$OUTPUT "Linking XL.TSK..."
$
$ ! Create TKB indirect command file
$ OPEN/WRITE TKBFILE TKB.CMD
$ WRITE TKBFILE "XL/-SP=XLMAIN,STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMRSX/LB:[1,1]F77FCS"
$ WRITE TKBFILE "//"
$ CLOSE TKBFILE
$
$ ! Run TKB with indirect command file
$ TKB @TKB.CMD
$
$ ! Clean up
$ DELETE TKB.CMD;*
$
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Link complete!"
$ WRITE SYS$OUTPUT "Executable: XL.TSK"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "To run: RUN XL"
$ EXIT
$
$ LINK_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** Link failed! ***"
$ WRITE SYS$OUTPUT "Check TKB errors above."
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Common issues:"
$ WRITE SYS$OUTPUT "  - Missing .OBJ files (run @BUILD first)"
$ WRITE SYS$OUTPUT "  - F77FCS library not found in [1,1]"
$ WRITE SYS$OUTPUT "  - Insufficient memory"
$ EXIT
