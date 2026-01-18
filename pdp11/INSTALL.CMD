$ ! INSTALL.CMD - Automated installation for XL Spreadsheet on RSX-11M
$ !
$ ! Purpose: Complete build and installation automation
$ !
$ ! Usage: @INSTALL
$ !
$ ! Prerequisites:
$ !   - PDP-11/70 with 256KB+ memory
$ !   - RSX-11M v3.2 or later
$ !   - PDP-11 FORTRAN IV compiler (F4 v2.1+)
$ !   - TKB (Task Builder)
$ !   - VT-100 or VT-52 compatible terminal
$ !
$ ! This script will:
$ !   1. Verify prerequisites
$ !   2. Compile all source modules
$ !   3. Link executable
$ !   4. Run terminal driver test
$ !   5. Install to system directory (optional)
$ !
$ ! Author: Claude Code
$ ! Date: 2026-01-19
$ !
$ !======================================================================
$
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "XL Spreadsheet - RSX-11M Installation"
$ WRITE SYS$OUTPUT "======================================"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "This will compile and install XL Spreadsheet."
$ WRITE SYS$OUTPUT ""
$
$ ! Set up error handling
$ ON ERROR THEN GOTO INSTALL_ERROR
$
$ ! Step 1: Verify prerequisites
$ WRITE SYS$OUTPUT "Step 1: Verifying prerequisites..."
$ WRITE SYS$OUTPUT ""
$
$ ! Check for FORTRAN compiler
$ IF F$SEARCH("SYS$SYSTEM:FOR.TSK") .EQS. "" THEN GOTO NO_FORTRAN
$ WRITE SYS$OUTPUT "  [OK] FORTRAN IV compiler found"
$
$ ! Check for TKB
$ IF F$SEARCH("SYS$SYSTEM:TKB.TSK") .EQS. "" THEN GOTO NO_TKB
$ WRITE SYS$OUTPUT "  [OK] Task Builder (TKB) found"
$
$ ! Check for source files
$ IF F$SEARCH("SRC:XLMAIN.FOR") .EQS. "" THEN GOTO NO_SOURCE
$ WRITE SYS$OUTPUT "  [OK] Source files found"
$
$ WRITE SYS$OUTPUT ""
$
$ ! Step 2: Compile
$ WRITE SYS$OUTPUT "Step 2: Compiling source modules..."
$ WRITE SYS$OUTPUT ""
$ @BUILD
$ IF $STATUS .NE. 1 THEN GOTO COMPILE_ERROR
$
$ ! Step 3: Link
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Step 3: Linking executable..."
$ WRITE SYS$OUTPUT ""
$ @XLBUILD
$ IF $STATUS .NE. 1 THEN GOTO LINK_ERROR
$
$ ! Step 4: Test terminal driver
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Step 4: Testing terminal driver..."
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Compiling TESTTERM..."
$ FOR TESTTERM=TESTTERM.FOR
$ IF $STATUS .NE. 1 THEN GOTO TEST_COMPILE_ERROR
$
$ WRITE SYS$OUTPUT "Linking TESTTERM..."
$ OPEN/WRITE TKBFILE TKB.CMD
$ WRITE TKBFILE "TESTTERM/-SP=TESTTERM,TERMRSX/LB:[1,1]F77FCS"
$ WRITE TKBFILE "//"
$ CLOSE TKBFILE
$ TKB @TKB.CMD
$ DELETE TKB.CMD;*
$ IF $STATUS .NE. 1 THEN GOTO TEST_LINK_ERROR
$
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Terminal test ready. Run with: RUN TESTTERM"
$ WRITE SYS$OUTPUT ""
$
$ ! Step 5: Installation complete
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "========================================="
$ WRITE SYS$OUTPUT "Installation Complete!"
$ WRITE SYS$OUTPUT "========================================="
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Executables created:"
$ WRITE SYS$OUTPUT "  XL.TSK      - Main spreadsheet program"
$ WRITE SYS$OUTPUT "  TESTTERM.TSK - Terminal driver test"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "To run XL Spreadsheet:"
$ WRITE SYS$OUTPUT "  $ RUN XL"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "For help:"
$ WRITE SYS$OUTPUT "  See README.md for user manual"
$ WRITE SYS$OUTPUT "  See INSTALL.md for detailed installation guide"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Optional: To install system-wide, copy XL.TSK to [1,54]"
$ WRITE SYS$OUTPUT ""
$ EXIT
$
$ ! Error handlers
$ NO_FORTRAN:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: FORTRAN IV compiler not found"
$ WRITE SYS$OUTPUT "Install PDP-11 FORTRAN IV (F4) before continuing."
$ EXIT
$
$ NO_TKB:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Task Builder (TKB) not found"
$ WRITE SYS$OUTPUT "TKB is required for linking. Install from RSX-11M distribution."
$ EXIT
$
$ NO_SOURCE:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Source files not found"
$ WRITE SYS$OUTPUT "Ensure src/ directory exists and contains .FOR files."
$ EXIT
$
$ COMPILE_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Compilation failed"
$ WRITE SYS$OUTPUT "Check compiler errors above."
$ EXIT
$
$ LINK_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Linking failed"
$ WRITE SYS$OUTPUT "Check TKB errors above."
$ EXIT
$
$ TEST_COMPILE_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Test compilation failed"
$ WRITE SYS$OUTPUT "Terminal test could not be built, but XL.TSK is ready."
$ EXIT
$
$ TEST_LINK_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** ERROR: Test linking failed"
$ WRITE SYS$OUTPUT "Terminal test could not be built, but XL.TSK is ready."
$ EXIT
$
$ INSTALL_ERROR:
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*** Installation failed ***"
$ WRITE SYS$OUTPUT "An unexpected error occurred."
$ EXIT
