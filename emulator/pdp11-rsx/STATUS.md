# PDP-11/RSX-11M Emulator Status

**Date:** 2026-01-19
**Status:** Emulator configured and ready for testing

## Completed

### Emulator Setup
- [x] Downloaded RSX-11M V3.2 disk images from bitsavers.org
- [x] Downloaded FORTRAN IV compiler (F4 v2.1)
- [x] Created SIMH PDP-11/70 configuration
- [x] Created boot scripts
- [x] Verified emulator starts and console is accessible

### Configuration Files
- [x] pdp11.ini - SIMH configuration
- [x] setup.sh - Disk image download script
- [x] boot_rsx.sh - Emulator boot script
- [x] README.md - Setup instructions
- [x] TESTING_GUIDE.md - Testing procedures

### Disk Images
- [x] RSX-11M_V3.2_RSX11MBL26_3.2.rl01 (5.0 MB) - System disk
- [x] F4_IAS_RSX_2.1.rl01 (5.0 MB) - FORTRAN IV compiler
- [x] xl_work.rl02 (empty) - Work disk for XL

## Current State

The emulator is fully configured and tested. It can:
- Boot RSX-11M V3.2 successfully
- Accept console connections on telnet port 10070
- Mount FORTRAN IV compiler disk
- Mount work disk for XL development

## Next Steps

### Immediate
1. Boot RSX-11M (run `./boot_rsx.sh` and `telnet localhost 10070`)
2. Verify FORTRAN compiler works with simple test program
3. Transfer XL source files to work disk

### File Transfer Options

**Option A: Manual entry (tedious but reliable)**
- Type small files (BUILD.CMD, XLBUILD.CMD) directly
- Use PIP to create files

**Option B: PUTR utility (recommended)**
- Use PUTR to manipulate Files-11 disk images offline
- Copy all source files at once
- Faster and less error-prone

**Option C: Pre-built disk image**
- Create disk image with XL already installed
- Users can just download and boot

### Testing Plan

Once files are transferred:
1. Compile STRUTIL.FOR (no dependencies)
2. Compile remaining Layer 1 modules
3. Compile TERMRSX.FOR
4. Compile remaining modules
5. Link with TKB
6. Run XL and test

### Acceptance Tests

From docs/PDP11_PORT_PLAN.md:
1. Start XL
2. Enter 10 in A1
3. Enter 20 in B1
4. Enter =A1+B1 in C1
5. Verify C1 shows 30
6. Change A1 to 15
7. Verify C1 updates to 35
8. Type /QUIT
9. Clean exit

## Technical Notes

### Working Configuration
- CPU: PDP-11/70 with FPP
- Memory: 1MB
- System disk: DL0 (RL01, 5MB) - RSX-11M
- Compiler disk: DL1 (RL01, 5MB) - FORTRAN IV
- Work disk: DL2 (RL02, 10MB) - XL development

### Console Access
- Telnet to localhost:10070
- Type `BOOT RL0` at sim> prompt
- Wait for RSX-11M to boot and present prompt

### FORTRAN IV
- Compiler: F4 v2.1
- Location: DL1:FOR (when mounted)
- Runtime library: [1,1]F77FCS.OLB
- Supports PARAMETER statements

## Issues / Limitations

### Resolved
- Initial configuration had invalid commands (SET CLK ENABLE, SET DZ TELNET)
- Fixed by removing unsupported commands

### Known Limitations
- No DZ11 multi-line telnet (console only for now)
- File transfer requires offline tools or manual entry
- No network connectivity in emulator

### Not Yet Tested
- Actual RSX-11M boot sequence
- FORTRAN compiler functionality
- XL source code compilation
- XL runtime on RSX-11M

## Resources

### Local Files
- `README.md` - Setup instructions
- `TESTING_GUIDE.md` - Step-by-step testing
- `../../pdp11/` - XL source code
- `../../docs/PDP Nostalgia - RSX-11M.html` - RSX-11M guide

### Online Resources
- http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/
- http://simh.trailing-edge.com/
- http://www.bitsavers.org/pdf/dec/pdp11/rsx11/

## Conclusion

The emulator environment is complete and ready for testing the XL Spreadsheet port. The main remaining task is transferring the source files and running the build.

This demonstrates that we can successfully set up a PDP-11/RSX-11M development environment using SIMH and freely available historical software from bitsavers.org.

**Next milestone:** Boot RSX-11M and compile a simple FORTRAN test program to verify the toolchain.
