# CP-V Emulator Setup - Complete

**Date**: 2026-01-18
**Status**: ✅ Ready for use
**System**: CP-V F00 (September 1978) on Xerox Sigma 7

---

## What Was Installed

### 1. Emulator Software
- **open-simh** installed via Homebrew
- Sigma 7 simulator available as `sigma` command
- Located at: `/opt/homebrew/bin/sigma`

### 2. CP-V Operating System
- **sigma-cpv-kit** repository cloned
- Location: `emulator/sigma-cpv-kit/`
- **CP-V F00** RAD swapper system configured
- System files extracted:
  - `rad` - RAM disk swapper (5.1 MB)
  - `sys1` - File system disk 1 (4.2 MB)
  - `sys2` - File system disk 2 (82 MB)

### 3. Boot Configuration
- **boot_cpv.ini** created and configured
- CPU: Sigma 7 with 512KB RAM
- FP, DECIMAL, and LASLMS features enabled
- MUX terminal controller on port 5000
- Tape drive configured for file transfer

### 4. Supporting Scripts
- **make_tape.sh** - Creates tape images for file transfer
- **setup_emulator.sh** - Installation script (fixed for open-simh)

### 5. Documentation
- **QUICKSTART.md** - Comprehensive usage guide
- **EMULATOR_SETUP_COMPLETE.md** - This file

---

## Directory Structure

```
emulator/
├── boot_cpv.ini              # Boot configuration
├── QUICKSTART.md             # Usage guide
├── scripts/
│   ├── setup_emulator.sh     # Installation script
│   └── make_tape.sh          # File transfer helper
├── work/
│   ├── transfer.tap          # Tape for file transfer
│   └── printer.txt           # Printer output
└── sigma-cpv-kit/
    └── f00/
        └── f00rad/
            ├── rad           # RAM disk swapper
            ├── sys1          # File system disk 1
            ├── sys2          # File system disk 2
            ├── f00rad.tap    # Installation tape
            └── cpf0.ini      # Original config
```

---

## How to Use

### Start CP-V

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
sigma boot_cpv.ini
```

The emulator console will show CP-V booting.

### Enable User Logons

In the emulator console:
```
Ctrl-E
sim> ON 107
sim> cont
```

### Connect via Telnet

In another terminal:
```bash
telnet localhost 5000
```

### Login

```
LOGON PLEASE: :SYS,LBE
```

No password required for first logon.

---

## System Specifications

**Hardware Configuration:**
- CPU: Xerox Sigma 7 (32-bit)
- RAM: 512 KB
- Floating Point Unit: Yes
- Decimal Arithmetic Unit: Yes

**Operating System:**
- Name: CP-V F00
- Vendor: Honeywell (formerly Xerox Data Systems)
- Release: F00 (September 28, 1978)
- Configuration: RAD swapper with file system disks
- Max Users: 121 total (107 online + 14 ghost jobs)

**Compilers Available:**
- FORTRAN IV (FORTRAN 66)
- BASIC
- APL
- PASCAL
- META-SYMBOL
- C (via Csystem)

---

## Testing FORTRAN IV Compilation

Once logged in to CP-V, you can compile and run FORTRAN IV programs:

### Example: Hello World

1. Create a FORTRAN source file (from host):
```bash
cd emulator
./scripts/make_tape.sh ../src/layer0/STRUTIL.FOR
```

2. In CP-V (after login):
```
$ COPY MT0: STRUTIL.FOR
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*LINK
*GO
```

---

## Current Development Status

### Layer 0: STRUTIL.FOR ✅ 100%
- **Status**: Complete and tested
- **Test Coverage**: 41/41 tests passing
- **Ready for CP-V**: Yes
- **Next Step**: Transfer to CP-V and compile

### Acceptance Tests ✅ Ready
- **Framework**: Complete
- **Scenarios**: 15 tests written
- **Status**: Skipped (awaiting Layers 1-3)

### Layers 1-3: ❌ Not Started
- **Layer 1**: CELLS, PARSE, EVAL, DEPS, RECALC
- **Layer 2**: UI, DISPLAY, COMMANDS, FILES
- **Layer 3**: TERMTEST, TERMCPV
- **Estimated Timeline**: 7 weeks

---

## What's Next

### Immediate (Optional Validation)

You can now test STRUTIL.FOR on the actual CP-V system:

1. Start CP-V emulator
2. Enable user logons
3. Connect via telnet
4. Transfer STRUTIL.FOR via tape
5. Compile and run on CP-V F00
6. Verify FORTRAN IV compatibility

### Continue Development

Or continue implementing Layers 1-3 using the Python test framework:
- Tests drive implementation
- Weekly validation on CP-V emulator
- Primary development on gfortran

---

## Shutdown Procedure

### Graceful Shutdown

From emulator console:
```
Ctrl-E
sim> ZAP
```

Wait for:
```
                 THAT'S ALL, FOLKS!!
```

Then:
```
sim> quit
```

---

## Troubleshooting

See **QUICKSTART.md** for detailed troubleshooting steps.

Common issues:
- Port 5000 already in use
- System files not found
- Emulator hangs on boot
- Terminal connection refused

---

## Resources

- **CP-V Documentation**: https://www.andrews.edu/~calkins/sigma/
- **simh Guide**: http://simh.trailing-edge.com/
- **sigma-cpv-kit**: https://github.com/kenrector/sigma-cpv-kit
- **CP-V Manuals**: http://bitsavers.org/pdf/sds/sigma/cp-v/

---

## Summary

✅ Emulator installed and configured
✅ CP-V F00 system ready to boot
✅ File transfer mechanism in place
✅ Documentation complete
✅ Ready for FORTRAN IV development

**The CP-V emulator is now fully set up and ready for use!**

When Layers 1-3 are complete, you'll be able to run XL spreadsheet on this authentic 1978 operating system.

---

**Created**: 2026-01-18
**System**: CP-V F00 on Xerox Sigma 7
**Purpose**: XL Spreadsheet development and testing
