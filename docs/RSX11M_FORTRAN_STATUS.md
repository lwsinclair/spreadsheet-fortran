# RSX-11M FORTRAN IV Setup Status

**Date**: 2026-01-27
**Platform**: PDP-11/70 on SimH, RSX-11M V3.2 BL26
**Goal**: Compile XL Spreadsheet FORTRAN sources on authentic 1978 hardware

---

## Summary

| Component | Status |
|-----------|--------|
| RSX-11M boot to 124K mapped | ✅ Working |
| FORTRAN IV compiler (FOR.TSK) | ✅ Built and installed |
| Compilation of .FTN files | ✅ Working |
| FOROTS.OLB runtime library | ✅ Created |
| Linking with TKB | ✅ Produces .TSK |
| Runtime execution | ❌ Fails - needs autopatch |
| File transfer from host | ❌ Blocked - needs PUTR |

---

## What's Working

### 1. RSX-11M Boot Sequence

Successfully boots RSX-11M V3.2 BL26 to 124K mapped system:

```
  RSX-11M V3.2 BL26   28K        ; Initial unmapped boot
>INS $BOO
>BOO [1,54]
  RSX-11M V3.2 BL26   124K MAPPED  ; Full memory system
```

### 2. FORTRAN IV Compiler

Built from FOR.OLB using Task Builder:

```
>SET /UIC=[1,24]
>TKB @FOR11M
>INS DL0:[1,54]FOR
```

- **Location**: DL0:[1,54]FOR.TSK
- **Size**: 253 blocks
- **Version**: FORTRAN IV V02.2-1

### 3. Successful Compilation

Test program compiles correctly:

```
>FOR FORTST,TI:=FORTST

FORTRAN IV      V02.2-1         THU 19-JAN-78 19:01:12

      C
      C     DEMONSTRATION TEST PROGRAM FOR RSX FORTRAN IV
      ...
.MAIN.

FORTRAN IV STORAGE MAP FOR PROGRAM UNIT .MAIN.
LOCAL VARIABLES, .PSECT $DATA, SIZE = 000006 (    3. WORDS)
NAME   TYPE  OFFSET
A      R*4   000000     I      I*2   000004
```

### 4. FOROTS Library Created

Runtime library built following documentation:

```
>SET /UIC=[1,1]
>INS $LBR
>LBR FOROTS/CR:170.:1000.:250.=DL1:[11,42]SHORT
>LBR FOROTS/DG:$ERTXT
>LBR FOROTS=DL1:[11,42]FOROTS,DL1:[11,42]FORFPU,DL1:[11,42]VIRP
```

- **Location**: DL0:[1,1]FOROTS.OLB
- **Size**: 154 blocks
- **Entry points**: 100+ including $CALL, $CLOSE, $CONV*, $FIO, etc.

### 5. Linking Succeeds

Task links without undefined symbols:

```
>TKB FORTST/FP=FORTST,LB:[1,1]FOROTS/LB
```

- **Output**: FORTST.TSK (32 blocks)
- **/FP flag**: Required for floating point support

---

## What's NOT Working

### Runtime Initialization Failure

```
>RUN FORTST
TT0  --  EXITING DUE TO ERROR 2
TASK INITIALIZATION FAILURE
  IN   ".MAIN." AT ?
```

**Root Cause**: The FORTRAN OTS from the F4_IAS_RSX_2.1 distribution has bugs in the $FIO (file I/O) module. The autopatch disk contains fixes:

- **OTSPAT.OBJ** - Patches $FIO module
- **VIRPAT.OBJ** - Patches $VIRIP module (virtual arrays)

### File Transfer Blocked

RT-11 intermediate disk method fails:

```
>FLX SY:[1,24]/RS=DL2:STRUTIL.FTN/RT
FLX -- FMTD ASCII RECORD FORMAT BAD
```

**Root Cause**: The `fsio` tool (and similar modern tools) don't create proper RT-11 ASCII record format. Need **PUTR** (Windows/DOS tool from dbit.com) to create properly formatted RT-11 disks.

---

## Disk Images

### Available

| File | Size | Description |
|------|------|-------------|
| `rsxm_work.rl01` | 5.2 MB | Working system - has FOR.TSK, FOROTS.OLB |
| `F4_IAS_RSX_2.1.rl01` | 5.2 MB | FORTRAN distribution - compiler sources |
| `RSX-11M_V3.2_RSX11MBL26_3.2.rl01` | 5.2 MB | Original baseline |
| `RSX-11M_V3.2_MAPSRC.rl01` | 5.2 MB | Mapping sources |
| `RSX-11M_V3.2_RLUTIL.rl01` | 5.2 MB | Utilities |

### Needed

| File | Source | Purpose |
|------|--------|---------|
| `RSX-11M_V3.2_AUTOPATCH1B.DSK` | [bitsavers](http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/RSX-11M_V3.2_AUTOPATCH1B.DSK.gz) | OTS bug fixes |

---

## Working Scripts

Location: `/Volumes/SECURE8/git/spreadsheet-fortran/emulator/pdp11-rsx/`

| Script | Purpose |
|--------|---------|
| `boot_rsx.exp` | Boot RSX to 124K mapped |
| `build_for2.exp` | Build FOR.TSK compiler |
| `forots_setup.exp` | Create FOROTS.OLB |
| `test_fortran.exp` | Full compile/link/run test |
| `full_for_setup.exp` | Complete setup following docs |

---

## Directory Structure on RSX Disks

### DL0: (rsxm_work.rl01) - System Disk

```
[1,1]   - System files, FOROTS.OLB
[1,2]   - Startup scripts
[1,24]  - FORTRAN work area, build files
[1,54]  - FOR.TSK, BOO.TSK (bootable executive)
```

### DL1: (F4_IAS_RSX_2.1.rl01) - FORTRAN Distribution

```
[11,41] - Compiler build files
         FOR.OLB, FOR11M.CMD, FOR11M.ODL, FORBLD.ODL
[11,42] - OTS files
         FOROTS.OBJ, FORFPU.OBJ, VIRP.OBJ, SHORT.OBJ
         FORTST.FTN (test program)
```

---

## To Continue This Work

### Step 1: Download Autopatch

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator/pdp11-rsx/media
curl -O http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/RSX-11M_V3.2_AUTOPATCH1B.DSK.gz
gunzip RSX-11M_V3.2_AUTOPATCH1B.DSK.gz
```

### Step 2: Apply Patches in RSX-11M

Mount autopatch disk as DL2: (need to configure third RL drive):

```
sim> SET RL2 RL01
sim> ATTACH RL2 media/RSX-11M_V3.2_AUTOPATCH1B.DSK
```

In RSX-11M:
```
>MOU DL2:AUTOPATCHB1
>SET /UIC=[1,24]
>PIP [1,24]=DL2:[250,1]*.*    ; Copy patch files
>@DL2:[250,200]FORT4          ; Run patch script
```

Answer prompts:
- Mapped system? **Y**
- OTS in SYSLIB? **N**
- OTS in FOROTS.OLB? **Y**
- Verify? **Y**
- Virtual array support? **Y**

### Step 3: Re-test

```
>TKB FORTST/FP=FORTST,LB:[1,1]FOROTS/LB
>RUN FORTST
***** RSX FORTRAN IV DEMONSTRATION TEST *****
INSTALLATION SUCCESSFUL IF NO ERROR MESSAGES
                WERE PRINTED ABOVE.
**** FORTRAN DEMONSTRATION TEST COMPLETE *****
```

### Step 4: File Transfer

For getting XL FORTRAN sources onto RSX-11M:

**Option A**: PUTR (if Windows available)
```
PUTR> format transfer.dsk /rl02 /rt11
PUTR> mount x: transfer.dsk /rt11
PUTR> copy c:\xl-sources\*.for x:
```

**Option B**: Paper tape (tedious but works)
```
sim> ATTACH PTR /path/to/STRUTIL.FOR
>PIP STRUTI.FTN=PC:
```

**Option C**: Kermit-11 (more complex setup)

---

## Reference Documentation

- `/Volumes/SECURE8/git/spreadsheet-fortran/docs/Revisiting RSX-11M v3.2.html` - Primary reference showing exact steps
- `/Volumes/SECURE8/git/spreadsheet-fortran/docs/pdp-11-rsx-11m-upload-guidance.md` - File transfer methods
- `/Volumes/SECURE8/git/spreadsheet-fortran/docs/SIMH_NATIVE_SCRIPTING.md` - SimH scripting

---

## Technical Notes

### RSX-11M Boot Process

1. Hardware bootstrap loads LBN 0 from RL01
2. Boot block loads unmapped executive (28K)
3. `INS $BOO` installs secondary bootstrap
4. `BOO [1,54]` loads mapped executive with memory management
5. Second startup script runs with full memory (124K on this config)

### FORTRAN Build Process

1. FOR.OLB contains compiler object modules
2. FOR11M.CMD is TKB command file for building FOR.TSK
3. FOR11M.ODL specifies overlay structure
4. TKB reads .CMD/.ODL, links from FOR.OLB, produces FOR.TSK

### FOROTS Library Structure

- **SHORT.OBJ** - Base OTS framework with non-default symbols
- **FOROTS.OBJ** - Main runtime routines
- **FORFPU.OBJ** - FPP (Floating Point Processor) support
- **VIRP.OBJ** - Virtual array paging support

The `/CR` switch creates library with parameters:
- 170 blocks initial allocation
- 1000 entry point table entries
- 250 module name table entries

---

**Status**: Paused - awaiting autopatch download
**Last Updated**: 2026-01-27
