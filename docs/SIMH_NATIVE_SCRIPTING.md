# SimH V4 Native Scripting Reference

**Replaces expect scripts with native SimH commands**

## Version Required

- Open SIMH V4.1-0 or later
- Installed via: `brew install --HEAD open-simh`

## Key Commands

### EXPECT - React to Console Output

```simh
EXPECT {dev:line} {[count]} {HALTAFTER=n,}"<string>" {action; action...}
NOEXPECT {dev:line} "<string>"
SHOW EXPECT {dev:line}
```

**Switches:**
- `-p` - Persistent rule (doesn't auto-remove after match)
- `-c` - Clear all pending expects when matched
- `-r` - Regular expression matching
- `-i` - Case insensitive (with -r only)

**Variables set on match:**
- `_EXPECT_MATCH_PATTERN` - The pattern that matched
- `_EXPECT_MATCH_GROUP_0` - Full matched string (regex)
- `_EXPECT_MATCH_GROUP_n` - Captured groups (regex)

### SEND - Inject Console Input

```simh
SEND {-t} {after=nn,}{delay=nn,}"<string>"
NOSEND
SHOW SEND
```

**Parameters:**
- `after=n` - Instructions before first character (default: delay value)
- `delay=n` - Instructions between characters (default: 1000)
- `-t` - Delay in microseconds instead of instructions

**Escape sequences:**
- `\r` - Carriage return
- `\n` - Linefeed
- `\e` - Escape (ASCII 27)
- `\x1a` - Ctrl-Z (hex notation)
- `\\` - Literal backslash

### Control Flow

```simh
:label           ; Define a label
GOTO label       ; Jump to label
CALL label       ; Call subroutine (RETURN to continue)
RETURN           ; Return from CALL
EXIT             ; Exit script

IF condition command
IF condition (command) ELSE (command)
```

### Error Handling

```simh
ON ERROR GOTO label
ON ERROR EXIT
ON ERROR IGNORE
ON ERROR (action)
```

## Example: Boot RSX-11M and Compile FORTRAN

```simh
; boot_and_compile.ini - Native SimH script for RSX-11M

SET CPU 11/70
SET CPU 1M
SET RL ENABLE
ATTACH RL0 media/RSX-11M_V3.2_RSX11MBL26_3.2.rl01
ATTACH RL1 media/F4_IAS_RSX_2.1.rl01

; Set up expects for boot sequence
EXPECT "HR:MN DD-MMM-YY" SEND "19:00 19-JAN-78\r"; GO
EXPECT "LINE WIDTH" SEND "80\r"; GO
EXPECT "<EOF>" GOTO :boot_complete

; Boot the system
BOOT RL0
GO

:boot_complete
echo Boot complete, setting up FORTRAN...

; Setup expects for mapped boot
EXPECT ">" SEND "INS $BOO\r"; GO
EXPECT ">" SEND "BOO [1,54]\r"; GO

; Second boot prompts
EXPECT "HR:MN DD-MMM-YY" SEND "19:01 19-JAN-78\r"; GO
EXPECT "LINE WIDTH" SEND "80\r"; GO
EXPECT "<EOF>" GOTO :mapped_ready

GO

:mapped_ready
echo Mapped system ready, installing FORTRAN...

; Install FORTRAN compiler
EXPECT ">" SEND "SET /UIC=[1,1]\r"; GO
EXPECT ">" SEND "INS DL0:[1,54]FOR\r"; GO
EXPECT ">" SEND "MOU DL1:FOR/OVR\r"; GO
EXPECT ">" SEND "SET /UIC=[1,24]\r"; GO

; Copy runtime objects
EXPECT ">" SEND "PIP FOROTS.OBJ=DL1:[11,42]FOROTS.OBJ\r"; GO
EXPECT ">" SEND "PIP FOREIS.OBJ=DL1:[11,42]FOREIS.OBJ\r"; GO
EXPECT ">" SEND "PIP NOVIR.OBJ=DL1:[11,42]NOVIR.OBJ\r"; GO

; Now ready for compilation
EXPECT ">" GOTO :ready
GO

:ready
echo System ready for FORTRAN development!
echo Use: SEND "FOR program,listing=source\r" to compile
echo Use: SEND "TKB prog,prog=objs,FOROTS,FOREIS,NOVIR,LB:[1,1]SYSLIB/LB\r" to link
```

## Comparison: Expect vs Native SimH

| Feature | Expect Script | Native SimH |
|---------|--------------|-------------|
| Boot system | `spawn pdp11 config.ini` | `BOOT RL0` |
| Wait for output | `expect "string"` | `EXPECT "string" action` |
| Send input | `send "text\r"` | `SEND "text\r"` |
| Control flow | `if/else` | `IF`, `GOTO`, `CALL` |
| Regex matching | Limited | `-r` switch with PCRE |
| Timing control | `sleep`, `after` | `delay=`, `after=` |
| External process | Required | Not needed |
| Script location | Separate .exp file | Can be in .ini file |

## Advantages of Native Scripting

1. **No external dependencies** - No expect/tcl required
2. **Better timing** - Uses instruction counts, more deterministic
3. **Integrated** - Script can be part of .ini config
4. **Portable** - Works on any SimH V4 platform
5. **Debuggable** - Use `SHOW EXPECT`, `SHOW SEND`

## Migration from Expect

Replace:
```expect
#!/usr/bin/expect -f
spawn pdp11 config.ini
expect "HR:MN"
send "19:00 19-JAN-78\r"
expect ">"
send "INS \$PIP\r"
```

With:
```simh
; config.ini
EXPECT "HR:MN" SEND "19:00 19-JAN-78\r"; GO
EXPECT ">" SEND "INS $PIP\r"; GO
BOOT RL0
GO
```

## Running Scripts

```bash
# Run script directly
pdp11 boot_and_compile.ini

# Or from command line
pdp11 -e boot_and_compile.ini
```

## Debugging

```simh
; Show pending expects
SHOW EXPECT

; Show pending sends
SHOW SEND

; Enable debugging
SET CONSOLE DEBUG=debug.log
```

## Practical Approaches for RSX-11M Automation

### Approach 1: Native SimH for Simple Sequences

Best for: Boot scripts, command chains, simple interactions

```simh
; boot_and_setup.ini
EXPECT ">" SEND "INS $PIP\r"
EXPECT ">" SEND "INS $FOR\r"
EXPECT ">" SEND "MOU DL1:FORTRAN/OVR\r"
BOOT RL0
```

### Approach 2: Paper Tape for File Transfer

Best for: Transferring source files without line-by-line terminal input

```simh
; Host side: attach source file to paper tape reader
ATTACH PTR /Volumes/path/to/STRUTIL.FOR

; In RSX-11M: copy from paper tape
EXPECT ">" SEND "PIP STRUTI.FTN=PC:\r"
```

### Approach 3: RT-11 Intermediate Disk (Recommended for Bulk)

Best for: Transferring multiple files, binaries

1. Create RT-11 disk image on host using PUTR or FSX
2. Attach in SimH as additional RL drive
3. Use FLX in RSX-11M to copy files

```simh
; Attach transfer disk
ATTACH RL3 transfer.rl02

; In RSX-11M:
; >MOU DL3:/FOR
; >FLX SY:[1,24]/RS=DL3:*.FOR/RT
```

### Approach 4: Hybrid (Native SimH + Minimal Expect)

Use native SimH for:
- Boot sequence
- Disk attachment
- System setup

Use expect only for:
- Complex file transfers (reading host files)
- Error handling with conditionals

### Limitations of Native SimH Scripting

1. **Cannot read host files** - No way to read file content into SEND
2. **No string variables** - Can't build dynamic commands
3. **Limited conditionals** - IF exists but limited compared to expect
4. **No loops** - Would need GOTO for iteration

### Recommended Workflow

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. Use native SimH script for boot and setup                    │
│    pdp11 boot_setup.ini                                         │
├─────────────────────────────────────────────────────────────────┤
│ 2. Transfer files via paper tape or intermediate disk           │
│    - Avoids slow line-by-line terminal transfer                │
│    - More reliable than console injection                       │
├─────────────────────────────────────────────────────────────────┤
│ 3. Use native SimH for compilation/linking                      │
│    EXPECT ">" SEND "FOR PROG,PROG=PROG\r"                      │
│    EXPECT ">" SEND "TKB PROG=...\r"                            │
├─────────────────────────────────────────────────────────────────┤
│ 4. Clean shutdown                                               │
│    EXPECT ">" SEND "\x05"; QUIT                                │
└─────────────────────────────────────────────────────────────────┘
```

---

## RSX-11M FORTRAN IV Setup Progress (2026-01-27)

### What's Working ✅

**1. Boot to 124K Mapped System**
```bash
# Use expect script (native SimH scripting unreliable for multi-boot)
./boot_rsx.exp
```

Boot sequence:
- First boot: 28K unmapped executive
- INS $BOO / BOO [1,54]: Loads 124K mapped executive
- Second boot with full memory management

**2. FORTRAN Compiler Built and Working**
```
>INS $TKB
>SET /UIC=[1,24]
>TKB @FOR11M              ; Build compiler from FOR.OLB
>INS DL0:[1,54]FOR        ; Install compiler
>FOR FORTST,TI:=FORTST    ; Compile test program

FORTRAN IV V02.2-1 ...
.MAIN.                    ; Success!
```

The compiler (FOR.TSK, 253 blocks) is built and stored at DL0:[1,54].

**3. FOROTS Library Creation**
```
>MOU DL1:FOR              ; Mount FORTRAN distribution disk
>SET /UIC=[1,1]
>INS $LBR
>LBR FOROTS/CR:170.:1000.:250.=DL1:[11,42]SHORT
>LBR FOROTS/DG:$ERTXT
>LBR FOROTS=DL1:[11,42]FOROTS,DL1:[11,42]FORFPU,DL1:[11,42]VIRP
```

Creates FOROTS.OLB (154 blocks) with all OTS entry points.

### What's NOT Working ❌

**Runtime Fails: "TASK INITIALIZATION FAILURE"**
```
>TKB FORTST/FP=FORTST,LB:[1,1]FOROTS/LB
>RUN FORTST
TT0  --  EXITING DUE TO ERROR 2
TASK INITIALIZATION FAILURE
  IN   ".MAIN." AT ?
```

The linked task (32 blocks) builds successfully but crashes at runtime.

**Root Cause**: The FOROTS library from F4_IAS_RSX_2.1 has bugs in the $FIO module that require patches from **RSX-11M_V3.2_AUTOPATCH1B.DSK** (available from bitsavers).

### To Continue This Work

**Option 1: Download and apply autopatch**
```bash
# Download autopatch disk
curl -O http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/RSX-11M_V3.2_AUTOPATCH1B.DSK.gz
gunzip RSX-11M_V3.2_AUTOPATCH1B.DSK.gz
mv RSX-11M_V3.2_AUTOPATCH1B.DSK media/
```

Then in RSX-11M:
```
>MOU DL3:AUTOPATCHB1
>SET /UIC=[1,24]
>PIP [1,24]=DL3:[250,1]*.*
>@DL3:[250,200]FORT4       ; Run autopatch script
; Answer: Y for mapped system
; Answer: N for SYSLIB, Y for FOROTS.OLB
; Answer: Y for virtual array support
```

This applies OTSPAT.OBJ and VIRPAT.OBJ patches to fix $FIO and $VIRIP modules.

**Option 2: Try a different OTS approach**
The docs at "Revisiting RSX-11M v3.2.html" lines 2390-2405 show it working before patches with /FP flag. May be a version difference.

### Disk Images Available

| File | Description | Status |
|------|-------------|--------|
| rsxm_work.rl01 | Working RSX system disk | Has FOR.TSK, FOROTS.OLB |
| F4_IAS_RSX_2.1.rl01 | FORTRAN distribution | Source for compiler/OTS |
| RSX-11M_V3.2_RSX11MBL26_3.2.rl01 | Baseline RSX | Original system |
| RSX-11M_V3.2_AUTOPATCH1B.DSK | **NEEDED** | Not downloaded yet |

### Working Expect Scripts

| Script | Purpose |
|--------|---------|
| `boot_rsx.exp` | Boot to 124K mapped system |
| `forots_setup.exp` | Create FOROTS.OLB library |
| `test_fortran.exp` | Full compile/link/run test |

### File Transfer Status

RT-11 intermediate disk method attempted but encountering "FMTD ASCII RECORD FORMAT BAD" errors with FLX. The fsio tool doesn't create proper RT-11 ASCII records. **Needs PUTR** (Windows/DOS tool) for proper RT-11 disk creation.

---

*Updated: 2026-01-27*
*SimH Version: Open SIMH V4.1-0*
