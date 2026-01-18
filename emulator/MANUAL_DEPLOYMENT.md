# Manual Deployment Guide - Connect Now and Deploy XL

The CP-V emulator is **RUNNING** and ready for deployment!

**Emulator Status:**
- ✅ Process running (PID 56412)
- ✅ Port 5001 listening
- ✅ MUX device accepting connections

---

## Quick Start - Deploy XL Right Now

### Step 1: Connect to CP-V

Open a new terminal and connect:

```bash
telnet localhost 5001
```

Or use netcat:
```bash
nc localhost 5001
```

**You should see:**
```
Connected to the XDS Sigma simulator MUX device, line 1

HI, CP-V HERE - F00
[TIME] [DATE]  USER# [X]  LINE# [Y]
LOGON PLEASE:
```

### Step 2: Login

At the `LOGON PLEASE:` prompt, type:
```
:SYS,LBE
```

Press RETURN. No password required.

**You should see:**
```
$
```

This is the CP-V command prompt.

### Step 3: Choose Deployment Method

You have two options:

---

## Option A: Batch Job Deployment (RECOMMENDED)

**Historically authentic 1978 workflow**

### Transfer Files to CP-V

You need to transfer 12 files:
- 11 .FOR files (source code)
- 1 .JOB file (batch job)

All files are in: `emulator/work/`

**Method: Copy and Paste via EDIT command**

For each file:

```
$ EDIT filename
```

Then **paste the entire file contents** from your clipboard.

After pasting, type:
```
:FILE
:QUIT
```

**Files to transfer (in this order):**

1. `$ EDIT STRUTIL.FOR` → Paste contents of `work/STRUTIL.FOR` → `:FILE` → `:QUIT`
2. `$ EDIT CELLS.FOR` → Paste contents of `work/CELLS.FOR` → `:FILE` → `:QUIT`
3. `$ EDIT DEPS.FOR` → Paste contents of `work/DEPS.FOR` → `:FILE` → `:QUIT`
4. `$ EDIT PARSE.FOR` → Paste contents of `work/PARSE.FOR` → `:FILE` → `:QUIT`
5. `$ EDIT EVAL.FOR` → Paste contents of `work/EVAL.FOR` → `:FILE` → `:QUIT`
6. `$ EDIT RECALC.FOR` → Paste contents of `work/RECALC.FOR` → `:FILE` → `:QUIT`
7. `$ EDIT UI.FOR` → Paste contents of `work/UI.FOR` → `:FILE` → `:QUIT`
8. `$ EDIT DISPLAY.FOR` → Paste contents of `work/DISPLAY.FOR` → `:FILE` → `:QUIT`
9. `$ EDIT MSG.FOR` → Paste contents of `work/MSG.FOR` → `:FILE` → `:QUIT`
10. `$ EDIT TERMCPV.FOR` → Paste contents of `work/TERMCPV.FOR` → `:FILE` → `:QUIT`
11. `$ EDIT XLMAIN.FOR` → Paste contents of `work/XLMAIN.FOR` → `:FILE` → `:QUIT`
12. `$ EDIT XLBUILD.JOB` → Paste contents of `work/XLBUILD.JOB` → `:FILE` → `:QUIT`

### Verify Files

```
$ CATALOG
```

You should see all 12 files listed.

### Submit Batch Job

```
$ SUBMIT XLBUILD.JOB
```

**CP-V will respond with:**
```
XLBUILD    ACCEPTED   [timestamp]
```

The batch system will compile all modules automatically. This may take a few seconds in the emulator.

### Check Job Status

```
$ STATUS JOBS
```

When complete, you should see:
```
XLBUILD    COMPLETE   [timestamp]
```

### View Job Output (if needed)

```
$ OUTPUT XLBUILD
```

This shows compilation messages and any errors.

### Run XL Spreadsheet!

```
$ RUN XL
```

You should see the full-screen VT-52 spreadsheet interface!

---

## Option B: Interactive Compilation (Manual)

If you prefer to compile modules one at a time:

### Transfer Source Files

Transfer only the 11 .FOR files (same method as above, skip XLBUILD.JOB).

### Compile Each Module

For each module, run:

```
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*LINK
$
```

Repeat for:
- STRUTIL.FOR
- CELLS.FOR
- DEPS.FOR
- PARSE.FOR
- EVAL.FOR
- RECALC.FOR
- UI.FOR
- DISPLAY.FOR
- MSG.FOR
- TERMCPV.FOR

### Compile and Link Main Program

```
$ RUN FORTRAN
*SOURCE=XLMAIN.FOR
*OBJECT=XLMAIN.OBJ
*LIBRARY=STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMCPV
*LINK
*MAP
*GO
$
```

### Save Executable

```
$ SAVE XL
```

### Run XL Spreadsheet

```
$ RUN XL
```

---

## Using XL Spreadsheet

Once running, you'll see:

```
A1  NAV
    A         B         C         D         E         F         G         H
────────────────────────────────────────────────────────────────────────────
 1
 2
 3
 ...
```

### Controls:

- **Arrow Keys**: Navigate between cells
- **Type a number**: Enter value
  - Example: `100` [RETURN]
- **Type a formula**: Enter formula
  - Example: `=A1+B1` [RETURN]
- **ESC**: Cancel entry
- **/QUIT**: Exit spreadsheet

### Example Session:

```
[Arrow to A1]
100 [RETURN]          → Cell A1 now contains 100

[Arrow to A2]
200 [RETURN]          → Cell A2 now contains 200

[Arrow to A3]
=A1+A2 [RETURN]       → Cell A3 now shows 300

[Arrow to B1]
=A1*2 [RETURN]        → Cell B1 now shows 200

/QUIT [RETURN]        → Exit to CP-V
```

---

## Terminal Requirements

**Your terminal must support VT-52 escape sequences:**

Modern terminal emulators that work:
- macOS Terminal (VT-100 compatible, handles VT-52)
- iTerm2
- xterm
- PuTTY (set to VT-100 mode)

If the display looks garbled, your terminal may not support VT-52/VT-100 properly.

---

## Troubleshooting

### "File not found" error

Make sure you transferred all files. Use `CATALOG` to list files.

### Compilation errors

View the errors:
```
$ OUTPUT XLBUILD
```

Common issues:
- **PARAMETER not supported**: CP-V F/IV version may not support PARAMETER statements
- **Syntax error**: Check that file was pasted correctly

### "Undefined reference" during link

Make sure all modules compiled successfully. Check:
```
$ CATALOG *.OBJ
```

You should see 11 .OBJ files.

### Screen doesn't clear properly

Your terminal may not be VT-52 compatible. Try setting:
```
export TERM=vt100
```

Before connecting.

### Can't connect to emulator

Check emulator is running:
```bash
ps aux | grep sigma
```

Check port 5001 is listening:
```bash
nc -z localhost 5001
```

---

## CP-V Commands Reference

**File Management:**
- `CATALOG` - List files
- `CATALOG *.FOR` - List all .FOR files
- `DELETE filename` - Delete file
- `RENAME old new` - Rename file

**System:**
- `DATE` - Show current date/time
- `LOGOFF` - Log out
- `STATUS SYSTEM` - System status
- `STATUS JOBS` - Batch job status

**Editing:**
- `EDIT filename` - Create/edit file
- `:FILE` - Save file (inside editor)
- `:QUIT` - Exit editor (inside editor)

**Batch Jobs:**
- `SUBMIT jobfile` - Submit batch job
- `STATUS JOBS` - Check job status
- `OUTPUT jobname` - View job output
- `CANCEL jobname` - Cancel running job

---

## Experience 1978 Computing!

This is as close as you can get to using a university timesharing system in 1978:

✅ **VT-52 CRT terminal** - Character-cell full-screen display
✅ **Batch job processing** - Submit jobs, wait for compilation
✅ **FORTRAN IV** - Period-correct programming language
✅ **CP-V operating system** - Real 1978 OS (emulated)
✅ **Interactive spreadsheet** - Like VisiCalc (1979)

The only difference: instead of waiting minutes or hours for compilation (with other users competing for CPU time), your batch job completes in seconds because you have the entire emulated Sigma 7 to yourself!

---

## Ready to Deploy!

**Right now, you can:**
1. Open terminal: `telnet localhost 5001`
2. Login: `:SYS,LBE`
3. Start transferring files
4. Build and run XL Spreadsheet
5. Experience 1978!

**All files are ready in:** `emulator/work/`

---

**Questions?** See:
- `BATCH_DEPLOYMENT.md` - Detailed batch job guide
- `CPV_DEPLOYMENT.md` - Detailed interactive guide
- `QUICKSTART.md` - CP-V emulator basics
