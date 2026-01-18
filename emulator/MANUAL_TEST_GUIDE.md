# Manual Testing Guide for XL Spreadsheet on CP-V

**Current Status:** All 12 files successfully transferred to f00 system. CP-V is running on port 5001.

---

## Quick Start - 3 Minute Test

### 1. Connect to CP-V
```bash
nc localhost 5001
```

**Wait patiently** - CP-V sends characters one at a time. You'll eventually see:
```
HONEYWELL CP-V AT YOUR SERVICE - SIMH
LOGON PLEASE:
```

### 2. Login
Type:
```
:SYS,LBE
```
Press Enter.

Wait for the `!` prompt.

### 3. List Files
Type:
```
L
```
Press Enter.

**Look for our 12 files:**
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
- XLMAIN.FOR
- XLBUILD.JOB

### 4. Test FORTRAN Compiler
Type:
```
FORTRAN
```
Press Enter.

**Possible responses:**
- **If FORTRAN exists:** You'll see FORTRAN compiler prompt or help
- **If A603 error:** FORTRAN not installed, need to switch to cpcp
- **If different error:** Note the error message

### 5. If FORTRAN Works - Submit Batch Job
Type:
```
SUBMIT XLBUILD.JOB
```
Press Enter.

Look for:
```
ACCEPTED
```

### 6. Check Batch Status
Type:
```
STATUS
```
Press Enter.

You should see the job in the queue or completed.

### 7. View Results
If compilation succeeded:
```
TYPE XLBUILD.LOG
```

If XL executable was created:
```
RUN XL
```

---

## If FORTRAN Doesn't Exist

### Switch to cpcp System

1. **Shutdown f00:**
```bash
# In another terminal
screen -S cpv -X quit
# or
pkill sigma
```

2. **Boot cpcp:**
```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
./scripts/boot_cpcp_screen.sh
```

3. **Transfer files again:**
```bash
./scripts/deploy_xl.exp
```
(This will take 3-4 minutes)

4. **Repeat manual test above**

---

## Commands Reference

| Command | Purpose |
|---------|---------|
| `L` | List files in current directory |
| `FILES` | Alternative file listing |
| `CATALOG` | System catalog (may not work on f00) |
| `FORTRAN` | FORTRAN compiler |
| `F77` | FORTRAN 77 compiler (alternative) |
| `ANSFORT` | ANS FORTRAN (on f00 system) |
| `SUBMIT filename` | Submit batch job |
| `STATUS` | Check batch job status |
| `TYPE filename` | Display file contents |
| `RUN programname` | Run executable |
| `LOGOFF` | Logout |
| `Esc F` | Emergency exit |

---

## Expected Timeline

- **File listing:** 10-30 seconds
- **FORTRAN test:** 5-15 seconds  
- **Batch job submit:** 5-10 seconds
- **Compilation:** 2-5 minutes (depends on system)

---

## Troubleshooting

### No response after login
- Be patient - CP-V is VERY slow
- Wait 1-2 minutes
- Try pressing Enter

### Can't see what you're typing
- Normal for some CP-V configurations
- Characters are being sent, just not echoed

### Connection drops
- CP-V may have crashed
- Check: `ps aux | grep sigma`
- Reboot: `./scripts/boot_cpv_screen.sh`

### Files not found
- They should be there from successful transfer
- Try `FILES` instead of `L`
- Try `CATALOG` (may show A603 error but try it)

---

## Success Indicators

✅ **Files found** - All 12 files listed  
✅ **FORTRAN responds** - No A603 error  
✅ **Job accepted** - Batch submission worked  
✅ **Job completes** - STATUS shows finished  
✅ **XL runs** - Spreadsheet starts  
✅ **VT-52 interface works** - Screen clears, cursor moves

---

## What to Report Back

Please let me know:
1. **Did files list successfully?** (Y/N + which command worked)
2. **FORTRAN response:** (exact error or success message)
3. **Batch job result:** (ACCEPTED or error)
4. **Any unexpected behavior**

This will help me determine next steps!

---

**Note:** CP-V is running and ready. Port 5001 is listening. All files are on disk. Just need manual verification of FORTRAN compiler!
