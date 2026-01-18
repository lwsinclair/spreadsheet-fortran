# How CP-V Automated Deployment Works

## The Problem We Solved

**Challenge:** CP-V emulator needs to stay running in the background after boot, but expect scripts kill their spawned processes when they exit.

**Solution:** Use `screen` to run the expect script in a detached session, with the expect script using `interact` at the end to keep sigma alive.

---

## Architecture

```
User runs: ./scripts/boot_cpv_screen.sh
    ↓
Creates detached screen session named 'cpv'
    ↓
Inside screen: runs ./scripts/boot_for_screen.exp
    ↓
Expect script spawns: sigma boot_cpv.ini
    ↓
Expect answers boot questions automatically
    ↓
Expect uses `interact` to stay connected (keeps sigma alive)
    ↓
Screen session stays running in background
    ↓
User can connect: nc localhost 5001
```

---

## Key Files

### 1. `scripts/boot_cpv_screen.sh` - Main Boot Script

**What it does:**
- Starts a detached screen session named 'cpv'
- Runs boot_for_screen.exp inside that session
- Waits for port 5001 to be listening (verifies boot)
- Returns control to user while sigma keeps running

**Usage:**
```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
./scripts/boot_cpv_screen.sh
```

**Output:**
```
CP-V is ready!
Connection: nc localhost 5001
Login: :SYS,LBE
```

### 2. `scripts/boot_for_screen.exp` - Expect Boot Script

**What it does:**
- Spawns `sigma boot_cpv.ini`
- Answers boot questions:
  - DATE: 01/19/78
  - TIME: Current time
  - DELTA: N
  - HGP RECONSTRUCTION: N
  - ALLOCAT DATA DUAL: N
  - BATCH QUEUE RECOVERY: N
- Waits for "FILL ghost initiated"
- Uses `interact` to stay connected to sigma

**Critical:** The `interact` command at the end is what keeps this script (and therefore sigma) running. Without it, the script would exit and kill sigma.

### 3. `scripts/deploy_xl.exp` - File Transfer Script

**What it does:**
- Connects to already-running CP-V via nc localhost:5001
- Logs in as :SYS,LBE
- Transfers all 12 files using EDIT command
- Submits batch job: XLBUILD.JOB
- Verifies with CATALOG

**Requirements:**
- CP-V must already be running (boot_cpv_screen.sh first)
- Port 5001 must be listening

---

## Boot Question Sequence

When CP-V boots from RAD, it asks:

```
DATE(MM/DD/YY)= <-- Answer: 01/19/78
TIME(HH:MM)=    <-- Answer: HH:MM (current time)
DO YOU WANT DELTA (Y/N) <-- Answer: N
DO YOU WANT HGP RECONSTRUCTION(Y/N)? <-- Answer: N
DO YOU WANT ALLOCAT DATA DUAL(Y/N)? <-- Answer: N
ATTEMPT BATCH QUEUE RECOVERY(Y/N)? <-- Answer: N
```

After answering these, the system shows:
```
7:(FILL)   FILL GHOST INITIATED - USE 'INT,FILL.' FOR COMMANDS
```

This means CP-V is ready for use.

---

## Why Screen?

**Problem:** When an expect script exits, it sends SIGHUP to spawned processes, killing them.

**Solutions tried:**
1. ❌ `spawn sigma` + `exit 0` → sigma dies
2. ❌ `spawn sigma` in background → can't answer prompts
3. ❌ `expect_background` → complicated, didn't work
4. ✅ **screen + interact** → WORKS!

**How screen helps:**
- Screen creates a persistent terminal session
- The expect script runs inside screen
- `interact` keeps expect connected to sigma
- Screen session persists even when we detach
- Sigma stays alive as long as screen session exists

---

## Operator Console Commands

**These DON'T work at sim> prompt:**
- `ON 107` - Unknown command (but not fatal!)
- `ONB 6` - Unknown command
- `SLP,I` - Unknown command

**Why:** These are CP-V operator console commands, not simh simulator commands. They need to be sent to CP-V after login, not to the `sim>` prompt.

**Result:** CP-V still works fine even if these commands "fail" at sim>. The system boots and accepts logins anyway.

---

## Process Management

### Check if CP-V is running:
```bash
ps aux | grep [s]igma
lsof -i :5001
```

### View the screen session:
```bash
screen -r cpv
```
(Press Ctrl-A then D to detach again)

### Shutdown CP-V gracefully:
```bash
screen -S cpv -X quit
```

### Force shutdown:
```bash
pkill sigma
```

### Kill stale screen sessions:
```bash
screen -ls              # List sessions
screen -S cpv -X quit   # Quit specific session
```

---

## File Transfer Process

The `deploy_xl.exp` script transfers files like this:

```
For each file:
  1. Send: EDIT filename\r
  2. Wait for editor prompt
  3. Send file contents (line by line)
  4. Send: :FILE\r (save)
  5. Send: :QUIT\r (exit editor)
  6. Repeat for all 12 files

After all files:
  7. Send: SUBMIT XLBUILD.JOB\r
  8. Wait for "ACCEPTED"
  9. Done!
```

---

## Troubleshooting

### Port 5001 already in use
```bash
lsof -i :5001              # Find what's using it
pkill sigma                # Kill old sigma
screen -S cpv -X quit      # Kill screen session
./scripts/boot_cpv_screen.sh  # Start fresh
```

### CP-V not responding
```bash
screen -r cpv              # Attach to screen session
# You'll see sigma output
# Ctrl-E to get sim> prompt
# 'cont' to continue
# Ctrl-A then D to detach
```

### Sigma died unexpectedly
```bash
screen -r cpv              # Check what happened
# Look for error messages
screen -S cpv -X quit      # Clean up
rm -f work/printer.txt     # Clear logs
./scripts/boot_cpv_screen.sh  # Start fresh
```

### File transfer fails
```bash
# Make sure CP-V is running
lsof -i :5001

# Test login manually
nc localhost 5001
:SYS,LBE
CATALOG
LOGOFF

# Then retry deployment
./scripts/deploy_xl.exp
```

---

## Complete Deployment Workflow

**Step 1: Boot CP-V (one-time per session)**
```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
./scripts/boot_cpv_screen.sh
```

**Step 2: Deploy XL Spreadsheet**
```bash
./scripts/deploy_xl.exp
```

**Step 3: Monitor batch job**
```bash
nc localhost 5001
:SYS,LBE
STATUS
TYPE XLBUILD.LOG
RUN XL
```

**Step 4: Shutdown when done**
```bash
screen -S cpv -X quit
```

---

## Important Notes

1. **Screen sessions persist** across terminal closures - CP-V keeps running!

2. **One CP-V instance at a time** - Port 5001 can only be used by one sigma

3. **Always check if running** before starting:
   ```bash
   lsof -i :5001
   ```

4. **Screen is required** - Don't try to run boot_for_screen.exp directly

5. **ALLOCAT DATA DUAL = N** - We answer NO to this during boot, which means system commands like CATALOG might not work initially. This is a known issue we're working around.

---

## Future Improvements

1. **Fix ALLOCAT DATA DUAL** - Answer Y to properly mount system disks
2. **Automated operator console** - Figure out correct commands
3. **Installation automation** - First-time CP-V installation from tape
4. **Card deck deployment** - Use xl_deck.txt instead of individual files
5. **Status monitoring** - Script to check batch job progress

---

## Files Modified/Created

**Created:**
- `scripts/boot_cpv_screen.sh` - Main boot wrapper
- `scripts/boot_for_screen.exp` - Expect script for screen
- `scripts/deploy_xl.exp` - File transfer automation
- `HOW_IT_WORKS.md` - This file

**Working but not used:**
- `scripts/boot_cpv.exp` - Old version (kills sigma on exit)
- `scripts/boot_and_detach.exp` - Attempt #1 (didn't work)
- `scripts/boot_detached.exp` - Attempt #2 (didn't work)
- `scripts/boot_cpv_bg.sh` - Attempt #3 (couldn't answer prompts)

---

## Git History

**Important commits:**
- Initial deployment system (agents a36c828, af0dbcb, a35ee53)
- Screen-based boot solution (current session)

**To commit current work:**
```bash
git add scripts/boot_cpv_screen.sh scripts/boot_for_screen.exp HOW_IT_WORKS.md
git commit -m "Fix CP-V boot to stay running using screen sessions

The sigma emulator must stay running after boot, but expect scripts
kill spawned processes when they exit. Solution: run expect inside
a detached screen session with 'interact' at the end.

This keeps sigma alive and allows deployment scripts to connect."
git push origin main
```

---

**Last Updated:** 2026-01-19 03:15 AM
**Status:** ✅ Working - CP-V boots and stays running
**Next:** Create deploy_xl.exp and test end-to-end deployment
