# CP-V Troubleshooting Guide

Common issues and solutions for CP-V deployment

---

## Boot Issues

### "A603 LOAD MODULE DOES NOT EXIST" Error

**Symptom:** When you try to run commands like `CATALOG`, `FORTRAN`, etc., you get:
```
A603 LOAD MODULE DOES NOT EXIST
```

**Cause:** CP-V system is not properly initialized. System commands are missing from the file system.

**Solution 1 - Reinstall from tape:**

```bash
cd emulator

# Stop any running emulator
pkill sigma

# Backup existing disk images (if you want to preserve them)
cd sigma-cpv-kit/f00/f00rad
cp rad rad.backup
cp sys1 sys1.backup
cp sys2 sys2.backup

# Re-extract fresh system images
unzip -o f00rad-system.zip

# Return to emulator directory
cd ../../../

# Install CP-V from tape
./scripts/install_cpv.exp
```

**Solution 2 - Use automated deployment:**

```bash
cd emulator
./deploy_to_cpv.sh
```

The script detects corrupted installations and reinstalls automatically.

**Solution 3 - Manual installation:**

```bash
cd emulator
sigma install_cpv.ini
```

Then respond to prompts:
- `FTPI` (for media assignment)
- `RET` (for C/LL/DC ASSIGN OK)
- `RET` (for SENSE SWITCH)

Wait for "FILL ghost is initiated" message.

---

### System Boots but Hangs

**Symptom:** Emulator starts but shows no activity or prompt.

**Cause:** Waiting for operator console input.

**Solution:**

1. Press `Ctrl-E` to get simh prompt
2. Type: `show cpu` to see CPU status
3. Type: `cont` to continue

If still hung:
1. Press `Ctrl-E`
2. Type: `quit`
3. Restart with: `./deploy_to_cpv.sh`

---

### Can't Connect to Port 5001

**Symptom:**
```
telnet localhost 5001
Trying 127.0.0.1...
telnet: connect to address 127.0.0.1: Connection refused
```

**Cause:** CP-V emulator not running or MUX device not attached.

**Solution:**

Check if emulator is running:
```bash
lsof -i :5001
```

If nothing is listening:
```bash
cd emulator
./scripts/boot_cpv.exp
```

If another process is using port 5001:
```bash
# Find and kill the process
lsof -i :5001
kill <PID>

# Start CP-V
./deploy_to_cpv.sh
```

---

### Operator Console Not Responding

**Symptom:** Can't enable users or batch jobs, commands ignored.

**Cause:** Not at operator console prompt.

**Solution:**

1. Press `Ctrl-E` (not Ctrl-P as some docs say)
2. Wait for `sim>` prompt
3. Enter operator commands:
   ```
   sim> ON 107
   sim> ONB 6
   sim> SLP,I
   sim> cont
   ```

---

## Installation Issues

### "CP-V system files not found"

**Symptom:**
```
ERROR: CP-V system files not found
Please unzip sigma-cpv-kit/f00/f00rad/f00rad-system.zip first
```

**Solution:**

```bash
cd emulator/sigma-cpv-kit/f00/f00rad
unzip f00rad-system.zip
ls -lh rad sys1 sys2  # Verify files exist
cd ../../../
./deploy_to_cpv.sh
```

---

### "expect is not installed"

**Symptom:**
```
ERROR: expect is not installed
```

**Solution:**

```bash
brew install expect

# Verify installation
which expect
expect --version
```

---

### "sigma emulator not found"

**Symptom:**
```
ERROR: sigma emulator (simh) is not installed
```

**Solution:**

Install simh from source or package:

**From Homebrew** (if available):
```bash
brew install simh
```

**From Source:**
```bash
git clone https://github.com/simh/simh.git
cd simh
make sigma
sudo cp BIN/sigma /usr/local/bin/
```

Verify:
```bash
which sigma
sigma --version
```

---

## File Transfer Issues

### Transfer Hangs During File Upload

**Symptom:** Deployment script stops responding while transferring a file.

**Cause:** Buffer overflow or system overload.

**Solution:**

The script already includes delays, but you can manually adjust them:

Edit `scripts/deploy_xl.exp` and increase the delay:
```tcl
# Change from:
if {[expr $line_count % 10] == 0} {
    sleep 0.05
}

# To:
if {[expr $line_count % 10] == 0} {
    sleep 0.1
}
```

---

### File Transfer Gets Garbled Text

**Symptom:** Files on CP-V contain garbage characters.

**Cause:** Terminal encoding issues.

**Solution:**

Ensure files are pure ASCII:
```bash
cd emulator/work
file *.FOR
# Should say "ASCII text"

# If not, convert:
iconv -f UTF-8 -t ASCII//TRANSLIT filename.FOR > filename.FOR.tmp
mv filename.FOR.tmp filename.FOR
```

---

### "EDIT command failed"

**Symptom:** File transfer fails with EDIT error.

**Cause:** File already exists or disk full.

**Solution:**

Connect manually and delete existing files:
```bash
telnet localhost 5001
# Login as :SYS,LBE

$ CATALOG
$ DELETE filename.FOR
$ LOGOUT
```

Then run deployment again.

---

## Batch Job Issues

### Batch Job Won't Submit

**Symptom:**
```
$ SUBMIT XLBUILD.JOB
BATCH NOT ENABLED
```

**Cause:** Batch jobs not enabled in operator console.

**Solution:**

1. Press `Ctrl-E` in emulator console
2. Type: `ONB 6`
3. Type: `cont`
4. Submit job again

---

### Batch Job Hangs

**Symptom:** Job submitted but STATUS shows it stuck in queue.

**Cause:** Job waiting for resources or symbiont not initialized.

**Solution:**

Initialize line printer symbiont:
1. Press `Ctrl-E`
2. Type: `SLP,I`
3. Type: `cont`

Check job status:
```
$ STATUS
```

---

### Can't Find Compilation Output

**Symptom:** Job completed but can't find output.

**Solution:**

Check multiple locations:

**In CP-V:**
```
$ CATALOG           # List files
$ TYPE XLBUILD.LOG  # View job log
$ TYPE *.MAP        # View link map
```

**On Host System:**
```bash
cat emulator/work/printer.txt
```

**In Emulator Console:**
Look for messages in the simh console window.

---

## Runtime Issues

### FORTRAN Compiler Not Found

**Symptom:**
```
$ RUN FORTRAN
A603 LOAD MODULE DOES NOT EXIST
```

**Cause:** System not properly installed (see "A603 Error" above).

**Solution:** Reinstall CP-V from tape using automated deployment.

---

### Can't Run XL Spreadsheet

**Symptom:**
```
$ RUN XL
A603 LOAD MODULE DOES NOT EXIST
```

**Cause:** XL not compiled yet or compilation failed.

**Solution:**

Check if compilation succeeded:
```
$ CATALOG
```

Look for `XL` in the file list.

View compilation output:
```bash
cat emulator/work/printer.txt
```

If XL is missing, compilation failed. Check the error messages and fix source files.

---

### Terminal Display Garbled

**Symptom:** XL runs but display is corrupted or arrow keys don't work.

**Cause:** Terminal not VT-52 compatible.

**Solution:**

Use a VT-52 compatible terminal emulator:
- macOS Terminal (set emulation to VT-100 compatible)
- iTerm2
- PuTTY (set terminal type to VT-100)

Test terminal:
```
$ RUN XL
```

If display still garbled, check terminal settings:
- 80 columns × 24 rows
- VT-52 or VT-100 emulation
- Local echo OFF
- ASCII character set

---

## System Shutdown Issues

### Can't Shutdown CP-V

**Symptom:** `ZAP` command doesn't work or system doesn't shutdown.

**Solution:**

1. Press `Ctrl-E` to get simh prompt
2. Type: `ZAP` (may need to wait a minute)
3. If no response, type: `quit` to force-quit simh

**Important:** Forcing quit may corrupt disk images. Better to wait patiently for proper shutdown.

---

### "THAT'S ALL, FOLKS!!" Never Appears

**Symptom:** After `ZAP`, system doesn't show shutdown message.

**Cause:** Ghost jobs still running or system hung.

**Solution:**

Wait 2-3 minutes. If still no message:

1. Press `Ctrl-E`
2. Type: `show cpu`
3. If CPU is looping at address 6, shutdown is complete
4. Type: `quit`

---

## Disk Image Issues

### Disk Images Corrupted

**Symptom:** System boots but files are missing or commands fail randomly.

**Solution:**

Restore from fresh images:

```bash
cd emulator/sigma-cpv-kit/f00/f00rad

# Backup current (corrupted) images
mv rad rad.corrupted
mv sys1 sys1.corrupted
mv sys2 sys2.corrupted

# Extract fresh images
unzip f00rad-system.zip

# Verify extraction
ls -lh rad sys1 sys2

# Return and reinstall
cd ../../../
./deploy_to_cpv.sh
```

---

### Disk Full Errors

**Symptom:** Can't save files, get disk full errors.

**Solution:**

Delete unnecessary files:
```
$ CATALOG
$ DELETE oldfile.FOR
$ DELETE *.OBJ    # Delete object files
$ DELETE *.MAP    # Delete map files
```

The F00 system has limited disk space. Clean up regularly.

---

## Network Issues

### Port 5001 Already in Use

**Symptom:**
```
Attach failure on mux
Port already in use
```

**Solution:**

Find and kill conflicting process:
```bash
lsof -i :5001
kill <PID>
```

Or change port in boot_cpv.ini:
```ini
# Change from:
att mux 5001

# To:
att mux 5002
```

Don't forget to update deployment script too!

---

## Performance Issues

### System Running Slowly

**Symptom:** Commands take a long time to respond.

**Cause:** Emulator CPU throttle or host system load.

**Solution:**

1. Close other applications
2. Check host CPU usage: `top`
3. Increase emulator CPU speed (if simh supports it)

---

### File Transfer Very Slow

**Symptom:** Deployment takes much longer than 4-5 minutes.

**Cause:** Network delays or system overload.

**Solution:**

1. Close other applications
2. Check network: `ping localhost` should be <1ms
3. Reduce transfer delay in deploy_xl.exp (see "Transfer Hangs" above)

---

## Common Error Messages

### "BAD BILLING NUMBER IN RECORD"

**Context:** Creating user accounts with SUPER.

**Solution:** Set `B$PRIV=C0` before completing create/modify operation.

---

### "CRA03 NOT OPERATIONAL"

**Context:** After using `SCR,I` operator command.

**Solution:**

**DON'T** initialize card reader symbiont. There's no card reader in the emulator.

To stop the errors:
1. Press `Ctrl-E`
2. Type: `quit`
3. Restart emulator

---

### "GHOST JOB FAILED"

**Context:** During system boot.

**Cause:** System initialization error.

**Solution:** Reinstall from tape (see "A603 Error" section).

---

## Diagnostic Commands

### Check System Status

From CP-V:
```
$ STATUS    # Show jobs
$ CATALOG   # List files
$ DATE      # Show date/time
$ SYSTEM    # System info (if available)
```

From simh operator console (Ctrl-E):
```
sim> show cpu       # CPU configuration
sim> show mux       # MUX status
sim> show rad       # RAD status
sim> show dpb       # Disk status
sim> show mt        # Tape status
```

---

### Verify Installation

Quick verification checklist:

```bash
# 1. Check emulator is running
lsof -i :5001

# 2. Connect to CP-V
telnet localhost 5001
# Should see logon prompt

# 3. Login
:SYS,LBE

# 4. Test commands
$ CATALOG   # Should list files
$ DATE      # Should show date
$ STATUS    # Should show system status

# 5. Test FORTRAN
$ RUN FORTRAN
*
# Should show FORTRAN prompt, type Ctrl-C to exit

# 6. Logout
$ LOGOUT
```

If all steps work, system is properly installed!

---

## Recovery Procedures

### Complete Reset

If everything is broken:

```bash
cd emulator

# 1. Stop everything
pkill sigma
pkill expect
pkill telnet

# 2. Clean up
rm -rf work/printer.txt

# 3. Reset disk images
cd sigma-cpv-kit/f00/f00rad
rm rad sys1 sys2
unzip f00rad-system.zip
cd ../../../

# 4. Reinstall everything
./deploy_to_cpv.sh
```

---

### Backup/Restore

**Backup working system:**
```bash
cd emulator/sigma-cpv-kit/f00/f00rad
tar czf backup-$(date +%Y%m%d).tar.gz rad sys1 sys2
```

**Restore from backup:**
```bash
cd emulator/sigma-cpv-kit/f00/f00rad
tar xzf backup-YYYYMMDD.tar.gz
```

---

## Getting Help

### Log Files

Important log files:
- `emulator/work/printer.txt` - CP-V printer output
- `simh console output` - System messages
- Script output - Error messages from deployment

Save these when asking for help!

### Useful Information

When reporting issues, include:

1. **System info:**
   ```bash
   uname -a
   sigma --version
   expect --version
   ```

2. **Error message:** Exact text of error

3. **Steps to reproduce:** What you did before the error

4. **Log files:** printer.txt, console output, etc.

---

## Known Issues

### VT-52 Arrow Keys

**Issue:** Some terminal emulators don't send correct VT-52 arrow key codes.

**Workaround:** Use VT-100 mode if available, or try different terminal emulator.

---

### Long Filenames

**Issue:** CP-V has filename length limits.

**Workaround:** Use short filenames (8 characters + 3 char extension).

---

### Line Endings

**Issue:** Windows line endings (\r\n) may cause issues.

**Solution:**
```bash
dos2unix emulator/work/*.FOR
```

Or on macOS:
```bash
sed -i '' 's/\r$//' emulator/work/*.FOR
```

---

## Additional Resources

- **CP-V Manuals:** http://bitsavers.org/pdf/sds/sigma/cp-v/
- **simh Documentation:** http://simh.trailing-edge.com/
- **Andrews University Archive:** https://www.andrews.edu/~calkins/sigma/
- **Project Documentation:** See other .md files in emulator directory

---

**Last Updated:** 2026-01-19
