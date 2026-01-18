# CP-V Deployment - Quick Start Guide

## One-Command Deployment

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
./deploy_to_cpv.sh
```

That's it! The script will:
- ✅ Check prerequisites
- ✅ Clean up any stale processes
- ✅ Install or boot CP-V automatically
- ✅ Configure the operator console
- ✅ Transfer all files
- ✅ Submit the batch job

## Prerequisites

Install expect (one-time):
```bash
brew install expect
```

## Common Commands

### Clean Shutdown
```bash
./scripts/shutdown_cpv.exp
```

### Force Kill (if stuck)
```bash
pkill -9 -f sigma
```

### Manual Testing

Test individual components:
```bash
# Test installation (first time)
./scripts/install_cpv.exp

# Test boot (subsequent times)
./scripts/boot_cpv.exp

# Test file deployment only
./scripts/deploy_xl.exp
```

### Check if CP-V is Running
```bash
lsof -i :5001
```

### Connect Manually
```bash
telnet localhost 5001
# Login: :SYS,LBE
```

## What's New

All scripts have been fixed for reliable automation:

1. **install_cpv.exp** - Fully automated installation from tape
   - Timeout: 180 seconds
   - Proper prompt detection
   - Operator console configuration
   - Exits cleanly for automation

2. **boot_cpv.exp** - Fully automated boot from disk
   - Fast boot from existing installation
   - Operator console setup
   - Terminal auto-configuration
   - Exits cleanly for automation

3. **deploy_to_cpv.sh** - Smart deployment wrapper
   - Detects if installation or boot needed
   - Cleans up stale processes
   - Verifies CP-V is actually running
   - Clear error messages

4. **shutdown_cpv.exp** - Clean shutdown automation
   - Graceful process termination
   - Fallback to force kill if needed

## Troubleshooting

### "Timeout waiting for prompt"
- Installation/boot took longer than 180 seconds
- Increase timeout in the .exp file
- Check disk images are not corrupted

### "Port 5001 already in use"
- Another process is using the port
- Run shutdown script or kill sigma processes
- Wait a few seconds and retry

### "Installation script not found"
- You're not in the emulator directory
- Run: `cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator`

### Scripts hang or don't respond
- Kill all sigma processes: `pkill -9 -f sigma`
- Wait 5 seconds
- Try again

## Directory Structure

```
emulator/
├── deploy_to_cpv.sh       ← Main deployment script
├── scripts/
│   ├── install_cpv.exp    ← Installation automation
│   ├── boot_cpv.exp       ← Boot automation
│   ├── deploy_xl.exp      ← File transfer automation
│   └── shutdown_cpv.exp   ← Shutdown automation
└── work/
    ├── *.FOR (11 files)   ← Fortran source files
    └── XLBUILD.JOB        ← Batch job control file
```

## Testing Checklist

Before considering deployment successful:

- [ ] CP-V starts without errors
- [ ] Port 5001 is listening
- [ ] Can connect via telnet
- [ ] Can login with :SYS,LBE
- [ ] All 12 files transferred
- [ ] Batch job submitted
- [ ] Can shutdown cleanly

## Next Steps After Deployment

1. Check batch job status:
   ```
   telnet localhost 5001
   :SYS,LBE
   STATUS
   ```

2. View compilation output:
   ```
   cat work/printer.txt
   ```

3. Run the spreadsheet (after successful compilation):
   ```
   RUN XL
   ```

## For More Information

- Full details: `temp/DEPLOYMENT_FIXES.md`
- Completion summary: `temp/COMPLETION_SUMMARY.md`
- Test scripts: `temp/test_deployment.sh`

---

**Need Help?** Check the troubleshooting section above or examine error messages - they now include helpful hints about what went wrong.
