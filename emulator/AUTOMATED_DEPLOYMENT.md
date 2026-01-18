# XL Spreadsheet - Automated Deployment to CP-V

**One-command deployment solution for XL Spreadsheet on CP-V F00**

---

## Quick Start

From the `emulator` directory:

```bash
./deploy_to_cpv.sh
```

That's it! The script will:
1. Check prerequisites
2. Install CP-V from tape (first time only)
3. Boot CP-V from RAD (subsequent runs)
4. Transfer all 12 files automatically
5. Submit the batch build job
6. Leave you at a telnet session ready to run XL

---

## What the Automation Does

### Script Components

1. **deploy_to_cpv.sh** - Main orchestration script
   - Checks prerequisites (expect, sigma emulator, files)
   - Detects if CP-V needs installation or just boot
   - Manages the entire deployment workflow
   - Provides clear progress feedback

2. **scripts/install_cpv.exp** - First-time installation
   - Boots from PO tape (f00rad.tap)
   - Responds to installation prompts automatically
   - Configures operator console (ON 107, ONB 6, SLP,I)
   - Installs CP-V to RAD and disk packs

3. **scripts/boot_cpv.exp** - Quick boot for subsequent runs
   - Boots from RAD swapper
   - Auto-configures operator console
   - Sets up terminal lines, batch jobs, printer

4. **scripts/deploy_xl.exp** - File transfer and job submission
   - Connects via telnet to localhost:5001
   - Logs in as :SYS,LBE
   - Transfers all 12 files using EDIT command
   - Submits XLBUILD.JOB batch job
   - Verifies catalog and job submission

---

## Prerequisites

### Required Software

1. **simh Sigma emulator**
   ```bash
   # Install from source or package manager
   # See: http://simh.trailing-edge.com/
   ```

2. **expect** (for automation scripts)
   ```bash
   brew install expect
   ```

3. **telnet client** (usually pre-installed on macOS)

### Required Files

The deployment script checks for:

- CP-V system files (in `sigma-cpv-kit/f00/f00rad/`):
  - `rad` (RAD swapper disk image)
  - `sys1` (file system disk 1)
  - `sys2` (file system disk 2)
  - `f00rad.tap` (installation tape)

- XL Spreadsheet files (in `work/`):
  - 11 FORTRAN source files (.FOR)
  - 1 batch job control file (.JOB)

If any are missing, the script will report the issue and exit.

---

## First Time Installation

### What Happens

On the first run, the script detects that CP-V has not been installed yet and:

1. Starts the emulator with `install_cpv.ini` configuration
2. Boots from the PO tape (`f00rad.tap`)
3. Responds to installation prompts:
   - Media assignment: `FTPI`
   - C/LL/DC ASSIGN OK: `RET`
   - SENSE SWITCH settings: `RET`
4. Waits for "FILL ghost is initiated" message
5. Configures operator console:
   - `ON 107` - Enable 107 online users
   - `ONB 6` - Enable 6 batch jobs
   - `SLP,I` - Initialize line printer symbiont
6. System is ready for file deployment

**Time:** 2-5 minutes (one-time setup)

### Console Interaction

The installation is fully automated, but you'll see the installation progress in the console:

```
=== CP-V F00 Installation Script ===

MEDIA ASSIGNMENT
> Responding: FTPI

C/LL/DC ASSIGN OK
> Responding: RET

SENSE SWITCH
> Responding: RET

[Installing patches and system files...]

FILL
=== CP-V F00 Installation Complete ===
=== CP-V F00 is now ready for use ===
```

---

## Subsequent Boots

After the initial installation, the script detects the existing installation and:

1. Boots directly from RAD swapper (fast!)
2. Auto-configures operator console
3. Ready for deployment in seconds

**Time:** 10-20 seconds

---

## File Deployment Process

Once CP-V is running, the deployment script:

1. **Connects via telnet** to localhost:5001
2. **Logs in** as :SYS,LBE (no password)
3. **Transfers each file** using the EDIT command:
   ```
   $ EDIT STRUTIL.FOR
   [pastes file content line by line]
   :FILE
   :QUIT
   ```
4. **Repeats** for all 12 files (11 .FOR + 1 .JOB)
5. **Verifies** with CATALOG command
6. **Submits** batch job: `SUBMIT XLBUILD.JOB`
7. **Leaves session open** for manual interaction

**Time:** 3-5 minutes for 12 files

### Progress Feedback

You'll see progress as files transfer:

```
--- Transferring STRUTIL.FOR ---
Successfully transferred STRUTIL.FOR (334 lines)
Progress: 1 / 12 files transferred

--- Transferring CELLS.FOR ---
Successfully transferred CELLS.FOR (519 lines)
Progress: 2 / 12 files transferred

...

=== All files transferred successfully ===

--- Submitting batch job XLBUILD.JOB ---
Batch job submitted successfully
```

---

## After Deployment

When deployment completes, you're left at an active telnet session connected to CP-V.

### Check Batch Job Status

```
$ STATUS
```

This shows running and completed batch jobs.

### View Compilation Output

Option 1 - From CP-V console:
```
$ TYPE XLBUILD.LOG
```

Option 2 - From host system:
```bash
cat work/printer.txt
```

### Run XL Spreadsheet

Once compilation succeeds:
```
$ RUN XL
```

You should see the full-screen spreadsheet interface!

### Exit Telnet Session

```
Press Ctrl-] then type "quit"
```

Or just close the terminal window.

---

## Manual Control Points

### If CP-V is Already Running

If port 5001 is already in use, the script asks:

```
⚠  CP-V appears to be already running on port 5001

Do you want to proceed with deployment? (y/n)
```

- `y` - Skip boot, proceed directly to file transfer
- `n` - Cancel deployment

### Shutting Down CP-V

The emulator continues running after deployment. To shut down:

1. In the **emulator console window**, press `Ctrl-E`
2. At the `sim>` prompt, type: `ZAP`
3. Wait for: `THAT'S ALL, FOLKS!!` (may take up to a minute)
4. Type: `quit`

**Important:** Always wait for the shutdown message before quitting!

---

## Troubleshooting

### "expect is not installed"

```bash
brew install expect
```

### "sigma emulator not found"

Install simh from: http://simh.trailing-edge.com/

### "CP-V system files not found"

Extract the installation files:
```bash
cd emulator/sigma-cpv-kit/f00/f00rad
unzip f00rad-system.zip
```

### "Expected 12 files, found X"

Check the work directory:
```bash
ls -l emulator/work/
```

Ensure you have:
- STRUTIL.FOR, CELLS.FOR, DEPS.FOR, PARSE.FOR, EVAL.FOR, RECALC.FOR
- UI.FOR, DISPLAY.FOR, MSG.FOR, TERMCPV.FOR, XLMAIN.FOR
- XLBUILD.JOB

### "Timeout waiting for logon prompt"

CP-V may not have started properly. Check:
1. Is the emulator console window open?
2. Does it show error messages?
3. Try killing any existing sigma processes:
   ```bash
   pkill sigma
   ```
4. Run the script again

### File Transfer Fails

If a file transfer fails partway through:
1. Press `Ctrl-C` to abort the script
2. Connect manually: `telnet localhost 5001`
3. Log in: `:SYS,LBE`
4. Check what files exist: `CATALOG`
5. Delete partial files: `DELETE filename`
6. Run the script again

### Batch Job Fails

Check the compilation output:
```bash
cat work/printer.txt
```

Look for FORTRAN compiler errors. Common issues:
- Syntax errors in source files
- Missing dependencies
- Incorrect build order in XLBUILD.JOB

---

## Advanced Usage

### Running Just the Boot Script

To boot CP-V without deploying files:

```bash
cd emulator
scripts/boot_cpv.exp
```

This boots CP-V with full operator console configuration.

### Running Just the Deployment Script

If CP-V is already running:

```bash
cd emulator
scripts/deploy_xl.exp
```

This transfers files and submits the batch job.

### Manual Installation

For complete control over the installation process:

```bash
cd emulator
sigma install_cpv.ini
```

Then respond to prompts manually:
- Media assignment: `FTPI`
- C/LL/DC ASSIGN OK: `RET`
- SENSE SWITCH: `RET`

After installation, press `Ctrl-E` and configure:
```
sim> ON 107
sim> ONB 6
sim> SLP,I
sim> cont
```

---

## File Locations

```
emulator/
├── deploy_to_cpv.sh          # Main deployment script
├── install_cpv.ini            # First-time installation config
├── boot_cpv.ini               # Quick boot config (existing system)
├── scripts/
│   ├── install_cpv.exp        # Installation automation
│   ├── boot_cpv.exp           # Boot automation
│   └── deploy_xl.exp          # File transfer automation
├── work/
│   ├── *.FOR                  # FORTRAN source files (11)
│   ├── XLBUILD.JOB            # Batch job control
│   ├── printer.txt            # CP-V printer output
│   └── transfer.tap           # Tape device (if needed)
└── sigma-cpv-kit/
    └── f00/f00rad/
        ├── f00rad.tap         # Installation tape
        ├── rad                # RAD swapper disk
        ├── sys1               # File system disk 1
        └── sys2               # File system disk 2
```

---

## Architecture

### Why This Approach?

The automated deployment solves several challenges:

1. **Complex Boot Sequence** - CP-V requires specific responses during installation
2. **Multiple Files** - Transferring 12 files manually is tedious and error-prone
3. **Operator Console Setup** - Several commands needed before users can log in
4. **Batch Job Submission** - Requires correct syntax and timing
5. **Historical Accuracy** - Mimics how deployment would have been automated in 1978

### 1978 Context

In 1978, system administrators would have:
- Created shell scripts for repetitive tasks
- Used expect-like tools for interactive automation
- Written batch jobs for compilation
- Set up operator console procedures

This deployment system recreates that authentic workflow!

---

## Error Handling

The scripts include comprehensive error handling:

- **Timeouts** - If expected prompts don't appear, script reports and exits
- **File verification** - Checks that all required files exist before starting
- **Port conflicts** - Detects if CP-V is already running
- **Installation detection** - Automatically chooses install vs. boot
- **Progress tracking** - Shows what's happening at each step
- **Graceful failures** - Reports clear error messages and recovery steps

---

## Performance

### Timing Breakdown

**First-time installation:**
- CP-V installation from tape: 2-3 minutes
- Operator console setup: 10 seconds
- File transfer (12 files): 3-4 minutes
- Batch job submission: 5 seconds
- **Total: ~6-8 minutes**

**Subsequent deployments:**
- CP-V boot from RAD: 10-15 seconds
- Operator console setup: 5 seconds
- File transfer (12 files): 3-4 minutes
- Batch job submission: 5 seconds
- **Total: ~4-5 minutes**

**Just file updates** (CP-V already running):
- File transfer only: 3-4 minutes

### Optimization

The deployment script includes:
- Small delays during file transfer to avoid overwhelming CP-V
- Automatic detection to skip unnecessary steps
- Parallel operations where possible
- Progress feedback to reduce perceived wait time

---

## Security Notes

### Default Credentials

The F00 system creates a default account:
- **Username:** :SYS,LBE
- **Password:** (none)

This is acceptable for a local emulator but would need proper security for a shared system.

### Network Access

The emulator binds to:
- **localhost:5001** - MUX terminal controller

This is only accessible from the local machine. To allow remote access, modify `boot_cpv.ini`:

```ini
# Change from:
att mux 5001

# To (listen on all interfaces):
att mux 0.0.0.0:5001
```

**Warning:** Only do this in a trusted network!

---

## Comparison with Manual Deployment

### Manual Method (MANUAL_DEPLOYMENT.md)

**Pros:**
- Full control over each step
- Good for learning CP-V
- Easy to debug individual files

**Cons:**
- Requires 30-45 minutes of manual work
- Easy to make typing errors
- Tedious for repeated deployments
- Must remember operator console commands

### Automated Method (This Document)

**Pros:**
- One command deploys everything
- Consistent results every time
- Fast (4-8 minutes total)
- No manual typing errors
- Handles operator console automatically

**Cons:**
- Less visibility into process
- Requires expect to be installed
- More complex if troubleshooting needed

---

## Future Enhancements

Possible improvements to the automation:

1. **Incremental updates** - Only transfer changed files
2. **Compilation verification** - Parse compiler output for errors
3. **Automatic testing** - Run XL and execute test cases
4. **Snapshot management** - Save/restore CP-V disk images
5. **Multi-project support** - Deploy different projects
6. **Interactive mode** - Pause at key points for user input

---

## Support Resources

- **CP-V Documentation:** https://www.andrews.edu/~calkins/sigma/
- **simh Guide:** http://simh.trailing-edge.com/
- **CP-V Manuals:** http://bitsavers.org/pdf/sds/sigma/cp-v/
- **Project Documentation:** See other .md files in this directory

---

## Credits

**CP-V F00 System:**
- Honeywell Release F00 (September 28, 1978)
- sigma-cpv-kit by Ken Rector
- Andrews University Computer Center archives

**Automation Scripts:**
- Built for XL Spreadsheet deployment
- Using expect for interactive automation
- Following 1978-era automation practices

---

**Last Updated:** 2026-01-19
**Status:** Ready for production use

**One command. Full deployment. 1978 style.**
