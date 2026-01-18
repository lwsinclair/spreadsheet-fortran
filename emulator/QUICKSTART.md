# CP-V Emulator Quick Start

## System Configuration

- **CPU**: Xerox Sigma 7 (512KB RAM)
- **OS**: CP-V F00 (Honeywell Release F00, September 28, 1978)
- **Configuration**: RAD swapper with file system disks

## Starting CP-V

```bash
cd emulator
sigma boot_cpv.ini
```

This will:
1. Start Sigma 7 emulator (512KB RAM)
2. Boot CP-V F00 from RAD (RAM disk)
3. Start MUX terminal controller on port 5001

**Note**: On first boot from the tape, you'll need to answer installation prompts. Subsequent boots will be faster.

## Operator Console

The emulator console window shows the CP-V **Operator Console (OC)** interface.

To activate Operator Console commands:
- Press `Ctrl-E` in the simh console window
- At the `sim>` prompt, type your command
- Type `cont` to continue execution

### Initial System Setup

After booting, enable time-share logons:
```
sim> Ctrl-E
sim> ON 107
sim> cont
```

This allows up to 107 online users (121 max - 14 ghost jobs).

## Connecting to Console

In another terminal:
```bash
telnet localhost 5001
```

The F00 system is configured with non-hardwired terminals and will present the logon salutation when you connect:

```
HI, CP-V HERE - F00
[TIME] [DATE]  USER# [X]  LINE# [Y]
LOGON PLEASE:
```

## Logging In

The F00 system creates a default user account without a password:

```
LOGON PLEASE: :SYS,LBE
```

No password required on first logon.

## Compiling FORTRAN on CP-V

CP-V F00 includes the FORTRAN IV compiler.

To compile a FORTRAN program:
```
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*LINK
*GO
```

To list available programs:
```
$ CATALOG
```

## Transferring Files to CP-V

Use the EDIT command to create files via copy/paste:

```
$ EDIT STRUTIL.FOR
[Paste file contents from emulator/work/STRUTIL.FOR]
:FILE
:QUIT
```

Repeat for each source file. See MANUAL_DEPLOYMENT.md for complete instructions.

## Running XL Spreadsheet

Once XL is compiled and linked:
```
$ RUN XL
```

## Batch Jobs

Enable batch processing (if needed):
```
sim> Ctrl-E
sim> ONB 6
sim> cont
```

This allows up to 6 batch jobs.

## Printer Output

CP-V sends printer output to the LP device through the Symbiont facility.

Enable the LP symbiont if needed:
```
sim> Ctrl-E
sim> SLP,I
sim> cont
```

Printer output is written to `work/printer.txt`.

## Shutting Down

### From User Console
```
$ LOGOUT
```

### From Operator Console

Shut down CP-V gracefully:
```
sim> Ctrl-E
sim> ZAP
```

CP-V will respond with:
```
                 THAT'S ALL, FOLKS!!
```

Wait for this message before quitting simh (may take up to a minute).

### Quit simh
```
sim> quit
```

## Troubleshooting

### Emulator won't start
- Check CP-V kit is downloaded: `emulator/sigma-cpv-kit/f00/f00rad/`
- Verify system files exist: `rad`, `sys1`, `sys2`
- Run `emulator/scripts/setup_emulator.sh` if not already done

### Can't connect to console
- Check telnet port 5001 is available: `lsof -i :5001`
- Verify firewall allows localhost connections
- Make sure CP-V booted successfully (check emulator console)

### FORTRAN compiler not found
- CP-V F00 includes FORTRAN IV compiler
- Run `$ CATALOG` to list available programs
- Check you're logged in as `:SYS,LBE`

### System hangs on boot
- Press `Ctrl-E` to get simh prompt
- Type `show cpu` to see CPU status
- Type `quit` and try again
- Check that system files are not corrupted

### Line printer symbiont errors
**DO NOT** initialize the card reader symbiont (SCR,I). There is no simh card reader simulator and this will cause repeated error messages.

## Advanced Operations

### Creating User Accounts

Use the SUPER utility to create and manage user accounts:
```
$ RUN SUPER
```

**Note**: SUPER requires the subject account to have C0 privilege for batch mode. Set `B$PRIV=C0` before completing the create or modify operation.

### Rebooting from RAD

After initial installation, you can boot directly from the RAD swapper:
```
sigma boot_cpv.ini
```

The boot script is already configured to boot from RAD.

## References

- **CP-V Documentation**: https://www.andrews.edu/~calkins/sigma/
- **simh Guide**: http://simh.trailing-edge.com/
- **sigma-cpv-kit**: https://github.com/kenrector/sigma-cpv-kit
- **CP-V Manuals**: http://bitsavers.org/pdf/sds/sigma/cp-v/

## File Locations

- Boot script: `emulator/boot_cpv.ini`
- System files: `emulator/sigma-cpv-kit/f00/f00rad/`
- Work directory: `emulator/work/`
- Transfer tape: `emulator/work/transfer.tap`
- Printer output: `emulator/work/printer.txt`
