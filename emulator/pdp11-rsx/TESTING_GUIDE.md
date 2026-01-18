# Testing XL Spreadsheet on RSX-11M

This guide walks you through testing the XL Spreadsheet port on the SIMH PDP-11 emulator running RSX-11M.

## Prerequisites

- RSX-11M disk images downloaded (run `./setup.sh` if not done)
- SIMH PDP-11 emulator installed (`brew install open-simh`)
- XL Spreadsheet source code in `../../pdp11/src/`

## Step 1: Boot RSX-11M

### Start the emulator

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator/pdp11-rsx
./boot_rsx.sh
```

Or manually:
```bash
pdp11 pdp11.ini
```

You should see:
```
PDP-11 simulator V3.12-3
Logging to file "console.log"
...
Listening on port 10070 (socket 8)
...
To boot RSX-11M, type: BOOT RL0
```

### Connect to console

Open another terminal and connect:

```bash
telnet localhost 10070
```

### Boot RSX-11M

At the `sim>` prompt in the telnet console:

```
BOOT RL0
```

RSX-11M will boot and you should see:

```
RSX-11M V3.2 BL26

>RED DL0:=SY:
>RED DL0:=LB:
>MOU DL0:
>@DL0:[1,2]STARTUP
...
```

Eventually you'll see a login prompt or system prompt.

## Step 2: Initialize Work Disk

If this is the first time, initialize the XL work disk:

### At the RSX-11M prompt (>)

```
>INI DL2:
>MOU DL2:XLWORK
```

This creates a Files-11 filesystem on the work disk.

## Step 3: Transfer Source Files

There are several methods to transfer files:

### Method 1: Copy files into disk image (Offline)

1. Stop the emulator (Ctrl-E, then QUIT)
2. Use PUTR or RT11DSK utility to copy files
3. Restart emulator

### Method 2: Type files manually (Small files)

For the build scripts which are small:

```
>PIP DL2:[200,200]BUILD.CMD;0=/CR/NM
; Type the BUILD.CMD file contents
; End with Ctrl-Z
```

### Method 3: Create minimal test first

Let's start with a minimal test to verify FORTRAN works:

```
>PIP DL2:[200,200]TEST.FOR;0=/CR/NM
      PROGRAM TEST
      WRITE(6,100)
 100  FORMAT(' HELLO FROM RSX-11M')
      STOP
      END
^Z

>ASS DL1:FOR
>FOR DL2:TEST=DL2:TEST.FOR
>TKB
TKB>DL2:TEST/-SP=DL2:TEST/LB:[1,1]F77FCS
TKB>//
>RUN DL2:TEST
 HELLO FROM RSX-11M
```

If this works, FORTRAN IV is working correctly!

## Step 4: Build XL Spreadsheet

### Copy all source files to DL2:[200,200]

You'll need to transfer:

**From ../../pdp11/src/:**
- layer0/STRUTIL.FOR
- layer1/CELLS.FOR, DEPS.FOR, PARSE.FOR, EVAL.FOR, RECALC.FOR
- layer2/UI.FOR, DISPLAY.FOR, MSG.FOR
- layer3/TERMRSX.FOR
- XLMAIN.FOR

**From ../../pdp11/:**
- BUILD.CMD
- XLBUILD.CMD

### Run the build

```
>SET DEFAULT DL2:[200,200]
>@BUILD
```

This compiles all modules.

### Link the executable

```
>@XLBUILD
```

This creates XL.TSK.

## Step 5: Run XL Spreadsheet

```
>RUN XL
```

You should see the XL Spreadsheet grid appear!

### Test the spreadsheet

1. Use arrow keys to move cursor
2. Type a number: `10` then RETURN
3. Move to another cell
4. Type a formula: `=A1*2` then RETURN
5. Verify it calculates correctly
6. Type `/QUIT` to exit

## Troubleshooting

### "FORTRAN compiler not found"

The FORTRAN disk isn't mounted. Mount it:

```
>MOU DL1:FOR
```

### "File not found" errors during build

Make sure all source files are in DL2:[200,200] directory.

Check with:
```
>DIR DL2:[200,200]*.FOR
```

### "Task too large"

The executable exceeds available memory. Try:
1. Reduce MAXCEL in CELLS.FOR
2. Use CPM configuration instead of FULL
3. Rebuild

### Terminal doesn't respond

Make sure terminal is in VT-52 or VT-100 mode.

### Escape sequences visible as text

Terminal not interpreting escape codes. Check terminal emulation settings.

## Alternative: Use Pre-built Test Image

If file transfer is too difficult, I can create a pre-configured disk image with XL already built. Let me know if you need this.

## Next Steps

Once XL is running on RSX-11M:

1. Run the acceptance tests from docs/PDP11_PORT_PLAN.md
2. Compare performance with CP-V version
3. Document any issues or differences
4. Update COMPARISON.md with actual results

## References

- RSX-11M documentation: /docs/PDP Nostalgia - RSX-11M.html
- XL Installation: ../../pdp11/INSTALL.md
- Build notes: ../../pdp11/BUILD_NOTES.md
