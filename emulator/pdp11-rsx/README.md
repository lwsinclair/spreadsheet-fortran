# PDP-11/RSX-11M Emulator for XL Spreadsheet Testing

This directory contains the SIMH PDP-11 emulator configuration for testing the XL Spreadsheet port to RSX-11M.

## Quick Start

### 1. Download RSX-11M disk images

```bash
./setup.sh
```

This downloads RSX-11M V3.2 and FORTRAN IV compiler from bitsavers.org.

### 2. Boot the emulator

```bash
./boot_rsx.sh
```

### 3. Connect to console

In another terminal:

```bash
telnet localhost 10070
```

### 4. Boot RSX-11M

At the `sim>` prompt in the console:

```
BOOT RL0
```

You should see RSX-11M boot messages.

## Files

- `pdp11.ini` - SIMH configuration for PDP-11/70
- `setup.sh` - Download and prepare disk images
- `boot_rsx.sh` - Boot the emulator
- `media/` - Disk images directory
  - `RSX-11M_V3.2_RSX11MBL26_3.2.rl01` - Base RSX-11M system
  - `F4_IAS_RSX_2.1.rl01` - FORTRAN IV compiler
  - `xl_work.rl02` - Work disk for XL source and build

## Configuration

### Hardware

- CPU: PDP-11/70
- Memory: 1MB
- FPP: Enabled (required for FORTRAN)
- Disks:
  - RL0: RSX-11M system disk (RL01, 5MB)
  - RL2: XL work disk (RL02, 10MB)
- Terminals:
  - Console: telnet port 10070
  - DZ11 lines: telnet ports 10071-10078

### Disk Layout

- DL0: (RL0) RSX-11M system disk - READ ONLY
- DL2: (RL2) XL work disk - READ/WRITE

## Testing XL Spreadsheet

### Transfer Source Files

1. Connect to RSX-11M console
2. Initialize work disk (first time only):
   ```
   $ INI DL2:
   $ MOU DL2:
   ```
3. Transfer files from host system (see transfer guide)

### Build XL

```
$ SET DEFAULT [USER.XL]
$ @BUILD
$ @XLBUILD
```

### Run XL

```
$ RUN XL
```

## References

- [PDP Nostalgia - RSX-11M](../../docs/PDP%20Nostalgia%20-%20RSX-11M.html)
- [SIMH Documentation](http://simh.trailing-edge.com/)
- [Bitsavers RSX-11M](http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/)

## Troubleshooting

### Cannot download disk images

Manually download from:
http://www.bitsavers.org/bits/DEC/pdp11/discimages/rl01/rsx11m_3.2/

Required files:
- RSX-11M_V3.2_RSX11MBL26_3.2.DSK.gz
- F4_IAS_RSX_2.1.DSK.gz

Place in `media/` directory and run `setup.sh` again.

### Emulator won't start

Check that SIMH is installed:
```bash
which pdp11
```

Install if needed:
```bash
brew install open-simh
```

### Console connection refused

Wait a few seconds after starting emulator for telnet port to open.

### RSX-11M won't boot

Check that disk image is attached correctly in `pdp11.ini`.
Try running SIMH manually and checking for errors.
