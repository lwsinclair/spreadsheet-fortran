# Transferring files to RSX-11M in SIMH

Getting files from a modern host into a PDP-11 running RSX-11M under SIMH requires bridging vastly different computing eras. **The most practical method is the RT-11 intermediate disk approach**—create an RT-11 formatted disk image on your host using PUTR, attach it to SIMH, then use RSX-11M's FLX utility to copy files across. For text files, paper tape emulation offers the simplest path; for binary files, Kermit-11 over a serial connection provides reliable transfers; and for ongoing development work, Johnny Billquist's BQTCP/IP stack enables FTP access.

## The RT-11 intermediate disk method works best for bulk transfers

This is the most commonly recommended approach in the retrocomputing community because it handles both text and binary files reliably and scales well for many files.

**Step 1: Create an RT-11 disk image on your host using PUTR**

Download PUTR from http://www.dbit.com/pub/putr/ (DOS/Windows command-line tool). While PUTR can only *read* RSX-11M Files-11 disks, it has full read/write access to RT-11 format, which RSX-11M can mount as a foreign volume.

```
C:\> putr
PUTR V2.01  Copyright (C) 1995-2001 by John Wilson.
(C:\)> format transfer.dsk /rl02 /rt11
(C:\)> mount x: transfer.dsk /rt11
(C:\)> set copy binary
(C:\)> copy c:\myfiles\*.mac x:
(C:\)> copy/b c:\myfiles\myprog.tsk x:
(C:\)> dismount x:
(C:\)> quit
```

**Step 2: Attach the disk in SIMH**

Add the transfer disk to your SIMH configuration:

```
sim> SET RL ENABLE
sim> SET RL1 RL02
sim> ATTACH RL1 transfer.dsk
sim> c
```

**Step 3: Use FLX in RSX-11M to copy files**

Mount the RT-11 disk as a foreign volume and use FLX (File Exchange Utility) with the `/RT` switch:

```
>MOU DL1:/FOR              ; Mount as foreign (RT-11) volume
>FLX SY:[200,200]/RS=DL1:*.MAC/RT    ; Copy all .MAC files
>FLX SY:[200,200]/RS=DL1:MYPROG.TSK/IM/RT   ; Binary with /IM switch
>DMO DL1:
```

The `/IM` (image mode) switch is **critical for binary files** like .TSK task images and .OBJ object files. Without it, FLX performs text conversion that corrupts binaries. After copying task images, make them contiguous: `PIP MYPROG.TSK/NV/CO=MYPROG.TSK`.

## Paper tape emulation handles small text files simply

For quick transfers of individual text files, SIMH's emulated paper tape reader provides the path of least resistance. The paper tape devices (PTR for reader, PTP for punch) accept raw 8-bit binary streams, making ASCII text files work directly.

**SIMH side:**
```
sim> ATTACH PTR myfile.mac    ; Attach host file to paper tape reader
sim> c
```

**RSX-11M side:**
```
>PIP MYFILE.MAC=PC:           ; Copy from paper tape device
```

For the reverse direction (RSX-11M to host):
```
sim> ATTACH PTP output.txt    ; Attach output file to punch
sim> c

; In RSX-11M:
>PIP PC:=MYFILE.MAC           ; Write to paper tape punch

; Back in SIMH:
sim> DETACH PTP               ; Flush and close file
```

**Important limitation**: RSX-11M's handling of paper tape loses newlines in some configurations. This method works best for single text files; it's unsuitable for binary transfers.

## Kermit-11 provides reliable serial file transfers

Kermit-11 by Brian Nelson offers the most reliable path for transferring files—especially binaries—when other methods prove problematic. It supports RSX-11M V4.1+ and later systems.

**Setting up SIMH for serial transfer:**

Enable the DZ11 terminal multiplexer and expose it on a TCP port:
```
sim> SET DZ ENABLE
sim> SET DZ LINES=8
sim> ATTACH DZ 10001          ; Listen on TCP port 10001
```

**In RSX-11M, install and run Kermit:**
```
>INS K11RSX/TASK=...KER
>KER
Kermit-11 T3.60
Kermit-11> SET ESCAPE 01
Kermit-11> RECEIVE
```

**On the host, use C-Kermit:**
```bash
$ kermit
C-Kermit> SET TELNET WAIT-FOR-NEGOTIATIONS OFF
C-Kermit> TELNET localhost 10001
# ... escape back to local Kermit (typically Ctrl-\ then C)
C-Kermit> SEND myfile.mac
```

For binary files, set binary mode on both ends: `SET FILE BINARY`. Pre-built Kermit-11 binaries (`k11rsx.tsk`) are available from http://www.columbia.edu/kermit/pdp11.html. If you need to bootstrap Kermit onto a fresh system, hex-encoded versions and dehexifier programs are provided.

## BRU tape images restore complete directory structures

BRU (Backup and Restore Utility) is RSX-11M's native backup tool, functionally similar to Unix's tar. Distribution media and DECUS tapes use this format, making it essential for restoring software collections.

**SIMH tape format**: SIMH uses a container format where each data record is bracketed by 4-byte little-endian length fields. A 100-byte record looks like:
```
64 00 00 00    ; Length prefix (100 decimal)
[100 bytes]    ; Data
64 00 00 00    ; Length suffix
```
Tape marks are four zero bytes; end-of-medium is four 0xFF bytes.

**Attaching tapes in SIMH:**
```
sim> SET TS ENABLE
sim> ATTACH TS0 rsxdist.tap           ; Standard SIMH tape format
sim> ATTACH -f TS0 TPC mytape.tpc     ; TPC format (some DECUS tapes)
```

**Restoring in RSX-11M:**
```
>MOU MS0:/FOR/DENS=1600       ; Mount tape foreign, 1600 BPI
>INS $BRU
>BRU /REW MS0: SY:            ; Restore to system disk
```

To list contents without restoring: `BRU /LIST MS0:`. Key switches include `/UFD` (create user directories automatically), `/NOINI` (don't reinitialize target), and `/BAC:name` to specify a particular saveset.

**Creating BRU tapes** on the host requires specialized tools. The simtools repository (https://github.com/simh/simtools) provides utilities like `tar2mt` for converting tar archives to SIMH tape format, though creating proper BRU-format tapes typically requires running BRU itself within RSX-11M.

## Modern host-side tools read Files-11 disks directly

Two utilities can directly access RSX-11M Files-11 ODS-1 disk images from your host:

**PUTR** (http://www.dbit.com/pub/putr/): Read-only access to Files-11, full read/write for RT-11. Essential for the intermediate disk workflow.
```
(C:\)> mount x: rsxdisk.dsk /files11 /ronly
(C:\)> dir x:[200,200]
(C:\)> copy x:[200,200]myfile.mac;1 c:\output\
```

**FSX** (https://github.com/kgober/FSX): A modern C# utility with read/write support for both Files-11 and RT-11:
```
FSX> load rsx: rsxdisk.dsk
FSX> dir rsx:[1,2]
FSX> save rsx:[1,2]MYFILE.MAC myfile.mac
```

FSX can potentially write files directly into RSX-11M disk images, though this should be done with the disk detached from SIMH to avoid filesystem corruption.

## BQTCP/IP enables network-based FTP transfers

For ongoing development work, Johnny Billquist's BQTCP/IP implementation provides a full TCP/IP stack for RSX-11M-PLUS, including FTP. This transforms your emulated PDP-11 into a networked system.

**SIMH network configuration:**
```
SET XQ ENABLE
SET XQ MAC=08-00-2B-AA-BB-CC
ATTACH XQ tap:tap0            ; Attach to host TAP interface
```

**In RSX-11M after installing BQTCP:**
```
>FTP
FTP> OPEN 192.168.1.100
FTP> USER anonymous
FTP> BINARY
FTP> GET MYFILE.TSK
FTP> QUIT
```

The BQTCP distribution is available as an RL02 disk image from ftp://ftp.dfupdate.se/pub/pdp11/rsx/tcpip/. Documentation at http://mim.stupi.net/tcpip.htm covers installation.

## RSX-11M device naming follows a consistent pattern

Understanding RSX device names is essential for file operations:

| SIMH Device | RSX Device | Description |
|-------------|------------|-------------|
| RQ0-RQ3 | DU0:-DU3: | MSCP disks (RA/RD series) |
| RL0-RL3 | DL0:-DL3: | RL01/RL02 cartridge disks |
| TS0 | MS0: | TS11 tape drive |
| TM0 | MT0: | TM11 tape drive |
| PTR | PC: | Paper tape reader |
| PTP | PP: | Paper tape punch |
| DZ0-DZ7 | TT0:-TT7: | Terminal lines |

## Choosing the right method depends on your use case

| Scenario | Recommended Method |
|----------|-------------------|
| Bulk source code files | RT-11 intermediate disk with FLX |
| Single text file, one-time | Paper tape (PTR/PTP) |
| Binary executables | Kermit-11 in binary mode |
| Software distribution restore | BRU tape images |
| Regular development workflow | BQTCP/IP with FTP |
| Extracting files from RSX disk | PUTR or FSX on host |

## Practical Experience Notes (2026-01-27)

### What We Tried

**fsio tool for RT-11 disk creation** - Does NOT work properly:
```bash
# This creates files that FLX rejects
fsio -rl02 disk.rl02
fsio write -disk disk.rl02 -file localfile.txt -rt11 DSTFILE.TXT
```

Result in RSX-11M:
```
>FLX SY:[1,24]/RS=DL2:DSTFILE.TXT/RT
FLX -- FMTD ASCII RECORD FORMAT BAD
```

The fsio tool doesn't create proper RT-11 ASCII record format. **PUTR is required** for RT-11 disk creation that FLX can read.

### Confirmed Working Methods

1. **Paper tape** - Works for individual text files
   ```
   sim> ATTACH PTR /path/to/source.for
   >PIP SOURCE.FTN=PC:
   ```

2. **PUTR + FLX** - Recommended but requires Windows/DOS or Wine

3. **Direct Files-11 manipulation** - FSX can write to RSX disks when detached from SIMH

### RSX-11M BL26 Device Limitations

The baseline RSX-11M V3.2 BL26 system only has **2 RL drives** configured (DL0:, DL1:). To use DL2: or DL3:, would need to run SYSGEN to add more drivers.

## Conclusion

The RT-11 intermediate disk method using PUTR and FLX represents the most versatile approach, handling both text and binary files while supporting bulk transfers. For one-off text files, paper tape emulation requires the least setup. When transferring binaries—particularly task images—Kermit-11's error-correcting protocol prevents the corruption that plagues simpler methods. The critical detail across all methods is proper binary mode handling: use `/IM` with FLX, `SET FILE BINARY` with Kermit, and BINARY mode with FTP. Task files must also be made contiguous (`PIP file/CO`) before they'll execute. For the most seamless experience, investing time in BQTCP/IP setup pays dividends through standard FTP access to your emulated RSX-11M system.