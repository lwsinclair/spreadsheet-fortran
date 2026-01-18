# CP-V Manual Installation - Simple 5-Minute Guide

This is a **one-time** installation. After this, deployment is automatic!

## Step 1: Start Installation

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
sigma install_cpv.ini
```

Wait for the menu:
```
ENTER ANY OF:
I=TTY I/O
P=LP OUTPUT
F=TAPE FILES
S=:SYS FILES
T=TAPE PATCHES
C=CARD PATCHES
D=XDELTA
```

**Type:** `F` and press RETURN

## Step 2: Complete Installation

You'll see prompts. Just press **RETURN** for each (accept defaults):

1. First prompt → Press **RETURN**
2. Second prompt → Press **RETURN**

The installation will run for 2-3 minutes. You'll see lots of output.

## Step 3: Wait for "FILL GHOST"

When you see:
```
7:(FILL)   FILL GHOST INITIATED - USE 'INT,FILL.' FOR COMMANDS
```

**Press Ctrl-E** to get to simulator console.

## Step 4: Configure Operator Console

At the `sim>` prompt, type:

```
ON 107
ONB 6
SLP,I
cont
```

## Step 5: Installation Complete!

CP-V is now installed to the RAD and disk packs.

Press **Ctrl-E** again, then:
```
ZAP
```

Wait for "THAT'S ALL, FOLKS!!" then:
```
quit
```

## Step 6: Deploy XL Spreadsheet

Now run the automated deployment:

```bash
./deploy_to_cpv.sh
```

This will boot CP-V (fast now) and deploy XL automatically!

---

**Total time:** 5 minutes for installation (one-time), then 4 minutes for deployment
