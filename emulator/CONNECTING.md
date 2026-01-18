# Connecting to CP-V Emulator - macOS Guide

## You Don't Need Telnet!

macOS no longer includes telnet by default, but you have better options.

---

## Quick Start: Using nc (netcat)

**You already have this!**

```bash
nc localhost 5001
```

That's it! `nc` (netcat) is built into macOS and works perfectly for CP-V.

---

## VT-52 Terminal Emulation

The CP-V emulator expects VT-52 escape sequences. Here's how to set it up:

### Option 1: macOS Terminal (Built-in)

**macOS Terminal supports VT-100 which is backward-compatible with VT-52.**

**Before connecting:**
```bash
export TERM=vt100
nc localhost 5001
```

**Or in one command:**
```bash
TERM=vt100 nc localhost 5001
```

### Option 2: Install telnet (If You Really Want It)

**Via Homebrew:**
```bash
brew install telnet
telnet localhost 5001
```

But `nc` is simpler and already installed!

---

## Full Connection Procedure

### Step 1: Set Terminal Type

```bash
export TERM=vt100
```

Add to `~/.zshrc` or `~/.bash_profile` to make permanent:
```bash
echo 'export TERM=vt100' >> ~/.zshrc
```

### Step 2: Connect to CP-V

```bash
nc localhost 5001
```

**You should see:**
```
Connected to the XDS Sigma simulator MUX device, line 1

HI, CP-V HERE - F00
[TIME] [DATE]  USER# [X]  LINE# [Y]
LOGON PLEASE:
```

### Step 3: Login

Type:
```
:SYS,LBE
```

Press RETURN.

**You should see:**
```
$
```

This is the CP-V command prompt. You're in!

---

## Testing VT-52 Escape Sequences

After logging in, test that VT-52 codes work:

```bash
$ PRINT "$(printf '\033')H"
```

This should move cursor to home position. If it works, VT-52 is functioning!

---

## Terminal Settings for XL Spreadsheet

**Required Terminal Settings:**

| Setting | Value | Why |
|---------|-------|-----|
| Terminal Type | `vt100` or `vt52` | Escape sequence support |
| Character Set | ASCII 7-bit | CP-V uses 7-bit |
| Lines | 24 minimum | XL uses 24-line display |
| Columns | 80 minimum | XL uses 80-column display |
| Local Echo | Off | CP-V echoes characters |

**In macOS Terminal:**
- Terminal → Preferences → Profiles → Text
- Font: Monaco 10pt or similar monospace
- Terminal → Preferences → Profiles → Window
- Window Size: 80 columns × 24 rows (or larger)

---

## Arrow Keys Configuration

XL Spreadsheet uses arrow keys for navigation. These send VT-52 escape sequences:

| Key | VT-52 Sequence | Hex |
|-----|----------------|-----|
| Up | ESC A | 1B 41 |
| Down | ESC B | 1B 42 |
| Right | ESC C | 1B 43 |
| Left | ESC D | 1B 44 |

**macOS Terminal handles this automatically when TERM=vt100.**

### If Arrow Keys Don't Work

Try this in terminal before connecting:
```bash
stty -icanon -echo
export TERM=vt100
nc localhost 5001
```

If still not working, try setting TERM=ansi or TERM=xterm.

---

## Complete Connection Script

Create `connect_cpv.sh`:

```bash
#!/bin/bash
# Connect to CP-V emulator with proper terminal settings

# Set terminal type
export TERM=vt100

# Set terminal to raw mode (optional, for better control)
# stty -icanon -echo

echo "Connecting to CP-V emulator on localhost:5001..."
echo "Terminal type: $TERM"
echo ""
echo "At LOGON PLEASE: type :SYS,LBE"
echo "To exit: Ctrl-C or type LOGOFF at $"
echo ""

# Connect using netcat
nc localhost 5001

# Restore terminal (if you used stty)
# stty icanon echo
```

Make executable:
```bash
chmod +x connect_cpv.sh
./connect_cpv.sh
```

---

## Troubleshooting

### Connection Refused

**Check emulator is running:**
```bash
ps aux | grep sigma
```

Should show sigma process.

**Check port:**
```bash
lsof -i :5001
```

Should show sigma listening on 5001.

**Start emulator if needed:**
```bash
cd emulator
sigma boot_cpv.ini &
```

Wait 10-15 seconds for boot, then connect.

### Screen Looks Garbled

**Wrong terminal type.** Set it:
```bash
export TERM=vt100
```

Or try:
```bash
export TERM=vt52
```

### Arrow Keys Don't Work

**Terminal not sending VT-52 sequences.**

**Fix:**
1. Try: `export TERM=ansi` or `export TERM=xterm`
2. Use raw terminal mode: `stty -icanon -echo` before connecting
3. Check Terminal → Preferences → Profiles → Keyboard for key mappings
4. Worst case: use `h,j,k,l` keys if we add those as alternatives

### Characters Not Echoing

**Good!** CP-V echoes characters. If you see double characters, that's wrong.

Local echo should be OFF (default in most terminals).

### Can't Exit

**To disconnect:**
- Type `LOGOFF` at CP-V `$` prompt
- Or press `Ctrl-C` to force disconnect
- Or close terminal window

---

## Alternative Connection Methods

### Using Screen

```bash
screen nc localhost 5001
```

Benefits:
- Better terminal handling
- Can detach/reattach
- Scrollback buffer

Exit: Ctrl-A then K

### Using Expect Script

We have automated scripts:
```bash
cd emulator
./scripts/connect_cpv.exp
```

But manual connection is more authentic!

---

## Terminal Emulator Compatibility

| Terminal | VT-52 Support | macOS | Notes |
|----------|--------------|-------|-------|
| **macOS Terminal** | ✅ Good (via VT-100) | Built-in | **Use this!** |
| **xterm** | ✅ Excellent | Via XQuartz | If you need perfect compatibility |

### macOS Terminal (Recommended)

**Built-in, works great:**
```bash
export TERM=vt100
nc localhost 5001
```

That's all you need!

### Alternative: xterm via XQuartz (Optional)

**Only if you have issues with macOS Terminal:**

```bash
# Install XQuartz
brew install --cask xquartz

# Log out and back in, then:
xterm -tn vt100 -e "nc localhost 5001"
```

`xterm` has the most complete VT-52/VT-100 compatibility, but macOS Terminal should work fine for XL Spreadsheet.

---

## Recommended Setup

**For Best Experience with macOS Terminal:**

1. **Configure macOS Terminal:**
   - Terminal → Preferences → Profiles → Window
   - Set to 80 columns × 24 rows (or larger)
   - Terminal → Preferences → Profiles → Text
   - Choose a monospace font (Monaco 10pt works well)

2. **Connect:**
   ```bash
   export TERM=vt100
   nc localhost 5001
   ```

3. **Login:**
   ```
   :SYS,LBE
   ```

4. **Deploy XL:**
   Follow MANUAL_DEPLOYMENT.md or BATCH_DEPLOYMENT.md

5. **Run:**
   ```
   $ RUN XL
   ```

---

## Quick Reference

**Connect:**
```bash
export TERM=vt100
nc localhost 5001
```

**Login:**
```
:SYS,LBE
```

**Deploy (batch job method):**
```
$ SUBMIT XLBUILD.JOB
```

**Run:**
```
$ RUN XL
```

**Exit XL:**
```
/QUIT
```

**Exit CP-V:**
```
$ LOGOFF
```

**Disconnect:**
```
Ctrl-C
```

---

## Testing Your Setup

### Test 1: Can You Connect?

```bash
echo "test" | nc localhost 5001
```

Should show "Connected to XDS Sigma..." then timeout. Good!

### Test 2: Can You Login?

```bash
(echo ""; sleep 2; echo ":SYS,LBE"; sleep 2; echo "LOGOFF") | nc localhost 5001
```

Should show login sequence. If yes, connection works!

### Test 3: Are VT-52 Codes Working?

After logging in manually:
```
$ PRINT "HELLO"
```

Should print HELLO. If it looks right, VT-52 is working!

---

## What You Need

**Everything you need is already installed:**
- ✅ macOS Terminal (built-in)
- ✅ nc/netcat (built-in)
- ✅ TERM=vt100 (just set it with `export TERM=vt100`)

**Optional enhancements:**
- Screen or tmux (session management)
- XQuartz + xterm (if you want perfect VT-52 compatibility, but not needed)

**You don't need:**
- ❌ telnet (nc works better)
- ❌ iTerm2 (macOS Terminal works fine)
- ❌ Special software (everything is built-in!)

---

## Ready to Connect!

**Right now you can:**

```bash
# Terminal 1 - Emulator (if not running)
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
sigma boot_cpv.ini

# Terminal 2 - Connect
export TERM=vt100
nc localhost 5001

# At prompt:
:SYS,LBE

# Deploy and run XL Spreadsheet!
```

No telnet needed! 🎉

---

**Last Updated:** 2026-01-19
**Works on:** macOS 10.15+, all modern macOS versions
