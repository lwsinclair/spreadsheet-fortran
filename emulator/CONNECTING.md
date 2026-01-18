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

### Option 2: iTerm2 (Recommended)

**Download:** https://iterm2.com/

**Why iTerm2:**
- ✅ Better VT-100/VT-52 support
- ✅ Customizable terminal emulation
- ✅ Better escape sequence handling
- ✅ Configurable key mappings

**Configuration:**
1. Install iTerm2
2. Preferences → Profiles → Terminal
3. Set "Terminal Emulation" to `xterm-256color`
4. Enable "Report terminal type" as `vt100`

**Connect:**
```bash
nc localhost 5001
```

### Option 3: Install telnet (If You Really Want It)

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
- Window Size: 80 columns × 24 rows (or larger)

**In iTerm2:**
- Preferences → Profiles → Window
- Columns: 80
- Rows: 24

---

## Arrow Keys Configuration

XL Spreadsheet uses arrow keys for navigation. These send VT-52 escape sequences:

| Key | VT-52 Sequence | Hex |
|-----|----------------|-----|
| Up | ESC A | 1B 41 |
| Down | ESC B | 1B 42 |
| Right | ESC C | 1B 43 |
| Left | ESC D | 1B 44 |

**macOS Terminal and iTerm2 handle this automatically when TERM=vt100.**

### If Arrow Keys Don't Work

Try this in terminal before connecting:
```bash
stty -icanon -echo
export TERM=vt100
nc localhost 5001
```

Or use iTerm2 which has better key handling.

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
1. Use iTerm2 instead of Terminal
2. Or configure Terminal key mappings
3. Or use Ctrl-based alternatives in XL (if we add them)

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

## Modern Terminal Emulators with VT-52 Support

| Terminal | VT-52 Support | macOS | Notes |
|----------|--------------|-------|-------|
| **iTerm2** | ✅ Excellent | Yes | Recommended |
| **macOS Terminal** | ✅ Good (via VT-100) | Built-in | Works fine |
| **Alacritty** | ✅ Good | Yes | Fast, minimal |
| **Kitty** | ✅ Good | Yes | Modern features |
| **xterm** | ✅ Perfect | Via XQuartz | Most compatible |

### Installing XQuartz + xterm (Most Compatible)

```bash
# Install XQuartz
brew install --cask xquartz

# Log out and back in, then:
xterm -tn vt100 -e "nc localhost 5001"
```

`xterm` has the best VT-52/VT-100 compatibility.

---

## Recommended Setup

**For Best Experience:**

1. **Use iTerm2:**
   ```bash
   brew install --cask iterm2
   ```

2. **Configure iTerm2:**
   - Profile → Terminal → Report terminal type: `vt100`
   - Profile → Window → 80 columns × 24 rows
   - Profile → Keys → Key Mappings: Default

3. **Connect:**
   ```bash
   TERM=vt100 nc localhost 5001
   ```

4. **Login:**
   ```
   :SYS,LBE
   ```

5. **Deploy XL:**
   Follow MANUAL_DEPLOYMENT.md or BATCH_DEPLOYMENT.md

6. **Run:**
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

**Minimum:**
- ✅ macOS Terminal (you have this)
- ✅ nc/netcat (you have this)
- ✅ TERM=vt100 (just set it)

**Recommended:**
- iTerm2 (better VT emulation)
- Screen or tmux (session management)

**Optional:**
- XQuartz + xterm (best compatibility)
- telnet (not needed, nc works fine)

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
