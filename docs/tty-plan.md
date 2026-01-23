# Teletype Model 35 (ASR-35) Support Plan

## Overview

Add Layer 3 terminal support for the Teletype Model 35, a 110 baud hardcopy terminal operating at 10 characters per second. This requires a fundamentally different UI paradigm than CRT terminals.

## Hardware Constraints

### Teletype Model 35 Characteristics

| Characteristic | Value |
|----------------|-------|
| Speed | 110 baud, 10 cps |
| Paper width | 72 columns |
| Character set | Uppercase ASCII only |
| Line ending | Separate CR and LF keys |
| Cursor control | None |
| Screen clear | Impossible (hardcopy) |

### Available Input Keys

| Key | ASCII | Notes |
|-----|-------|-------|
| A-Z | 65-90 | Uppercase only |
| 0-9 | 48-57 | |
| CR (Return) | 13 | Carriage return |
| LF (Line Feed) | 10 | Line feed |
| TAB (Ctrl-I) | 9 | Horizontal tab |
| BS (Ctrl-H) | 8 | Backspace |
| DEL (Rubout) | 127 | Delete |
| Space | 32 | |
| Punctuation | Various | `! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _` |

### Keyboard Layout Notes

- Shifted characters = unshifted ASCII - 16 (mostly)
- `(` and `)` are above `8` and `9` (not `9` and `0`)
- `"` is above `2` (not `'`)
- Control key subtracts 64 from unshifted value
- No arrow keys

## Design Principles

1. **No auto-refresh** - Only print when explicitly requested
2. **Minimal output** - Every character costs paper and time
3. **Command-driven** - All navigation via commands, not cursor keys
4. **Uppercase only** - Force all I/O to uppercase
5. **72-column format** - Respect paper width

## Architecture

### Layer Structure

```
Layer 1: Core Computation (existing)
  - PARSE.FOR   - Formula parser
  - EVAL.FOR    - Expression evaluator
  - CELLS.FOR   - Cell storage
  - STRUTIL.FOR - String utilities
  - FILEIO.FOR  - File I/O

Layer 2: Application Logic (to be extracted)
  - Command parsing and dispatch
  - State management (current cell, viewport, modes)
  - Recalculation control

Layer 3: Terminal Presentation (device-specific)
  - VT100.FOR / termio.c    - CRT terminal (existing, to be refactored)
  - TTY35.FOR / termtty.c   - Teletype terminal (new)
```

### File Structure

```
native/
  layer1/           # Core computation (unchanged)
    PARSE.FOR
    EVAL.FOR
    CELLS.FOR
    STRUTIL.FOR
    FILEIO.FOR
  layer2/           # Application logic (extract from XLMAIN)
    COMMANDS.FOR    # Command parsing
    STATE.FOR       # Application state
  layer3/           # Terminal presentation
    vt100/
      TRMVT1.FOR    # VT-100 display routines
      termio.c      # VT-100 I/O with escape codes
    tty35/
      TRMTTY.FOR    # TTY35 display routines
      termtty.c     # Simple line I/O
  XLMAIN.FOR        # Main program (uses layer2 + selected layer3)
  build_small/      # Small CRT build
  build_medium/     # Medium CRT build
  build_large/      # Large CRT build
  build_tty35/      # Teletype build
```

## Input Handling

### Key Mapping

| Key | Action |
|-----|--------|
| CR (Return) | Confirm entry, move down one row, show new cell prompt |
| LF (Line Feed) | Same as CR |
| TAB (Ctrl-I) | Confirm entry, move right one column, show new cell prompt |
| BS (Ctrl-H) | Delete last character during entry |
| DEL (Rubout) | Delete last character during entry |
| `/` | Start command mode |
| Other printable | Cell content entry |

### Entry Behavior

- Prompt shown on same line as input: `A1: 100`
- CR moves down, TAB moves right
- Empty CR on empty cell = move down without change
- Empty CR on cell with content = keep content, move down
- Any typing replaces cell content

## Commands

All commands start with `/` and are uppercase.

### Navigation

| Command | Description |
|---------|-------------|
| `/P A1` or `/POINT A1` | Set current cell to A1 (like using arrow keys) |
| `/G A1` or `/GOTO A1` | Set viewport origin to A1 (for next /DISPLAY) |

### Display

| Command | Description |
|---------|-------------|
| `/D` or `/DISPLAY` | Print grid from current viewport |
| `/D A1:C10` | Print specific range only |
| `/C A1` or `/CELL A1` | Show single cell: formula and computed value |

### Editing

| Command | Description |
|---------|-------------|
| `/DEL` or `/DELETE` | Clear current cell contents |
| `/CALC` | Force recalculation (if manual mode) |

### Settings

| Command | Description |
|---------|-------------|
| `/W 8` or `/WIDTH 8` | Set default column width |
| `/AUTO` | Set automatic recalculation mode |
| `/MANUAL` | Set manual recalculation mode |

### File Operations

| Command | Description |
|---------|-------------|
| `/SAVE FILENAME` | Save spreadsheet to file |
| `/LOAD FILENAME` | Load spreadsheet from file |
| `/NEW` | Clear spreadsheet and start fresh |

### Other

| Command | Description |
|---------|-------------|
| `/Q` or `/QUIT` | Exit program |
| `/?` or `/HELP` | Show command summary |

## Output Format

### Grid Display (72 columns)

```
      A       B       C       D       E
  1   100     200     300     400     500
  2   150     250     350     450     550
  3   300
  4
  5
```

- 5 columns x 8 characters = 40 characters for data
- 6 characters for row labels
- Total: 46 characters (fits in 72)
- All uppercase
- Numbers right-justified in column width
- Text left-justified
- Empty cells blank

### Cell Status Prompt

After every operation, show current cell on new line:

```
A1: 123.45
```

Or for formulas:
```
B2: =@SUM(A1:A10) -> 550
```

Or for empty cells:
```
C3:
```

User input appears on same line after the prompt.

## Interaction Examples

### Basic Data Entry

```
A1: 100
A2: 200
A3: =@SUM(A1:A2)
A4:
```

### Navigation and Display

```
A4: /G A1
A4: /D
      A       B       C       D       E
  1   100
  2   200
  3   300
  4
  5
A4: /P B1
B1:
```

### Editing Existing Cell

```
B1: /P A1
A1: 100
50
A2: /D
      A       B       C       D       E
  1    50
  2   200
  3   250
  4
  5
A2:
```

### Showing Cell Details

```
A2: /C A3
A3: =@SUM(A1:A2) -> 250
A2:
```

### Using Tab for Horizontal Entry

```
A1: 100	B1: 200	C1: 300	D1: 400
D2:
```

(Tab key shows as movement to next cell on same line until CR)

## Recalculation Behavior

- **Auto mode (default)**: Recalculate before any `/D` or `/C` display
- **Manual mode**: Only recalculate on explicit `/CALC` command
- Values always update in memory on cell entry
- Display shows current calculated values

## termtty.c Interface

```c
void TRMINI(void);      // Initialize (minimal - just set raw mode)
void TRMEND(void);      // Cleanup (restore terminal mode)
void TRMOUT(int ch);    // Output character (force uppercase)
int  TRMIN(void);       // Input character (with echo, force uppercase)
void TRMNL(void);       // Output newline (CR+LF)
void TRMSTR(char *s);   // Output string
void TRMFLUSH(void);    // Flush output buffer
```

No cursor positioning, no screen clear, no highlighting.

## Build Configuration

### build_tty35/Makefile

- Link layer1 + layer2 + layer3/tty35
- Compile with TTY35 preprocessor flag if needed
- Target: xl_tty35

### Memory Configuration

Same options as CRT builds:
- Small: 10x10 grid, 100 cells
- Medium: 15x20 grid, 300 cells
- Large: 26x77 grid, 2002 cells

Default tty35 build: Medium (reasonable for paper-based work)

## Implementation Steps

1. **Refactor existing code** - Separate layer 2 from layer 3 in current XLMAIN.FOR
2. **Create layer2/** - Extract command processing and state management
3. **Create layer3/vt100/** - Move VT-100 specific code
4. **Create layer3/tty35/** - Implement teletype routines
5. **Create termtty.c** - Simple I/O without escape codes
6. **Create build_tty35/** - Build configuration
7. **Test on actual teletype or simulator**

## Testing

### Simulator Testing

Use `screen` or `minicom` at 110 baud to simulate teletype speed:
```bash
stty 110
./xl_tty35
```

### Functional Tests

1. Basic data entry with CR and TAB
2. Formula entry and calculation
3. /POINT navigation
4. /GOTO and /DISPLAY
5. /CELL detail view
6. /SAVE and /LOAD
7. 72-column output formatting
8. Uppercase enforcement

## Future Enhancements

- `/PRINT` - Print range to paper tape punch
- `/TAPE` - Read from paper tape reader
- Support for other hardcopy terminals (LA36, etc.)
- Batch mode for paper tape input

## References

- Teletype Model 35 Technical Manual
- ASCII Character Set (1967)
- https://en.wikipedia.org/wiki/Teletype_Model_33 (similar keyboard)
