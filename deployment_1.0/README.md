# XL Spreadsheet - Deployment 1.0

## Build Date: 2026-01-21

## Changes from Development Build

- **Removed dead code**: COMMANDS.FOR excluded (69 active lines, never called)
- All compiled executables contain only functioning code

## Executables

| Variant | Cells | Description |
|---------|-------|-------------|
| xl_small | 100 | Minimal systems, 64KB |
| xl_medium | 300 | Standard systems |
| xl_large | 2,000 | Full-featured systems |

## Usage

```
./xl_large
/open sample.xl
```

## Commands

| Command | Description |
|---------|-------------|
| /quit | Exit application |
| /save filename | Save spreadsheet |
| /open filename | Load spreadsheet |
| /copy A1:B5 C1 | Copy range to destination |

## Features

- Formula evaluation with operators: + - * / ^
- Parentheses for grouping: (A1+B1)*2
- Functions: @SUM, @AVG, @MIN, @MAX, @N
- Cell references: A1 (relative), $A$1 (absolute), $A1, A$1 (mixed)
- Text labels with alignment prefixes: 'left "right ^center
- Arrow key navigation, Ctrl+Arrow for edge jump
- VT-100/ANSI terminal support
