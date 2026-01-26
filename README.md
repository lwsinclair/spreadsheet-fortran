# XL: A Counterfactual 1978 Spreadsheet

**What if the first spreadsheet had been written in FORTRAN?**

In reality, VisiCalc—the first electronic spreadsheet—was written in 1978-79 by Dan Bricklin and Bob Frankston in 6502 assembly language for the Apple II. It was a revolutionary piece of software that helped launch the personal computer revolution.

But here's a thought experiment: What if someone had built a spreadsheet on a *minicomputer* instead? In 1978, universities and businesses had access to machines like the PDP-11 running RSX-11M, or Xerox Sigma systems running CP-V. These were serious computers with real operating systems, terminals, and—crucially—FORTRAN compilers.

**XL** is that counterfactual spreadsheet. It's written in FORTRAN IV (the 1966 standard), uses only features available in 1978, and is designed to run on authentic vintage hardware through emulation.

---

## The Premise

It's 1978. You're a programmer at a university or corporation with access to a PDP-11 minicomputer. You've seen how researchers use paper spreadsheets for financial modeling and "what-if" analysis. You think: *what if the computer could do this interactively?*

You don't have an Apple II. You don't know 6502 assembly. But you *do* know FORTRAN—the lingua franca of scientific computing—and you have a VT-52 terminal on your desk.

So you write XL.

---

## What This Project Actually Is

This is a working spreadsheet implementation in strict FORTRAN IV/66:

- **Formula engine** with cell references (`=A1+B2*C3`)
- **Functions** like `SUM`, `AVG`, `MIN`, `MAX`, `IF`, `SQRT`
- **Automatic recalculation** with dependency tracking
- **Circular reference detection**
- **Interactive cursor-based navigation** on VT-52/VT-100 terminals

It compiles and runs on:
- **PDP-11** with RSX-11M (the primary target)
- **Xerox Sigma 7** with CP-V
- **CP/M** systems (Z80/8080) with appropriate memory constraints
- **Modern systems** via gfortran (for development and testing)

## Screenshots

*Coming soon: terminal captures from SimH emulator sessions*

---

## Technical Constraints (Authenticity)

To maintain historical authenticity, XL follows strict 1978-era constraints:

### FORTRAN IV Only
- No `CHARACTER` type (uses INTEGER arrays for strings)
- No block `IF/THEN/ELSE` (uses arithmetic IF and GO TO)
- No recursion (uses explicit stacks)
- All identifiers ≤ 6 characters
- Fixed-format source (columns 1-72)

### 16-bit Integer Math
- All integers fit in ±32,767 range
- Cell addresses encoded as `row*256 + col`
- Hash tables sized for 16-bit arithmetic

### Memory Constraints
- **CP/M config**: 300 cells, fits in 39 KB
- **PDP-11 config**: 500 cells, fits in 64 KB
- **Full config**: 2000 cells for larger systems

### Terminal I/O
- VT-52 escape sequences for cursor control
- No ANSI colors (not standard until later)
- 80×24 display assumed

---

## Building and Running

### Prerequisites

```bash
# macOS
brew install gcc  # Includes gfortran

# Linux
sudo apt install gfortran

# Python test dependencies
pip3 install -r requirements.txt
```

### Build and Test

```bash
# Build for modern system (development)
make clean && make

# Run test suite (140 tests)
python -m pytest test/ -v

# Configure for specific target
./configure.sh cpm      # CP/M (300 cells, 39 KB)
./configure.sh pdp11    # PDP-11 (500 cells)
./configure.sh full     # Full (2000 cells)
```

### Running on Emulated Hardware

#### PDP-11 with RSX-11M (SimH)

```bash
cd emulator/pdp11-rsx
./boot_rsx.exp          # Boot RSX-11M to 124K mapped
# Then compile and run XL (see docs/RSX11M_FORTRAN_STATUS.md)
```

#### Xerox Sigma 7 with CP-V (SimH)

```bash
cd emulator
sigma boot_cpv.ini      # Boot CP-V
# Transfer sources and compile (see emulator/QUICKSTART.md)
```

---

## Project Structure

```
spreadsheet-fortran/
├── native/                    # Platform-independent FORTRAN source
│   ├── layer0/               # String utilities (STRUTIL.FOR)
│   ├── layer1/               # Calculation engine (CELLS, PARSE, EVAL, DEPS, RECALC)
│   ├── layer2/               # UI and commands (UI, DISPLAY, MSG, COMMANDS, FILES)
│   └── layer3/               # Terminal drivers (VT-52, VT-100, TTY)
├── pdp11/                    # PDP-11/RSX-11M specific build
│   └── src/                  # Configured for RSX-11M FORTRAN IV
├── emulator/
│   ├── pdp11-rsx/           # PDP-11 SimH configuration and scripts
│   │   └── media/           # Disk images (RSX-11M, FORTRAN distribution)
│   └── sigma-cpv-kit/       # CP-V emulator setup
├── test/
│   ├── unit/                # 102 unit tests
│   └── portability/         # 38 portability validation tests
└── docs/
    ├── RSX11M_FORTRAN_STATUS.md   # Current PDP-11 progress
    ├── SIMH_NATIVE_SCRIPTING.md   # SimH automation guide
    ├── PORTABILITY.md             # FORTRAN IV constraints
    └── ...
```

---

## Current Status

### What Works ✅

- **Calculation engine**: Formula parsing, evaluation, dependency tracking, recalculation
- **All 140 tests passing**: 102 unit tests + 38 portability tests
- **Compiles with gfortran**: Using `-std=legacy` for FORTRAN 66 mode
- **RSX-11M boot**: Emulator boots to 124K mapped system
- **FORTRAN IV compiler on RSX**: Built FOR.TSK, compiles .FTN files successfully
- **FOROTS runtime library**: Created from distribution

### What's In Progress 🔄

- **PDP-11 runtime**: Linking works but execution fails with "TASK INITIALIZATION FAILURE"
  - Root cause: OTS bugs requiring autopatch (RSX-11M_V3.2_AUTOPATCH1B.DSK)
  - See `docs/RSX11M_FORTRAN_STATUS.md` for details

### What's Planned 📋

- Apply autopatch and complete PDP-11 runtime testing
- File transfer from host (PUTR tool needed for RT-11 disk creation)
- Interactive testing on emulated VT-52 terminal
- CP-V deployment validation

---

## The Historical Context

### What Actually Happened (1978-79)

Dan Bricklin conceived VisiCalc while at Harvard Business School, watching a professor erase and recalculate a financial model on a blackboard. He and Bob Frankston implemented it in 6502 assembly for the Apple II, releasing it in 1979. It became the "killer app" that drove Apple II sales and helped establish the personal computer market.

VisiCalc was brilliantly optimized for the Apple II's constraints: 48KB RAM, 40-column display, no operating system to speak of. The assembly language implementation was essential for performance and memory efficiency.

### The Road Not Taken

But minicomputers were everywhere in 1978. The PDP-11 was the most popular computer architecture of its era. Universities, corporations, and government agencies all had them. They had:

- Real operating systems (RSX-11M, RSTS/E, Unix)
- FORTRAN compilers
- Terminal support
- More memory than microcomputers
- Multi-user capability

A FORTRAN spreadsheet on a PDP-11 would have been:
- **Slower to develop** (FORTRAN vs assembly optimization)
- **Easier to port** (FORTRAN was everywhere)
- **More expensive to run** (minicomputer time-sharing vs personal Apple II)
- **Available to different users** (corporate/academic vs hobbyist)

The microcomputer won because it was *personal*—one person, one computer, immediate feedback. But it's interesting to imagine the alternative timeline.

---

## Why FORTRAN IV?

FORTRAN IV (ANSI X3.9-1966) was the standard in 1978. FORTRAN 77 wouldn't be finalized until late 1978 and wouldn't be widely implemented until the early 1980s.

Key limitations we work within:
- No CHARACTER type for strings
- No block IF/THEN/ELSE
- No DO WHILE
- Six-character identifier limit
- No recursion
- Fixed-format source code

These constraints force a certain programming style—lots of GO TO statements, careful array indexing for "strings," and explicit stack management. It's authentic to the era.

---

## Contributing

This is primarily a historical recreation project, but contributions are welcome:

- Bug fixes (maintaining FORTRAN IV compliance)
- Additional terminal drivers
- Documentation improvements
- Testing on actual vintage hardware (!)

Please ensure all code:
- Passes the existing 140 tests
- Uses only FORTRAN IV features
- Fits within the portability constraints documented in `docs/PORTABILITY.md`

---

## References

### Hardware and Software

- [SimH PDP-11 Emulator](http://simh.trailing-edge.com/)
- [RSX-11M Documentation](http://bitsavers.org/pdf/dec/pdp11/rsx11/)
- [PDP-11 FORTRAN IV Reference](http://bitsavers.org/pdf/dec/pdp11/fortran/)
- [Sigma CP-V Kit](https://github.com/kenrector/sigma-cpv-kit)

### Historical

- [VisiCalc History](http://www.bricklin.com/visicalc.htm) - Dan Bricklin's account
- [The VisiCalc Story](https://www.computer.org/publications/tech-news/chasing-the-ghost/the-visicalc-story)
- [A Brief History of Spreadsheets](http://www.j-walk.com/ss/history/)

---

## License

MIT License - See LICENSE file.

This is an educational/historical recreation project. No affiliation with the creators of VisiCalc, DEC, Xerox, or any vintage computing companies.

---

*"The spreadsheet is a tool that has made a significant contribution to the art of management."*
— VisiCalc advertisement, 1979

*"But what if it had been written in FORTRAN?"*
— This project, 2026
