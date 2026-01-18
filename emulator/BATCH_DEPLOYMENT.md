# Batch Job Deployment for CP-V

This guide explains how to deploy XL Spreadsheet using CP-V batch jobs, as would have been done in 1978.

## Overview

Rather than compiling interactively, we use CP-V's batch job system to automate the entire build process. This is historically accurate - in 1978, batch processing was the standard way to compile large programs.

## Files Required

**11 Source Files:**
- `STRUTIL.FOR` (Layer 0)
- `CELLS.FOR`, `DEPS.FOR`, `PARSE.FOR`, `EVAL.FOR`, `RECALC.FOR` (Layer 1)
- `UI.FOR`, `DISPLAY.FOR`, `MSG.FOR` (Layer 2)
- `TERMCPV.FOR` (Layer 3)
- `XLMAIN.FOR` (Main program)

**Batch Job Control File:**
- `XLBUILD.JOB` - Automated build script

## CP-V Batch Job Format

CP-V batch jobs use Job Control Language (JCL):

```
$ JOB jobname - description
$ ACCOUNT :SYS,LBE
$
$ RUN programname
*option1=value1
*option2=value2
$
$ EOJ
```

### Key Components:

- `$ JOB` - Job header with name and description
- `$ ACCOUNT` - Account to charge for resources
- `$ RUN` - Execute a program
- `*` - Program control statements (passed to the program)
- `$ EOJ` - End of job marker

## Deployment Steps

### Step 1: Transfer Files to CP-V

You need to get 12 files onto CP-V:
- 11 FORTRAN source files (.FOR)
- 1 batch job file (.JOB)

**Via Terminal (paste):**
```bash
# Connect to CP-V (port 5001)
telnet localhost 5001

# Log in
LOGON PLEASE: :SYS,LBE

# Create each file
$ EDIT STRUTIL.FOR
[Paste contents]
:FILE
:QUIT

[Repeat for all 12 files]
```

### Step 2: Submit Batch Job

Once all files are on CP-V, submit the build job:

```
$ SUBMIT XLBUILD.JOB
```

CP-V will:
1. Accept the job into the batch queue
2. Assign a job number
3. Process the job when resources are available
4. Compile all 11 modules
5. Link them together
6. Save the XL executable

### Step 3: Monitor Job Progress

Check batch job status:

```
$ STATUS JOBS
```

View job output when complete:

```
$ OUTPUT XLBUILD
```

### Step 4: Run XL Spreadsheet

After successful compilation:

```
$ RUN XL
```

## Batch Job Contents

The `XLBUILD.JOB` file contains:

```
$ JOB XLBUILD - Build XL Spreadsheet for CP-V
$ ACCOUNT :SYS,LBE
$
$ * Compile Layer 0
$ RUN FORTRAN
*SOURCE=STRUTIL.FOR
*OBJECT=STRUTIL.OBJ
*LINK
$
$ * Compile Layer 1
$ RUN FORTRAN
*SOURCE=CELLS.FOR
*OBJECT=CELLS.OBJ
*LINK
$
[... continues for all 11 files ...]
$
$ * Link Main Program
$ RUN FORTRAN
*SOURCE=XLMAIN.FOR
*OBJECT=XLMAIN.OBJ
*LIBRARY=STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMCPV
*LINK
*MAP
*GO
$
$ SAVE XL
$
$ EOJ
```

## Expected Output

When the batch job completes successfully, you should see:

```
XLBUILD    COMPLETE   [timestamp]
```

The XL executable will be saved in your account directory.

## Troubleshooting

### Job Fails with Compile Errors

View the job output to see error messages:
```
$ OUTPUT XLBUILD
```

Look for:
- **Syntax errors**: Check .FOR file line numbers
- **Missing files**: Verify all source files were transferred
- **PARAMETER errors**: CP-V F/IV may not support PARAMETER

### Job Stuck in Queue

Check system status:
```
$ STATUS SYSTEM
```

Cancel and resubmit if needed:
```
$ CANCEL XLBUILD
$ SUBMIT XLBUILD.JOB
```

### Link Errors

Common issues:
- **Undefined references**: Module not compiled or missing from LIBRARY list
- **Duplicate symbols**: Module compiled/linked twice

## Historical Context

This batch job approach is historically accurate:

- **1978 Practice**: University computer systems used batch processing for all compilation
- **Time-sharing**: Interactive terminals were for editing and job submission only
- **Resource Management**: Batch system allocated CPU time fairly among users
- **Turnaround Time**: Jobs might wait in queue for minutes or hours depending on load
- **Economic**: Batch processing was more efficient than interactive compilation

At a 1978 university, a student would:
1. Write/edit source code interactively
2. Submit batch job before leaving for class
3. Return later to check output
4. Debug and resubmit if needed

## Modern Emulator Advantages

Using the CP-V emulator, batch jobs complete almost instantly since:
- No other users competing for resources
- Modern CPU is vastly faster than 1978 Sigma 7
- No actual disk I/O delays

This makes development much faster while preserving the authentic workflow.

## Alternative: Interactive Build

For quick testing during development, you can also compile interactively:

```
$ RUN FORTRAN
*SOURCE=XLMAIN.FOR
*OBJECT=XLMAIN.OBJ
*LIBRARY=STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMCPV
*LINK
*MAP
*GO
```

But batch jobs are preferred for:
- Reproducible builds
- Multiple modules
- Automation
- Historical accuracy

## Next Steps

After successful deployment:
1. Run `$ RUN XL` to start the spreadsheet
2. Test with VT-52 terminal
3. Enter formulas and verify calculations
4. Experience 1978 computing!

---

**See also:**
- `CPV_DEPLOYMENT.md` - Interactive deployment guide
- `QUICKSTART.md` - CP-V emulator basics
- `docs/LAYER3_COMPLETE.md` - Architecture documentation
