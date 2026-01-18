# XL Spreadsheet - Quick Deploy Guide

**Get XL Spreadsheet running on CP-V in one command!**

---

## The One Command

```bash
cd /Volumes/SECURE8/git/spreadsheet-fortran/emulator
./deploy_to_cpv.sh
```

That's it! Wait 4-8 minutes and you're done.

---

## What It Does

1. ✅ Checks prerequisites
2. ✅ Installs CP-V (first time only)
3. ✅ Boots CP-V from RAD
4. ✅ Transfers all 12 files
5. ✅ Submits batch build job
6. ✅ Leaves you at CP-V console

---

## After Deployment

At the CP-V console, check job status:

```
$ STATUS
```

View compilation output:

```
$ TYPE XLBUILD.LOG
```

Or from your Mac:

```bash
cat work/printer.txt
```

Run XL Spreadsheet:

```
$ RUN XL
```

---

## First Time Setup

Install expect if you don't have it:

```bash
brew install expect
```

Extract CP-V system files (one-time):

```bash
cd sigma-cpv-kit/f00/f00rad
unzip f00rad-system.zip
cd ../../..
```

---

## Troubleshooting

**Port already in use?**
```bash
pkill sigma
./deploy_to_cpv.sh
```

**Deployment failed?**
Check the error message and try again. The script is idempotent.

**Need manual control?**
See AUTOMATED_DEPLOYMENT.md for detailed documentation.

---

## Shutdown CP-V

In the emulator console window:

1. Press `Ctrl-E`
2. Type: `ZAP`
3. Wait for: `THAT'S ALL, FOLKS!!`
4. Type: `quit`

---

## Manual Alternative

If you prefer hands-on control, see:
- MANUAL_DEPLOYMENT.md - Step-by-step manual process
- BATCH_DEPLOYMENT.md - Manual batch job method

---

## Time Estimate

- **First run:** 6-8 minutes (includes CP-V installation)
- **Subsequent runs:** 4-5 minutes (just boot + deploy)
- **Updates only:** 3-4 minutes (if CP-V already running)

---

## Next Steps

1. Run the deployment script
2. Wait for completion
3. Run XL: `RUN XL`
4. Test the spreadsheet
5. Enjoy 1978 computing!

---

**For complete documentation, see AUTOMATED_DEPLOYMENT.md**
