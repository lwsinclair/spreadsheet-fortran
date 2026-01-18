# Card Deck Deployment - 1978 Authentic Batch Processing

**Most Historically Accurate Method**

## Overview

In 1978, programmers submitted batch jobs using **punched cards**. A large program might require thousands of cards carried in card trays or boxes.

For the XL Spreadsheet, we've created a simulated card deck of **4,003 cards** representing:
- Job control cards (JCL)
- 11 FORTRAN source files
- Compilation commands
- Link instructions
- End-of-job marker

## The Card Deck

**File:** `emulator/work/xl_deck.txt`
**Size:** 316 KB (324,243 bytes)
**Cards:** 4,003 cards (80 columns each)

### What's in the Deck

```
Card 1-3:      Job control (JOB, ACCOUNT statements)
Card 4-342:    STRUTIL.FOR source code + compile commands
Card 343-875:  CELLS.FOR source code + compile commands
Card 876-1149: DEPS.FOR source code + compile commands
...
[continues for all 11 modules]
...
Card 3990-4001: Link all modules, create XL executable
Card 4002:      SAVE XL
Card 4003:      EOJ (End of Job)
```

This is exactly how it would have been done in 1978!

## Historical Context

### 1978 Workflow:

**Monday Morning:**
1. Student writes FORTRAN code on coding sheets
2. Takes coding sheets to keypunch room
3. Keypunch operator creates punched cards
4. Student picks up card deck (could be shoe-box sized!)

**Monday Afternoon:**
5. Student carries card deck to computer center
6. Submits deck to batch window
7. Operator loads cards into card reader
8. Computer reads and processes cards
9. Job runs when CPU time available

**Monday Evening:**
10. Student returns to pick up printout
11. If errors, fix code and resubmit Tuesday
12. If success, executable is saved in account

**Turnaround time:** Hours or overnight (minutes in our emulator!)

### The Horror of Dropped Cards

If you dropped your card deck:
- **Best case:** Cards were numbered, spent hour resorting
- **Worst case:** No numbers, impossible to reconstruct
- **Solution:** Rubber band around deck, prayer before walking

Our card deck is safely digital! 😅

## Creating the Card Deck

Already done! Run to regenerate:

```bash
cd emulator
./scripts/make_card_deck.sh
```

This creates `work/xl_deck.txt` with 4,003 80-column card images.

## Method 1: Card Reader Submission (If Supported)

**Check if CP-V emulator has card reader configured:**

The Sigma emulator should support a card reader device. Check `boot_cpv.ini` for:
```
set cr enable
att cr work/xl_deck.txt
```

If configured, CP-V can read the deck directly:

```
$ SUBMIT CR
```

The system will:
1. Read all 4,003 cards from card reader
2. Process job control
3. Create source files
4. Compile modules
5. Link program
6. Save XL executable

## Method 2: Batch Job via File (Current Approach)

Since the card reader may not be configured, use the batch job file:

```
$ SUBMIT XLBUILD.JOB
```

(This is equivalent - the .JOB file is the modern equivalent of the card deck)

## Method 3: Simulating Card Reader Input

If you want the authentic card reader experience:

### Configure Card Reader in Emulator

Stop emulator and edit `boot_cpv.ini`:

```ini
# Card Reader (80-column cards)
set cr enable
set cr chan=a,dva=1
att cr work/xl_deck.txt
```

Restart emulator:
```bash
sigma boot_cpv.ini
```

### Submit Card Deck

In CP-V:
```
$ SUBMIT CR:
```

CP-V will read from the card reader device (our deck file).

## Advantages of Card Deck Method

**Historical Authenticity:**
- ✅ Exactly how it was done in 1978
- ✅ Simulates real punched card workflow
- ✅ All-in-one submission (source + JCL)

**Technical:**
- ✅ Self-contained (doesn't require manual file transfer)
- ✅ Automated (single submit command)
- ✅ Reproducible (same deck, same result)

**Educational:**
- ✅ Experience 1970s computing
- ✅ Understand batch processing
- ✅ Appreciate modern development tools!

## Card Deck Statistics

```
Total Cards:      4,003 cards
Weight:           ~13 pounds (if real cards!)
Stack Height:     ~26 inches (2+ feet!)
Read Time (1978): ~3 minutes @ 1200 cards/min
Read Time (2026): Instantaneous
```

In 1978, 4,003 cards would fill:
- 5 standard card trays
- Or 1 medium-sized box
- Students often used shopping bags!

## Troubleshooting

### Card Reader Not Found

If `SUBMIT CR:` fails with "device not found":
- Card reader not configured in emulator
- Use Method 2 (batch job file) instead

### Deck Read Error

If card reader stops mid-deck:
- Check for non-ASCII characters in source
- Verify all files are 80 columns wide
- Regenerate deck with `make_card_deck.sh`

### Compilation Errors

View output:
```
$ OUTPUT XLBUILD
```

If errors occur partway through deck:
- Some files may have compiled successfully
- Use `CATALOG *.OBJ` to see which modules succeeded
- Can manually compile failed modules

## Real-World Card Deck Examples

### Famous Programs on Cards:

**IBM OS/360 (1964):**
- Source code: ~1 million cards
- Weight: Over 1 ton
- Required forklift to move!

**FORTRAN Compiler (1960s):**
- Typical size: 20,000-30,000 cards
- Carried in multiple boxes

**Student Programs (1970s):**
- Small program: 50-200 cards (fits in pocket)
- Medium program: 500-1000 cards (card tray)
- Large program: 2000-5000 cards (box)

Our XL Spreadsheet (4,003 cards) would have been considered a substantial undergraduate project!

## The Card Deck Experience

### What Students Would Do:

**Before Submission:**
1. Check every card for correct punching
2. Number cards in upper-right corner
3. Put rubber band around deck
4. Walk very carefully to computer center
5. Submit to batch window operator

**While Waiting:**
6. Go to class or cafeteria
7. Check batch queue status board
8. Wait for printout ready light

**After Completion:**
9. Pick up printout from output window
10. Debug any errors
11. Re-punch corrected cards
12. Resubmit (entire deck again!)

### Common Disasters:

- **Dropped deck:** Spend afternoon resorting cards
- **Missed comma:** Entire job fails, resubmit tomorrow
- **EOF card missing:** Job runs forever, wastes CPU time
- **Wrong job class:** Sits in queue for days
- **Cards out of order:** Mysterious syntax errors

Our digital card deck avoids all these problems! 🎉

## Modern Appreciation

Try carrying a 13-pound box of papers around campus for a day. That was normal for computer science students in 1978!

Modern equivalents:
- **4,003 cards** = ~320 KB
- One floppy disk (1440 KB) = ~17,600 cards!
- One iPhone photo (3 MB) = ~115,200 cards!!

## Conclusion

The card deck method is the most historically authentic way to deploy XL Spreadsheet to CP-V. While we use a digital simulation, it follows the exact workflow that a 1978 student would have experienced.

**To experience 1978 computing the authentic way:**

1. Generate card deck: `./scripts/make_card_deck.sh`
2. Configure card reader in emulator
3. Submit deck: `SUBMIT CR:`
4. Wait for compilation (seconds, not hours!)
5. Run: `RUN XL`

You've just experienced 1970s batch computing! 🖥️📇

---

## References

- **IBM 80-column Cards:** 7⅜" × 3¼" × 0.007" thick
- **Card Reader Speed:** 600-1200 cards/minute (1970s)
- **Hollerith Code:** 12 rows × 80 columns = 960 punch positions per card
- **Storage Density:** ~80 bytes per card (worst case: 1 byte per KB!)

## See Also

- `BATCH_DEPLOYMENT.md` - Batch job file method
- `MANUAL_DEPLOYMENT.md` - Interactive deployment
- `CPV_DEPLOYMENT.md` - Detailed compilation guide
- `QUICKSTART.md` - CP-V emulator basics

---

**Last Updated:** 2026-01-19
**Card Deck Size:** 4,003 cards
**Historical Authenticity:** ✅ Maximum
