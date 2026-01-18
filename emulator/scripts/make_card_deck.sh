#!/bin/bash
#
# make_card_deck.sh - Create card image deck for CP-V batch job submission
#
# In 1978, batch jobs were submitted as punched card decks. This script
# creates a "card image" file that simulates a deck of 80-column cards.
#
# Card deck format:
#   1. JOB card (job control)
#   2. Source files (as *INCLUDE or inline)
#   3. EOJ card (end of job)
#
# Usage: ./make_card_deck.sh
#

set -e

WORK_DIR="$(dirname "$0")/../work"
DECK_FILE="$WORK_DIR/xl_deck.txt"

echo "Creating CP-V card image deck for XL Spreadsheet build"
echo ""

# Start with empty deck
: > "$DECK_FILE"

# Helper function to add 80-column card
add_card() {
    local text="$1"
    # Pad or truncate to 80 columns
    printf "%-80.80s\n" "$text" >> "$DECK_FILE"
}

# Helper function to add file as card images
add_file() {
    local filename="$1"
    local filepath="$WORK_DIR/$filename"

    if [ ! -f "$filepath" ]; then
        echo "ERROR: File not found: $filepath"
        exit 1
    fi

    echo "  Adding: $filename"

    # Add each line as an 80-column card
    while IFS= read -r line || [ -n "$line" ]; do
        printf "%-80.80s\n" "$line" >> "$DECK_FILE"
    done < "$filepath"
}

echo "Building card deck..."
echo ""

# Job header
echo "Job control cards:"
add_card "\$ JOB XLBUILD - Build XL Spreadsheet for CP-V"
add_card "\$ ACCOUNT :SYS,LBE"
add_card "\$"

# For each source file, we need to create it on CP-V first
# Then compile it

# Layer 0
echo "Layer 0:"
add_card "\$ * Create and compile STRUTIL.FOR"
add_card "\$ EDIT STRUTIL.FOR"
add_file "STRUTIL.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=STRUTIL.FOR"
add_card "*OBJECT=STRUTIL.OBJ"
add_card "*LINK"
add_card "\$"

# Layer 1 - Cells
echo "Layer 1:"
add_card "\$ * Create and compile CELLS.FOR"
add_card "\$ EDIT CELLS.FOR"
add_file "CELLS.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=CELLS.FOR"
add_card "*OBJECT=CELLS.OBJ"
add_card "*LINK"
add_card "\$"

# DEPS
add_card "\$ * Create and compile DEPS.FOR"
add_card "\$ EDIT DEPS.FOR"
add_file "DEPS.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=DEPS.FOR"
add_card "*OBJECT=DEPS.OBJ"
add_card "*LINK"
add_card "\$"

# PARSE
add_card "\$ * Create and compile PARSE.FOR"
add_card "\$ EDIT PARSE.FOR"
add_file "PARSE.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=PARSE.FOR"
add_card "*OBJECT=PARSE.OBJ"
add_card "*LINK"
add_card "\$"

# EVAL
add_card "\$ * Create and compile EVAL.FOR"
add_card "\$ EDIT EVAL.FOR"
add_file "EVAL.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=EVAL.FOR"
add_card "*OBJECT=EVAL.OBJ"
add_card "*LINK"
add_card "\$"

# RECALC
add_card "\$ * Create and compile RECALC.FOR"
add_card "\$ EDIT RECALC.FOR"
add_file "RECALC.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=RECALC.FOR"
add_card "*OBJECT=RECALC.OBJ"
add_card "*LINK"
add_card "\$"

# Layer 2
echo "Layer 2:"
add_card "\$ * Create and compile UI.FOR"
add_card "\$ EDIT UI.FOR"
add_file "UI.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=UI.FOR"
add_card "*OBJECT=UI.OBJ"
add_card "*LINK"
add_card "\$"

# DISPLAY
add_card "\$ * Create and compile DISPLAY.FOR"
add_card "\$ EDIT DISPLAY.FOR"
add_file "DISPLAY.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=DISPLAY.FOR"
add_card "*OBJECT=DISPLAY.OBJ"
add_card "*LINK"
add_card "\$"

# MSG
add_card "\$ * Create and compile MSG.FOR"
add_card "\$ EDIT MSG.FOR"
add_file "MSG.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=MSG.FOR"
add_card "*OBJECT=MSG.OBJ"
add_card "*LINK"
add_card "\$"

# Layer 3
echo "Layer 3:"
add_card "\$ * Create and compile TERMCPV.FOR"
add_card "\$ EDIT TERMCPV.FOR"
add_file "TERMCPV.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=TERMCPV.FOR"
add_card "*OBJECT=TERMCPV.OBJ"
add_card "*LINK"
add_card "\$"

# Main program
echo "Main program:"
add_card "\$ * Create and compile XLMAIN.FOR"
add_card "\$ EDIT XLMAIN.FOR"
add_file "XLMAIN.FOR"
add_card ":FILE"
add_card ":QUIT"
add_card "\$"
add_card "\$ RUN FORTRAN"
add_card "*SOURCE=XLMAIN.FOR"
add_card "*OBJECT=XLMAIN.OBJ"
add_card "*LIBRARY=STRUTIL,CELLS,DEPS,PARSE,EVAL,RECALC,UI,DISPLAY,MSG,TERMCPV"
add_card "*LINK"
add_card "*MAP"
add_card "*GO"
add_card "\$"

# Save executable
add_card "\$ * Save executable"
add_card "\$ SAVE XL"
add_card "\$"

# End of job
add_card "\$ EOJ"

echo ""
echo "✓ Card deck created: $DECK_FILE"
echo ""

# Show statistics
CARDS=$(wc -l < "$DECK_FILE")
SIZE=$(wc -c < "$DECK_FILE")
SIZE_KB=$((SIZE / 1024))

echo "Deck statistics:"
echo "  Cards: $CARDS"
echo "  Size:  $SIZE_KB KB ($SIZE bytes)"
echo ""
echo "This simulates a stack of $CARDS punched cards!"
echo ""
echo "To submit to CP-V:"
echo "  1. Attach card reader to deck file"
echo "  2. System will read and execute cards"
echo "  3. XL will be compiled and ready to run"
echo ""
echo "See CARD_DEPLOYMENT.md for instructions"
