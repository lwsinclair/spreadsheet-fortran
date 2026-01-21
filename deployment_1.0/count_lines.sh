#!/bin/bash
# Count lines by category for FORTRAN files
# Categories: Active code, Comments, Blank

count_file() {
    local file=$1
    local name=$(basename "$file")

    local total=$(wc -l < "$file")
    local blank=$(grep -c '^[[:space:]]*$' "$file")
    local comment=$(grep -c '^[Cc*]' "$file")
    local active=$((total - blank - comment))

    printf "%-20s %6d %6d %6d %6d\n" "$name" "$active" "$comment" "$blank" "$total"
}

echo "=============================================================================="
echo "XL Spreadsheet Deployment 1.0 - Line Count Analysis"
echo "=============================================================================="
echo ""
printf "%-20s %6s %6s %6s %6s\n" "File" "Active" "Comment" "Blank" "Total"
echo "------------------------------------------------------------------------------"

# Layer 0
echo ""
echo "=== LAYER 0: Utilities ==="
layer0_active=0
layer0_comment=0
layer0_blank=0
layer0_total=0
for f in layer0/*.FOR; do
    count_file "$f"
    total=$(wc -l < "$f")
    blank=$(grep -c '^[[:space:]]*$' "$f")
    comment=$(grep -c '^[Cc*]' "$f")
    active=$((total - blank - comment))
    layer0_active=$((layer0_active + active))
    layer0_comment=$((layer0_comment + comment))
    layer0_blank=$((layer0_blank + blank))
    layer0_total=$((layer0_total + total))
done
printf "%-20s %6d %6d %6d %6d\n" "Layer 0 Subtotal:" "$layer0_active" "$layer0_comment" "$layer0_blank" "$layer0_total"

# Layer 1
echo ""
echo "=== LAYER 1: Engine ==="
layer1_active=0
layer1_comment=0
layer1_blank=0
layer1_total=0
for f in layer1/*.FOR; do
    count_file "$f"
    total=$(wc -l < "$f")
    blank=$(grep -c '^[[:space:]]*$' "$f")
    comment=$(grep -c '^[Cc*]' "$f")
    active=$((total - blank - comment))
    layer1_active=$((layer1_active + active))
    layer1_comment=$((layer1_comment + comment))
    layer1_blank=$((layer1_blank + blank))
    layer1_total=$((layer1_total + total))
done
printf "%-20s %6d %6d %6d %6d\n" "Layer 1 Subtotal:" "$layer1_active" "$layer1_comment" "$layer1_blank" "$layer1_total"

# Layer 2 (only .FOR files, not .excluded)
echo ""
echo "=== LAYER 2: UI & Files ==="
layer2_active=0
layer2_comment=0
layer2_blank=0
layer2_total=0
for f in layer2/*.FOR; do
    count_file "$f"
    total=$(wc -l < "$f")
    blank=$(grep -c '^[[:space:]]*$' "$f")
    comment=$(grep -c '^[Cc*]' "$f")
    active=$((total - blank - comment))
    layer2_active=$((layer2_active + active))
    layer2_comment=$((layer2_comment + comment))
    layer2_blank=$((layer2_blank + blank))
    layer2_total=$((layer2_total + total))
done
printf "%-20s %6d %6d %6d %6d\n" "Layer 2 Subtotal:" "$layer2_active" "$layer2_comment" "$layer2_blank" "$layer2_total"

# Layer 3 - only files used in native build
echo ""
echo "=== LAYER 3: Terminal (Native Build) ==="
layer3_active=0
layer3_comment=0
layer3_blank=0
layer3_total=0
for f in layer3/TERMINAL.FOR layer3/PROTVT100.FOR layer3/IOUNIX.FOR layer3/FIOUNIX.FOR; do
    count_file "$f"
    total=$(wc -l < "$f")
    blank=$(grep -c '^[[:space:]]*$' "$f")
    comment=$(grep -c '^[Cc*]' "$f")
    active=$((total - blank - comment))
    layer3_active=$((layer3_active + active))
    layer3_comment=$((layer3_comment + comment))
    layer3_blank=$((layer3_blank + blank))
    layer3_total=$((layer3_total + total))
done
printf "%-20s %6d %6d %6d %6d\n" "Layer 3 Subtotal:" "$layer3_active" "$layer3_comment" "$layer3_blank" "$layer3_total"

# Main
echo ""
echo "=== MAIN PROGRAM ==="
main_active=0
main_comment=0
main_blank=0
main_total=0
for f in XLMAIN.FOR BRIDGE.FOR; do
    count_file "$f"
    total=$(wc -l < "$f")
    blank=$(grep -c '^[[:space:]]*$' "$f")
    comment=$(grep -c '^[Cc*]' "$f")
    active=$((total - blank - comment))
    main_active=$((main_active + active))
    main_comment=$((main_comment + comment))
    main_blank=$((main_blank + blank))
    main_total=$((main_total + total))
done
printf "%-20s %6d %6d %6d %6d\n" "Main Subtotal:" "$main_active" "$main_comment" "$main_blank" "$main_total"

# Grand total
echo ""
echo "=============================================================================="
grand_active=$((layer0_active + layer1_active + layer2_active + layer3_active + main_active))
grand_comment=$((layer0_comment + layer1_comment + layer2_comment + layer3_comment + main_comment))
grand_blank=$((layer0_blank + layer1_blank + layer2_blank + layer3_blank + main_blank))
grand_total=$((layer0_total + layer1_total + layer2_total + layer3_total + main_total))

printf "%-20s %6d %6d %6d %6d\n" "GRAND TOTAL:" "$grand_active" "$grand_comment" "$grand_blank" "$grand_total"
echo ""

# Calculate pages (27 lines/page double-spaced)
active_pages=$(echo "scale=0; ($grand_active + 26) / 27" | bc)
total_pages=$(echo "scale=0; ($grand_total + 26) / 27" | bc)
comment_pct=$(echo "scale=1; $grand_comment * 100 / $grand_total" | bc)
active_pct=$(echo "scale=1; $grand_active * 100 / $grand_total" | bc)

echo "=== SUMMARY ==="
echo "Active code lines:  $grand_active ($active_pct%)"
echo "Comment lines:      $grand_comment ($comment_pct%)"
echo "Blank lines:        $grand_blank"
echo ""
echo "Pages (active only, 27 lines/page double-spaced): $active_pages"
echo "Pages (all lines):  $total_pages"
echo ""

# Excluded files
echo "=== EXCLUDED FILES (Dead Code) ==="
if [ -f layer2/COMMANDS.FOR.excluded ]; then
    exc_total=$(wc -l < layer2/COMMANDS.FOR.excluded)
    exc_blank=$(grep -c '^[[:space:]]*$' layer2/COMMANDS.FOR.excluded)
    exc_comment=$(grep -c '^[Cc*]' layer2/COMMANDS.FOR.excluded)
    exc_active=$((exc_total - exc_blank - exc_comment))
    printf "%-20s %6d %6d %6d %6d\n" "COMMANDS.FOR" "$exc_active" "$exc_comment" "$exc_blank" "$exc_total"
    echo "(Not compiled - dead code removed from build)"
fi
