#!/bin/bash
# Create tape image for file transfer to CP-V

set -e

if [ $# -lt 1 ]; then
    echo "Usage: $0 <file1.FOR> [file2.FOR ...]"
    exit 1
fi

WORK_DIR="$(dirname "$0")/../work"
TAPE_IMAGE="$WORK_DIR/transfer.tap"

echo "Creating tape image: $TAPE_IMAGE"

# simh tap tool
echo "Files:" "$@"

# Simple text file concatenation (CP-V can read ASCII tape)
: > "$TAPE_IMAGE"

for file in "$@"; do
    echo "Adding: $file"
    cat "$file" >> "$TAPE_IMAGE"
done

echo "✓ Tape created: $TAPE_IMAGE"
