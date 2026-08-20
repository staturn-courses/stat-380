#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

for file in "$ROOT_DIR"/old/Notes/*.pptx; do
    [ -e "$file" ] || continue

    filename="$(basename "${file%.pptx}")"
    quarto pandoc "$file" -t markdown -o "$SCRIPT_DIR/$filename.qmd"
done