#! /usr/bin/env nix-shell
#! nix-shell -p imagemagick
#! nix-shell -i bash

set -e

readonly OUTPUT_SIZE="320x240"
readonly BKG_COL="black"

readonly FILE="$*"

if [ ! -e "${FILE}" ]; then
    echo "${FILE} doesn't exist." 2>&1
    exit 1
fi

# A 1-bit RGB Colorpalette
readonly PALETTE='/tmp/3bit_palette.gif'

convert xc:black \
  xc:blue \
  xc:lime \
  xc:cyan \
  xc:red \
  xc:magenta \
  xc:yellow \
  xc:white \
  +append "${PALETTE}"

readonly OUTPUT=$(basename "${FILE}" | sed 's/\(.*\)\..*/\1/').mif

# Convert to onebit color of correct size and pipe to
# srec_cat to produce a intel hex file

convert -thumbnail "${OUTPUT_SIZE}" \
        -extent "${OUTPUT_SIZE}"    \
        -gravity center             \
        -background "${BKG_COL}"    \
        -dither FloydSteinberg      \
        -remap "${PALETTE}"         \
        -size "${OUTPUT_SIZE}+0"    \
        -depth 1                    \
        "${FILE}"  RGB: |           \
        bin2mif -w 3 -o "${OUTPUT}"
