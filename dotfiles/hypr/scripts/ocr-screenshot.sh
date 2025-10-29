#!/usr/bin/env sh

set -euo pipefail

temp_img=$(mktemp --suffix=.png)
temp_ocr=$(mktemp --suffix=.md)
trap "rm -f $temp_img $temp_ocr" EXIT

grim -g "$(slurp)" "$temp_img"
gowall ocr "$temp_img" -s tes --output "$temp_ocr"
wl-copy <"$temp_ocr"

notify-send "OCR Screenshot" "Text extracted to clipboard"
