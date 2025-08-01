#!/usr/bin/env zsh

grim -g "$(slurp)" - | tee ~/Pictures/$(date +'%Y-%m-%d-%H%M%S_grim.png') | wl-copy
