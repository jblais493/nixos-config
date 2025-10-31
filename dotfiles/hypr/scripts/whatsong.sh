#!/usr/bin/env bash

# Query MPD for current track, handle disconnection gracefully
if ! /run/current-system/sw/bin/mpc current -f '%artist% - %title%' 2>/dev/null; then
    echo ""
fi
