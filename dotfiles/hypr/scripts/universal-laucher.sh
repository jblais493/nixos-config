#!/usr/bin/env sh

workspace_id=$(hyprctl clients -j | jq '.[] | select(.class | contains("Emacs")) | .workspace.id' | head -n1)
if [ -n "$workspace_id" ]; then
    hyprctl dispatch workspace "$workspace_id"
    sleep 0.1
    emacsclient -n -e '(progn (select-frame-set-input-focus (selected-frame)) (universal-launcher-popup))'
fi
