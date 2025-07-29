#!/usr/bin/env sh

# Directory containing wallpapers
WALLPAPER_DIR="$HOME/Pictures"

# Use wofi to select a wallpaper
selected_wallpaper=$(ls -1 "$WALLPAPER_DIR" | grep -E "\.(jpg|jpeg|png|gif)$" | wofi --dmenu -p "Select wallpaper")

# If a wallpaper was selected, set it
if [ -n "$selected_wallpaper" ]; then
    # Full path to the selected wallpaper
    wallpaper_path="$WALLPAPER_DIR/$selected_wallpaper"

    # Create or update hyprpaper.conf
    echo "preload = $wallpaper_path" >~/.config/hypr/hyprpaper.conf

    # Get all connected monitors
    monitors=$(hyprctl monitors -j | jq -r '.[].name')

    # Set the wallpaper for each monitor
    for monitor in $monitors; do
        echo "wallpaper = $monitor,$wallpaper_path" >>~/.config/hypr/hyprpaper.conf
    done

    echo "splash = false" >>~/.config/hypr/hyprpaper.conf

    # Restart hyprpaper to apply changes
    pkill hyprpaper
    hyprpaper &
fi
