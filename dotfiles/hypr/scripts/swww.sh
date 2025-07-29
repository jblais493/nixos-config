#!/usr/bin/env sh

# Configuration
WALLPAPER_DIR="$HOME/Pictures/Wallpapers"
SWWW_TRANSITION="simple"
SWWW_TRANSITION_STEP=2
SWWW_TRANSITION_FPS=60
SWWW_TRANSITION_DURATION=1
CACHE_DIR="$HOME/.cache/wallpaper-previews"

# Create cache directory if it doesn't exist
mkdir -p "$CACHE_DIR"

# Find all image files with valid extensions only
IMAGES=$(find "$WALLPAPER_DIR" -type f \( -name "*.jpg" -o -name "*.jpeg" -o -name "*.png" -o -name "*.gif" \) | sort)

if [ -z "$IMAGES" ]; then
    echo "No image files found in $WALLPAPER_DIR"
    exit 1
fi

# Generate preview images
for img in $IMAGES; do
    if [ ! -f "$img" ]; then
        continue
    fi
    filename=$(basename "$img")
    preview="$CACHE_DIR/$filename"

    # Only create preview if it doesn't exist or source image is newer
    if [ ! -f "$preview" ] || [ "$img" -nt "$preview" ]; then
        magick "$img" -resize 200x200 "$preview"
    fi
done

# Create a temporary file for wofi menu
WOFI_INPUT=$(mktemp)

# Populate the wofi input file with proper formatting
for img in $IMAGES; do
    if [ ! -f "$img" ]; then
        continue
    fi
    filename=$(basename "$img")
    preview="$CACHE_DIR/$filename"

    # Skip if preview doesn't exist
    if [ ! -f "$preview" ]; then
        continue
    fi

    # Format for wofi with icon
    echo -e "$filename\0icon\x1f$preview" >>"$WOFI_INPUT"
done

# Check if we have any valid entries
if [ ! -s "$WOFI_INPUT" ]; then
    echo "No valid wallpapers found to display"
    rm "$WOFI_INPUT"
    exit 1
fi

# Show wofi menu with previews
SELECTED_NAME=$(wofi --dmenu -i --width 500 --height 500 --cache-file /dev/null --prompt "Select wallpaper:" --show-icons <"$WOFI_INPUT")

# Clean up the temporary file
rm "$WOFI_INPUT"

# Exit if nothing was selected
if [ -z "$SELECTED_NAME" ]; then
    exit 0
fi

# Find the full path from the selection
SELECTED_WALLPAPER=""
for img in $IMAGES; do
    filename=$(basename "$img")
    if [ "$filename" = "$SELECTED_NAME" ]; then
        SELECTED_WALLPAPER="$img"
        break
    fi
done

if [ -z "$SELECTED_WALLPAPER" ]; then
    echo "Could not find the selected wallpaper path"
    exit 1
fi

# Set the selected wallpaper (daemon will start automatically if not running)
swww img "$SELECTED_WALLPAPER" \
    --transition-type "$SWWW_TRANSITION" \
    --transition-step "$SWWW_TRANSITION_STEP" \
    --transition-fps "$SWWW_TRANSITION_FPS" \
    --transition-duration "$SWWW_TRANSITION_DURATION"

# Save the selection for next boot (optional)
echo "$SELECTED_WALLPAPER" >"$HOME/.cache/current_wallpaper"

echo "Wallpaper set successfully"
