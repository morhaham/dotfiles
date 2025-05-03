#!/usr/bin/env bash

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

sid="$1"

apps=$(aerospace list-windows --workspace "$sid" | awk -F'|' '{gsub(/^ *| *$/, "", $2); print $2}' | sed '/^\s*$/d')
icon_strip=""

# Hide if no apps
if [ -z "$apps" ]; then
  sketchybar --set space.$sid drawing=off
  exit 0
fi

# Generate icon strip
while read -r app; do
  icon_strip+=" $("$CONFIG_DIR/plugins/icon_map_fn.sh" "$app")"
done <<< "$apps"

# Determine if this is the focused workspace
focused=$(aerospace list-workspaces --focused)
if [ "$sid" = "$focused" ]; then
  sketchybar --set space.$sid \
    drawing=on \
    label="$icon_strip" \
    background.color=0xFF232136 \
    label.shadow.drawing=on \
    icon.shadow.drawing=on \
    background.border_width=2
else
  sketchybar --set space.$sid \
    drawing=on \
    label="$icon_strip" \
    background.color=0x44FFFFFF \
    label.shadow.drawing=off \
    icon.shadow.drawing=off \
    background.border_width=0
fi
