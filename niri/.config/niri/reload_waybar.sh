#!/bin/sh

killall waybar
waybar -c ~/.config/waybar-niri/config.jsonc -s ~/.config/waybar-niri/styles.css &
