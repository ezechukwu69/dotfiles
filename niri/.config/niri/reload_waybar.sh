#!/bin/sh

killall waybar
waybar -c ~/.config/waybar-niri/config -s ~/.config/waybar-niri/style.css &
