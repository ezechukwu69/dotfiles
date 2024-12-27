#!/bin/bash

nm-applet &
blueman-applet &
dunst &
xfce4-power-manager &
picom --corner-radius 12 --transparent-clipping --blur-size 0 --shadow-exclude 'bounding_shaped && !rounded_corners' --shadow-opacity 0 --no-fading-openclose &
xrandr --output DP-1 --auto --right-of HDMI-2
nitrogen --restore &
killall conky &
conky &
xrandr --output DP-1 --right-of HDMI-2
~/.config/i3/launch_polybar.sh &
