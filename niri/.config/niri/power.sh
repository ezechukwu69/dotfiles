#!/bin/bash

commandWithDelay() {
  notify-send "$1"
  sleep 2s
}

choice=$(tofi <~/.config/niri/options | awk '{print $1}')

case $choice in
"Dim")
  commandWithDelay "Powering off monitors"
  sleep 2s
  niri msg action power-off-monitors
  ;;
"Lock")
  commandWithDelay "Locking screen"
  hyprlock
  ;;
"Shutdown")
  commandWithDelay "Shutting down"
  systemctl poweroff
  ;;
"Reboot")
  commandWithDelay "Rebooting"
  systemctl reboot
  ;;
"Suspend")
  commandWithDelay "Suspending"
  systemctl suspend
  ;;
"Exit")
  commandWithDelay "Exiting"
  niri msg action quit
  ;;
esac
