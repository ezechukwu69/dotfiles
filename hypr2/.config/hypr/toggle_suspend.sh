#!/bin/bash

# File to store the toggle state
STATE_FILE="/tmp/suspend_toggle_state"

# Check if the state file exists; if not, create it and default to OFF
if [[ ! -f "$STATE_FILE" ]]; then
    echo "OFF" > "$STATE_FILE"
fi

# Read the current state
STATE=$(cat "$STATE_FILE")

if [[ "$STATE" == "OFF" ]]; then
    # Turn suspend prevention ON
    echo "Preventing suspend..."
    nohup systemd-inhibit --what=handle-suspend:handle-lid-switch --why="Preventing suspend via toggle" sleep infinity &
    echo $! > /tmp/inhibit_pid
    echo "ON" > "$STATE_FILE"
    notify-send "Suspend Prevention" "Suspend prevention is now ON."
else
    # Turn suspend prevention OFF
    echo "Allowing suspend..."
    if [[ -f "/tmp/inhibit_pid" ]]; then
        kill "$(cat /tmp/inhibit_pid)" 2>/dev/null
        rm /tmp/inhibit_pid
    fi
    echo "OFF" > "$STATE_FILE"
    notify-send "Suspend Prevention" "Suspend prevention is now OFF."
fi
