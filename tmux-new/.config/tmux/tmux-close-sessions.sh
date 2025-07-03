#!/bin/bash

# Must be run inside tmux
if [ -z "$TMUX" ]; then
  echo "You must run this inside a tmux session."
  exit 1
fi

# Get the current client TTY (to resume from the popup)
TMUX_CLIENT=$(tmux display-message -p '#{client_tty}')

tmux popup -E -w 80% -h 60% -T "Pick session to close" '
  selected=$(tmux list-sessions -F "#{session_name}" | fzf --prompt="Session: ")
  if [ -n "$selected" ]; then
    tmux display-message "Closing $selected..."
    # Switch client to the new session
    tmux kill-session -t "$selected"
  # else
    # echo "Cancelled. Press any key to close..."
    # read -n 1 -s
  fi
'
