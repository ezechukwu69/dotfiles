#!/bin/bash

# Must be run inside tmux
if [ -z "$TMUX" ]; then
  echo "You must run this inside a tmux session."
  exit 1
fi

# Get the current client TTY (to resume from the popup)
TMUX_CLIENT=$(tmux display-message -p '#{client_tty}')

tmux popup -E -w 80% -h 60% -T "Pick directory for new session" '
  selected=$(find ${1:-$HOME} -type d 2>/dev/null | fzf --prompt="Directory: ")
  if [ -n "$selected" ]; then
    session_name=$(basename "$selected" | tr "." "_" | tr -cd "[:alnum:]_")
    tmux new-session -d -s "$session_name" -c "$selected"
    # Switch client to the new session
    tmux switch-client -t "$session_name"
  # else
  #   echo "Cancelled. Press any key to close..."
  #   read -n 1 -s
  fi
'
