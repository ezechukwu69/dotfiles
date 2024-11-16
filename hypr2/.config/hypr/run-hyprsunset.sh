#!/bin/bash

# Check if 'hyprsunset' is running
pid=$(pgrep -x 'hyprsunset')

echo "PID: $pid"

if [ -n "$pid" ]; then
  # If it's running, kill it
  echo "Terminating hyprsunset process with PID $pid..."
  kill "$pid"
else
  # If it's not running, start it
  echo "Starting hyprsunset..."
  hyprsunset &
fi
