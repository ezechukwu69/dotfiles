#!/bin/bash

# Use a temporary file for yazi output
temp_file=$(mktemp)

# Run yazi and redirect output to the temporary file
yazi ~/wallpaper --chooser-file $temp_file

# Check if yazi executed successfully
if [ $? -eq 0 ]; then
  # Set the file picked variable to the temporary file
  feh --bg-scale $(cat "$temp_file")
  cat ~/.fehbg | awk '{print $4}' | xargs wal -i
else
  rm "$temp_file"
  exit 1
fi

# Clean up the temporary file after use (optional, depending on your needs)
rm "$temp_file"

