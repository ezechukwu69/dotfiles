#!/bin/bash

# check if ~/.supermaven/agent file does not exists
if [ ! -f ~/.supermaven/agent ]; then
  url="https://supermaven.com/api/download-path-v2?platform=linux&arch=x86_64&editor=neovim"
  # get download path
  path=$(curl -s $url)
  # try fetch downloadUrl from json returned and return error if not found
  downloadUrl=$(echo $path | jq -r '.downloadUrl')
  if [ -z "$downloadUrl" ]; then
    echo "Error: Failed to fetch downloadUrl from $url"
    exit 1
  fi
  # download supermaven agent
  curl -L $downloadUrl -o ~/.supermaven/agent
  # make the file executable
  chmod +x ~/.supermaven/agent

fi
