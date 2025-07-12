#!/bin/bash

# getting the list of files in user run directory by running neovim lua command
#
result=$(nvim --headless -c 'lua print(vim.fn.stdpath("run"))' -c 'quit' 2>&1 | tail -n 1)

build_glob=$result/nvim*

selection=$(ls $build_glob | fzf)

if [ -z "$selection" ]; then
    echo "No selection made"
    exit
fi

nvim --remote-ui --server $selection 

