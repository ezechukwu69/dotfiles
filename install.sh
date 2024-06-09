#!/bin/bash

create_dotfiles() {
    # get current script directory
    script_dir=$(pwd)

    # get list of all folders in the current directory starting with a dot
    files=$(find . -maxdepth 1 -type f -name ".*")

    for file in $files; do
        name=$(basename $file)
        echo "Creating symlink for $name in home directory"
        rm -rf ~/$name
        ln -s $script_dir/$name ~/$name
    done

    # check if the folder nvim exists and it exists in ~/.config/nvim also
    if [ ! -d "nvim" && ! -d "~/.config/nvim" ]; then
        echo "Creating .config/nvim folder"
        git clone https://github.com/ezechukwu69/micro-nvim.git nvim
        mkdir -p ~/.config
        ln -s $script_dir/nvim ~/.config
    fi

    if [ -d "nvim" && ! -d "~/.config/nvim" ]; then
        echo "Creating .config/nvim folder"
        mkdir -p ~/.config
        ln -s $script_dir/nvim ~/.config
    fi
}

create_dotfiles
