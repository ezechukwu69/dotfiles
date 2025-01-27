#!/bin/sh

fish -c "$HOME/.cargo/bin/kanata -c $HOME/.config/kanata/config.kbd &"
fish -c "gBar bar 0 &"
fish -c "gBar bar 1 &"
sleep 5
zsh -c "waypaper --restore &"
