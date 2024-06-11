#!/bin/bash

main() {
    pkill rofi || rofi -show drun -modi drun,filebrowser,run,window 
}

main
