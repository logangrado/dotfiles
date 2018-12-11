#!/bin/bash

#Check for folder existence, create if none
if [ ! -d ~/Downloads/Today ]; then
    mkdir ~/Downloads/Today
fi
if [ ! -d ~/Downloads/Yesterday ]; then
    mkdir ~/Downloads/Yesterday
fi
if [ ! -d ~/Downloads/Last\ Week ]; then
    mkdir ~/Downloads/Last\ Week
fi

# Empty trash
rm -rf ~/.Trash/*

# Clean up downloads folder. Move from Today -> Yesterday, Yesterday -> Last Week, and remove old entries in Last Week
# Meant to be run at 4AM
find ~/Downloads/Today/* -Btime +4h -exec mv "{}" ~/Downloads/Yesterday/ \;
find ~/Downloads/Yesterday/* -Btime +1d4h -exec mv "{}" ~/Downloads/Last\ Week/ \;
find ~/Downloads/Last\ Week/* -Btime +7d -exec rm -rf "{}" \;
