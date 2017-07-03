#!/bin/bash

DOT_DIR=$PWD
DATE_TIME=$(date '+%Y-%m-%d_%H:%M:%S')
BACKUP_DIR="$DOT_DIR/backups/$DATE_TIME"

FILES="zshrc tmux.conf emacs.d"

# create dotfiles.bak
echo "Making backup directory: $BACKUP_DIR"
mkdir -p $BACKUP_DIR

for f in $FILES; do
    echo "  backing up $f"
    mv ~/.$f $BACKUP_DIR/
    echo "  symlinking $f from $DOT_DIR to ~/"
    ln -s $DOT_DIR/.$f ~/.$f
done
echo "Done"
