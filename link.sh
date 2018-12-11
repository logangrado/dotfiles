#!/bin/bash

declare -a ARR=(".zshrc $HOME/.zshrc"
                ".tmux.conf $HOME/.tmux.conf"
                ".tmux-darwin.conf $HOME/.tmux-darwin.conf"
                ".emacs.d $HOME/.emacs.d"
                "grado.zsh-theme $HOME/.oh-my-zsh/themes/grado.zsh-theme"
                ".gitconfig $HOME/.gitconfig"
                ".gitignore_global $HOME/.gitignore_global"
                ".gitmessage $HOME/.gitmessage"
               )

DOT_DIR=$PWD
DATE_TIME=$(date '+%Y-%m-%d_%H:%M:%S')
BACKUP_DIR="$DOT_DIR/backups/$DATE_TIME"

# create dotfiles.bak
echo "Making backup directory: $BACKUP_DIR"
mkdir -p $BACKUP_DIR

for PAIR in "${ARR[@]}"; do
    IFS=' ' read -a FROMTO <<< "$PAIR"
    FROM=$DOT_DIR/${FROMTO[0]}
    TO=${FROMTO[1]}
    
    if [ -e $TO ] || [ -L $TO ]; then
        mv $TO $BACKUP_DIR
    fi        
    
    echo "  backed up and symlinked: $FROM"
    ln -s $FROM $TO
done
exit
