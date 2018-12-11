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
DATE_TIME=$(date '+%Y-%m-%d')
BACKUP_DIR="$DOT_DIR/backups/$DATE_TIME"

# create dotfiles.bak
echo -e "Making backup directory...\n  $BACKUP_DIR"
mkdir -p $BACKUP_DIR

echo "Linking..."

for PAIR in "${ARR[@]}"; do
    IFS=' ' read -a FROMTO <<< "$PAIR"
    FROM=$DOT_DIR/${FROMTO[0]}
    TO=${FROMTO[1]}

    echo $TO
    if [ -L $TO ]; then
        rm $TO
        echo "  old symlink removed"
    elif [ -f $TO ]; then
        mv $TO $BACKUP_DIR
        echo "  old file copied to backup directory"
    fi

    ln -s $FROM $TO
    echo "  symlinked from $FROM"
    echo
done
exit
