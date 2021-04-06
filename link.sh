#!/bin/bash

ALL=("zsh/zshrc $HOME/.zshrc"
     "zsh/zshrc-work $HOME/.zshrc-work"
     "zsh/grado.zsh-theme $HOME/.oh-my-zsh/themes/grado.zsh-theme"
     "tmux/tmux.conf $HOME/.tmux.conf"                
     "emacs.d $HOME/.emacs.d"
     "git/gitconfig $HOME/.gitconfig"
     "git/gitignore_global $HOME/.gitignore_global"
     "git/gitmessage $HOME/.gitmessage"
     "matplotlib/matplotlibrc $HOME/.matplotlib/matplotlibrc"
    )

OSX=("tmux/tmux-darwin.conf $HOME/.tmux-darwin.conf"
     "iterm2/com.googlecode.iterm2.plist $HOME/Library/Preferences/com.googlecode.iterm2.plist"
    )

# Determine OS
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)  ;;
    Darwin*) ALL=("${ALL[@]}" "${OSX[@]}");;
    CYGWIN*) ;;
    MINGW*)  ;;
esac

DOT_DIR=$PWD
DATE_TIME=$(date '+%Y-%m-%d')
BACKUP_DIR="$DOT_DIR/backups/$DATE_TIME"

# create dotfiles.bak
echo -e "Making backup directory...\n  $BACKUP_DIR"
mkdir -p $BACKUP_DIR

echo "Linking..."

for PAIR in "${ALL[@]}"; do
    IFS=' ' read -a FROMTO <<< "$PAIR"
    FROM=$DOT_DIR/${FROMTO[0]}
    TO=${FROMTO[1]}

    if [[ -f $FROM ]]; then
        echo $TO
        if [ -L $TO ]; then
            rm $TO
            echo "  old symlink removed"
        elif [[ -f $TO || -d $TO ]]; then
            mv $TO $BACKUP_DIR
            echo "  old file copied to backup directory"
        fi

        if [[ ! -d $(dirname $TO) ]]; then
            mkdir -p $(dirname $TO)
        fi
        ln -s $FROM $TO
        if [ $? -eq 0 ]; then
            echo "  symlink created"
        fi
        echo
    fi
done
exit
