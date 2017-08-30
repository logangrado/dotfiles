# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Path to personal bin
#=====================
export PATH=$PATH:$HOME/.bin

# Set ZSH 
#=========================================================
# zsh env variables
ZSH_THEME="grado"

# zsh themes
plugins=(git)

# source zsh
source $ZSH/oh-my-zsh.sh

# Make ls colors work properly
unset LSCOLORS


# Set environment variables
#======================================================
export EDITOR='emacs'


# ALIASES
#======================================================
alias ls='ls -CFG'
alias lsa='ls -CFGa'
alias del='rmtrash'
alias g++='g++ -std=c++11'
#alias emacs='emacs -nw'

setopt nosharehistory
bindkey "\eOA" up-line-or-history
bindkey "\eOB" down-line-or-history

# SOURCING
#======================================================
# source all files in .dotfiles/scripts
for f in $HOME/.dotfiles/scripts/*.sh; do
    source $f
done

# BLACKYNN ONLY
BLACKFYNNRC="$HOME/.blackfynnrc"
if [ -e $BLACKFYNNRC ]; then
    source $BLACKFYNNRC
fi
