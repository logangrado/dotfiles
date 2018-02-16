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

alias clear='printf "\n%.0s" {1..50} && clear'

#MSI
alias msi='ssh msi'
alias mesabi='ssh msi -t ssh mesabi'
alias itasca='ssh msi -t ssh itasca'
alias msilab='ssh msi -t ssh lab'

#python aliases
alias python='python3'
alias pip='pip3'
alias ipython='ipython3'
alias pylab='ipython3 --pylab'

#git aliases
alias gitlog='git log --oneline --graph --color --all --decorate'
alias gitrm='git ls-files --deleted -z | xargs -0 git rm'

setopt nosharehistory
bindkey "\eOA" up-line-or-history
bindkey "\eOB" down-line-or-history

# SOURCING
#======================================================
# source all files in .dotfiles/scripts
for f in $HOME/.dotfiles/scripts/*.sh; do
    source $f
done
