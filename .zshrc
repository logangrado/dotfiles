# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# PATH
#=====================
export PATH=$HOME/.bin:$PATH       # Personal bin

# Yarn (package manaager installed by Brew)

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

alias latexbuild='latexmk -pdf -pvc --outdir=build'

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
alias gitrm='git ls-files --deleted -z | xargs -0 git rm'
alias gitlog2='git log --oneline --graph --color --all --decorate | reverse_log.py'
#alias gitlog='git log --graph --branches --all --color --decorate --date=format:"%Y-%m-%d %H:%M:%S" --pretty=format:"%C(auto)%h%Creset%C(auto)%d%Creset %s %C(bold yellow)(%cd) %C(bold green)<%an>%Creset"' '| reverse_log.py'
alias gitlog='git log --oneline --graph --branches --all --color --decorate --date=format:"%y-%m-%d %H:%M" --pretty=format:"%C(auto)%h%Creset%C(auto)%d%Creset %s %C(#323232)(%cd, %an)%Creset" | reverse_log.py'

alias openscad='/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD'

setopt nosharehistory
bindkey "\eOA" up-line-or-history
bindkey "\eOB" down-line-or-history

# SOURCING
#======================================================
# source all files in .dotfiles/scripts
for f in $HOME/.dotfiles/scripts/*.sh; do
    source $f
done
