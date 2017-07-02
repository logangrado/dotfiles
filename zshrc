# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Set ZSH 
#=========================================================
# Path to your oh-my-zsh installation.
export ZSH=/Users/Grado/.oh-my-zsh

# Set ZSH environment variables
ZSH_THEME="grado"

# Load ZSH plugins
plugins=(git)

# Finally, source oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Make ls colors work properly
unset LSCOLORS
#export CLICOLOR=1 #not sure if this is necessary


# Set environment variables
#=========================================================
export EDITOR='emacs'


# Set aliases
#=========================================================
alias ls='ls -CFG'
alias lsa='ls -CFGa'
