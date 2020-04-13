# This is noecessary to avoid soem new error on the new work mac
ZSH_DISABLE_COMPFIX=true

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# PATH
#=========================================================
export PATH=$HOME/.dotfiles/bin:$PATH     # Personal bin
export PATH=/usr/local/sbin:$PATH          # Homebrew sbin

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
export EDITOR='emacs -nw'

# ALIASES
#======================================================
alias emacs='emacs -nw'

alias ls='ls -CFG'
alias lsa='ls -CFGa'
alias del='rmtrash'
alias g++='g++ -std=c++11'

#python aliases
#alias python='python3'
#alias pip='pip3'
#alias ipython='ipython3'
#alias pylab='ipython3 --pylab'

alias pyenv='pipenv run python'
alias ipyenv='pipenv run ipython'

alias activate="pipenv shell"
export PIPENV_VENV_IN_PROJECT=1   # Place pipenv virtualenv's in current directory
export PIPENV_SKIP_LOCK=1

#git aliases
alias grm='git ls-files --deleted -z | xargs -0 git rm'
alias gf='git fetch --all --prune'
alias gitlog2='git log --oneline --graph --color --all --decorate | reverse_log.py'
alias gitlog='git log --oneline --graph --branches --all --color --decorate --date=format:"%y-%m-%d %H:%M" --pretty=format:"%C(auto)%h%Creset%C(auto)%d%Creset %s %C(#505050)(%cd, %an)%Creset" | $HOME/.dotfiles/scripts/reverse_log.py'

#google cloud
alias gci="gcloud compute instances"

setopt nosharehistory
bindkey "\eOA" up-line-or-history
bindkey "\eOB" down-line-or-history

# SOURCING
#======================================================
# source all files in .dotfiles/scripts
for f in $HOME/.dotfiles/scripts/*.sh; do
    source $f
done

# OS-Specific Config
#======================================================
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)        
        alias ls='ls -CFG --color'
        DIST=$(cat /etc/*-release)
        if echo $DIST | grep -q debian; then
            export LS_COLORS="$LS_COLORS:ow=1;34:tw=1;34:"
            export DISPLAY=localhost:0.0
        fi
        ;;
    Darwin*) ;;
    CYGWIN*)
        alias ls='ls -CFG --color'
        ;;
    MINGW*)  ;;
esac
    
# Computer specific config
#======================================================
case $HOST in
    "rpic_00")
        export CLUSTER_HOME=~/rpi-cluster
        PATH=$PATH:$CLUSTER_HOME/bin
        ;;
    "grado_mpb")
        # MSI aliases
        alias msi='ssh msi'
        alias mesabi='ssh msi -t ssh mesabi'
        alias itasca='ssh msi -t ssh itasca'
        alias msilab='ssh msi -t ssh lab'
esac
# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/grado/Applications/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/grado/Applications/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/grado/Applications/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/grado/Applications/google-cloud-sdk/completion.zsh.inc'; fi
