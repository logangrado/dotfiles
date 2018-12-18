#!/bin/bash

function install_osx{
    #Install brew
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    #Install brew things
    brew install git
    brew install wget
    brew install curl
    brew install reattach-to-user-namespace
    brew install emacs
    brew install libevent
    brew install tmux
    brew install python
    brew install python2
}

# Determine OS
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)  ;;
    Darwin*) install_osx;;
    CYGWIN*) ;;
    MINGW*)  ;;
esac

# Install and update submodules (to get hidehsow-orgmode)
git submodule init
git submodule update

# zsh
brew install zsh
sudo -s 'echo /usr/local/bin/zsh >> /etc/shells'
chsh -s /usr/local/bin/zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# Install TPM (tmux plugin manager)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
echo "Don't forget to install TMUX plugins by running `prefix+I`"
