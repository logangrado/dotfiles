#!/bin/bash

function install_osx {
    #Install brew
    if ! type "brew"; then
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi
    
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
    brew install zsh

    # Set zsh as default
    sudo -s 'echo /usr/local/bin/zsh >> /etc/shells'
    chsh -s /usr/local/bin/zsh
}

function install_cygwin {
    # Install apt-cyg
    if ! type "apt-cyg"; then
        wget https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg -O /bin/apt-cyg
        chmod +x /bin/apt-cyg
    fi
    
    # Install apt-cyg things
    apt-cyg install emacs
    apt-cyg install git
    apt-cyg install tmux
    apt-cyg install python
    apt-cyg install python3
    apt-cyg install zsh

    # Set zsh as default
    mkpasswd > /etc/passwd  # Make passwd file
    sed -i "s&$HOME:/bin/bash&$HOME:/bin/zsh&g" /etc/passwd  # Change default shell for user
}

# Determine OS
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)  ;;
    Darwin*) install_osx;;
    CYGWIN*) install_cygwin;;
    MINGW*)  ;;
esac

# Install and update submodules (to get hidehsow-orgmode)
git submodule init
git submodule update

# oh-my-zsh
if [ ! -d "$HOME/.oh-my-zsh" ]; then
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi
    
# Install TPM (tmux plugin manager)
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
echo "Don't forget to install TMUX plugins by running \`prefix+I\`"
