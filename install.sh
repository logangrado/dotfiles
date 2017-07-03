#Install brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#Install brew things
brew install wget
brew install curl
brew install reattach-to-user-namespace
brew install emacs
brew install libevent

#ZSH
brew install zsh
sudo -s 'echo /usr/local/bin/zsh >> /etc/shells'
chsh -s /usr/local/bin/zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

#Install TPM (tmux plugin manager)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
