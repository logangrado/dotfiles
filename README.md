# Grado's Dotfiles

## Install and link

The script `install_osx.sh` installs things like homebrew, various utilities (git, emacs, etc), zsh, and tmux. `link.sh` links dotfiles, themes, etc into the appropriate locations.

```shell
$ ./install_osx.sh
$ ./link.sh
```

## Extras

#### TMUX
Download TMUX [here](https://github.com/tmux/tmux/wiki)

> `install_osx.sh` should already have installed tmux.

#### Solarized Theme
Download OSX Terminal Theme [here](https://github.com/tomislav/osx-terminal.app-colors-solarized)  

> The solarized theme for iTerm2 is already included

Download emacs theme [here](https://github.com/sellout/emacs-color-theme-solarized)  

> The solarized theme for emacs is already included

## TODO
- [ ] Add git commit message
- [ ] Add .gitignore_global
    - [ ] Add .gitignore_global to linking script
- [ ] Put linked files/directories in their own folder

## Done
- [x] Document where to get emacs theme
- [x] Document where to get terminal theme
- [x] Figure out how to put the emacs theme in to my dotfiles repo
- [x] Figure out weird colors on line-numbers
- [x] Figure out why emacs is highlighting things in .md files (and fix it)
- [x] Add oh-my-zsh theme

## MaybeDo
- Ony have init.el and use-package.el? 
