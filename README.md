# Grado's Dotfiles

## Install and link

The script `install.sh` installs things like homebrew, various utilities (git, emacs, etc), zsh, and tmux. `link.sh` links dotfiles, themes, etc into the appropriate locations.

```shell
$ ./osx_install.sh
$ ./link.sh
```

## Extras

#### TMUX
Download TMUX [here](https://github.com/tmux/tmux/wiki)

#### Solarized Theme
Download OSX Terminal Theme [here](https://github.com/tomislav/osx-terminal.app-colors-solarized)  

Download emacs theme [here](https://github.com/sellout/emacs-color-theme-solarized)  

**Note**: The solarized theme for emacs should be included in .emacs

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
