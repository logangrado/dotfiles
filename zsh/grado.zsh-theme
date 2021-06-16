#!/bin/bash
# function git_current_branch_remote() {
#     echo $(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2> /dev/null)
# }


# function git_behind() {
#     n_behind=$(git rev-list --ancestry-path $(git_current_branch)..$(git_current_branch_remote) | wc -w) || $(echo 0)
#     n_behind=$(echo $n_behind | sed 's/ //g')
#     echo $n_behind
# }

# function git_ahead() {
#     n_ahead=$(git rev-list --ancestry-path $(git_current_branch_remote)..$(git_current_branch) | wc -w) || $(echo 0)
#     n_ahead=$(echo $n_ahead | sed 's/ //g')
#     echo $n_ahead
# }

function git_diverged() {
    regex=".*\ ([0-9]*)\ and\ ([0-9]*)\ different\ commits\ each,\ respectively\."
    if [[ $(git status) =~ $regex ]]; then
        AHEAD="${match[1]}"
        BEHIND="${match[2]}"
        #echo "${ZSH_THEME_GIT_PROMPT_AHEAD}${AHEAD}${ZSH_THEME_GIT_PROMPT_BEHIND}${BEHIND}"
        echo $AHEAD $BEHIND
    fi
}

function git_ahead() {
    regex="Your\ branch\ is\ ahead\ of\ .*\ by\ ([0-9]*)\ commit.*"
    if [[ $(git status) =~ $regex ]]; then
        AHEAD="${match[1]}"
        echo $AHEAD
    fi
}

function git_behind() {
    regex="Your\ branch\ is\ behind\ .*\ by\ ([0-9]*)\ commit.*"
    if [[ $(git status) =~ $regex ]]; then
        BEHIND="${match[1]}"
        echo "${ZSH_THEME_GIT_PROMPT_BEHIND}${BEHIND}"
    fi
}

function git_ahead_behind_diverged() {
    if [ -n "$(git_diverged)" ]; then
        DIVERGED=($(git_diverged))
        echo "${ZSH_THEME_GIT_PROMPT_DIVERGED_COLOR}${ZSH_THEME_GIT_PROMPT_AHEAD_SYMBOL}${DIVERGED[1]}${ZSH_THEME_GIT_PROMPT_BEHIND_SYMBOL}${DIVERGED[2]}"
    fi
        
    if [ -n "$(git_ahead)" ]; then
        echo "${ZSH_THEME_GIT_PROMPT_AHEAD_COLOR}${ZSH_THEME_GIT_PROMPT_AHEAD_SYMBOL}$(git_ahead)"
    fi
        
    if [ -n "$(git_behind)" ]; then
        echo "${ZSH_THEME_GIT_PROMPT_BEHIND_COLOR}${ZSH_THEME_GIT_PROMPT_BEHIND_SYMBOL}$(git_behind)"
    fi
}

function my_git_prompt() {
  tester=$(git rev-parse --git-dir 2> /dev/null) || return
  
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""

  # Are we ahead/behind/diverged?
  if [ -n "$(git_ahead_behind_diverged)" ]; then
      STATUS="${STATUS}$(git_ahead_behind_diverged)"
  fi

  
  # is anything untracked?
  if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
    STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_UNTRACKED"
  fi

  # is anything unstaged?
  if $(echo "$INDEX" | command grep -E -e '^[ MARC][MD] ' &> /dev/null); then
    STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_UNSTAGED"
  fi

  # is anything staged?
  if $(echo "$INDEX" | command grep -E -e '^(D[ M]|[MARC][ MD]) ' &> /dev/null); then
    STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_STAGED"
  fi
  
  # # is branch ahead?
  # if $(echo "$(git log origin/$(git_current_branch)..HEAD 2> /dev/null)" | grep '^commit' &> /dev/null); then
  #   STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_AHEAD"
  # fi

  # is anything unmerged?
  if $(echo "$INDEX" | command grep -E -e '^(A[AU]|D[DU]|U[ADU]) ' &> /dev/null); then
    STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_UNMERGED"
  fi

  if [[ -n $STATUS ]]; then
    STATUS=" $STATUS"
  fi
  
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX$(my_current_branch)$STATUS$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

function my_current_branch() {
  echo $(git_current_branch || echo "(no branch)")
}

function ssh_connection() {
  if [[ -n $SSH_CONNECTION ]]; then
    echo "%{$fg_bold[red]%}(ssh) "
  fi
}

# This function returns the venv's actual name
function get_virtualenv_name() {
  if [ ! -z "${VIRTUAL_ENV-}" ]; then
      ENV=$(basename $VIRTUAL_ENV)
      #ENV=$(basename $(dirname $VIRTUAL_ENV))
      echo "[$ENV]"
  fi
}

# This function just returns "(pyenv) " if there is an active pyenv
function get_virtualenv_indicator() {
  if [ ! -z "${VIRTUAL_ENV-}" ]; then
     echo "(pyenv) "
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=true


local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

# BEGIN PROMPT
#==============================================================
PS1='┌ %{$fg[green]%}%n\
%{$reset_color%}@\
%{$fg[green]%}%m\
%{$fg[cyan]%} $(get_virtualenv_name)
%{$reset_color%}└ \
%{$fg[red]%}%c\
$(my_git_prompt) %{$fg[red]%}%(!.#.»)%{$reset_color%} '

#PROMPT2='%{$fg[red]%}\ %{$reset_color%}'

RPS1='${return_code} %{$fg[blue]%}%~%{$reset_color%} [%*]'

# Git prompt variables
#==============================================================
ZSH_THEME_GIT_PROMPT_PREFIX=" %{$reset_color%}%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}%{$fg[yellow]%})"

ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[red]%} ✔%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$fg[yellow]%}"

ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$reset_color%}●"
ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg[red]%}●"     
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[cyan]%}●"      
ZSH_THEME_GIT_PROMPT_AHEAD_SYMBOL="↑"
ZSH_THEME_GIT_PROMPT_BEHIND_SYMBOL="↓"
ZSH_THEME_GIT_PROMPT_AHEAD_COLOR="%{$fg[cyan]%}"
ZSH_THEME_GIT_PROMPT_BEHIND_COLOR="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_DIVERGED_COLOR="%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[red]%}§"

#↑
#⇅


# Unused symbols ◒ ✚ ✔ ● ✘

TMOUT=1
TRAPALRM() {
	   zle reset-prompt
	   }
