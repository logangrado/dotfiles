#!/bin/bash
function my_git_prompt() {
  tester=$(git rev-parse --git-dir 2> /dev/null) || return
  
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""

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

  # is branch ahead?
  if $(echo "$(git log origin/$(git_current_branch)..HEAD 2> /dev/null)" | grep '^commit' &> /dev/null); then
    STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_AHEAD"
  fi

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
      ENV=$(basename $(dirname $VIRTUAL_ENV))
      echo "($ENV) "
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
%{$fg[green]%}%m
%{$reset_color%}└ \
$(get_virtualenv_name)\
%{$fg[red]%}%c\
$(my_git_prompt) %{$fg[red]%}%(!.#.»)%{$reset_color%} '

PROMPT2='%{$fg[red]%}\ %{$reset_color%}'

RPS1='${return_code} %{$fg[blue]%}%~%{$reset_color%} [%*]'



# Git prompt variables
#==============================================================
ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[yellow]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}%{$fg[yellow]%})"

ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[red]%} ✔%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$fg[yellow]%}"

ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$reset_color%}●"
ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg[red]%}●"     #✘
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[cyan]%}●"      #✚✔● 
ZSH_THEME_GIT_PROMPT_AHEAD="%{$fg[magenta]%}↑"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[red]%}✕"

TMOUT=1
TRAPALRM() {
	   zle reset-prompt
	   }
