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

function git_ahead_behind_diverged() {
    N_AHEAD=$(git rev-list @{u}..HEAD 2> /dev/null | wc -l | xargs)
    N_BEHIND=$(git rev-list HEAD..@{u} 2> /dev/null | wc -l | xargs)

    if [[ $N_AHEAD != "0" && $N_BEHIND != "0" ]]; then
        echo "${ZSH_THEME_GIT_PROMPT_DIVERGED_COLOR}${ZSH_THEME_GIT_PROMPT_AHEAD_SYMBOL}${N_AHEAD}${ZSH_THEME_GIT_PROMPT_BEHIND_SYMBOL}${N_BEHIND}"
    elif [[ $N_AHEAD != "0" ]]; then
        echo "${ZSH_THEME_GIT_PROMPT_AHEAD_COLOR}${ZSH_THEME_GIT_PROMPT_AHEAD_SYMBOL}${N_AHEAD}"
    elif [[ $N_BEHIND != "0" ]]; then
        echo "${ZSH_THEME_GIT_PROMPT_BEHIND_COLOR}${ZSH_THEME_GIT_PROMPT_BEHIND_SYMBOL}${N_BEHIND}"
    else;
        echo ""
    fi    
}

function k8s_info() {
  (( $+commands[kubectl] )) || return

  local ctx ns
  ctx=$(kubectl config current-context 2>/dev/null) || return
  ns=$(kubectl config view --minify --output 'jsonpath={..namespace}' 2>/dev/null)
  [[ -z "$ns" ]] && ns="default"

  ktx_str="<k8s:$ctx/$ns>"
  echo "%{$fg[magenta]%}$ktx_str%{$reset_color%} "
}

function my_git_prompt() {
  tester=$(git rev-parse --git-dir 2> /dev/null) || return
  
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""

  #Are we ahead/behind/diverged?
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
      echo "[$ENV] "
  fi
}

# This function just returns "(pyenv) " if there is an active pyenv
function get_virtualenv_indicator() {
  if [ ! -z "${VIRTUAL_ENV-}" ]; then
     echo "(pyenv) "
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=true


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
# #↑
# #⇅

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

function prompt-length() {
  emulate -L zsh
  local -i COLUMNS=${2:-COLUMNS}
  local -i x y=${#1} m
  if (( y )); then
    while (( ${${(%):-$1%$y(l.1.0)}[-1]} )); do
      x=y
      (( y *= 2 ))
    done
    while (( y > x + 1 )); do
      (( m = x + (y - x) / 2 ))
      (( ${${(%):-$1%$m(l.x.y)}[-1]} = m ))
    done
  fi
  typeset -g REPLY=$x
}

function fill-line() {
  emulate -L zsh
  prompt-length $1
  local -i left_len=REPLY
  prompt-length $2 9999
  local -i right_len=REPLY
  local -i pad_len=$((COLUMNS - left_len - right_len - ${ZLE_RPROMPT_INDENT:-1}))
  if (( pad_len < 1 )); then
    # Not enough space for the right part. Drop it.
    typeset -g REPLY=$1
  else
    local pad=${(pl.$pad_len.. .)}  # pad_len spaces
    typeset -g REPLY=${1}${pad}${2}
  fi
}

function set-prompt() {
  emulate -L zsh
  local git_branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
  git_branch=${git_branch//\%/%%}  # escape '%'

  # ~/foo/bar                     master
  # % █                            10:51
  #
  # Top left:      Blue current directory.
  # Top right:     Green Git branch.
  # Bottom left:   '#' if root, '%' if not; green on success, red on error.
  # Bottom right:  Yellow current time.


  local top_left="┌ %{$fg[green]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$fg[cyan]%} $(get_virtualenv_name)$(k8s_info)"
  local top_right="%{$fg[blue]%}%~%{$reset_color%}"
  local bottom_left="└ %{$fg[blue]%}%c$(my_git_prompt) %{$fg[red]%}%(!.#.»)%{$reset_color%} "
  local bottom_right="${return_code} [%*]"

  local REPLY
  fill-line "$top_left" "$top_right"
  PROMPT=$REPLY$'\n'$bottom_left
  RPROMPT=$bottom_right
}

# MULTILINE PROMPT CODE ADAPTED FROM HERE: https://www.reddit.com/r/zsh/comments/cgbm24/multiline_prompt_the_missing_ingredient/
setopt no_prompt_{bang,subst} prompt_{cr,percent,sp}
autoload -Uz add-zsh-hook
add-zsh-hook precmd set-prompt

# Reset prompt every second (so clock ticks). Also updates if context switches, etc
# TMOUT=1
# TRAPALRM() {
#   zle reset-prompt
# }
