#!/usr/bin/env bash
# Claude Code status line — styled after the "grado" zsh theme.
#
# Top:    ┌ user@host  ~/path/to/dir  (branch ●↑1)  Model Name
# Middle: ├ ⬡ task-name  hatchery/branch    (only when HATCHERY_TASK is set)
# Bottom: └ [HH:MM:SS]  [████████████░░░░░░░░] 62%

input=$(cat)

# --- Extract fields from JSON ---
if command -v jq >/dev/null 2>&1; then
    cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // ""')
    model=$(echo "$input" | jq -r '.model.display_name // ""')
    remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
else
    # grep/sed fallback for minimal environments (e.g. Docker containers without jq)
    cwd=$(echo "$input" | grep -o '"current_dir":"[^"]*"' | head -1 | sed 's/"current_dir":"//;s/"$//')
    [ -z "$cwd" ] && cwd=$(echo "$input" | grep -o '"cwd":"[^"]*"' | head -1 | sed 's/"cwd":"//;s/"$//')
    model=$(echo "$input" | grep -o '"display_name":"[^"]*"' | head -1 | sed 's/"display_name":"//;s/"$//')
    remaining=$(echo "$input" | grep -o '"remaining_percentage":[0-9.]*' | head -1 | sed 's/.*://')
fi

# --- Shorten cwd: replace $HOME with ~ ---
home="$HOME"
short_cwd="${cwd/#$home/\~}"

# --- Git info (non-blocking, skip optional locks) ---
git_branch=$(git -C "$cwd" --no-optional-locks rev-parse --abbrev-ref HEAD 2>/dev/null)

git_status=""
if [ -n "$git_branch" ]; then
    index=$(git -C "$cwd" --no-optional-locks status --porcelain 2>/dev/null)

    # Untracked files
    if echo "$index" | grep -q '^?? '; then
        git_status="${git_status}●"
    fi
    # Unstaged changes
    if echo "$index" | grep -qE '^[ MARC][MD] '; then
        git_status="${git_status}$(printf '\033[0;31m')●$(printf '\033[0;33m')"
    fi
    # Staged changes
    if echo "$index" | grep -qE '^(D[ M]|[MARC][ MD]) '; then
        git_status="${git_status}$(printf '\033[0;36m')●$(printf '\033[0;33m')"
    fi
    # Unmerged
    if echo "$index" | grep -qE '^(A[AU]|D[DU]|U[ADU]) '; then
        git_status="${git_status}$(printf '\033[0;31m')§$(printf '\033[0;33m')"
    fi

    # Ahead / behind
    n_ahead=$(git -C "$cwd" --no-optional-locks rev-list @{u}..HEAD 2>/dev/null | wc -l | xargs)
    n_behind=$(git -C "$cwd" --no-optional-locks rev-list HEAD..@{u} 2>/dev/null | wc -l | xargs)

    if [ "$n_ahead" != "0" ] && [ "$n_behind" != "0" ]; then
        git_status="${git_status} $(printf '\033[0;31m')↑${n_ahead}↓${n_behind}$(printf '\033[0;33m')"
    elif [ "$n_ahead" != "0" ]; then
        git_status="${git_status} $(printf '\033[0;36m')↑${n_ahead}$(printf '\033[0;33m')"
    elif [ "$n_behind" != "0" ]; then
        git_status="${git_status} $(printf '\033[0;31m')↓${n_behind}$(printf '\033[0;33m')"
    fi

    if [ -n "$git_status" ]; then
        git_status=" $git_status"
    fi

    git_info="$(printf '\033[0;33m')(${git_branch}${git_status}$(printf '\033[0;33m'))$(printf '\033[0m')"
fi

# --- ANSI helpers ---
reset=$(printf '\033[0m')
green=$(printf '\033[0;32m')
blue=$(printf '\033[0;34m')
cyan=$(printf '\033[0;36m')
yellow=$(printf '\033[0;33m')
red=$(printf '\033[0;31m')

# --- Build top line ---
user_host="${green}$(whoami)${reset}@${green}$(hostname -s)${reset}"
dir_part="${blue}${short_cwd}${reset}"
model_part="${cyan}${model}${reset}"

top="┌ ${user_host}  ${dir_part}"
[ -n "$git_info" ] && top="${top}  ${git_info}"
[ -n "$model_part" ] && top="${top}  ${model_part}"

# --- Build hatchery middle line (only when HATCHERY_TASK is set) ---
hatchery_line=""
if [ -n "$HATCHERY_TASK" ]; then
    hatchery_branch=$(git -C "$HATCHERY_REPO" --no-optional-locks \
        rev-parse --abbrev-ref HEAD 2>/dev/null)
    task_part="${cyan}⬡ ${HATCHERY_TASK}${reset}"
    branch_part="${yellow}${hatchery_branch}${reset}"
    hatchery_line="├ ${task_part}  ${branch_part}"
fi

# --- Build bottom line ---
time_part="${yellow}[$(date +%H:%M:%S)]${reset}"
bottom="└ ${time_part}"

if [ -n "$remaining" ]; then
    # Color thresholds based on used%: green ≤50%, yellow ≤80%, red >80%
    pct=$(printf '%.0f' "$remaining")
    used=$(( 100 - pct ))
    if [ "$used" -le 50 ]; then
        pct_color="${green}"
    elif [ "$used" -le 80 ]; then
        pct_color="${yellow}"
    else
        pct_color="${red}"
    fi
    filled=$(( used * 20 / 100 ))
    empty=$(( 20 - filled ))
    bar=""
    [ "$filled" -gt 0 ] && bar=$(printf '█%.0s' $(seq 1 $filled))
    [ "$empty" -gt 0 ] && bar="${bar}$(printf '░%.0s' $(seq 1 $empty))"
    bottom="${bottom}  [${pct_color}${bar}${reset}] ${pct_color}${used}%${reset}"
fi

if [ -n "$hatchery_line" ]; then
    printf '%s\n%s\n%s\n' "$top" "$hatchery_line" "$bottom"
else
    printf '%s\n%s\n' "$top" "$bottom"
fi
