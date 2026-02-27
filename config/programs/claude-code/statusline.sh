#!/usr/bin/env bash
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')

# Git/Jujutsu info
vcs_info=""

# Check for jj
if command -v jj &>/dev/null && jj root --quiet 2>/dev/null; then
    jj_change=$(jj log --ignore-working-copy --no-graph -r @ -T 'separate("", change_id.shortest(), if(!empty, "*"))' 2>/dev/null)
    vcs_info=$(printf '\033[35m⎇ jj:%s\033[0m ' "$jj_change")
fi

# Check for git
if [ -d "$cwd/.git" ] || git -C "$cwd" rev-parse --git-dir &>/dev/null; then
    branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null || echo "detached")
    status=""
    if ! git -C "$cwd" --no-optional-locks diff-index --quiet HEAD -- 2>/dev/null; then
        status="*"
    fi
    vcs_info="${vcs_info}$(printf '\033[35m⎇ git:%s%s\033[0m ' "$branch" "$status")"
fi

# Model info
model=$(echo "$input" | jq -r '.model.id // .model.name // empty')
model_info=""
if [ -n "$model" ]; then
    model_info=$(printf '\033[34m◆ %s\033[0m ' "$model")
fi

# Cost info (only if cost > 0)
total_cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')
cost_info=""
if [ -n "$total_cost" ] && [ "$total_cost" != "0" ] && [ "$total_cost" != "0.0" ]; then
    # Round to 2 decimal places
    rounded_cost=$(printf "%.2f" "$total_cost")
    cost_info=$(printf '\033[37m▲ $%s\033[0m ' "$rounded_cost")
fi

# Session time (convert from milliseconds to days/hours/minutes/seconds)
session_duration_ms=$(echo "$input" | jq -r '.cost.total_duration_ms // empty')
session_info=""
if [ -n "$session_duration_ms" ]; then
    session_duration=$((session_duration_ms / 1000))
    days=$((session_duration / 86400))
    hours=$(((session_duration % 86400) / 3600))
    minutes=$(((session_duration % 3600) / 60))

    # Build time string with only relevant units
    time_str=""
    [ "$days" -gt 0 ] && time_str="${days}d "
    [ "$hours" -gt 0 ] && time_str="${time_str}${hours}h "
    time_str="${time_str}${minutes}m"

    session_info=$(printf '\033[36m◷ %s\033[0m ' "$time_str")
fi

# Context window info
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
context_info=""
if [ -n "$used_pct" ]; then
    # Create visual bar (10 blocks total, each represents 10%)
    bar=$(awk -v pct="$used_pct" 'BEGIN {
        filled = int(pct / 10)
        empty = 10 - filled
        for (i = 0; i < filled; i++) printf "▮"
        for (i = 0; i < empty; i++) printf "▯"
    }')

    # Color based on usage: blue < 50%, yellow >= 50%, red >= 75%
    if [ "$used_pct" -ge 75 ]; then
        color='\033[31m'  # red
    elif [ "$used_pct" -ge 50 ]; then
        color='\033[33m'  # yellow
    else
        color='\033[34m'  # blue (same as model)
    fi

    context_info=$(printf "${color}%s %s%%\033[0m " "$bar" "$used_pct")
fi

# Build status line
printf '%s%s%s%s%s' \
    "$vcs_info" \
    "$model_info" \
    "$context_info" \
    "$cost_info" \
    "$session_info"
