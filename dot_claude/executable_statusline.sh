#!/bin/bash
input=$(cat)

MODEL=$(echo "$input" | jq -r '.model.display_name')
OUTPUT_STYLE=$(echo "$input" | jq -r 'if .output_style.name != "default" then ":" + .output_style.name else "" end')
PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)

# Context bar
BAR_WIDTH=10
FILLED=$((PCT * BAR_WIDTH / 100))
EMPTY=$((BAR_WIDTH - FILLED))
BAR=""
[ "$FILLED" -gt 0 ] && BAR=$(printf "%${FILLED}s" | tr ' ' '▓')
[ "$EMPTY" -gt 0 ] && BAR="${BAR}$(printf "%${EMPTY}s" | tr ' ' '░')"

# Git branch with clickable PR link (cached)
GIT_INFO="?"
if git rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        CACHE_FILE="/tmp/statusline-pr-$(echo "$BRANCH" | tr '/' '-')"
        CACHE_MAX_AGE=30
        if [ ! -s "$CACHE_FILE" ] || [ $(($(date +%s) - $(stat -f %m "$CACHE_FILE" 2>/dev/null || stat -c %Y "$CACHE_FILE" 2>/dev/null || echo 0))) -gt $CACHE_MAX_AGE ]; then
            PR_RESULT=$(gh pr view --json url -q .url 2>/dev/null)
            [ -n "$PR_RESULT" ] && printf '%s' "$PR_RESULT" > "$CACHE_FILE"
        fi
        PR_URL=$([[ -f "$CACHE_FILE" ]] && tr -d '\n' < "$CACHE_FILE")
    fi
fi

if [ -n "$PR_URL" ] && [ -z "$TMUX" ]; then
    printf '%b' "[$MODEL$OUTPUT_STYLE] \e]8;;${PR_URL}\a${BRANCH}\e]8;;\a | $BAR ${PCT}%\n"
else
    printf '%b' "[$MODEL$OUTPUT_STYLE] ${BRANCH:-?} | $BAR ${PCT}%\n"
fi
