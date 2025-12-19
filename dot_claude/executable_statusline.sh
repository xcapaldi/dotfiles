#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract values using jq
MODEL_DISPLAY=$(echo "$input" | jq -r '.model.display_name')
OUTPUT_STYLE=$(echo "$input" | jq -r 'if .output_style.name != "default" then ":" + .output_style.name else "" end')
CONTEXT_SIZE=$(echo "$input" | jq '.context_window.context_window_size')
USAGE=$(echo "$input" | jq '.context_window.current_usage')

# Calculate approximate token usage
PERCENT_USED=0
CURRENT_TOKENS=0
if [ "$USAGE" != "null" ]; then
    # Plus 45K for autocompact buffer and plus 5K mystery
    CURRENT_TOKENS=$(echo "$USAGE" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens + 45000 + 5000')
    PERCENT_USED=$((CURRENT_TOKENS * 100 / CONTEXT_SIZE))
fi

# Build usage bar (10 chars: ⛁ purple=filled 10%, ⛀ purple=partial 5%, ⛁ black=empty)
FILLED=$((PERCENT_USED / 10))
[ $FILLED -gt 10 ] && FILLED=10
PARTIAL=$(( (PERCENT_USED % 10) >= 5 && FILLED < 10 ? 1 : 0 ))
EMPTY=$((10 - FILLED - PARTIAL))
USAGE_BAR=""
for ((i=0; i<FILLED; i++)); do USAGE_BAR+="\033[35m⛁\033[0m "; done
[ $PARTIAL -eq 1 ] && USAGE_BAR+="\033[35m⛀\033[0m "
for ((i=0; i<EMPTY; i++)); do USAGE_BAR+="\033[30m⛁\033[0m "; done
USAGE_BAR="${USAGE_BAR% }"

# Show git branch if in a git repo
GIT_BRANCH="?"
if git rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        GIT_BRANCH="$BRANCH"
    fi
fi

echo -e "[$MODEL_DISPLAY$OUTPUT_STYLE] $GIT_BRANCH | $USAGE_BAR ~$PERCENT_USED% | $((CURRENT_TOKENS / 1000))K/$((CONTEXT_SIZE / 1000))K"
