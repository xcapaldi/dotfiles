#!/bin/bash
# Don't show statusline in vterm
if [ "$INSIDE_EMACS" = "vterm" ] || [ -n "$VTERM" ]; then
    exit 0
fi

# Read JSON input from stdin
input=$(cat)

# Extract values using jq
MODEL_DISPLAY=$(echo "$input" | jq -r '.model.display_name')
CURRENT_DIR=$(echo "$input" | jq -r '.workspace.current_dir')
OUTPUT_STYLE=$(echo "$input" | jq -r 'if .output_style.name != "default" then ":" + .output_style.name else "" end')
LINES_ADDED=$(echo "$input" | jq -r '.cost.total_lines_added')
LINES_REMOVED=$(echo "$input" | jq -r '.cost.total_lines_removed')

# Show git branch if in a git repo
GIT_BRANCH=""
if git rev-parse --git-dir > /dev/null 2>&1; then
    BRANCH=$(git branch --show-current 2>/dev/null)
    if [ -n "$BRANCH" ]; then
        GIT_BRANCH=" | 🌳 $BRANCH"
    fi
fi

echo "[$MODEL_DISPLAY$OUTPUT_STYLE] 📁 ${CURRENT_DIR##*/}$GIT_BRANCH | +$LINES_ADDED -$LINES_REMOVED"
