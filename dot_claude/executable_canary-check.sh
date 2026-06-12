#!/usr/bin/env bash

set -uo pipefail

PATTERN='^///'   # must match the marker in your CLAUDE.md

INPUT=$(cat)
command -v jq >/dev/null 2>&1 || exit 0
T=$(printf '%s' "$INPUT" | jq -r '.transcript_path // empty')
[ -f "$T" ] || exit 0

# Last assistant text block = the turn that just finished.
last=$(jq -rs '
  [ .[] | select(.type=="assistant")
        | .message.content[]? | select(.type=="text") | .text ] | last // empty
' "$T" 2>/dev/null)
[ -n "$last" ] || exit 0                       # tool-only turn: nothing to check

# Canary alive — stay silent.
printf '%s' "$last" | sed 's/^[[:space:]]*//' | grep -qE "$PATTERN" && exit 0

# Canary missed — warn the user (systemMessage shows in the TUI).
jq -n '{systemMessage:"🐤 Context canary: Claude dropped the /// marker — context may be degrading. Consider /compact or /clear."}'
