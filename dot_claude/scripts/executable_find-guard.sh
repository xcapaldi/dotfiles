#!/bin/bash
INPUT=$(cat)
CMD=$(echo "$INPUT" | jq -r '.tool_input.command')

if echo "$CMD" | grep -qE '\bfind\b' && echo "$CMD" | grep -qE '\-(exec|delete)\b'; then
  jq -n '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: "find with -exec or -delete blocked; use Glob tool instead"
    }
  }'
else
  exit 0
fi
