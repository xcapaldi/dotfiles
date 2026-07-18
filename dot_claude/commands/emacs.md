---
description: Open this agent session's worktree in Emacs (launches Emacs if needed)
disable-model-invocation: true
allowed-tools: Bash(~/.claude/scripts/claude-emacs:*)
---
Open the current working directory (this session's worktree) in Emacs. If no Emacs
server is running, this launches Emacs, waits for it, then opens the directory.
Do nothing else — the line below does all the work.

!~/.claude/scripts/claude-emacs "$PWD"
