---
name: emacs
description: "Open a file or the current project's root in Emacs, reusing the running Emacs instance when possible."
user-invocable: true
disable-model-invocation: true
argument-hint: [dir|file]
---

# emacs

Opens files or directories in running Emacs via `emacsclient`.
Falls back to spawning emacs if server not reachable.

## Why `-s ~/.cache/emacs/server`

Claude Code's shell runs with a sandboxed `TMPDIR` (e.g. `/tmp/claude-501`), which differs from the GUI Emacs's `TMPDIR` (e.g. `/var/folders/.../T/`). Without an explicit socket path, `emacsclient` looks in the sandbox TMPDIR and never finds the GUI's socket. Init.el pins `server-socket-dir` to `~/.cache/emacs/`, and the skill passes the same path via `-s`.

## Default behavior

With no argument: open the **repository root** in dired.

- Resolve root with `git rev-parse --show-toplevel` from the current working directory.
- If in a git worktree, that command still returns the worktree's root — correct.
- If the cwd is not inside a git repo, fall back to opening the cwd itself.

```sh
emacsclient -s ~/.cache/emacs/server -n -a "" "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
```

The command must begin with `emacsclient` (not a `target=...` prefix) so it matches the `Bash(emacsclient:*)` allowlist rule.

`-n` returns immediately. No `-c`: the file/dir is shown in the existing frame, not a new one.

## With an argument

If the user names a file or directory, open that instead:

```sh
emacsclient -s ~/.cache/emacs/server -n -a "" path/to/file.py
```

Line/column targeting uses Emacs's `+LINE[:COL]` form:

```sh
emacsclient -s ~/.cache/emacs/server -n -a "" +42:7 path/to/file.py
```

## Notes

- Don't pass `-c` unless the user asks for a new frame; the default is to reuse the existing one.
- Don't pass `-t` (terminal frame) — it blocks the shell until the buffer is killed.
- If `emacsclient` errors with `can't find socket`, the user's Emacs is running but doesn't have the server started.
  Tell them to evaluate `(server-start)` once, or restart Emacs.
