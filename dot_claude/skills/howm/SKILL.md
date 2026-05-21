---
name: howm
description: "Interact with a howm personal wiki by driving Emacs in batch mode. Lists notes, follows links, manages tasks (todos/deadlines/schedules/reminders) using howm's own algorithms — never re-implement them."
user-invocable: false
---

# howm

howm is a flat-file personal wiki implemented as an Emacs minor mode. Notes are plain text; links are search triggers; tasks have time-dependent priority. Howm's motto: **"Write fragmentarily and read collectively."**

This skill drives howm through `emacs --batch` so you get the real howm semantics — priority calculation, come-from link resolution, keyword index handling — without re-implementing any of it.

## The interface: `howm-cli.el`

A helper script lives next to this file: `~/.claude/skills/howm/howm-cli.el`. It defines `howm-cli-*` functions that print a single JSON document to stdout. The user's full Emacs init runs first, so `howm-directory`, file format, and other settings come from their config.

### Canonical invocation

```sh
emacs --batch \
  -l ~/.config/emacs/early-init.el \
  -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-todo)' 2>/dev/null
```

Two `-l` for the user's init are required because `--batch` skips init files by default and `init.el` depends on `custom-file` set by `early-init.el`. Redirect stderr — Emacs prints loading messages there.

Tips:
- Wrap output in `| jq` or `python3 -m json.tool` for human-readable inspection.
- For one-off ad-hoc work, append more `--eval '...'` after the cli load.
- Override the directory for a single call: `--eval '(setq howm-directory "/path/")' --eval '(setq howm-keyword-file (expand-file-name ".howm-keys" howm-directory))'` placed **before** `-l howm-cli.el`.

### Available commands

All commands print JSON. Items have shape `{file, basename, summary, place, priority?}` where `place` is a byte offset inside the file and `priority` is present only for todo-style queries.

| Function | Purpose |
|----------|---------|
| `(howm-cli-config)` | Echo active howm settings (sanity check). |
| `(howm-cli-all)` | Every note in the wiki. |
| `(howm-cli-recent &optional days)` | Notes modified in the last N days (default `howm-list-recent-days`). |
| `(howm-cli-today)` | Notes whose content references today's date. |
| `(howm-cli-yesterday)` | Notes whose content references yesterday's date. |
| `(howm-cli-search-date "YYYY-MM-DD")` | Notes referencing a specific date. |
| `(howm-cli-todo)` | Open todos/reminders/defers, sorted by howm's computed priority (high→low). |
| `(howm-cli-schedule)` | Schedule events (`@`) and deadlines (`!`). |
| `(howm-cli-all-tasks)` | Every task of every marker. |
| `(howm-cli-search "PATTERN")` | Fixed-string grep (default). |
| `(howm-cli-search-regexp "PATTERN")` | Regex grep. |
| `(howm-cli-backlinks "FILENAME")` | Notes that reference FILENAME (filename or full path). |
| `(howm-cli-keywords)` | Every `<<<` keyword registered in `.howm-keys`. |
| `(howm-cli-keyword-add "KW")` | Append KW to `.howm-keys` if missing. |
| `(howm-cli-keyword-remove "KW")` | Remove KW from `.howm-keys`. |
| `(howm-cli-create "TITLE" &optional "BODY")` | Create a new note with the user's filename format and header style. |

### Example output

```sh
$ emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
    -l ~/.claude/skills/howm/howm-cli.el --eval '(howm-cli-todo)' 2>/dev/null
[{"priority":77777.92,"summary":"Fri  6 [2026-05-15]! overdue deadline",
  "basename":"2026-05-21T100000.md","file":"/Users/x/Notes/2026-05-21T100000.md","place":4},
 {"priority":-5.52,"summary":"Wed  1 [2026-05-20]+ overdue todo", ...}]
```

The `summary` for todo queries has howm's day-offset prefix: `"Fri  6 "` means Friday, 6 days overdue. For non-todo queries the summary is the raw line text.

## Howm conceptual model

You can usually answer the user without explaining howm, but if needed:

- **Notes**: plain text files (Markdown in this setup, `*.md`). One file may contain multiple `#`-headed sections; each section is a logical sub-note.
- **Filename = ID**: timestamp like `2026-05-21T143000.md` (user's `howm-file-name-format`). The user's setup is set to `%Y-%m-%dT%H%M%S.md`.
- **Menu file**: `0000-00-00-000000.md` — Emacs-rendered dashboard. Treat as user-managed.
- **`.howm-keys`**: index of every `<<<` keyword. Lives at `<howm-directory>/.howm-keys`.
- **Goto link** `>>> keyword` — follow = grep for `keyword`.
- **Come-from link** `<<< keyword` — every occurrence of `keyword` in any note implicitly links here. Aliases share a line: `<<< heuristics <<< serendipity`.
- **Tasks** `[YYYY-MM-DD]MARKER[N] text` inline anywhere. Markers: `@` schedule, `+` todo, `!` deadline, `-` reminder, `~` defer, `.` done. Floating priority is recomputed daily.

## Common workflows

### "What's on my plate today?"

```sh
emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-schedule)' --eval '(howm-cli-todo)' 2>/dev/null
```

### "Find notes about X"

```sh
emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-search "X")' 2>/dev/null
```

### "What references this note?" (backlinks)

```sh
emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-backlinks "2024-01-15T103000.md")' 2>/dev/null
```

### "Create a note titled X"

```sh
emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-create "Meeting notes" "Discussed roadmap.")' 2>/dev/null
```

Then follow up with file edits via the Edit tool if more content is needed.

### Marking a task done

Tasks are inline text. Done state is `.` prefixed with today's stamp; original date and marker are preserved:

```
[2026-05-01]+ Read howm source code
     ↓
[2026-05-21]. [2026-05-01]:+ Read howm source code
```

Use the Edit tool with the exact original line to replace. The marker transition is mechanical, but the wrapping date stamp must be today's local date.

### Adding a `<<<` keyword

```sh
emacs --batch -l ~/.config/emacs/early-init.el -l ~/.config/emacs/init.el \
  -l ~/.claude/skills/howm/howm-cli.el \
  --eval '(howm-cli-keyword-add "jabberwock")' 2>/dev/null
```

Then place `<<< jabberwock` in the canonical note (using Edit). After saving, every other occurrence of `jabberwock` is automatically a link to that note.

## Ad-hoc elisp

If a command isn't in `howm-cli.el`, fall back to direct elisp. Key APIs:

- `(howm-folder-items howm-directory t)` — enumerate items.
- `(howm-all-items)` — items in primary search path.
- `(howm-call-view-search-internal PATTERN FIXED-P EMACS-REGEXP)` → `(kw name items)`; grep wrapper used by `howm-search`.
- `(howm-reminder-search TYPES)` where TYPES is a string of marker chars (e.g. `"@!"` for schedule + deadline, `"+!\\-~"` for todo-style, `howm-reminder-types` covers all).
- `(howm-todo-priority ITEM)` — computed buoyancy for a task item.
- `(howm-list-todo-sub-setup-items nil)` — sorted todo list with priorities (the data backing `howm-list-todo`).
- `(howm-keyword-list)` — keyword index entries.
- Item accessors: `howm-item-name`, `howm-item-summary`, `howm-item-page`, `howm-item-place`, `howm-item-offset`.

Pattern for emitting JSON from ad-hoc code:

```elisp
(require 'json)
(princ (json-encode (mapcar (lambda (it) ...) items)))
```

## When to load `references/tasks.md`

Load only if the user wants to understand howm's task semantics — how priorities are computed, what grace periods mean, recurring tasks, the difference between done and cancel. For most "show me my tasks" questions, `howm-cli-todo` and `howm-cli-schedule` are enough.
