---
name: todo-txt
description: "Read and write tasks using the todo.txt format (https://github.com/todotxt/todo.txt). Use this skill whenever creating, editing, completing, or querying todo.txt-style task files (typically todo.txt / done.txt)."
user-invocable: false
---

# todo.txt

[todo.txt](https://github.com/todotxt/todo.txt) is a plain-text task format: one task per line, designed to be readable, greppable, and sortable without special tooling. Don't reach for a different ad-hoc format when editing `todo.txt`/`done.txt` files — follow the spec below so the file stays compatible with todo.txt-cli and its addons.

## Line format

**Incomplete task:**

```
(A) 2026-06-15 Call Mom +Family @phone due:2026-06-20
```

Field order, left to right, all optional except the description:

1. **Priority** — `(A)`–`(Z)`, uppercase letter in parens, must be the very first thing on the line.
2. **Creation date** — `YYYY-MM-DD`, comes after priority (if present).
3. **Description** — free text containing any number of:
   - **Projects** — `+ProjectName` (no spaces; case-sensitive)
   - **Contexts** — `@context` (e.g. `@phone`, `@errands`)
   - **Key:value metadata** — `key:value`, e.g. `due:2026-06-20`. No spaces around `:`. Common keys: `due:`, `t:` (threshold/defer date), `rec:` (recurrence, e.g. `rec:1d`, `rec:+1w`).

**Completed task:**

```
x 2026-06-16 2026-06-15 Call Mom +Family @phone due:2026-06-20
```

1. `x ` (lowercase x, followed by a space) — marks completion. Must be first character.
2. **Completion date** — `YYYY-MM-DD`, immediately after `x`.
3. **Creation date** — `YYYY-MM-DD`, if it was present on the original task. Don't add one that wasn't there.
4. Rest of the description, unchanged.

Priority is dropped when a task is completed. If it's worth preserving, move it into the description as `pri:A`.

## Rules of thumb

- One task per line; no multi-line tasks, no blank-line significance (but avoid blank lines).
- Never rewrite or reorder unrelated lines — treat the file as a flat list and edit/append in place.
- To complete a task: prepend `x YYYY-MM-DD ` (today's date), strip any `(X)` priority (optionally append `pri:X`), and keep everything else as-is.
- To add a new task: append a line. Include a creation date (today's date) unless the user's existing tasks consistently omit dates.
- Sorting/filtering by priority, project, context, or due date should be done by reading the file and reasoning over the plain text — don't invent new syntax.
- `done.txt` holds archived completed tasks in the same completed-task format; moving a task there means deleting it from `todo.txt` and appending it to `done.txt`.

## Examples

```
(A) 2026-06-15 Schedule dentist appointment +Health @phone due:2026-06-20
(B) Submit quarterly report +Work @computer due:2026-06-30
Buy birthday gift for Mom +Family @errands
x 2026-06-14 2026-06-10 Renew car registration +Admin @errands
x 2026-06-12 Pay electricity bill +Bills @computer pri:A
```
