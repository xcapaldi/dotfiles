# howm task management — semantic reference

This reference explains how howm models tasks so you can interpret what `howm-cli-todo` / `howm-cli-schedule` return and how to edit task lines correctly. **The actual priority calculation and sorting is done by howm itself** — call `howm-cli-todo` to get a sorted list with computed priorities.

## Syntax

```
[YYYY-MM-DD]MARKER[NUM] DESCRIPTION
[YYYY-MM-DD HH:mm]MARKER[NUM] DESCRIPTION
```

- The bracketed date is mandatory; `HH:mm` is optional.
- `MARKER` is one of `@ + ! - ~ . x` (see table below).
- `NUM` (immediately after the marker, no space) is the **grace period** that modifies how fast the task floats / sinks. Defaults vary per marker.

## Markers

| Marker | Type     | Behavior                                                                                          |
|--------|----------|---------------------------------------------------------------------------------------------------|
| `@`    | Schedule | Fixed calendar event. Visible roughly seven days ahead, sinks the day after.                       |
| `+`    | Todo     | Submerged until date. Floats up the longer it's overdue (oldest overdue todos rise highest).      |
| `!`    | Deadline | Surfaces about seven days before, pinned at the top after the date.                                |
| `-`    | Reminder | Peak on the date, sinks one unit/day after.                                                       |
| `~`    | Defer    | 30-day oscillation through the middle band.                                                       |
| `.`    | Done     | Terminal. Permanently submerged.                                                                  |
| `x`    | Cancel   | Visual marker — see "Cancelling" below; the cancel transition uses `.` plus a `cancel` annotation. |

## Priority bands

Howm's `howm-todo-priority` returns a number. Three rough bands:

- **~ +77,777** — urgent / overdue → top of `howm-list-todo`.
- **~ 0** — currently relevant → middle of list.
- **~ –77,777** — out of season → bottom; hidden from the menu summary.

You don't compute this yourself. `howm-cli-todo` returns items already sorted high→low with priorities attached.

## Grace period (`NUM`)

Larger `NUM` = slower / wider behavior; smaller = sharper. From the manual:

| Example         | Effect                                                              |
|-----------------|---------------------------------------------------------------------|
| `[2026-06-25]@4`  | Schedule visible for 3 extra days after the date.                |
| `[2026-06-25]+14` | Todo floats up twice as slowly.                                  |
| `[2026-01-20]!14` | Deadline starts rising 14 days early.                            |
| `[2026-08-15]-2`  | Reminder sinks twice as slowly.                                  |
| `[2026-03-09]~15` | Defer oscillates twice as often (15-day cycle).                  |

## Editing tasks (file-level operations)

Tasks are plain text. Use the Edit tool with the exact original line as `old_string`.

### Change date / marker / grace

Just edit the brackets and characters in place:

- `[2026-05-01]+ foo` → `[2026-05-08]+ foo` (postpone)
- `[2026-05-01]+ foo` → `[2026-05-01]! foo` (promote to deadline)
- `[2026-05-01]+ foo` → `[2026-05-01]+14 foo` (slow the float)

### Mark done

Prepend a completion stamp with today's date and `.`, keeping the original marker and date as history:

```
[2026-05-01]+ Read howm source code
       ↓
[2026-05-21]. [2026-05-01]:+ Read howm source code
```

Pattern: `[TODAY]. [ORIGINAL_DATE]:ORIGINAL_MARKER description`.

### Cancel

Same shape as Done plus the literal word `cancel`:

```
[2026-05-01]+ Read Moby-Dick
       ↓
[2026-05-21]. cancel [2026-05-01]:+ Read Moby-Dick
```

### Recurring

Howm prompts interactively for a stop-date (`~YYMMDD`) and a cadence (daily, every-N-days, `w`/`m`/`y`). The result is a block of repeated lines with different dates. From the CLI, generate them inline:

```elisp
;; 12 monthly clones starting 2026-06-01
(dotimes (i 12)
  (let ((d (format-time-string "%Y-%m-%d"
                               (time-add (date-to-time "2026-06-01T00:00:00")
                                         (days-to-time (* i 30))))))
    (insert (format "[%s]+ pay rent\n" d))))
```

Or use `date -v+Nm` on macOS / `date -d +N months` on GNU.

## Note: schedule + deadline live in `howm-cli-schedule`

`howm-list-todo` (and therefore `howm-cli-todo`) intentionally excludes `@` schedule events and `!` deadlines. Those go through `howm-list-schedule` / `howm-cli-schedule`. To see "everything," call both, or use `howm-cli-all-tasks` which returns every reminder of any marker.

## Diagnostic: the simulator

Howm ships with `howm-simulate-todo` — an interactive time-machine that recomputes priorities as if today were a different date. From batch you can't drive its keymap, but you can simulate by binding `current-time`:

```elisp
;; Pretend today is 30 days from now
(cl-letf (((symbol-function 'current-time)
           (lambda () (time-add (current-time) (days-to-time 30)))))
  (howm-cli-todo))
```

Useful for "what will my list look like next month?"
