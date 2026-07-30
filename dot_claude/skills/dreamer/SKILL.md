---
name: dreamer
description: "Consolidate new Claude memories and howm captures into the persistent note graph. Reads scratch memories, unprocessed howm captures and recent session transcripts, then merges, promotes or drops each one and rewrites MEMORY.md as an index. Use when the user asks to dream, to process memories, or to tidy the note graph."
user-invocable: true
---

# Dreamer

Claude writes memories during the day. They are fast, local and unverified, so they duplicate each other and go stale. This skill is the curation pass that turns them into notes in the howm graph.

The graph is the store of truth. `MEMORY.md` is only an index that points into it.

## Layout

| Path | Role |
|---|---|
| `~/Notes/` | howm graph. Persistent notes. Git tracked. |
| `~/Notes/memory/` | `autoMemoryDirectory`. Scratch memories Claude wrote today. |
| `~/Notes/memory/MEMORY.md` | Index loaded into every session. Excluded from howm indexing. |
| `~/Notes/memory/.dream-state` | ISO timestamp of the last run. Dot prefix hides it from howm. |
| `~/Notes/.howm-keys` | Controlled vocabulary. |

On the WSL host the notes root is `/mnt/c/Users/xavie/notes/` instead. Read `howm-directory` from `dot_config/emacs/init.el` if unsure.

`~/Notes/memory/` is a subdirectory on purpose. Claude's auto memory only writes inside `autoMemoryDirectory`, so it can never edit or delete a persistent note. This skill is the only actor that touches `~/Notes/*.md`.

## Three states

| Form | Location | Meaning |
|---|---|---|
| `some-slug.md` | `memory/` | Scratch memory. Needs promotion. |
| `20260730T140800.md` | `~/Notes/` | howm quick capture. Needs a name and tags. |
| `20260730T140800--title__kw1_kw2.md` | `~/Notes/` | Processed. |

Consume the first two. Only ever emit the third. Move a scratch file out of `memory/` when you promote or merge it, so presence in `memory/` always means unprocessed. Never invent bookkeeping that duplicates this signal.

## Procedure

### 1. Read the vocabulary first

```sh
cat ~/Notes/.howm-keys
```

Graph edges come from keyword co-occurrence, so an off-vocabulary tag creates a note that connects to nothing. Tag only from this list. A keyword that is not in the list is a tier 3 hold, never a silent addition.

### 2. Collect the inputs

Scratch memories and captures:

```sh
ls ~/Notes/memory/*.md                       # exclude MEMORY.md
find ~/Notes -maxdepth 1 -name '[0-9]*T[0-9]*.md' ! -name '*--*'
```

Transcripts, scoped to sessions since the last run:

```sh
cat ~/Notes/memory/.dream-state              # ISO timestamp, absent on first run
find ~/.claude/projects -name '*.jsonl' -newermt "$(cat ~/Notes/memory/.dream-state)"
```

Transcripts run to tens of megabytes per day. Never read them directly. Spawn one subagent per transcript file and have each return only candidate insights, in this shape:

```
{claim, evidence_path, confidence, suggested_keywords}
```

Tell each subagent to return nothing rather than pad. Most sessions hold no durable insight. A transcript yielding zero candidates is the normal case.

### 3. Decide per item

Search the graph before deciding, so you find the note that already covers the topic:

```sh
rg -l --no-heading 'topic phrase' ~/Notes/
```

| Action | When |
|---|---|
| **merge** | A note already covers the topic. Keep its ID and filename. Add only the new fact. |
| **promote** | Nothing covers it and the fact is durable. Write a new note. |
| **drop** | Trivial, superseded, or already true in the graph. |
| **hold** | Ambiguous. Leave the file in `memory/` and ask in the digest. |

### 4. Tier the actions

Apply tiers 1 and 2 without asking. Every action is a git commit, so `git revert` is the undo.

- **Tier 1, apply silently.** Name and tag captures. Remove exact duplicates. Promote a clean scratch memory to a new note.
- **Tier 2, apply and report.** Merges. Contradictions you resolved by recency. List each in the digest.
- **Tier 3, hold and ask.** Two notes that may be one concept. A contradiction with no clear recency winner. Any keyword not already in `.howm-keys`. Vocabulary changes are the user's call because every edge depends on them.

### 5. Write notes

Filename: `YYYYmmddTHHMMSS--title-slug__kw1_kw2.md`, minted from the current local time for a new note, or kept unchanged for a merge.

Header, OKF style. `type` is required:

```yaml
---
type: note
title: Cloud SQL connection pressure
tags: [infra, database]
timestamp: 2026-07-30T14:08:00
description: One line on why this note exists.
---
```

The YAML is the source of truth. The filename slug and keywords are derived from `title` and `tags`, so change the YAML first and then the name.

### 6. Guardrails when editing a note

howm links are search based. `>>> foo` finds the literal text `foo`, so any edit to body text is a potential edit to a link, and a broken link fails silently instead of dangling.

- Copy `>>>` and `<<<` lines through unchanged.
- Append or make a surgical edit. Never rewrite a note body wholesale.
- Never reword a heading or phrase that other notes point at. Check first: `rg -F '>>> phrase' ~/Notes/`.
- Never delete a note the user wrote. Downgrade to a tier 3 hold and ask.

### 7. Rewrite MEMORY.md

One line per entry, pointing at a note path:

```markdown
- [Cloud SQL connection pressure](~/Notes/20260615T103300--cloudsql-connection-pressure__infra_database.md) — pool exhaustion under batch load
```

Only the first 200 lines or 25KB load into a session, and this index is shared by every project, so it fills fast. Keeping it short is the main job of this step. Push detail down into the note and leave a pointer. Merge or drop stale lines.

### 8. Write the digest, then commit

Write the digest as a note so it lands in the graph and stays searchable:

`YYYYmmddTHHMMSS--dream-digest__meta.md`

Include the counts for each action, every tier 2 item, and a questions section for the tier 3 holds. Put a howm reminder marker on the questions so the menu keeps showing them until they are resolved. Use `-` for a plain reminder, or `!` for a deadline if an answer blocks later work:

```markdown
## Questions

[2026-07-31]- Is "connection pooling" the same concept as the existing "pool pressure" note?
```

The user answers in the digest. Read the previous digest at the start of the next run and act on the answers.

Then record the run and commit:

```sh
date -u +%Y-%m-%dT%H:%M:%SZ > ~/Notes/memory/.dream-state
git -C ~/Notes add -A && git -C ~/Notes commit -m "chore: dream <date>"
```

Report to the user what changed. State the counts plainly and name any holds.

## First run

`~/Notes/` starts empty and `.howm-keys` holds no real vocabulary, so the first run is a bootstrap, not a maintenance pass. It seeds the graph from the memories that already exist in the per-project directories under `~/.claude/projects/*/memory/`.

Run this one interactively. Two things make it different:

1. **Propose the vocabulary and stop.** Cluster the memories, propose a keyword list, and get approval before writing any note. Everything downstream depends on this list, and a bad list is expensive to undo once notes are tagged.
2. **Propose the groupings.** Many of the memories overlap. Show the merge plan before you write.

Only after both are approved, promote the notes, write `.howm-keys`, and make the first commit.

## Notes on tooling

Plain shell is enough: `cat` for the vocabulary, `rg` for search, `Write` for notes, an append for a new keyword. The old `howm-cli.el` helper is recoverable at `git show 49528830^:dot_claude/skills/howm/howm-cli.el`. Restore it only if this skill needs howm's real task priority or come-from resolution semantics, which plain text handling cannot reproduce.
