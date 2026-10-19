---
name: dream
description: "Read stored memories and past chat transcripts, and propose new skills or updates to existing skills. Proposes general skills and skills for one repository. Shows proposals in chat for review. Changes a skill only after approval."
---

# Dream

Reads memories and chat transcripts, finds the patterns that a skill must capture, and shows proposals in chat for review.

## Inputs

- **Memories.** The memory directory that your system prompt names. Read `MEMORY.md` first, then the memory files it points to.
- **Transcripts.** The chat transcripts in `~/.claude/projects/`. See [Transcripts](#transcripts).
- **Skills.** Every `SKILL.md` under `~/.claude/skills/`, every `SKILL.md` under `.claude/skills/` in each repository that the transcripts name, and the skill list in your context. Read the frontmatter of each, and the body of each skill that a candidate touches.
- **Repository instructions.** The `CLAUDE.md` of each repository that a candidate touches. A rule that is already in `CLAUDE.md` is not a candidate.

Read the inputs again on each run.
The skill files are the only record of what exists.
A proposal from an earlier run in this chat is not a skill, even if it looks complete.
Do not treat it as done and do not build on it.
If it is still valid and nobody applied it, propose it again.

## Transcripts

Each directory in `~/.claude/projects/` holds the transcripts for one working directory.
The directory name is the working directory path with each `/` and `.` changed to `-`.
Each `<session-id>.jsonl` file is one chat. Each line is one JSON event.

- Read only the last 30 days of transcripts, unless the user gives a different range. Use the file modification time.
- Do not read the transcript of the current chat.
- Get the repository of a chat from the `cwd` field of its events. Run `git -C <cwd> rev-parse --show-toplevel` to find the repository root. A worktree under `<repo>/.claude/worktrees/` belongs to `<repo>`.
- Prompts from the user are the strongest evidence. Read the `assistant` events near them for context.
- Transcripts are large. Extract the prompts with `jq` before you read more. This removes tool results, subagent events and command output:

  ```sh
  jq -r 'select(.type=="user" and (.isMeta|not) and (.isSidechain|not)) | .message.content | if type=="string" then . else map(select(.type=="text") | .text) | join("\n") end | select(length>0 and (startswith("<")|not))' <file>.jsonl
  ```

- If there are many transcripts, give each repository to a subagent. Tell it to return the repeated workflows, corrections and instructions it finds, with the session ID and a short quote for each.

In a transcript, look for:

- The same instruction or correction in two or more chats.
- The same multi-step workflow that the user walks Claude through in two or more chats.
- The same mistake by Claude that the user fixes in two or more chats.
- A long explanation from the user of a process, a system or a domain rule.

## Scope

Give each candidate a scope:

- **General.** The evidence comes from two or more repositories, or the pattern does not depend on one repository. The default path is `~/.claude/skills/<name>/SKILL.md`. If `~/.local/share/chezmoi/dot_claude/skills/` exists, the default path is `~/.local/share/chezmoi/dot_claude/skills/<name>/SKILL.md`.
- **Repository.** All evidence comes from one repository, and the pattern depends on its code, tools or process. The default path is `<repo>/.claude/skills/<name>/SKILL.md`.

If a pattern has a general part and a part for one repository, make two candidates.

## Rejections

If the user rejected a proposal in this chat, do not propose it again.
Propose it again only if new memories or transcripts give new evidence, and state the new evidence.

## Steps

1. Read all inputs.
2. Make a list of candidates. A candidate is one of:
   - **New skill.** Two or more pieces of evidence describe the same repeated workflow, rule or domain, and no skill covers it. A piece of evidence is one memory or one chat.
   - **Skill update.** One or more pieces of evidence correct, extend or conflict with an existing skill.
3. Give each candidate a scope. See [Scope](#scope).
4. Remove each candidate that is a one-time fact, a project status or a personal preference with no repeated use. These stay as memories.
5. Remove each candidate that `CLAUDE.md` or an existing skill already covers in full.
6. Remove each candidate that the user rejected. See [Rejections](#rejections).
7. Sort the remaining candidates by evidence, strongest first. Strong evidence is more chats and memories, `feedback` memories, and corrections the user gave in more than one chat.
8. Show each candidate in chat. See [Proposal format](#proposal-format). Show the general candidates first, then the repository candidates grouped by repository.
9. Ask the user to approve or reject each proposal by its number.
10. If no candidate is left, print `No new proposals.` and stop.

## After review

- **Approved.** Ask the user where to put the skill. Give the default path for the scope as the first option. For an update, give the path of the existing skill as the first option. Make the change only at the path that the user gives. If the path is in `~/.local/share/chezmoi/`, tell the user to run `chezmoi apply`.
- **Rejected.** Make no change. Remember the rejection for the rest of this chat.
- **No answer.** Make no change.

## Rules

- Do not change a skill before the user approves the proposal.
- Do not edit or delete any memory or transcript.
- Cite each memory and each chat that a proposal uses. A proposal with no citation is not valid.
- Do not copy secrets, tokens, credentials or customer data from a transcript into a proposal or a skill.
- Write all prose in [Simplified Technical English](../simplified-technical-english/SKILL.md).

## Proposal format

```markdown
### <N>. <new|update> `<skill-name>` (<general|repo: repo-name>): <one line that states the change>

**Evidence**
- [[memory-slug]]: <what this memory shows>
- `<project-dir>/<session-id>` (<date>): "<short quote>" — <what this chat shows>

**Why a skill:** <why this pattern must be a skill and not stay as memories or chats>

**Path:** <default path for the scope>

**Change**
<For a new skill: the full SKILL.md, frontmatter included.>
<For an update: the existing text and the replacement text, section by section.>
```
