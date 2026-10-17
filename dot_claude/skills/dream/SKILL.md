---
name: dream
description: "Read stored memories and propose new skills or updates to existing skills. Shows proposals in chat for review. Changes a skill only after approval."
---

# Dream

Reads memories, finds the patterns that a skill must capture, and shows proposals in chat for review.

## Inputs

- **Memories.** The memory directory that your system prompt names. Read `MEMORY.md` first, then the memory files it points to.
- **Skills.** Every `SKILL.md` under `~/.claude/skills/` and `.claude/skills/` in the working directory, and the skill list in your context. Read the frontmatter of each, and the body of each skill that a candidate touches.

Read the inputs again on each run.
The skill files are the only record of what exists.
A proposal from an earlier run in this chat is not a skill, even if it looks complete.
Do not treat it as done and do not build on it.
If it is still valid and nobody applied it, propose it again.

## Rejections

If the user rejected a proposal in this chat, do not propose it again.
Propose it again only if new memories give new evidence, and state the new evidence.

## Steps

1. Read all inputs.
2. Make a list of candidates. A candidate is one of:
   - **New skill.** Two or more memories describe the same repeated workflow, rule or domain, and no skill covers it.
   - **Skill update.** One or more memories correct, extend or conflict with an existing skill.
3. Remove each candidate that is a one-time fact, a project status or a personal preference with no repeated use. These stay as memories.
4. Remove each candidate that the user rejected. See [Rejections](#rejections).
5. Sort the remaining candidates by evidence, strongest first. Strong evidence is more memories, `feedback` memories, and corrections the user gave more than once.
6. Show each candidate in chat. See [Proposal format](#proposal-format).
7. Ask the user to approve or reject each proposal by its number.
8. If no candidate is left, print `No new proposals.` and stop.

## After review

- **Approved.** Ask the user where to put the skill. For an update, give the path of the existing skill as the first option. Make the change only at the path that the user gives. If the path is in `~/.local/share/chezmoi/`, tell the user to run `chezmoi apply`.
- **Rejected.** Make no change. Remember the rejection for the rest of this chat.
- **No answer.** Make no change.

## Rules

- Do not change a skill before the user approves the proposal.
- Do not edit or delete any memory.
- Cite each memory that a proposal uses. A proposal with no cited memory is not valid.
- Write all prose in [Simplified Technical English](../simplified-technical-english/SKILL.md).

## Proposal format

```markdown
### <N>. <new|update> `<skill-name>`: <one line that states the change>

**Evidence**
- [[memory-slug]]: <what this memory shows>

**Why a skill:** <why this pattern must be a skill and not stay as memories>

**Change**
<For a new skill: the full SKILL.md, frontmatter included.>
<For an update: the existing text and the replacement text, section by section.>
```
