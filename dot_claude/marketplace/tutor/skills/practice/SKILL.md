---
name: practice
description: Run a practice session on things the user learned with the tutor. Problems need the knowledge to come to mind without a prompt. Use when the user runs /tutor:practice or asks to practice or review what they learned.
argument-hint: "[topic, optional]"
allowed-tools: Bash(${CLAUDE_PLUGIN_ROOT}/scripts/graph.py *) AskUserQuestion
---

# Practice

Topic from the user (can be empty): $ARGUMENTS

This is practice, not teaching. The learner must retrieve and apply what they
hold when the problem does not name it.

## Graph

Use `${CLAUDE_PLUGIN_ROOT}/scripts/graph.py`; `graph.py` below means that full path. Do not edit the file.

```
graph.py review [--limit N]   # gaps first, then held claims checked longest ago
graph.py find <words>
graph.py show <id>
graph.py deps <id>
graph.py check <id> ok|fail [--note "<short note>"] --src ${CLAUDE_SESSION_ID}
```

## Choose material

- With a topic: `find` the topic and use the claims and chains you get back.
- Without a topic: use `review`.
- Prefer claims that are conclusions of chains. A problem on a conclusion also
  exercises its premises.
- Mix claims from different sessions in one set when they connect.

## Run problems

Give one problem at a time. Use `AskUserQuestion` when the answer fits 2–4
options. Ask as plain text when the learner must work something out. Then end
your turn and wait.

- Vary the surface: new numbers, a new context, a different direction (given the
  result, find the cause), and spotting an error in a worked example.
- Do not name the claim the problem tests.
- Grade the reasoning, not only the result. The learner often writes their
  reasoning and confidence in the notes.
- If they are stuck, give the smallest hint that unblocks them. Hints go from a
  question, to the relevant idea, to the first step.
- After each problem, record a `check` for each claim it tested. A right answer
  after a big hint is `fail` with a note.
- Correct a mistake in one or two sentences. If it shows a real gap, say so and
  offer `/tutor:learn` on it later. Do not teach it now.

Stop after about 5–8 problems, or when the learner asks. End with one line on
what held and what did not.
