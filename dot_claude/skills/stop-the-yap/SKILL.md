---
name: stop-the-yap
description: "Decide if a code comment must exist, and make the ones that stay explain why, not what."
---

# Stop the yap

The default is no comment.
Clear code and names carry most of the meaning.
A comment earns its place only when it tells the reader something the code cannot.

## The gate

Before you write a comment, answer one question:

> What does this tell a future reader that the code does not?

If the answer is "it repeats the code", delete it.
Rename the variable or extract a function instead.

Comments that pass the gate:

- `// Stripe sends the amount in cents; the rest of the system uses dollars`
- `// Kept in sync with the enum in migrations/0042; update both`

## Delete these

**Narration.** `// increment the counter` above `counter += 1`.
If a block needs narration, write smaller functions and better names.

**Change history and chat context.** This belongs in the commit message and the PR description, where it is attached to the diff.
In the source it goes stale immediately.

- `// previously used a set, switched to a list for ordering`
- `// per PR #1234`, `// as discussed`
- `// AI: generated this helper`
- `// TODO(2024-01): remove after migration`, long after the migration

**Perishable numbers and current-state stamps.** Measured times, counts and rates rot in silence.
Nothing makes them update, and a stale number misleads the next person who sizes a timeout.
Drop "currently" and "today" hedges, because the sentence states the same fact without them.
State the durable relation the number stood for.

Numbers that stay:

- A dated snapshot: `// as of August 2024, Homebrew ships 4.13.2`
- A restated code literal: `// runs longer than 5 min (300 seconds)` beside the `300`
- A platform constant: `// GitHub comment size limit (~64KB)`
- A target: `// Target: ~15 min per shard`
- Cited evidence: `// 30% peak memory on 16-core runs (#46853)`

**Commented-out code.** The version history has it.
The next reader cannot tell if it is a note, a rollback plan or an accident.

**Docstrings that repeat the signature.** `"""Gets the user by id."""` on `get_user_by_id`. `// type: string` on a typed field.

## Keep these

- A **why** that is not obvious: a workaround, a trade-off, a spec quirk, an order constraint.
- A **warning** about a consequence elsewhere: "changing this breaks the cache key", "callers rely on this being sorted".
- A **pointer** to context the reader cannot rebuild from the repo: a spec link, a ticket, the reason for a surprising value.

## Style

- Write in [Simplified Technical English](../simplified-technical-english/SKILL.md): active voice, simple tenses, one idea per sentence, consistent terms.
- Be explicit. Name the conditions, values and consequences. Do not make the reader rebuild your reasoning from a hint.
- Let length follow the content. One line is enough when one line covers it. Neither short nor long is the goal.
- No em-dash. Use a connective: "because", "so that", "which means", "to avoid". `// batch here to avoid an N+1 query`, not `// batch here — avoids N+1`.
- Keep existing comments when you move or refactor code, unless the change makes them wrong.
- Match the density around you. Do not comment every line of a file that had none.

## Go

Doc comments follow <https://go.dev/doc/comment>.
Start the sentence with the name of the thing: `// Reload reads the config from disk.`
Comment exported identifiers when the name alone does not carry the contract.
Do not write a doc comment that only repeats the function name.

## When you want to comment

Try, in order: a better name, a smaller function, a type.
Reach for a comment only when none of those carries the meaning.
