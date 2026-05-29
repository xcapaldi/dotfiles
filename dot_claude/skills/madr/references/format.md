# MADR Writing Format

Reference for authoring ADRs in the MADR 4.0 format. See <https://adr.github.io/madr/> for the upstream spec.

## File layout

Store records under `docs/decisions/` (or `docs/adr/`) using the pattern:

```
NNNN-short-kebab-title.md
```

`NNNN` is a zero-padded sequence number (`0001`, `0002`, …). Numbers are immutable once assigned, even if the ADR is later rejected or superseded.

## Full template

```markdown
---
status: "{proposed | rejected | accepted | deprecated | superseded by ADR-0123}"
date: {YYYY-MM-DD when the status last changed}
decision-makers: {list everyone involved in the decision}
consulted: {list everyone whose opinions were sought}
informed: {list everyone kept up-to-date on progress}
---

# {short title — the problem and the chosen solution}

## Context and Problem Statement

{Describe the context and problem in 2–3 sentences. You may want to articulate
the problem as a question and add links to collaboration boards or issue trackers.}

## Decision Drivers

- {driver 1, e.g. a force, facing concern, …}
- {driver 2}
- …

## Considered Options

- {option 1}
- {option 2}
- {option 3}
- …

## Decision Outcome

Chosen option: "{option N}", because {justification — e.g. only option that
meets driver X | resolves force Y | comes out best (see below)}.

### Consequences

- Good, because {positive consequence}
- Bad, because {negative consequence}
- …

### Confirmation

{How will the implementation of this decision be confirmed? E.g. peer review,
ArchUnit test, automated check in CI. Without it, the ADR is just an aspiration.}

## Pros and Cons of the Options

### {option 1}

{example | description | pointer to more information | …}

- Good, because {argument a}
- Neutral, because {argument b}
- Bad, because {argument c}
- …

### {option 2}

…

## More Information

{Links to related ADRs, design docs, tickets, follow-up tasks, the team that
will be informed of the decision, etc.}
```

## Minimal template

Only these are required:

- `status` frontmatter
- Title
- *Context and Problem Statement*
- *Considered Options*
- *Decision Outcome*

Add the other sections when they carry weight.

## Writing rules

- **One decision per file.** If you find yourself documenting two, split them.
- **Past-tense for context, present-tense for the decision.** "We were seeing X" / "We choose Y."
- **Name real alternatives.** "Doing nothing" counts. A single-option ADR is a red flag — there was always at least one other path.
- **Be honest about trade-offs.** Every "Good, because" deserves a matching "Bad, because." If you can't find one, you haven't thought hard enough.
- **Link, don't duplicate.** Point at tickets, RFCs, benchmarks — don't inline them.
- **Date the status, not the file.** The frontmatter date tracks when the *status* last changed (e.g. `accepted` → `superseded`), not when the file was edited for typos.

## Updating an existing ADR

Once accepted, an ADR is **append-only in spirit**:

- Fix typos and broken links freely.
- Do **not** rewrite the context or decision to match later reality. Write a new ADR instead, and mark the old one `superseded by ADR-NNNN`.
- When superseding, edit the *old* ADR's frontmatter (`status`, `date`) and add a short note under *More Information* pointing forward. Edit the *new* ADR to point back.
