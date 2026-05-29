---
name: madr
description: "Write and read Architectural Decision Records using the MADR (Markdown Any Decision Records) format. Use this skill whenever creating, updating, or consulting ADRs."
user-invocable: false
---

# MADR — Markdown Any Decision Records

ADRs capture a single architectural decision at the moment it was made: the context, the options considered, and the choice.
MADR ([adr.github.io/madr](https://adr.github.io/madr/)) is a lightweight markdown template for that record.

ADRs typically live under `docs/decisions/` (or `docs/adr/`) as `NNNN-short-kebab-title.md`.

## Writing an ADR

When creating or updating an ADR, load [references/format.md](references/format.md) for the full template, frontmatter fields, minimal-subset rules, and writing/updating conventions.

Quick triggers — write an ADR when a decision is:

- Architecturally significant (hard to reverse, affects multiple components)
- Non-obvious or contested (reviewers will ask "why not X?")
- Worth explaining to someone who joins the team next year

Skip ADRs for trivial or easily reversible choices.

## Reading ADRs

> [!CAUTION]
> **ADRs are a log, not a spec.** They describe the state of thinking on the day each one was written. They are never retroactively edited to reflect later reality.

Before treating an ADR as authoritative for *current* behavior:

1. **Check the status.** Possible values: `proposed`, `accepted`, `rejected`, `deprecated`, `superseded by ADR-NNNN`. Ignore `rejected`; treat `deprecated` and `superseded` as historical only.
2. **Follow supersession chains forward.** An old ADR may point to a newer one that revises or replaces it.
3. **Cross-check against code.** A decision recorded years ago may have been silently undone since — the ADR will not tell you.
4. **Use ADRs for the "why".** For the "what is true now", read the code, tests, or current architecture docs.

When mapping a system's decision history, sort ADRs by number and skim titles + statuses first; only then drill into bodies.
