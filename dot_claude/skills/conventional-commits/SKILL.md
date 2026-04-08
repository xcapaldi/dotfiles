---
name: conventional-commits
description: "Commit using Conventional Commits spec limited to fix/feat/docs/chore types. Use this skill whenever committing code changes."
user-invocable: false
---

# Conventional Commits

When committing, use the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) specification with only these types:

- **fix**: bug fix (correlates with PATCH in semver)
- **feat**: new feature (correlates with MINOR in semver)
- **docs**: documentation-only changes
- **chore**: maintenance tasks (deps, CI, config, tooling, etc.)

## Format

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

## Rules

- Type is required, scope is optional
- Description: imperative mood, lowercase start, no period at end
- Breaking changes: append `!` after type/scope (e.g. `feat!: remove X`) and/or add `BREAKING CHANGE:` footer
- Keep subject line concise; use body for additional context only when needed
- One logical change per commit

## Examples

```
fix: prevent duplicate entries in cache
feat(auth): add OAuth2 support
docs: update API usage examples
chore: bump dependencies
feat!: drop Node 14 support

BREAKING CHANGE: minimum Node version is now 16
```
