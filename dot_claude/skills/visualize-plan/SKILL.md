---
name: visualize-plan
description: Render the current Claude Code plan as an HTML page and open it in the browser
argument-hint: "[path-to-plan.md] [css-file] [pandoc|gh]"
---

# Visualize Plan

Turn the plan from the current conversation into a clean HTML page and open it in the browser for easy human review.

## Steps

1. **Parse the skill arguments.**

- The skill accepts up to three positional arguments: `[path-to-plan.md] [css-file] [pandoc|gh]`.
- All three are optional.
- The second is a CSS file that replaces the built-in stylesheet, and the third forces the rendering engine (`pandoc` or `gh`).
- Forward the second and third arguments to the script unchanged.

2. **Get the plan markdown.**

- If a plan path is given as the first argument, treat it as an existing markdown file and skip to step 4.
- Otherwise use the most recent plan in the conversation -- the one just produced in plan mode / `ExitPlanMode`, or the latest implementation plan you proposed.
- If there is no plan in context, say so and stop.
- Do not invent one.

3. **Write the plan to a temp file** verbatim (no edits, no summarizing) using the Write tool, e.g. `${TMPDIR:-/tmp}/plan.md`.

4. **Render and open** by running the bundled script, forwarding any CSS file and engine arguments:

```bash
bash ~/.claude/skills/visualize-plan/scripts/render.sh "${TMPDIR:-/tmp}/plan.md" "<css-file>" "<engine>"
```

- Pass `""` for the CSS argument when only the engine is set.
- The script writes a styled HTML file and opens it in the default browser.
- The script prints the HTML path, so relay it to the user in one line.

## Notes

- Conversion uses `pandoc` (GFM) if available, otherwise falls back to the GitHub markdown API via `gh api /markdown`.
- The built-in stylesheet is driven by CSS variables and follows the system light/dark setting at view time via `prefers-color-scheme`, and it covers the GFM constructs both engines emit.
- `pandoc` is installed via the Brewfile, while the `gh` fallback needs network and an authenticated `gh` and POSTs the plan text to GitHub.
- Keep your own chat reply short; the rendered page is the deliverable.
