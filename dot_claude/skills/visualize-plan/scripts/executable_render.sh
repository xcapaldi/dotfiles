#!/usr/bin/env bash
# Render a markdown plan to a styled, readable HTML page and open it in the browser.
# Usage: render.sh [plan.md] [css-file] [engine]
#   plan.md   markdown source; reads stdin when "-" or omitted
#   css-file  path to a stylesheet to use instead of the built-in one (skip with "")
#   engine    force "pandoc" or "gh"; default auto-detects (pandoc, else gh)
# Markdown->HTML conversion uses pandoc if available, else falls back to the
# GitHub markdown API via `gh`. The built-in stylesheet is driven by CSS
# variables and follows the system light/dark setting at view time via
# prefers-color-scheme. It covers the GFM constructs both engines emit
# (headings, code, tables, task lists, and alert/admonition blocks).
# Prints the path of the generated HTML file on stdout.
set -euo pipefail

SRC="${1:--}"
[ "$SRC" = "-" ] && SRC=/dev/stdin
CSS_OVERRIDE="${2:-}"
ENGINE="${3:-}"

[ -z "$CSS_OVERRIDE" ] || [ -r "$CSS_OVERRIDE" ] || {
  echo "render.sh: css file not readable: $CSS_OVERRIDE" >&2; exit 1; }

MD=$(cat "$SRC")
[ -n "$MD" ] || { echo "render.sh: empty plan" >&2; exit 1; }

# Render the GFM fragment with the chosen (or auto-detected) engine.
render_fragment() {
  local engine="$ENGINE"
  if [ -z "$engine" ]; then
    if command -v pandoc >/dev/null 2>&1; then engine=pandoc
    elif command -v gh >/dev/null 2>&1; then engine=gh
    else
      echo "render.sh: need pandoc or gh — install pandoc (brew install pandoc) or authenticate gh" >&2
      exit 1
    fi
  fi
  case "$engine" in
    pandoc)
      command -v pandoc >/dev/null 2>&1 || { echo "render.sh: engine 'pandoc' not found" >&2; exit 1; }
      printf '%s' "$MD" | pandoc -f gfm -t html ;;
    gh)
      command -v gh >/dev/null 2>&1 || { echo "render.sh: engine 'gh' not found" >&2; exit 1; }
      printf '%s' "$MD" | gh api --method POST /markdown -f mode=gfm --field text=@- ;;
    *)
      echo "render.sh: unknown engine '$engine' (use pandoc or gh)" >&2; exit 1 ;;
  esac
}
FRAGMENT=$(render_fragment)

# Built-in stylesheet. Colors live in CSS variables: light values in :root, with
# a prefers-color-scheme override so the browser picks light/dark at view time.
builtin_css() {
  cat <<'CSS'
:root {
  color-scheme: light dark;
  --bg:#fff; --fg:#1a1a1a; --accent:#0969da; --code-bg:#f6f8fa; --border:#e2e2e2; --muted:#555;
  --note:#0969da; --tip:#1a7f37; --important:#8250df; --warning:#9a6700; --caution:#cf222e;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg:#1a1a1a; --fg:#d4d4d4; --accent:#6cb6ff; --code-bg:#2a2a2a; --border:#333; --muted:#aaa;
    --note:#4493f8; --tip:#3fb950; --important:#ab7df8; --warning:#d29922; --caution:#f85149;
  }
}
* { box-sizing: border-box; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  line-height: 1.65; font-size: 15px; max-width: 64rem;
  margin: 0 auto; padding: 3rem 1.5rem 6rem;
  color: var(--fg); background: var(--bg); -webkit-font-smoothing: antialiased;
}
h1, h2, h3, h4 { line-height: 1.25; margin: 2rem 0 1rem; font-weight: 650; }
h1 { font-size: 2rem; border-bottom: 1px solid var(--border); padding-bottom: .3rem; }
h2 { font-size: 1.5rem; border-bottom: 1px solid var(--border); padding-bottom: .3rem; }
h3 { font-size: 1.2rem; }
p, ul, ol, blockquote, table, pre { margin: 0 0 1rem; }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
code { font-family: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace;
  font-size: .88em; background: var(--code-bg); padding: .15em .4em; border-radius: 4px; }
pre { background: var(--code-bg); padding: 1rem; border-radius: 8px; overflow-x: auto; }
pre code { background: none; padding: 0; font-size: .85em; }
blockquote { margin-left: 0; padding-left: 1rem; border-left: 4px solid var(--border); color: var(--muted); }
table { border-collapse: collapse; width: 100%; }
th, td { border: 1px solid var(--border); padding: .5rem .75rem; text-align: left; }
th { background: rgba(127,127,127,.1); }
hr { border: none; border-top: 1px solid var(--border); margin: 2rem 0; }
ul, ol { padding-left: 1.5rem; }
li { margin: .25rem 0; }
li.task-list-item { list-style: none; margin-left: -1.5rem; }
input[type=checkbox] { margin-right: .5em; }
img { max-width: 100%; }
/* GFM alerts/admonitions. The two engines emit different markup, so style both:
   pandoc -> <div class="note"> with inner <div class="title">
   gh      -> <div class="markdown-alert markdown-alert-note"> with <p class="markdown-alert-title"> */
.markdown-alert, .note, .tip, .important, .warning, .caution {
  padding: .6rem 1rem; margin: 0 0 1rem;
  border-left: .25rem solid var(--alert, #888); border-radius: 0 6px 6px 0;
}
.markdown-alert > :first-child, .note > :first-child, .tip > :first-child,
.important > :first-child, .warning > :first-child, .caution > :first-child { margin-top: 0; }
.markdown-alert > :last-child, .note > :last-child, .tip > :last-child,
.important > :last-child, .warning > :last-child, .caution > :last-child { margin-bottom: 0; }
.markdown-alert-title,
.note > .title, .tip > .title, .important > .title,
.warning > .title, .caution > .title { font-weight: 600; color: var(--alert, #888); }
.note > .title > p, .tip > .title > p, .important > .title > p,
.warning > .title > p, .caution > .title > p { margin: 0; }
.markdown-alert .octicon { vertical-align: text-bottom; margin-right: .4em; fill: currentColor; }
.markdown-alert-note, .note { --alert: var(--note); }
.markdown-alert-tip, .tip { --alert: var(--tip); }
.markdown-alert-important, .important { --alert: var(--important); }
.markdown-alert-warning, .warning { --alert: var(--warning); }
.markdown-alert-caution, .caution { --alert: var(--caution); }
CSS
}

# Tab title from the first level-1 heading, if any.
TITLE=$(printf '%s\n' "$MD" | sed -n 's/^#[[:space:]]\{1,\}//p' | head -1)
TITLE=${TITLE:-Plan}
OUT="${TMPDIR:-/tmp}/claude-plan-$(date +%Y%m%d-%H%M%S)-$$.html"

{
  printf '<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n'
  printf '<meta name="viewport" content="width=device-width, initial-scale=1">\n'
  printf '<title>%s</title>\n' "$TITLE"
  printf '<style>\n'
  if [ -n "$CSS_OVERRIDE" ]; then
    cat "$CSS_OVERRIDE"
  else
    builtin_css
  fi
  printf '</style>\n</head>\n<body>\n'
  printf '%s\n' "$FRAGMENT"
  printf '</body>\n</html>\n'
} > "$OUT"

if command -v open >/dev/null 2>&1; then
  open "$OUT"
elif command -v xdg-open >/dev/null 2>&1; then
  xdg-open "$OUT" >/dev/null 2>&1 &
fi

echo "$OUT"
