---
name: codebase-to-course
description: "Turn any codebase into an educational course that teaches top-down from the observed product down to the underlying code. Use this skill whenever someone wants to create an interactive course, tutorial, or educational walkthrough from a codebase or project. Also trigger when users mention 'turn this into a course,' 'explain this codebase interactively,' 'teach this code,' 'interactive tutorial from code,' 'codebase walkthrough,' 'learn from this codebase,' or 'make a course from this project.'"
disable-model-invocation: true
user-invocable: true
argument-hint: "[codebase] [format:html|markdown]"
allowed-tools: Bash(gh repo clone *) Bash(open *)
metadata:
  inspiration: https://github.com/zarazhangrui/codebase-to-course
---

# Codebase-to-Course

Transform any codebase into an educational course that teaches top-down from the observed product down to the underlying code.

## Arguments

- `$ARGUMENTS[0]` — **codebase**: path to a local folder or a GitHub URL
- `$ARGUMENTS[1]` — **format**: `html` or `markdown` (default: `html`)

If no format is specified, default to `html`. The format determines the output structure — see [Format: HTML](#format-html) and [Format: Markdown](#format-markdown) below.

## First-Run Welcome

When the skill is first triggered and the user hasn't specified a codebase at `$ARGUMENTS[0]`, introduce yourself:

> **I can turn any codebase into an interactive course that teaches how it works.**
>
> Just point me at a project:
> - **A local folder** — e.g., `./my-project`
> - **A GitHub link** — e.g., `https://github.com/user/repo`
>
> Output formats:
> - **`html`** (default) — single self-contained HTML file with animations, quizzes, and interactive elements
> - **`markdown`** — comprehensive multi-file markdown course with code walkthroughs, diagrams, and self-assessment questions

If the user provides a GitHub link, clone the repo first (`gh repo clone <url> $TMPDIR/<repo-name>`) before starting the analysis.

## Who This Is For

The target learner is someone who wants to understand a codebase without a deep technical background. They may have built the project themselves using AI coding tools, or found an interesting open-source project and want to understand how it's built.

**Assume limited technical background.** CS concepts — from variables to APIs to databases — need plain language explanation when first introduced. No jargon without definition. The tone should be like a knowledgeable friend explaining things, not a textbook.

**Learner goals:**
- Understand how the codebase works end-to-end
- Build enough technical knowledge to make informed architectural and design decisions
- Detect when something is wrong — spot bad patterns, understand failure modes
- Debug issues and unblock themselves
- Acquire precise technical vocabulary to communicate clearly about the system
- Confidently discuss technical decisions

## Why This Approach Works

This skill inverts traditional CS education. Instead of: memorize concepts → eventually build something → finally see the point, this model is: **experience the product → see what it does → understand how it works.**

The learner already has context — they've used the app, they know what it does. The course meets them where they are: "You know that button you click? Here's what happens under the hood."

Every module answers **"why should I care?"** before "how does it work?" The answer is always practical: *because this knowledge helps you understand the system, debug problems, or make better decisions.*

---

## The Process (4 Phases)

### Phase 1: Codebase Analysis

Before writing any course content, deeply understand the codebase. Read all key files, trace data flows, identify the main components, and map how they communicate.

**What to extract:**
- The main "actors" (components, services, modules) and their responsibilities
- The primary user journey (what happens when someone uses the app end-to-end)
- Key APIs, data flows, and communication patterns
- Clever engineering patterns (caching, lazy loading, error handling, etc.)
- Real bugs or gotchas (if visible in git history or comments)
- The tech stack and why each piece was chosen

**Figure out what the app does yourself** by reading the README, the main entry points, and the UI code. Don't ask the user to explain the product. The course should open by explaining what the app does in plain language before diving into how it works.

### Phase 2: Curriculum Design

Structure the course as 5-8 modules. The arc starts from what the learner already knows (user-facing behavior) and moves toward what they don't (the code underneath). Zoom in progressively: start wide with the experience, then peel back layers.

| Module Position | Purpose | Why it matters |
|---|---|---|
| 1 | "Here's what this app does — and what happens when you use it" | Start with the product, then trace a core user action into the code |
| 2 | Meet the actors | Know which components exist and what each one does |
| 3 | How the pieces talk | Understand data flow to debug "it's not showing up" problems |
| 4 | The outside world (APIs, databases) | Understand external dependencies, costs, rate limits, failure modes |
| 5 | The clever tricks | Learn patterns (caching, chunking, error handling) worth understanding |
| 6 | When things break | Build debugging intuition |
| 7 | The big picture | See the full architecture for better decision-making |

Adapt the arc to the codebase's complexity — a simple CLI might need 4-5 modules, a microservices app might need 8.

**Key principle:** Every module should connect back to practical understanding. If a module doesn't help the learner understand, debug, or reason about the system better, cut or reframe it.

**Each module should contain:**
- 3-6 sections that flow within the module
- At least one code-with-English translation
- At least one interactive/engagement element (quiz, visualization, diagram, or walkthrough)
- One or two "aha!" callout boxes with universal CS insights
- A metaphor that grounds the technical concept in everyday life — NEVER reuse the same metaphor across modules, and NEVER default to the "restaurant" metaphor. Pick metaphors that organically fit the specific concept.

**Mandatory elements (every course must include ALL of these):**
- **Code <-> English Translation Blocks** — at least one per module
- **Quizzes / Self-Assessment** — at least one per module
- **Glossary definitions** — on every technical term, first use per module
- **Data Flow Walkthrough** — at least one across the course (how data moves between components)
- **Component Conversation** — at least one across the course (how components interact, presented as a dialogue)

**Do NOT present the curriculum for approval — just build it.** Design the curriculum internally, then go straight to generating the output.

### Phase 3: Build the Course

Generate the course in the requested format. See format-specific sections below.

### Phase 4: Review and Open

After generating:
- **HTML format**: open in the browser for the user to review
- **Markdown format**: summarize the file structure and point to the entry file

Walk through what was built and ask for feedback.

---

## Content Philosophy

### Show, Don't Tell

**Text limits:**
- Max **2-3 sentences** per text block. A fourth sentence means you need a visual or structural element instead.
- Every section must be **at least 50% non-prose** (diagrams, code blocks, cards, lists, structured elements).

**Convert text to visuals/structure:**
- A list of 3+ items → cards, tables, or structured callouts
- A sequence of steps → numbered step walkthrough or flow diagram
- "Component A talks to Component B" → data flow visualization or dialogue
- "This file does X, that file does Y" → file tree with annotations
- Explaining what code does → code <-> English translation block
- Comparing two approaches → side-by-side contrast

### Code <-> English Translations
Every code snippet gets a side-by-side (or interleaved in markdown) plain English translation. This is the single most valuable teaching tool for learners new to a codebase.

**Critical: Use original code exactly as-is.** Never modify, simplify, or trim code snippets. Choose naturally short, punchy snippets (5-10 lines) from the codebase that illustrate the concept well.

### One Concept Per Section
Each section teaches exactly one idea. If you need more space, add another section — don't cram.

### Metaphors First, Then Reality
Introduce every new concept with a metaphor from everyday life. Then immediately ground it: "In our code, this looks like..." Each concept deserves its own metaphor that feels natural to *that specific idea*.

### Learn by Tracing
Follow what actually happens when someone uses the app — trace the data flow end-to-end. "You know that button you click? Here's the journey your data takes."

### Glossary — No Term Left Behind
Every technical term gets a definition on first use in each module. Be extremely aggressive with definitions. If there is even a 1% chance someone doesn't know a word, define it. This includes: software names, developer terms (JSON, CLI, API, SDK), programming concepts (function, variable, class), infrastructure terms (PATH, pip, namespace), and ALL acronyms.

**The vocabulary IS the learning.** Each definition should teach the term in a way that helps the learner USE it — e.g., "A **flag** is an option you add to a command to change its behavior — like adding `--json` to get structured data instead of plain text."

### Quizzes That Test Application, Not Memory

**What to quiz (in order of value):**
1. **"What would you do?" scenarios** — new situations where the learner applies what they learned
2. **Debugging scenarios** — "A user reports X is broken. Where would you look?"
3. **Architecture decisions** — "Would you put this in the frontend or backend? Why?"
4. **Tracing exercises** — "When a user does X, trace the data path"

**What NOT to quiz:** definitions, file name recall, syntax details, anything answerable by scrolling up.

**Quiz tone:** encouraging, non-judgmental. Wrong answers get helpful explanations. Correct answers reinforce the underlying principle. No scores.

---

## Format: HTML

Generate a single self-contained HTML file with embedded CSS and JavaScript.

Read `references/design-system.md` for the complete CSS design tokens, typography, and color system. Read `references/interactive-elements.md` for implementation patterns.

### Build order:
1. **Foundation** — HTML shell with all module sections (empty), complete CSS design system, navigation with progress tracking, scroll-snap, keyboard navigation, scroll-triggered animations
2. **One module at a time** — fill in each module's content and interactive elements sequentially
3. **Polish pass** — transitions, mobile responsiveness, visual consistency

### HTML-specific interactive elements:
- **Group Chat Animation** — iMessage-style conversations between components
- **Message Flow Animation** — step-by-step packet animation between actors
- **Drag-and-Drop Quizzes** — matching concepts to descriptions
- **Spot the Bug Challenges** — click the buggy line
- **Layer Toggle Demos** — see how layers build on each other
- **Interactive Architecture Diagrams** — click components for descriptions
- **Glossary Tooltips** — hover/tap for definitions (use `cursor: pointer`, `position: fixed`, append to `document.body`)

### Critical implementation rules:
- File must be completely self-contained (only external dependency: Google Fonts CDN)
- `scroll-snap-type: y proximity` (NOT `mandatory`)
- `min-height: 100dvh` with `100vh` fallback for sections
- Only animate `transform` and `opacity` for GPU performance
- Wrap all JS in an IIFE, use `passive: true` on scroll listeners, throttle with `requestAnimationFrame`
- Touch support for drag-and-drop, keyboard navigation (arrow keys), ARIA attributes
- `white-space: pre-wrap` on all code — no horizontal scrollbars ever
- Tooltips use `position: fixed` + `getBoundingClientRect()` appended to `document.body` (prevents clipping by `overflow: hidden` containers)

### Design identity:
- Warm palette: off-white backgrounds, warm grays, NO cold whites or blues
- Bold accent: one confident color (vermillion, coral, teal — NOT purple gradients)
- Distinctive typography: Bricolage Grotesque for headings, DM Sans for body, JetBrains Mono for code
- Generous whitespace, alternating module backgrounds, dark code blocks with Catppuccin syntax highlighting
- Subtle warm shadows, never black drop shadows

---

## Format: Markdown

Generate a structured set of markdown files suitable for reading in any markdown viewer, GitHub, or static site generator.

Read `references/markdown-conventions.md` for the complete element conventions, module structure template, and design principles.

---

## Gotchas — Common Failure Points

### Tooltip/Definition Clipping (HTML)
Translation blocks use `overflow: hidden`. Tooltips must use `position: fixed` appended to `document.body`. This is the #1 HTML bug.

### Not Enough Definitions
The most common failure. Non-technical learners don't know REPL, JSON, flag, entry point, PATH, pip, namespace, function, class, module, PR, E2E. When in doubt, define it.

### Walls of Text
The course looks like a textbook. Keep to 2-3 sentences per text block max. Convert lists to cards/tables, sequences to step cards, explanations to code translations.

### Recycled Metaphors
Using "restaurant" for everything. Every module needs its own metaphor that feels inevitable for that specific concept.

### Code Modifications
Never trim, simplify, or clean up code snippets. Choose naturally short snippets from the codebase instead.

### Quiz Questions That Test Memory
"What does API stand for?" tests recall not understanding. Every quiz question should present a new scenario.

### Scroll-Snap Mandatory (HTML)
Always use `proximity`, not `mandatory`.

### Module Quality Degradation
Writing all modules in one pass causes later modules to be thin. Build one at a time.

### Missing Interactive/Engagement Elements
Every module needs at least one interactive or engagement element beyond plain text and code.

---

## Reference Files

The `references/` directory contains detailed implementation specs. Read the relevant files when you reach Phase 3:

- **`references/design-system.md`** — (HTML) CSS custom properties, color palette, typography, spacing, shadows, animations, scrollbar styling
- **`references/interactive-elements.md`** — (HTML) Implementation patterns for every interactive element type
- **`references/markdown-conventions.md`** — (Markdown) Element conventions, module template, design principles
