---
name: pull-requests
description: "Write PR descriptions that serve as a durable, readable record for both the current reviewer and future readers. Use this skill whenever opening or editing a pull request description."
user-invocable: false
---

# Pull Requests

A PR description is a **permanent record**, not a disposable ticket. The current reviewer reads it once; future engineers read it when they `git blame` a line of code months or years later trying to understand *why* a change was made. Write for both audiences.

Aim for a description that answers, without the reader needing to click anywhere else:

- **Why** the change exists (motivation, linked issue, user report)
- **What** changed at a high level (not a line-by-line diff — that's what the diff is for)
- **How** to verify it works (test plan, manual steps, screenshots)
- **Risks / follow-ups** the reader should know about

Use GitHub's advanced markdown features to keep long descriptions scannable. Below are the tools worth reaching for.

## 1. Alerts (callouts)

Reference: <https://github.com/orgs/community/discussions/16925>

Highlight critical information so reviewers cannot miss it. Five types, case-sensitive, on their own line:

```markdown
> [!NOTE]
> General information the reader should notice.

> [!TIP]
> Optional guidance that makes the reviewer's life easier.

> [!IMPORTANT]
> Crucial for understanding or reviewing this change.

> [!WARNING]
> Demands immediate attention — breaking change, migration required, etc.

> [!CAUTION]
> Negative consequences possible — data loss risk, security implication.
```

Use sparingly. Everything-is-important means nothing is.

## 2. Collapsed sections

Reference: <https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/organizing-information-with-collapsed-sections>

Hide long output (logs, full stack traces, verbose test results, auxiliary screenshots) behind a `<details>` block so the main narrative stays readable. The content remains searchable and permanently archived.

```markdown
<details>
<summary>Full test output (142 lines)</summary>

```
$ cargo test --all
    Finished test [unoptimized + debuginfo] target(s) in 0.12s
     Running unittests src/lib.rs
...
```

</details>
```

Add `open` to start expanded: `<details open>`. Leave one blank line after `<summary>` so nested markdown renders correctly.

## 3. Code blocks with syntax highlighting

Reference: <https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks>

Always tag fenced blocks with a language identifier. Highlighted code is faster to read and signals intent (shell command vs. config vs. source).

~~~markdown
```rust
fn main() {
    println!("Hello, world!");
}
```

```bash
$ cargo run --release
```

```diff
- let timeout = Duration::from_secs(5);
+ let timeout = Duration::from_secs(30);
```
~~~

The `diff` language is especially useful in PR descriptions to show the conceptual change without quoting the real diff.

## 4. Diagrams

Reference: <https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-diagrams>

When the change touches control flow, data flow, or system architecture, a small diagram saves paragraphs of prose. GitHub renders Mermaid, GeoJSON, TopoJSON, and ASCII STL from fenced code blocks.

**Flowchart:**

~~~markdown
```mermaid
flowchart LR
    A[Client] -->|POST /auth| B(API)
    B --> C{Valid token?}
    C -->|yes| D[Issue session]
    C -->|no| E[401]
```
~~~

**Sequence diagram — useful for request/response changes:**

~~~markdown
```mermaid
sequenceDiagram
    Client->>+API: GET /users/123
    API->>+Cache: lookup(123)
    Cache-->>-API: miss
    API->>+DB: SELECT ...
    DB-->>-API: row
    API-->>-Client: 200 OK
```
~~~

Prefer Mermaid over pasted screenshots of diagrams: it stays editable, diffable, and readable on mobile.

## Putting it together — example skeleton

```markdown
## Summary

Fixes the 5-second auth timeout that caused intermittent 504s for users on
high-latency connections (see #4821).

> [!IMPORTANT]
> Increases default request timeout from 5s → 30s. Downstream services
> relying on the old timeout should review before merging.

## Change

```diff
- let timeout = Duration::from_secs(5);
+ let timeout = Duration::from_secs(30);
```

## Flow

```mermaid
sequenceDiagram
    Client->>API: request
    API->>Upstream: forward (now 30s budget)
    Upstream-->>API: response
    API-->>Client: 200
```

## Test plan

- [x] Unit test covering new timeout value
- [x] Manual: reproduced original 504 on staging, confirmed fixed

<details>
<summary>Repro logs (before fix)</summary>

```
2026-04-22T14:03:11Z ERROR upstream timeout after 5.01s
...
```

</details>
```

## Checklist before submitting

- [ ] Title is imperative and under ~70 chars
- [ ] Reader can understand *why* without opening linked tickets
- [ ] Long logs / output hidden behind `<details>`
- [ ] Code blocks have language tags
- [ ] Alerts used only for genuinely critical notes
- [ ] A future reader doing `git blame` six months from now will have what they need
