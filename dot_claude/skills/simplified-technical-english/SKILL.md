---
name: simplified-technical-english
description: "Write clear, unambiguous technical prose in the spirit of ASD-STE100 (Simplified Technical English). Use whenever writing prose meant to be read by others -- PR descriptions, commit bodies, docs, READMEs, comments, issues"
user-invocable: false
---

# Simplified Technical English

Reference: <https://www.asd-ste100.org/>

ASD-STE100 (Simplified Technical English, STE) is a controlled language for technical documentation.
It exists to make text easy to understand for every reader -- non-native English speakers, and strangers reading it years later.
Write in its spirit: plain, direct, and unambiguous.

STE is a *style*, not a specific format.
Apply these rules to whatever you are writing.

## Core rules

- **Short sentences.** One idea per sentence. Aim for ≤ 20 words; ≤ 25 at most.
- **Active voice.** "The worker updates the record", not "The record is updated".
- **Present tense** where possible. Describe what the code or change does, not what it did.
- **One word, one meaning.** Pick a term and reuse it. Do not vary wording for style -- call the same thing by the same name every time.
- **One meaning, one word.** Do not use a single word for several ideas (e.g. don't use "follow" for both "obey" and "come after").
- **Simple, common words.** Prefer plain words over jargon, Latinate verbs, and idioms. "use" not "utilize"; "about" not "regarding"; "start" not "initiate".
- **No idioms, slang, or clever phrasing.** They confuse non-native readers.
- **Say it directly.** Cut hedging ("it seems", "arguably"), filler ("in order to" -> "to"), and throat-clearing. State what changed and why.
- **One instruction per sentence** in procedures. Split compound steps.
- **Positive over negative.** "Keep the file open" reads faster than "Do not close the file".
- **Spell out or define acronyms** on first use unless they are universal.

## Quick before/after

Before:

> In order to facilitate the resolution of the aforementioned issue, the timeout
> configuration was subsequently modified such that requests are no longer
> prematurely terminated.

After:

> This raises the request timeout from 5s to 30s. Requests no longer time out
> early.

## When to apply

Apply STE to any prose others will read: PR descriptions, commit message bodies, docs, READMEs, code comments, issue reports, and design notes.
It does not apply to code identifiers or to direct quotes you must preserve verbatim.
