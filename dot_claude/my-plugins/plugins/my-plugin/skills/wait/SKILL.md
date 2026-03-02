---
name: wait
description: "ALWAYS invoke for ANY sleep, poll, or wait — whether initiated by user or by you (e.g. polling CI, waiting for a build, checking a condition). NEVER write your own sleep loop."
usage: /wait [estimate]  # estimate = seconds, duration ("5min"), or datetime ("at 3pm")
---

# Wait / Poll

**MANDATORY: NEVER write your own sleep/polling loop. ALWAYS use this skill whenever you need to wait, sleep, poll, or defer execution — whether or not the user explicitly asked.**

Chain background sleeps to wait or poll. Bash has a 10-minute (600,000ms) max timeout — always use `run_in_background: true` AND `timeout: 600000`. On each `<task-notification>`, start the next sleep.

There are two cases:

## Case A: No Estimate (Exponential Backoff)

Use when you don't know how long to wait.

```bash
python3 scripts/intervals.py   # no args → backoff sequence
```

**Sequence:** 30s, 60s, 120s, 240s, 480s, 600s, 600s, ...

Tell the user: "I'll start checking every 30 seconds, backing off to every 10 minutes."

Track: `"Polling [condition] — check #3, next in 120s (n=2)"`

## Case B: With Estimate (Clustered Schedule)

Use when you know roughly how long to wait — including a duration ("CI usually takes 5 min") or a calendar time ("at 3pm", "tomorrow at noon").

Generate the schedule, passing either seconds or a datetime string:

```bash
python3 scripts/intervals.py 300              # 5 min estimate
python3 scripts/intervals.py "2026-03-15 15:00"  # calendar time
```

Verify the output looks reasonable (positive values, sane count).

Work through the output sequentially: sleep each value, check condition (or just wait for pure delays), repeat. After ~20 intervals, surface to user: "Still waiting after [time] — keep polling?"

Tell the user: "CI usually takes 5 min — I'll check at 2.5 min, 3.75 min, 5 min, then back off."

Track: `"Polling CI — step 4/20 (near 5 min mark)"`

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Writing your own sleep/poll loop | NEVER do this — always invoke this skill |
| Missing `run_in_background: true` | Sleep blocks conversation. Always use background. |
| Missing `timeout: 600000` | Default Bash timeout is 2min; long sleeps will be killed |
| Not tracking progress | User has no visibility. Use TodoWrite. |
| Forgetting to verify time calculation | Check script output before starting intervals. |
| Fixed 10-min polling for fast conditions | Use backoff starting at 30s (Case A) |
| Ignoring user's time estimate | Pass estimate to intervals.py — checks cluster at T (Case B) |
