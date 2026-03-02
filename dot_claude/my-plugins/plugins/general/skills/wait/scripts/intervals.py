#!/usr/bin/env python3
"""Generate a wait/poll interval schedule.

Usage:
  intervals.py                   # no estimate: exponential backoff
  intervals.py <seconds>         # seconds estimate: clustered schedule
  intervals.py <datetime string> # calendar time: calculate then cluster

Output: one integer sleep duration per line (30-600s each), up to 20 values.
"""

import sys
from datetime import datetime

MAX = 590  # Bash hard limit is 600s; leave buffer
MIN = 30
LIMIT = 20


def backoff():
    n, out = 0, []
    while len(out) < LIMIT:
        out.append(min(30 * (2 ** n), MAX))
        n += 1
    return out


def clustered(T):
    out = []
    if T < MIN:
        out.append(MIN)
    else:
        accumulated, step = 0, T / 2
        while step >= MIN:
            s = min(int(step), MAX)
            out.append(s)
            accumulated += s
            step /= 2
        remaining = T - accumulated
        if remaining >= MIN:
            out.append(min(int(remaining), MAX))
        for gap in [30, 60, 120]:
            out.append(gap)

    val = 120
    while len(out) < LIMIT:
        out.append(min(val, MAX))
        val = min(val * 2, MAX)
    return out[:LIMIT]


def parse_datetime(s):
    for fmt in [
        "%a %b %d %H:%M:%S %Z %Y",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M",
        "%Y-%m-%d",
        "%b %d %H:%M:%S %Y",
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y",
    ]:
        try:
            return datetime.strptime(s, fmt)
        except ValueError:
            continue
    raise ValueError(f"Cannot parse: {s!r}")


if len(sys.argv) == 1:
    intervals = backoff()
else:
    arg = " ".join(sys.argv[1:])
    try:
        T = int(arg)
    except ValueError:
        try:
            dt = parse_datetime(arg)
            T = int((dt - datetime.now()).total_seconds())
            if T <= 0:
                print("Target time is in the past", file=sys.stderr)
                sys.exit(1)
        except ValueError as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)
    intervals = clustered(T)

for s in intervals:
    print(s)
