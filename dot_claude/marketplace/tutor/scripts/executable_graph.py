#!/usr/bin/env python3
"""Sparse learning graph for the tutor plugin.

Append-only JSONL. Every line is one record; current state is folded on read.

  {"t":"s","id":S,"goal":G,"d":D}             session and its goal
  {"t":"p","id":"p1","s":TEXT,"src":S}        proposition
  {"t":"c","id":"c1","pre":[..],"con":P,"src":S}  premises jointly imply con
  {"t":"e","p":P,"ok":BOOL,"n":NOTE,"src":S,"d":D}  check evidence
  {"t":"m","from":P,"to":P}                   merge duplicate into target

Path: $TUTOR_GRAPH, else ~/.claude/tutor/graph.jsonl.
"""

import argparse
import datetime
import json
import os
import re
import sys
from pathlib import Path


def graph_path():
    env = os.environ.get("TUTOR_GRAPH")
    return Path(env) if env else Path.home() / ".claude" / "tutor" / "graph.jsonl"


def norm(text):
    return " ".join(re.findall(r"\w+", text.lower()))


class Graph:
    def __init__(self, path):
        self.path = path
        self.sessions = {}
        self.props = {}
        self.chains = {}
        self.evidence = {}
        self.merged = {}
        if path.exists():
            with path.open() as f:
                for line in f:
                    try:
                        self._load(json.loads(line))
                    except (ValueError, KeyError, TypeError):
                        continue
        # Fold merges: chains and evidence point at surviving propositions.
        for c in self.chains.values():
            c["pre"] = sorted({self.resolve(p) for p in c["pre"]})
            c["con"] = self.resolve(c["con"])
        folded = {}
        for p, evs in self.evidence.items():
            folded.setdefault(self.resolve(p), []).extend(evs)
        for evs in folded.values():
            evs.sort(key=lambda e: e.get("d", ""))
        self.evidence = folded

    def _load(self, r):
        t = r["t"]
        if t == "s":
            self.sessions[r["id"]] = r
        elif t == "p":
            self.props[r["id"]] = r
        elif t == "c":
            self.chains[r["id"]] = {**r, "pre": list(r["pre"])}
        elif t == "e":
            self.evidence.setdefault(r["p"], []).append(r)
        elif t == "m":
            self.merged[r["from"]] = r["to"]

    def resolve(self, pid):
        seen = set()
        while pid in self.merged and pid not in seen:
            seen.add(pid)
            pid = self.merged[pid]
        return pid

    def live(self):
        return {k: v for k, v in self.props.items() if k not in self.merged}

    def require(self, pid):
        rid = self.resolve(pid)
        if rid not in self.props:
            sys.exit(f"unknown proposition: {pid}")
        return rid

    def status(self, pid):
        evs = self.evidence.get(pid)
        if not evs:
            return "unchecked"
        last = evs[-1]
        return ("held " if last["ok"] else "gap ") + last.get("d", "?")

    def next_id(self, prefix, existing):
        nums = [int(k[1:]) for k in existing if k[1:].isdigit()]
        return f"{prefix}{max(nums, default=0) + 1}"

    def append(self, record):
        self.path.parent.mkdir(parents=True, exist_ok=True)
        with self.path.open("a") as f:
            f.write(json.dumps(record, ensure_ascii=False, separators=(",", ":")) + "\n")

    def line(self, pid):
        return f"{pid} [{self.status(pid)}] {self.props[pid]['s']}"


def today():
    return datetime.date.today().isoformat()


def cmd_session(g, a):
    g.append({"t": "s", "id": a.id, "goal": a.goal, "d": today()})
    print(a.id)


def cmd_find(g, a):
    terms = set(norm(" ".join(a.text)).split())
    scored = []
    for pid, p in g.live().items():
        words = set(norm(p["s"]).split())
        hit = len(terms & words) + sum(
            1 for t in terms for w in words if t != w and len(t) > 3 and (t in w or w in t)
        ) * 0.5
        if hit:
            scored.append((hit, pid))
    scored.sort(key=lambda x: (-x[0], int(x[1][1:])))
    for _, pid in scored[: a.limit]:
        print(g.line(pid))


def cmd_show(g, a):
    pid = g.require(a.id)
    print(g.line(pid))
    for c in g.chains.values():
        if c["con"] == pid:
            print(f"  from {c['id']}: " + " + ".join(c["pre"]))
        elif pid in c["pre"]:
            print(f"  supports {c['con']} via {c['id']}")
    for e in g.evidence.get(pid, []):
        mark = "ok" if e["ok"] else "fail"
        note = f" {e['n']}" if e.get("n") else ""
        print(f"  {e.get('d', '?')} {mark}{note}")


def cmd_deps(g, a):
    root = g.require(a.id)
    seen = set()

    def walk(pid, depth):
        print("  " * depth + g.line(pid))
        if pid in seen:
            return
        seen.add(pid)
        for c in g.chains.values():
            if c["con"] == pid:
                for pre in c["pre"]:
                    walk(pre, depth + 1)

    walk(root, 0)


def cmd_add(g, a):
    text = a.text.strip()
    key = norm(text)
    for pid, p in g.live().items():
        if norm(p["s"]) == key:
            print(f"{pid} (existing)")
            return
    pid = g.next_id("p", g.props)
    g.append({"t": "p", "id": pid, "s": text, "src": a.src})
    print(pid)


def cmd_chain(g, a):
    pre = sorted({g.require(p) for p in a.pre})
    con = g.require(a.to)
    if con in pre:
        sys.exit("conclusion cannot be its own premise")
    for c in g.chains.values():
        if c["con"] == con and c["pre"] == pre:
            print(f"{c['id']} (existing)")
            return
    cid = g.next_id("c", g.chains)
    g.append({"t": "c", "id": cid, "pre": pre, "con": con, "src": a.src})
    print(cid)


def cmd_check(g, a):
    pid = g.require(a.id)
    rec = {"t": "e", "p": pid, "ok": a.result == "ok", "src": a.src, "d": today()}
    if a.note:
        rec["n"] = a.note
    g.append(rec)
    print(f"{pid} {a.result}")


def cmd_merge(g, a):
    src, dst = g.require(a.src_id), g.require(a.into)
    if src == dst:
        sys.exit("already the same proposition")
    g.append({"t": "m", "from": src, "to": dst})
    print(f"{src} -> {dst}")


def cmd_review(g, a):
    rows = []
    for pid in g.live():
        evs = g.evidence.get(pid)
        if not evs:
            continue
        last = evs[-1]
        rows.append((last["ok"], last.get("d", ""), pid))
    rows.sort(key=lambda r: (r[0], r[1]))
    for _, _, pid in rows[: a.limit]:
        print(g.line(pid))


def cmd_sessions(g, a):
    for s in sorted(g.sessions.values(), key=lambda s: s.get("d", "")):
        print(f"{s.get('d', '?')} {s['id']} {s['goal']}")


def main(argv=None):
    ap = argparse.ArgumentParser(prog="graph.py", description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)

    p = sub.add_parser("session", help="record a session and its goal")
    p.add_argument("id")
    p.add_argument("goal")
    p.set_defaults(fn=cmd_session)

    p = sub.add_parser("find", help="search propositions by words")
    p.add_argument("text", nargs="+")
    p.add_argument("--limit", type=int, default=15)
    p.set_defaults(fn=cmd_find)

    p = sub.add_parser("show", help="one proposition with its chains and evidence")
    p.add_argument("id")
    p.set_defaults(fn=cmd_show)

    p = sub.add_parser("deps", help="dependency tree under a proposition")
    p.add_argument("id")
    p.set_defaults(fn=cmd_deps)

    p = sub.add_parser("add", help="add a proposition (reuses an exact match)")
    p.add_argument("text")
    p.add_argument("--src", required=True)
    p.set_defaults(fn=cmd_add)

    p = sub.add_parser("chain", help="record premises that jointly imply a conclusion")
    p.add_argument("pre", nargs="+")
    p.add_argument("--to", required=True)
    p.add_argument("--src", required=True)
    p.set_defaults(fn=cmd_chain)

    p = sub.add_parser("check", help="record check evidence for a proposition")
    p.add_argument("id")
    p.add_argument("result", choices=["ok", "fail"])
    p.add_argument("--note")
    p.add_argument("--src", required=True)
    p.set_defaults(fn=cmd_check)

    p = sub.add_parser("merge", help="merge a duplicate proposition into another")
    p.add_argument("src_id", metavar="from")
    p.add_argument("into")
    p.set_defaults(fn=cmd_merge)

    p = sub.add_parser("review", help="practice candidates: gaps, then stalest held")
    p.add_argument("--limit", type=int, default=15)
    p.set_defaults(fn=cmd_review)

    p = sub.add_parser("sessions", help="list sessions and goals")
    p.set_defaults(fn=cmd_sessions)

    a = ap.parse_args(argv)
    a.fn(Graph(graph_path()), a)


if __name__ == "__main__":
    main()
