import contextlib
import importlib.util
import io
import json
import os
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent.parent / "scripts" / "executable_graph.py"
if not SCRIPT.exists():
    SCRIPT = SCRIPT.with_name("graph.py")
spec = importlib.util.spec_from_file_location("graph", SCRIPT)
graph = importlib.util.module_from_spec(spec)
spec.loader.exec_module(graph)


class GraphTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.path = Path(self.dir.name) / "sub" / "graph.jsonl"
        os.environ["TUTOR_GRAPH"] = str(self.path)

    def tearDown(self):
        self.dir.cleanup()
        del os.environ["TUTOR_GRAPH"]

    def run_cmd(self, *argv):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            graph.main(list(argv))
        return out.getvalue().strip()

    def records(self):
        return [json.loads(line) for line in self.path.read_text().splitlines()]

    def test_add_reuses_exact_match(self):
        self.assertEqual(self.run_cmd("add", "e^x is its own derivative", "--src", "s1"), "p1")
        self.assertEqual(self.run_cmd("add", "E^X is its own derivative.", "--src", "s2"), "p1 (existing)")
        self.assertEqual(self.run_cmd("add", "ln is the inverse of exp", "--src", "s1"), "p2")
        self.assertEqual(len(self.records()), 2)

    def test_check_sets_status(self):
        self.run_cmd("add", "a", "--src", "s1")
        self.assertIn("[unchecked]", self.run_cmd("find", "a"))
        self.run_cmd("check", "p1", "fail", "--note", "mixed up x", "--src", "s1")
        self.assertIn("[gap ", self.run_cmd("find", "a"))
        self.run_cmd("check", "p1", "ok", "--src", "s1")
        self.assertIn("[held ", self.run_cmd("find", "a"))
        self.assertIn("fail mixed up x", self.run_cmd("show", "p1"))

    def test_chain_and_deps(self):
        for text in ["base one", "base two", "middle", "goal"]:
            self.run_cmd("add", text, "--src", "s1")
        self.assertEqual(self.run_cmd("chain", "p2", "p1", "--to", "p3", "--src", "s1"), "c1")
        self.assertEqual(self.run_cmd("chain", "p1", "p2", "--to", "p3", "--src", "s1"), "c1 (existing)")
        self.run_cmd("chain", "p3", "--to", "p4", "--src", "s1")
        lines = self.run_cmd("deps", "p4").splitlines()
        self.assertTrue(lines[0].startswith("p4"))
        self.assertTrue(lines[1].startswith("  p3"))
        self.assertEqual({l.strip().split()[0] for l in lines[2:]}, {"p1", "p2"})

    def test_chain_rejects_self_premise(self):
        self.run_cmd("add", "x", "--src", "s1")
        with self.assertRaises(SystemExit):
            self.run_cmd("chain", "p1", "--to", "p1", "--src", "s1")

    def test_merge_moves_chains_and_evidence(self):
        self.run_cmd("add", "derivative of exp is exp", "--src", "s1")
        self.run_cmd("add", "exp differentiates to itself", "--src", "s2")
        self.run_cmd("add", "premise", "--src", "s2")
        self.run_cmd("chain", "p3", "--to", "p2", "--src", "s2")
        self.run_cmd("check", "p2", "ok", "--src", "s2")
        self.assertEqual(self.run_cmd("merge", "p2", "p1"), "p2 -> p1")
        show = self.run_cmd("show", "p1")
        self.assertIn("from c1: p3", show)
        self.assertIn(" ok", show)
        self.assertNotIn("p2", self.run_cmd("find", "exp"))
        self.assertIn("p1 [held", self.run_cmd("show", "p2"))

    def test_find_ranks_by_overlap(self):
        self.run_cmd("add", "chain rule for composite functions", "--src", "s1")
        self.run_cmd("add", "product rule", "--src", "s1")
        self.run_cmd("add", "unrelated fact", "--src", "s1")
        lines = self.run_cmd("find", "chain", "rule").splitlines()
        self.assertEqual([l.split()[0] for l in lines], ["p1", "p2"])

    def test_review_orders_gaps_then_stalest(self):
        for text in ["a", "b", "c", "d"]:
            self.run_cmd("add", text, "--src", "s1")
        with self.path.open("a") as f:
            for p, ok, d in [("p1", True, "2026-01-01"), ("p2", True, "2025-01-01"), ("p3", False, "2026-05-01")]:
                f.write(json.dumps({"t": "e", "p": p, "ok": ok, "src": "s1", "d": d}) + "\n")
        ids = [l.split()[0] for l in self.run_cmd("review").splitlines()]
        self.assertEqual(ids, ["p3", "p2", "p1"])

    def test_bad_lines_are_skipped(self):
        self.run_cmd("add", "kept", "--src", "s1")
        with self.path.open("a") as f:
            f.write("not json\n{\"t\":\"p\"}\n")
        self.assertEqual(self.run_cmd("add", "next", "--src", "s1"), "p2")

    def test_sessions(self):
        self.run_cmd("session", "abc", "understand Bayes' theorem")
        self.assertIn("abc understand Bayes' theorem", self.run_cmd("sessions"))


if __name__ == "__main__":
    unittest.main()
