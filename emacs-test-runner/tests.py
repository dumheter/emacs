"""Runner regressions: python emacs-test-runner\\tests.py RUNNER."""

import json
import pathlib
import struct
import subprocess
import sys
import tempfile
import threading
import unittest
import xml.etree.ElementTree as ET

UNKNOWN = 2**64 - 1
HEADER = struct.Struct("=8sIIQQ")


def write_cache(path, names, durations, overhead):
    block = bytearray()
    offsets = []
    for name in names:
        offsets.append(len(block))
        block.extend(name.encode("utf-8") + b"\0")
    path.write_bytes(HEADER.pack(b"ETRCACHE", 1, len(names), len(block), overhead)
                     + struct.pack(f"={len(names)}Q", *durations)
                     + struct.pack(f"={len(names)}I", *offsets) + block)


def read_cache(path):
    data = path.read_bytes()
    magic, version, count, size, overhead = HEADER.unpack_from(data)
    assert magic == b"ETRCACHE" and version == 1
    assert len(data) == HEADER.size + count * 12 + size
    return struct.unpack_from(f"={count}Q", data, HEADER.size), overhead


def fake_gtest():
    scenario = json.loads(pathlib.Path(sys.argv[2]).read_text())
    if "--gtest_list_tests" in sys.argv:
        if scenario["forbid_discovery"]:
            sys.exit("Cached runs must not list tests")
        for name in scenario["names"]:
            suite, case = name.split(".", 1)
            print(f"{suite}.\n  {case}")
        return
    filter_arg = next(arg for arg in sys.argv if arg.startswith("--gtest_filter="))
    names = filter_arg.partition("=")[2].split(":")
    document = ET.Element("testsuites")
    for name in names:
        assert name in scenario["names"]
        suite, case = name.split(".", 1)
        node = ET.SubElement(document, "testsuite", name=suite)
        ET.SubElement(node, "testcase", classname=suite, name=case,
                      status="run", time=str(scenario["observed_us"] / 1e6))
    output = next(arg for arg in sys.argv if arg.startswith("--gtest_output=xml:"))
    ET.ElementTree(document).write(output.partition("xml:")[2], encoding="utf-8")


class RunnerTests(unittest.TestCase):
    def run_batch(self, names, durations, threads=2, filter_text=None,
                  exclude_slow=False, invalid_cache=False, rediscover=False,
                  fresh=False, cached_names=None):
        with tempfile.TemporaryDirectory(prefix="emacs-runner-test-") as directory:
            outdir = pathlib.Path(directory)
            cache = outdir / "timings.etr"
            scenario = outdir / "scenario.json"
            scenario.write_text(json.dumps(dict(
                names=names, observed_us=1000,
                forbid_discovery=not (invalid_cache or rediscover or fresh))))
            overhead = 2_000_000
            if invalid_cache:
                cache.write_bytes(b"invalid")
            else:
                write_cache(cache, names if cached_names is None else cached_names,
                            durations, overhead)
            original_cache = cache.read_bytes()
            original_mtime = cache.stat().st_mtime_ns
            commands = [f"exe\t{sys.executable}", f"cwd\t{directory}",
                        f"outdir\t{directory}", f"arg\t{pathlib.Path(__file__).resolve()}",
                        "arg\t--fake-gtest", f"arg\t{scenario}",
                        f"threads\t{threads}"]
            if not fresh:
                commands.append(f"cache\t{cache}")
            if filter_text is not None:
                commands.append(f"filter\t{filter_text}")
            if exclude_slow:
                commands.append("exclude-slow")
            if rediscover:
                commands.append("rediscover")
            commands.extend(["run", "quit"])
            with subprocess.Popen([RUNNER], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  text=True, encoding="utf-8") as process:
                deadline = threading.Timer(30, process.kill)
                deadline.start()
                try:
                    process.stdin.write("\n".join(commands) + "\n")
                    process.stdin.flush()
                    output = process.stdout.read()
                    process.wait(timeout=10)
                    self.assertEqual(process.returncode, 0, process.stderr.read())
                finally:
                    deadline.cancel()
            events = [line.split("\t") for line in output.splitlines()]
            self.assertEqual(events[0], ["hello", "emacs-test-runner", "3"])
            self.assertEqual(events[-1], ["run-finished"])
            self.assertFalse(any(e[0] in ("error", "discover-failed", "chunk-failed")
                                 for e in events), events)
            chunks = [e for e in events if e[0] == "chunk-done"]
            self.assertTrue(all(e[1] == "0" for e in chunks))
            selected = [e[1] for e in events if e[0] == "test"]
            reported = [name for e in chunks for name in e[4:]]
            self.assertCountEqual(reported, selected)
            self.assertEqual(len(reported), len(set(reported)))
            self.assertEqual(sum(e[0] == "cache-saved" for e in events), 0 if fresh else 1)
            if fresh:
                self.assertFalse(any(e[0] == "cache-failed" for e in events), events)
                self.assertEqual(cache.read_bytes(), original_cache)
                self.assertEqual(cache.stat().st_mtime_ns, original_mtime)
                return events, selected, chunks, None
            return events, selected, chunks, read_cache(cache)

    def test_cached_run_refreshes_timings_without_discovery(self):
        names = ["Suite.Fast", "Suite.SLOW_Other", "Suite.Unselected"]
        events, selected, _, (durations, overhead) = self.run_batch(
            names, [UNKNOWN, 8000, 9000], filter_text="Fast")
        self.assertEqual(selected, ["Suite.Fast"])
        self.assertEqual(durations, (1000, 8000, 9000))
        self.assertLess(overhead, 2_000_000)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4], "cache")

    def test_unknown_timings_use_round_robin(self):
        names = [f"Suite.Case{i}" for i in range(7)]
        events, _, chunks, (durations, _) = self.run_batch(names, [UNKNOWN] * 7)
        chunks.sort(key=lambda e: int(pathlib.Path(e[2]).stem.split("-")[1]))
        self.assertEqual([e[4:] for e in chunks], [names[::2], names[1::2]])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[5], "0")
        self.assertEqual(durations, (1000,) * 7)

    def test_partial_timings_use_mean_for_unknown_tests(self):
        events, _, _, (durations, _) = self.run_batch(
            ["Suite.One", "Suite.Two", "Suite.Six"], [2000, UNKNOWN, UNKNOWN])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[5], "2004")
        self.assertEqual(durations, (1000, 1000, 1000))

    def test_split_chunks_run_longest_first_and_estimate_shared_queue(self):
        names = [f"Suite.Case{i:02d}_" + "x" * 7980 for i in range(30)]
        durations = [1000] * 12 + [100_000] * 12 + [2000] * 6
        expected = dict(zip(names, durations))
        for threads in (1, 2):
            with self.subTest(threads=threads):
                events, _, chunks, _ = self.run_batch(names, durations, threads=threads)
                self.assertGreater(len(chunks), threads)
                chunks.sort(key=lambda e: int(pathlib.Path(e[2]).stem.split("-")[1]))
                costs = [sum(expected[name] for name in e[4:]) + 2_000_000
                         for e in chunks]
                self.assertEqual(costs, sorted(costs, reverse=True))
                loads = [0] * threads
                for cost in costs:
                    loads[loads.index(min(loads))] += cost
                estimate = next(e for e in events if e[0] == "discovered")[5]
                self.assertEqual(int(estimate), max(loads) // 1000)

    def test_filter_and_slow_exclusion_preserve_unrun_timings(self):
        names = ["Suite.Fast", "Suite.Fast/0", "Suite.Other",
                 "SLOW_Suite.Fast", "Suite.SLOW_Fast", "Instance/SLOW_Suite.Fast"]
        _, selected, _, (durations, _) = self.run_batch(
            names, [5000] * len(names), filter_text="Fast", exclude_slow=True)
        self.assertEqual(selected, names[:2])
        self.assertEqual(durations, (1000, 1000, 5000, 5000, 5000, 5000))

    def test_empty_selection_preserves_cached_timings(self):
        events, selected, chunks, (durations, overhead) = self.run_batch(
            ["Suite.One"], [6000], filter_text="Missing")
        self.assertFalse(selected)
        self.assertFalse(chunks)
        self.assertEqual(durations, (6000,))
        self.assertEqual(overhead, 2_000_000)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[3], "0")

    def test_invalid_cache_is_reported_and_rediscovered(self):
        events, _, _, (durations, _) = self.run_batch(
            ["Suite.One", "Suite.Two"], [UNKNOWN] * 2, invalid_cache=True)
        self.assertEqual(sum(e[0] == "cache-failed" for e in events), 1)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4], "listed")
        self.assertEqual(durations, (1000, 1000))

    def test_explicit_rediscovery_still_lists_tests(self):
        events, _, _, (durations, _) = self.run_batch(
            ["Suite.One", "Suite.Two"], [5000] * 2, rediscover=True)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4], "listed")
        self.assertEqual(durations, (1000, 1000))

    def test_fresh_run_ignores_cached_list_and_timings(self):
        names = [f"Suite.Case{i}" for i in range(7)]
        events, selected, chunks, _ = self.run_batch(
            names, [90_000_000, 1_000_000] + [1000] * 6, fresh=True,
            cached_names=["Cached.Stale", *names])
        self.assertEqual(selected, names)
        chunks.sort(key=lambda e: int(pathlib.Path(e[2]).stem.split("-")[1]))
        self.assertEqual([e[4:] for e in chunks], [names[::2], names[1::2]])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4:], ["listed", "0"])

    def test_fresh_run_ignores_invalid_cache_and_applies_filters(self):
        names = ["Suite.Fast", "Suite.Fast/0", "Suite.Other",
                 "SLOW_Suite.Fast", "Suite.SLOW_Fast"]
        events, selected, _, _ = self.run_batch(
            names, [], fresh=True, invalid_cache=True,
            filter_text="Fast", exclude_slow=True)
        self.assertEqual(selected, names[:2])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4:], ["listed", "0"])


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--fake-gtest":
        fake_gtest()
    else:
        RUNNER = str(pathlib.Path(sys.argv.pop(1)).resolve())
        unittest.main()
