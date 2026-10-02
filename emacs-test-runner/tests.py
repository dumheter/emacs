"""Runner regressions: python emacs-test-runner\\tests.py RUNNER."""

import json
import pathlib
import struct
import subprocess
import sys
import tempfile
import threading
import time
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
        assert not any(arg.startswith("--gtest_repeat=") for arg in sys.argv)
        if scenario["forbid_discovery"]:
            sys.exit("Cached runs must not list tests")
        for name in scenario["names"]:
            suite, case = name.split(".", 1)
            print(f"{suite}.\n  {case}")
        return
    filter_arg = next(arg for arg in sys.argv if arg.startswith("--gtest_filter="))
    names = filter_arg.partition("=")[2].split(":")
    repeat_arg = next(arg for arg in sys.argv if arg.startswith("--gtest_repeat="))
    native_repeat = int(repeat_arg.partition("=")[2])
    assert native_repeat == scenario["native_repeat"]
    output = next((arg for arg in sys.argv if arg.startswith("--gtest_output=xml:")), None)
    xml_path = pathlib.Path(output.partition("xml:")[2]) if output else None
    chunk_id = int(xml_path.stem.split("-")[1]) if xml_path else 0
    started = time.perf_counter()
    time.sleep(scenario["sleep_seconds"])
    failed = False
    final_failed = False
    for iteration in range(native_repeat):
        final_failed = scenario["fail_first_process"] and chunk_id == 2
        if scenario["fail_early_native"]:
            final_failed = iteration == 0
        failed |= final_failed
        for name in names:
            print(f"Native iteration {iteration + 1}: {name}"
                  + (" FAILED" if final_failed else " PASSED"))
    document = ET.Element("testsuites")
    for name in names:
        assert name in scenario["names"]
        suite, case = name.split(".", 1)
        node = ET.SubElement(document, "testsuite", name=suite)
        case_node = ET.SubElement(node, "testcase", classname=suite, name=case,
                                 status="run", time=str(scenario["observed_us"] / 1e6))
        if final_failed:
            ET.SubElement(case_node, "failure", message="simulated failure")
    if xml_path:
        ET.ElementTree(document).write(xml_path, encoding="utf-8")
        xml_path.with_suffix(".trace").write_text(json.dumps(
            dict(start=started, end=time.perf_counter())))
    sys.exit(1 if failed else 0)


class RunnerTests(unittest.TestCase):
    def run_batch(self, names, durations, threads=2, filter_text=None,
                  exclude_slow=False, invalid_cache=False, rediscover=False,
                  fresh=False, cached_names=None, repeat=1, native_repeat=1,
                  fail_first_process=False, fail_early_native=False,
                  min_concurrency=1, rerun=False):
        with tempfile.TemporaryDirectory(prefix="emacs-runner-test-") as directory:
            outdir = pathlib.Path(directory)
            cache = outdir / "timings.etr"
            scenario = outdir / "scenario.json"
            scenario.write_text(json.dumps(dict(
                names=names, observed_us=1000,
                native_repeat=native_repeat,
                fail_first_process=fail_first_process,
                fail_early_native=fail_early_native,
                sleep_seconds=0.4 if min_concurrency > 1 else 0,
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
                        f"rerun-arg\t{pathlib.Path(__file__).resolve()}",
                        "rerun-arg\t--fake-gtest", f"rerun-arg\t{scenario}",
                        f"threads\t{threads}", f"repeat\t{repeat}",
                        f"gtest-repeat\t{native_repeat}"]
            if not fresh:
                commands.append(f"cache\t{cache}")
            if filter_text is not None:
                commands.append(f"filter\t{filter_text}")
            if exclude_slow:
                commands.append("exclude-slow")
            if rediscover:
                commands.append("rediscover")
            commands.extend(["run"] if rerun else ["run", "quit"])
            with subprocess.Popen([RUNNER], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  text=True, encoding="utf-8") as process:
                deadline = threading.Timer(30, process.kill)
                deadline.start()
                try:
                    process.stdin.write("\n".join(commands) + "\n")
                    process.stdin.flush()
                    if rerun:
                        lines = []
                        for line in process.stdout:
                            lines.append(line)
                            if line.startswith("run-finished"):
                                process.stdin.write(f"rerun\t0\t{names[0]}\n")
                                process.stdin.flush()
                            elif line.startswith(("rerun-done", "rerun-failed")):
                                process.stdin.write("quit\n")
                                process.stdin.flush()
                        output = "".join(lines)
                    else:
                        output = process.stdout.read()
                    process.wait(timeout=10)
                    self.assertEqual(process.returncode, 0, process.stderr.read())
                finally:
                    deadline.cancel()
            events = [line.split("\t") for line in output.splitlines()]
            self.assertEqual(events[0], ["hello", "emacs-test-runner", "4"])
            if rerun:
                self.assertEqual(events[-1][:3], ["rerun-done", "0", "0"])
                rerun_log = pathlib.Path(events[-1][3]).read_text()
                self.assertEqual(rerun_log.count("Native iteration "), native_repeat)
            else:
                self.assertEqual(events[-1], ["run-finished"])
            self.assertFalse(any(e[0] in ("error", "discover-failed", "chunk-failed")
                                 for e in events), events)
            chunks = [e for e in events if e[0] == "chunk-done"]
            if not (fail_first_process or fail_early_native):
                self.assertTrue(all(e[2] == "0" for e in chunks))
            selected = [e[1] for e in events if e[0] == "test"]
            selected_iterations = [(e[1], e[2]) for e in events if e[0] == "test"]
            reported = [(name, e[1]) for e in chunks for name in e[5:]]
            self.assertCountEqual(reported, selected_iterations)
            self.assertEqual(len(reported), len(set(reported)))
            points = []
            for chunk in chunks:
                log = pathlib.Path(chunk[4]).read_text()
                self.assertEqual(log.count("Native iteration "),
                                 len(chunk[5:]) * native_repeat)
                trace = json.loads(pathlib.Path(chunk[3]).with_suffix(".trace").read_text())
                points.extend([(trace["start"], 1), (trace["end"], -1)])
                if fail_early_native:
                    self.assertEqual(chunk[2], "1")
                    self.assertFalse(ET.parse(chunk[3]).findall(".//failure"))
                    self.assertIn("FAILED", log)
                    self.assertIn("PASSED", log)
            active = peak = 0
            for _, change in sorted(points):
                active += change
                peak = max(peak, active)
            if chunks:
                self.assertGreaterEqual(peak, min_concurrency)
                self.assertLessEqual(peak, threads)
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
        chunks.sort(key=lambda e: int(pathlib.Path(e[3]).stem.split("-")[1]))
        self.assertEqual([e[5:] for e in chunks], [names[::2], names[1::2]])
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
                chunks.sort(key=lambda e: int(pathlib.Path(e[3]).stem.split("-")[1]))
                costs = [sum(expected[name] for name in e[5:]) + 2_000_000
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
        chunks.sort(key=lambda e: int(pathlib.Path(e[3]).stem.split("-")[1]))
        self.assertEqual([e[5:] for e in chunks], [names[::2], names[1::2]])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[4:], ["listed", "0"])

    def test_parallel_repeat_spreads_one_case_across_workers(self):
        events, selected, chunks, (durations, _) = self.run_batch(
            ["Suite.One"], [5000], threads=3, repeat=7, min_concurrency=3)
        self.assertEqual(selected, ["Suite.One"] * 7)
        self.assertEqual(len(chunks), 7)
        self.assertCountEqual([e[1] for e in chunks], [str(i) for i in range(1, 8)])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[1:4],
                         ["1", "7", "3"])
        self.assertEqual(next(e for e in events if e[0] == "discovered")[5], "6015")
        self.assertEqual(durations, (1000,))

    def test_repeats_combine_and_preserve_single_iteration_cache_timings(self):
        events, selected, chunks, (durations, overhead) = self.run_batch(
            ["Suite.One"], [5000], threads=2, repeat=3, native_repeat=4)
        self.assertEqual(selected, ["Suite.One"] * 3)
        self.assertEqual(len(chunks), 3)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[5], "4040")
        self.assertEqual(durations, (1000,))
        self.assertEqual(overhead, 2_000_000)

    def test_fresh_parallel_repeats_apply_filters_without_cache_changes(self):
        events, selected, chunks, _ = self.run_batch(
            ["Suite.One", "Suite.SLOW_One", "Suite.Other"], [9000] * 3,
            threads=4, repeat=5, fresh=True, filter_text="One", exclude_slow=True,
            min_concurrency=4)
        self.assertEqual(selected, ["Suite.One"] * 5)
        self.assertEqual(len(chunks), 5)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[3:],
                         ["4", "listed", "0"])

    def test_parallel_iterations_keep_distinct_failure_outcomes(self):
        _, _, chunks, _ = self.run_batch(
            ["Suite.One"], [5000], repeat=4, native_repeat=2,
            fail_first_process=True, rerun=True)
        failed = [e for e in chunks if e[2] == "1"]
        self.assertEqual(len(failed), 1)
        self.assertEqual(failed[0][1], "1")
        self.assertEqual(sum(e[2] == "0" for e in chunks), 3)

    def test_parallel_repeats_do_not_duplicate_names_inside_a_process(self):
        names = [f"Suite.Case{i}" for i in range(7)]
        _, selected, chunks, (durations, _) = self.run_batch(
            names, [UNKNOWN] * 7, threads=2, repeat=3)
        self.assertEqual(selected, names * 3)
        self.assertEqual(len(chunks), 6)
        for chunk in chunks:
            self.assertEqual(len(chunk[5:]), len(set(chunk[5:])))
        self.assertEqual(durations, (1000,) * 7)

    def test_native_failure_is_reported_even_when_final_xml_passes(self):
        self.run_batch(["Suite.One"], [5000], native_repeat=3, fail_early_native=True)

    def test_empty_repeated_selection_has_no_workers(self):
        events, selected, chunks, _ = self.run_batch(
            ["Suite.One"], [5000], repeat=7, filter_text="Missing")
        self.assertFalse(selected)
        self.assertFalse(chunks)
        self.assertEqual(next(e for e in events if e[0] == "discovered")[2:4], ["0", "0"])

    def test_repeat_counts_reject_invalid_values(self):
        for command in ("repeat", "gtest-repeat"):
            for value in ("0", "-1", "-4294967295", "1.5", "oops", "1000001"):
                with self.subTest(command=command, value=value):
                    result = subprocess.run(
                        [RUNNER], input=f"{command}\t{value}\nquit\n",
                        capture_output=True, text=True, encoding="utf-8", timeout=10)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(f"error\t{command} must be between 1 and 1000000",
                                  result.stdout)

    def test_repeat_counts_accept_boundary_values(self):
        for command in ("repeat", "gtest-repeat"):
            for value in ("1", "1000000"):
                with self.subTest(command=command, value=value):
                    result = subprocess.run(
                        [RUNNER], input=f"{command}\t{value}\nquit\n",
                        capture_output=True, text=True, encoding="utf-8", timeout=10)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout.splitlines(),
                                     ["hello\temacs-test-runner\t4"])

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
