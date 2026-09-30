#!/usr/bin/env python3
"""Interleaved, fixed-setting baseline for the embedded compiler.

Run on Linux. Each sample gets a fresh project copy. Preparation for no-op and
edit builds is outside the timed command.
"""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[2]
WORD_BYTES = (sys.maxsize.bit_length() + 1) // 8
SCENARIOS = (
    "clean",
    "noop",
    "edit",
    "shared-interface",
    "gentype",
    "ppx",
    "restart",
    "wide-clean",
    "wide-noop",
    "wide-edit",
    "wide-shared-interface",
)
ANNOTATIONS = ("unset", "0", "1")


def digest(path):
    value = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def fixture(scenario, destination):
    if scenario.startswith("wide-"):
        subprocess.run(
            [
                sys.executable,
                str(ROOT / "rewatch-ocaml/bench/make_immutable_interface_fixture.py"),
                str(destination),
            ],
            check=True,
            stdout=subprocess.DEVNULL,
        )
        if scenario == "wide-shared-interface":
            with (destination / "src/Api.res").open("a") as source:
                source.write("\nlet extra = 1\n")
        return
    name = {
        "shared-interface": "session-interface",
        "gentype": "gentype",
    }.get(scenario, "basic")
    shutil.copytree(ROOT / "rewatch-ocaml/tests" / name, destination)
    if scenario == "gentype":
        modules = destination / "node_modules"
        modules.mkdir()
        shutil.copytree(ROOT / "rewatch-ocaml/tests/shared-dep", modules / "dep")
    if scenario == "shared-interface":
        with (destination / "src/Api.res").open("a") as source:
            source.write("\nlet extra = 1\n")
    if scenario == "ppx":
        ppx = destination / "identity-ppx.sh"
        ppx.write_text('#!/bin/sh\nset -eu\ncp "$1" "$2"\n')
        ppx.chmod(0o755)
        config = json.loads((destination / "rescript.json").read_text())
        config["ppx-flags"] = [str(ppx)]
        (destination / "rescript.json").write_text(json.dumps(config) + "\n")


def generated_files(project, original_files):
    suffixes = (
        ".ast", ".iast", ".cmi", ".cmj", ".cmt", ".cmti",
        ".ts", ".js", ".mjs", ".map",
    )
    paths = [
        p
        for p in project.rglob("*")
        if p.is_file()
        and p.suffix in suffixes
        and p.relative_to(project) not in original_files
    ]
    return len(paths), sum(p.stat().st_size for p in paths)


def command(executable, project, env, output, measured, original_files=frozenset()):
    stdout = output.with_suffix(".stdout")
    stderr = output.with_suffix(".stderr")
    argv = [str(executable), "build", str(project)]
    if measured:
        env["REWATCH_GC_STATS_FILE"] = str(output.with_suffix(".gc.json"))
    else:
        env.pop("REWATCH_GC_STATS_FILE", None)
    started = time.perf_counter_ns()
    with stdout.open("wb") as out, stderr.open("wb") as err:
        process = subprocess.Popen(argv, cwd=ROOT, env=env, stdout=out, stderr=err)
        _, status, usage = os.wait4(process.pid, 0)
        process.returncode = os.waitstatus_to_exitcode(status)
    elapsed_ms = (time.perf_counter_ns() - started) / 1_000_000
    if process.returncode:
        raise RuntimeError(
            f"build failed ({process.returncode}): {stderr}\n{stderr.read_text()}"
        )
    if not measured:
        return None
    gc = json.loads(output.with_suffix(".gc.json").read_text())
    files, artifact_bytes = generated_files(project, original_files)
    return {
        "elapsed_ms": round(elapsed_ms, 3),
        "user_s": round(usage.ru_utime, 6),
        "system_s": round(usage.ru_stime, 6),
        "allocated_bytes": int(gc["allocated_words"]) * WORD_BYTES,
        "peak_rss_kib": usage.ru_maxrss,
        "artifact_files": files,
        "artifact_bytes": artifact_bytes,
    }


def sample(scenario, annotation, iteration, output, executable, base_env):
    project = output / f"{scenario}-{ANNOTATIONS.index(annotation)}-{iteration:02d}"
    fixture(scenario, project)
    original_files = {
        p.relative_to(project) for p in project.rglob("*") if p.is_file()
    }
    env = base_env.copy()
    if annotation == "unset":
        env.pop("REWATCH_BIN_ANNOT", None)
    else:
        env["REWATCH_BIN_ANNOT"] = annotation
    prefix = output / f"{scenario}-{annotation}-{iteration}"
    if scenario in (
        "noop", "edit", "shared-interface", "restart",
        "wide-noop", "wide-edit", "wide-shared-interface",
    ):
        command(executable, project, env, output / f"{prefix.name}-prepare", False)
    if scenario == "edit":
        with (project / "src/B.res").open("a") as source:
            source.write("\n// measured implementation edit\n")
    elif scenario == "shared-interface":
        with (project / "src/Api.resi").open("a") as source:
            source.write("\nlet extra: int\n")
    elif scenario == "restart":
        # The preparation build exits. This measured request starts a new
        # process against its persisted artifacts after a source change.
        with (project / "src/B.res").open("a") as source:
            source.write("\n// edit after process restart\n")
    elif scenario == "wide-edit":
        with (project / "src/Consumer0.res").open("a") as source:
            source.write("\n// measured leaf edit\n")
    elif scenario == "wide-shared-interface":
        with (project / "src/Api.resi").open("a") as source:
            source.write("\nlet extra: int\n")
    result = command(executable, project, env, prefix, True, original_files)
    result.update(scenario=scenario, annotation=annotation, iteration=iteration)
    shutil.rmtree(project)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "output", type=Path, help="new directory for raw data and results"
    )
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--domains", type=int, default=4)
    parser.add_argument(
        "--executable", type=Path,
        default=ROOT / "_build/default/rewatch-ocaml/rescript_ocaml.exe",
    )
    parser.add_argument("--scenarios", nargs="+", choices=SCENARIOS, default=SCENARIOS)
    args = parser.parse_args()
    if args.runs < 3 or args.runs % 2 == 0 or args.domains < 1:
        parser.error("--runs must be odd and at least 3; --domains must be positive")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    executable = args.executable.resolve()
    compiler = Path(
        os.environ.get(
            "RESCRIPT_BSC_EXE",
            ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe",
        )
    ).resolve()
    runtime = Path(
        os.environ.get("RESCRIPT_RUNTIME", ROOT / "packages/@rescript/runtime")
    ).resolve()
    if not hasattr(os, "wait4") or not executable.is_file() or not compiler.is_file():
        parser.error("Linux wait4, the compiler, and the executable are required")
    env = os.environ.copy()
    env.update(
        RESCRIPT_BSC_EXE=str(compiler),
        RESCRIPT_RUNTIME=str(runtime),
        REWATCH_COMPILER_DOMAINS=str(args.domains),
    )
    metadata = {
        "host": platform.platform(),
        "python": platform.python_version(),
        "word_bytes": WORD_BYTES,
        "commit": subprocess.check_output(
            ["git", "-c", f"safe.directory={ROOT}", "rev-parse", "HEAD"],
            cwd=ROOT,
            text=True,
        ).strip(),
        "executable_sha256": digest(executable),
        "compiler_sha256": digest(compiler),
        "runtime": str(runtime),
        "domains": args.domains,
        "runs": args.runs,
        "scenarios": args.scenarios,
        "annotation_modes": ANNOTATIONS,
    }
    (output / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n")
    fields = (
        "scenario", "annotation", "iteration", "elapsed_ms", "user_s",
        "system_s", "allocated_bytes", "peak_rss_kib", "artifact_files",
        "artifact_bytes",
    )
    with (output / "results.csv").open("w", newline="") as source:
        writer = csv.DictWriter(source, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        for scenario in args.scenarios:
            for iteration in range(args.runs + 1):
                order = (
                    ANNOTATIONS
                    if iteration % 2 == 0
                    else tuple(reversed(ANNOTATIONS))
                )
                for annotation in order:
                    result = sample(scenario, annotation, iteration, output, executable, env)
                    if iteration:
                        writer.writerow(result)
                        source.flush()
                        print(
                            f"{scenario:16} {annotation:5} {iteration}: "
                            f"{result['elapsed_ms']:8.1f} ms",
                            flush=True,
                        )


if __name__ == "__main__":
    main()
