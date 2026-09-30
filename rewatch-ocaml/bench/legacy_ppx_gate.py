#!/usr/bin/env python3
"""Measure the frozen AST 0 boundary and check its no-PPX bypass.

Run from the repository root with a new output directory. The identity PPX
copies the frozen AST 0 file, preserving the exact external wire protocol.
"""

import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[2]
REWATCH = ROOT / "_build/default/rewatch-ocaml/rescript_ocaml.exe"
BSC = ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe"
RUNTIME = ROOT / "packages/@rescript/runtime"
PHASES = (
    "ppx.convert.to0",
    "ppx.serialize",
    "ppx.write",
    "ppx.execute",
    "ppx.read",
    "ppx.deserialize",
    "ppx.convert.from0",
)


def manifest(project: Path):
    return {
        str(path.relative_to(project)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in project.rglob("*")
        if path.is_file()
        and (
            path.name.endswith(".mjs")
            or ("lib" in path.relative_to(project).parts and path.name.endswith((".cmi", ".cmj")))
        )
    }


def phases(path: Path):
    result = {}
    for line in path.read_text().splitlines():
        columns = line.split("\t")
        name = columns[2]
        if name.startswith("ppx."):
            seconds, bytes_allocated, calls = result.get(name, (0.0, 0.0, 0))
            result[name] = (
                seconds + float(columns[3]),
                bytes_allocated + float(columns[4]),
                calls + int(columns[5]),
            )
    return result


def run(output: Path, project: Path, ppx: Path, iteration: int, mode: str):
    config_path = project / "rescript.json"
    config = json.loads(config_path.read_text())
    if mode == "identity":
        config["ppx-flags"] = [str(ppx)]
    else:
        config.pop("ppx-flags", None)
    config_path.write_text(json.dumps(config) + "\n")
    shutil.rmtree(project / "lib", ignore_errors=True)
    prefix = output / f"{iteration}-{mode}"
    env = os.environ.copy()
    env.update(
        {
            "RESCRIPT_BSC_EXE": str(BSC),
            "RESCRIPT_RUNTIME": str(RUNTIME),
            "REWATCH_COMPILER_DOMAINS": "1",
            "REWATCH_FROZEN_VALUES": "1",
            "REWATCH_BIN_ANNOT": "0",
            "REWATCH_TYPECHECK_TRACE": str(prefix.with_suffix(".trace.tsv")),
        }
    )
    started = time.perf_counter()
    result = subprocess.run(
        [str(REWATCH), "build", str(project)],
        cwd=ROOT,
        env=env,
        capture_output=True,
    )
    wall_ms = (time.perf_counter() - started) * 1000
    prefix.with_suffix(".stdout").write_bytes(result.stdout)
    prefix.with_suffix(".stderr").write_bytes(result.stderr)
    if result.returncode:
        raise AssertionError(f"{iteration}/{mode} failed: {result.stderr.decode(errors='replace')}")
    if b"Compiled 4 modules" not in result.stdout:
        raise AssertionError(f"{iteration}/{mode} did not compile all four modules")
    trace = phases(prefix.with_suffix(".trace.tsv"))
    if mode == "plain" and trace:
        raise AssertionError("AST 0 conversion ran without an external PPX")
    if mode == "identity" and set(trace) != set(PHASES):
        raise AssertionError(f"missing AST 0 boundary phases: {set(PHASES) - set(trace)}")
    return wall_ms, trace, manifest(project)


def main():
    if len(sys.argv) != 3:
        raise SystemExit(f"Usage: {sys.argv[0]} OUTPUT_DIRECTORY ODD_RUN_COUNT")
    count = int(sys.argv[2])
    if count < 5 or count % 2 != 1:
        raise SystemExit("Run count must be odd and at least five")
    output = Path(sys.argv[1]).resolve()
    output.mkdir(parents=True, exist_ok=False)
    project = output / "project"
    shutil.copytree(ROOT / "rewatch-ocaml/tests/basic", project)
    ppx = output / "identity-ppx.sh"
    ppx.write_text('#!/bin/sh\nset -eu\ncp "$1" "$2"\n')
    ppx.chmod(0o755)
    with (output / "samples.csv").open("w", newline="") as channel:
        writer = csv.writer(channel)
        writer.writerow(["iteration", "mode", "wall_ms", *[f"{name}_ms" for name in PHASES]])
        for iteration in range(1, count + 1):
            baseline = None
            for mode in (("plain", "identity") if iteration % 2 else ("identity", "plain")):
                wall_ms, trace, files = run(output, project, ppx, iteration, mode)
                if baseline is not None and files != baseline:
                    changed = sorted(
                        path for path in baseline.keys() | files.keys()
                        if baseline.get(path) != files.get(path)
                    )
                    raise AssertionError(f"{iteration} generated artifacts differ: {changed}")
                baseline = files
                writer.writerow(
                    [iteration, mode, f"{wall_ms:.3f}"]
                    + [f"{trace.get(name, (0.0, 0.0, 0))[0] * 1000:.3f}" for name in PHASES]
                )
                print(iteration, mode, f"{wall_ms:.1f} ms", len(files), "artifacts", flush=True)


if __name__ == "__main__":
    main()
