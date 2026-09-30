#!/usr/bin/env python3
"""Interleaved GenType summary cost across annotation modes.

Run from the repository root with old/new OCaml Rewatch binaries in the same
directory. Each sample uses the same project path and a fresh fixture copy.
"""

import argparse
import csv
import hashlib
import os
from pathlib import Path
import shutil

import embedded_baseline as baseline


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "rewatch-ocaml/tests/gentype-summary"
SCENARIOS = ("clean", "unchanged", "restart-edit")
ANNOTATIONS = ("unset", "0", "1")


def sample(executable, project, output, annotation, scenario, iteration):
    if project.exists():
        shutil.rmtree(project)
    shutil.copytree(FIXTURE, project)
    original_files = {
        path.relative_to(project) for path in project.rglob("*") if path.is_file()
    }
    env = os.environ.copy()
    env.update(
        RESCRIPT_BSC_EXE=str(
            ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe"
        ),
        RESCRIPT_RUNTIME=str(ROOT / "packages/@rescript/runtime"),
        REWATCH_COMPILER_DOMAINS="1",
    )
    if annotation == "unset":
        env.pop("REWATCH_BIN_ANNOT", None)
    else:
        env["REWATCH_BIN_ANNOT"] = annotation
    if scenario != "clean":
        baseline.command(executable, project, env, output.with_name(output.name + "-prepare"), False)
    if scenario == "restart-edit":
        with (project / "src/Main.res").open("a") as source:
            source.write("\n// measured edit after process restart\n")
    result = baseline.command(executable, project, env, output, True, original_files)
    summary_files = list(project.rglob("*.gts"))
    result.update(
        scenario=scenario,
        annotation=annotation,
        iteration=iteration,
        summary_files=len(summary_files),
        summary_bytes=sum(path.stat().st_size for path in summary_files),
    )
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("old", type=Path)
    parser.add_argument("new", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--runs", type=int, default=5)
    args = parser.parse_args()
    if args.runs < 5 or args.runs % 2 != 1:
        parser.error("--runs must be odd and at least five")
    old = args.old.resolve(strict=True)
    new = args.new.resolve(strict=True)
    if old.parent != new.parent:
        parser.error("put both binaries in the same directory")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    project = output / "project"
    rows = []
    for annotation in ANNOTATIONS:
        for scenario in SCENARIOS:
            for iteration in range(1, args.runs + 1):
                order = (("old", old), ("new", new))
                if iteration % 2 == 0:
                    order = tuple(reversed(order))
                for revision, executable in order:
                    prefix = output / f"{annotation}-{scenario}-{iteration}-{revision}"
                    result = sample(executable, project, prefix, annotation, scenario, iteration)
                    rows.append({"revision": revision, **result})
                    print(annotation, scenario, iteration, revision, result["elapsed_ms"])
    with (output / "results.csv").open("w", newline="") as target:
        writer = csv.DictWriter(target, fieldnames=rows[0].keys())
        writer.writeheader()
        writer.writerows(rows)
    with (output / "binaries.sha256").open("w") as target:
        for revision, executable in (("old", old), ("new", new)):
            target.write(f"{revision} {hashlib.sha256(executable.read_bytes()).hexdigest()}\n")


if __name__ == "__main__":
    main()
