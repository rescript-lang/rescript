#!/usr/bin/env python3
"""Compare GenType TypeScript across clean, edit, and restart builds.

Run from the repository root with the checkpoint-before and checkpoint-after
OCaml Rewatch executables and a new output directory. Both revisions use the
same project path, standalone compiler, runtime, and worker count.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "rewatch-ocaml/tests/gentype-summary"
SETTINGS = ("unset", "0", "1")
SCENARIOS = ("clean", "edit", "restart")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(executable, project, trace, annotation, scenario):
    env = os.environ.copy()
    env["RESCRIPT_BSC_EXE"] = str(
        ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe"
    )
    env["RESCRIPT_RUNTIME"] = str(ROOT / "packages/@rescript/runtime")
    env["REWATCH_COMPILER_DOMAINS"] = "1"
    env["REWATCH_TYPECHECK_TRACE"] = str(trace)
    if annotation == "unset":
        env.pop("REWATCH_BIN_ANNOT", None)
    else:
        env["REWATCH_BIN_ANNOT"] = annotation
    result = subprocess.run(
        [str(executable), "build", str(project)],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode:
        raise RuntimeError(
            f"{annotation}/{scenario} failed with {executable}:\n"
            f"{result.stdout}\n{result.stderr}"
        )
    return {
        str(path.relative_to(project)): digest(path)
        for path in (project / "src").glob("*.gen.ts")
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("old", type=Path)
    parser.add_argument("new", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    old = args.old.resolve(strict=True)
    new = args.new.resolve(strict=True)
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    project = output / "project"
    results = {}
    for annotation in SETTINGS:
        for revision, executable in (("old", old), ("new", new)):
            if project.exists():
                shutil.rmtree(project)
            shutil.copytree(FIXTURE, project)
            previous = None
            for scenario in SCENARIOS:
                if scenario != "clean":
                    with (project / "src/Main.res").open("a") as source:
                        source.write(f"\n// {scenario} with unchanged types\n")
                trace = output / f"{annotation}-{revision}-{scenario}.tsv"
                manifest = run(executable, project, trace, annotation, scenario)
                if previous is not None and manifest != previous:
                    raise AssertionError(
                        f"TypeScript changed on {scenario}: {annotation}/{revision}"
                    )
                previous = manifest
                if revision == "new":
                    trace_text = trace.read_text()
                    if (
                        "src/Main.ast" not in trace_text
                        or "dependency.gentype_summary_read" not in trace_text
                        or "dependency.gentype_dependency_cmt_read" in trace_text
                    ):
                        raise AssertionError(
                            f"summary lookup missing or fell back: {annotation}/{scenario}"
                        )
                    if not (project / "lib/bs/src/Middle.cmt.gts").is_file():
                        raise AssertionError("dependency summary was not produced")
                results[(annotation, revision, scenario)] = manifest
        for scenario in SCENARIOS:
            before = results[(annotation, "old", scenario)]
            after = results[(annotation, "new", scenario)]
            if before != after:
                raise AssertionError(f"TypeScript differs: {annotation}/{scenario}")
            print(annotation, scenario, len(after), "TypeScript files identical")
    (output / "results.json").write_text(
        json.dumps(
            {
                "/".join(key): value
                for key, value in sorted(results.items())
            },
            indent=2,
            sort_keys=True,
        )
        + "\n"
    )


if __name__ == "__main__":
    main()
