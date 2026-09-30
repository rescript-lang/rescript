#!/usr/bin/env python3
"""Compare GenType output while changing the editor annotation policy.

Run from the repository root with the checkpoint-before and checkpoint-after
OCaml Rewatch executables and a new output directory.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess


ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "rewatch-ocaml/tests/gentype"
SHARED_DEP = ROOT / "rewatch-ocaml/tests/shared-dep"
SETTINGS = ("unset", "0", "1")
SCENARIOS = ("clean", "edit", "restart")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def manifest(project, suffixes):
    return {
        str(path.relative_to(project)): digest(path)
        for path in project.rglob("*")
        if path.is_file() and path.name.endswith(suffixes)
    }


def run(executable, project, trace, annotation, revision, scenario, *, expect_failure=False):
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
    if expect_failure:
        if result.returncode == 0 or not result.stderr:
            raise AssertionError(f"expected a diagnostic: {annotation}/{revision}")
        return result.stderr
    if result.returncode:
        raise RuntimeError(
            f"{annotation}/{revision}/{scenario} failed:\n"
            f"{result.stdout}\n{result.stderr}"
        )
    cmt = project / "lib/bs/src/Pair.cmti"
    summary = project / "lib/bs/src/Pair.cmti.gts"
    copied_source = project / "lib/ocaml/Pair.resi"
    if not summary.is_file():
        raise AssertionError("interface summary is missing")
    if revision == "new":
        editor_enabled = annotation == "1"
        if cmt.is_file() != editor_enabled or copied_source.is_file() != editor_enabled:
            raise AssertionError(f"editor artifacts differ: {annotation}/{scenario}")
        if not editor_enabled:
            trace_text = trace.read_text()
            if "dependency.gentype_interface_summary_read" not in trace_text:
                raise AssertionError("implementation did not read the interface input")
    return manifest(project / "src", (".gen.ts", ".js"))


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
    editor_manifests = {}
    diagnostics = {}
    for annotation in SETTINGS:
        for revision, executable in (("old", old), ("new", new)):
            if project.exists():
                shutil.rmtree(project)
            shutil.copytree(FIXTURE, project)
            (project / "node_modules").mkdir()
            shutil.copytree(SHARED_DEP, project / "node_modules/dep")
            previous = None
            for scenario in SCENARIOS:
                if scenario != "clean":
                    with (project / "src/Pair.res").open("a") as source:
                        source.write(f"\n// {scenario} with unchanged exports\n")
                trace = output / f"{annotation}-{revision}-{scenario}.tsv"
                generated = run(
                    executable, project, trace, annotation, revision, scenario
                )
                if previous is not None and generated != previous:
                    raise AssertionError(
                        f"generated code changed: {annotation}/{revision}/{scenario}"
                    )
                previous = generated
                results[(annotation, revision, scenario)] = generated
                if annotation == "1" and scenario == "clean":
                    editor_manifests[revision] = manifest(
                        project / "lib", (".ast", ".iast", ".cmi", ".cmj", ".cmt", ".cmti")
                    )
            with (project / "src/Pair.res").open("a") as source:
                source.write('\nlet wrong: int = "wrong"\n')
            trace = output / f"{annotation}-{revision}-error.tsv"
            diagnostics[(annotation, revision)] = run(
                executable, project, trace, annotation, revision, "error",
                expect_failure=True,
            )
        for scenario in SCENARIOS:
            before = results[(annotation, "old", scenario)]
            after = results[(annotation, "new", scenario)]
            if before != after:
                raise AssertionError(f"generated code differs: {annotation}/{scenario}")
            print(annotation, scenario, len(after), "TypeScript/JavaScript files identical")
        if annotation == "1":
            if editor_manifests["old"] != editor_manifests["new"]:
                raise AssertionError("enabled editor artifacts differ")
            print("1", "clean", len(editor_manifests["new"]), "editor files identical")
        if diagnostics[(annotation, "old")] != diagnostics[(annotation, "new")]:
            raise AssertionError(f"diagnostics differ: {annotation}")
        print(annotation, "error", "diagnostics identical")
    (output / "results.json").write_text(
        json.dumps(
            {
                **{"/".join(key): value for key, value in sorted(results.items())},
                **{
                    f"{annotation}/{revision}/error": hashlib.sha256(message.encode()).hexdigest()
                    for (annotation, revision), message in sorted(diagnostics.items())
                },
            },
            indent=2,
            sort_keys=True,
        )
        + "\n"
    )


if __name__ == "__main__":
    main()
