#!/usr/bin/env python3
"""Check GenType and mixed-package imports under both lookup policies.

Run from the repository root with a new output directory. Each policy builds
the same project path, so artifact paths and source digests are comparable.
"""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[2]
REWATCH = ROOT / "_build/default/rewatch-ocaml/rescript_ocaml.exe"
BSC = ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe"
RUNTIME = ROOT / "packages/@rescript/runtime"
CMT_COMPARE = ROOT / "_build/default/rewatch-ocaml/bench/cmt_compare.exe"
FIXTURES = ROOT / "rewatch-ocaml/tests"
SETTINGS = ("unset", "0", "1")
MODES = ("summary", "pair", "inverse")
POLICIES = ("classic", "frozen")
GENERATED_SUFFIXES = (".gen.ts", ".js", ".mjs", ".cmi", ".cmj", ".gts")


def create_project(mode: str, project: Path):
    if mode == "summary":
        shutil.copytree(FIXTURES / "gentype-summary", project)
        return project / "src/Main.res"
    if mode == "pair":
        shutil.copytree(FIXTURES / "gentype", project)
        shutil.copytree(FIXTURES / "shared-dep", project / "node_modules/dep")
        return project / "src/Pair.res"
    (project / "src").mkdir(parents=True)
    dependency = project / "node_modules/dep"
    (dependency / "src").mkdir(parents=True)
    (project / "rescript.json").write_text(
        json.dumps(
            {
                "name": "frozen-gentype-inverse",
                "sources": "src",
                "dependencies": ["dep"],
                "package-specs": {"module": "esmodule", "in-source": True},
            }
        )
    )
    (project / "src/Consumer.res").write_text("let result = Dep.value\n")
    (dependency / "rescript.json").write_text(
        json.dumps(
            {
                "name": "dep",
                "sources": "src",
                "gentypeconfig": {
                    "module": "esmodule",
                    "generatedFileExtension": ".gen.ts",
                },
            }
        )
    )
    (dependency / "src/Dep.res").write_text("@genType let value = 42\n")
    return project / "src/Consumer.res"


def environment(policy: str, annotation: str):
    env = os.environ.copy()
    env.update(
        {
            "RESCRIPT_BSC_EXE": str(BSC),
            "RESCRIPT_RUNTIME": str(RUNTIME),
            "REWATCH_COMPILER_DOMAINS": "1",
            "REWATCH_FROZEN_VALUES": "1" if policy == "frozen" else "0",
        }
    )
    if annotation == "unset":
        env.pop("REWATCH_BIN_ANNOT", None)
    else:
        env["REWATCH_BIN_ANNOT"] = annotation
    return env


def build(output: Path, project: Path, mode: str, annotation: str, policy: str, phase: str):
    prefix = output / f"{mode}-{annotation}-{policy}-{phase}"
    env = environment(policy, annotation)
    env["REWATCH_TYPECHECK_TRACE"] = str(prefix.with_suffix(".trace.tsv"))
    result = subprocess.run(
        [str(REWATCH), "build", str(project)],
        cwd=ROOT,
        env=env,
        capture_output=True,
    )
    prefix.with_suffix(".stdout").write_bytes(result.stdout)
    prefix.with_suffix(".stderr").write_bytes(result.stderr)
    if result.returncode:
        raise AssertionError(f"{mode}/{annotation}/{policy}/{phase} failed: {result.stderr.decode(errors='replace')}")
    trace_file = prefix.with_suffix(".trace.tsv")
    return result.stdout, trace_file.read_text() if trace_file.exists() else ""


def build_error(output: Path, project: Path, annotation: str, policy: str):
    prefix = output / f"pair-{annotation}-{policy}-error"
    result = subprocess.run(
        [str(REWATCH), "build", str(project)],
        cwd=ROOT,
        env=environment(policy, annotation),
        capture_output=True,
    )
    prefix.with_suffix(".stdout").write_bytes(result.stdout)
    prefix.with_suffix(".stderr").write_bytes(result.stderr)
    if result.returncode == 0 or not result.stderr:
        raise AssertionError(f"pair/{annotation}/{policy} accepted a type error")
    return result.stderr


def manifest(project: Path):
    return {
        str(path.relative_to(project)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in project.rglob("*")
        if path.is_file() and path.name.endswith(GENERATED_SUFFIXES)
    }


def public_manifest(files):
    return {path: digest for path, digest in files.items() if not path.endswith(".gts")}


def check_annotations(output: Path, project: Path, mode: str, files, policy: str):
    snapshot = output / f"{mode}-annotation-classic"
    paths = sorted(str(path.relative_to(project)) for path in files)
    if policy == "classic":
        for relative in paths:
            destination = snapshot / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(project / relative, destination)
        (output / f"{mode}-annotation-paths.txt").write_text("\n".join(paths) + "\n")
    else:
        previous = (output / f"{mode}-annotation-paths.txt").read_text().splitlines()
        if paths != previous:
            raise AssertionError(f"{mode} annotation file sets differ")
        subprocess.run(
            [
                str(CMT_COMPARE),
                "--semantic-tree",
                str(snapshot),
                str(project),
                str(output / f"{mode}-annotation-paths.txt"),
            ],
            cwd=ROOT,
            check=True,
        )
        shutil.rmtree(snapshot)


def check_trace(mode: str, policy: str, trace: str):
    frozen_lines = [line for line in trace.splitlines() if "dependency.frozen_open" in line]
    if policy == "classic" and frozen_lines:
        raise AssertionError(f"{mode} classic lookup used frozen imports")
    if policy == "frozen":
        if mode == "pair":
            if not any("src/Dep.ast" in line for line in frozen_lines):
                raise AssertionError("non-GenType dependency did not use frozen imports")
            if any("src/Pair.ast" in line for line in frozen_lines):
                raise AssertionError("GenType Pair request used frozen imports")
        elif mode == "inverse":
            if not any("src/Consumer.ast" in line for line in frozen_lines):
                raise AssertionError("non-GenType consumer did not use frozen imports")
            if any("src/Dep.ast" in line for line in frozen_lines):
                raise AssertionError("GenType dependency used frozen imports")
        elif frozen_lines:
            raise AssertionError("GenType summary request used frozen imports")


def main():
    if len(sys.argv) != 2:
        raise SystemExit(f"Usage: {sys.argv[0]} OUTPUT_DIRECTORY")
    output = Path(sys.argv[1]).resolve()
    output.mkdir(parents=True, exist_ok=False)
    if not CMT_COMPARE.exists():
        raise SystemExit(f"Build the annotation comparison tool: {CMT_COMPARE}")
    results = {}
    for annotation in SETTINGS:
        for mode in MODES:
            project = output / f"{mode}-{annotation}" / "project"
            source = create_project(mode, project)
            original = source.read_text()
            baseline = None
            baseline_error = None
            for policy in POLICIES:
                source.write_text(original)
                stdout, trace = build(output, project, mode, annotation, policy, "clean")
                if policy == "frozen" and b"Cleaned previous build due to compiler update" not in stdout:
                    raise AssertionError(f"{mode}/{annotation} policy switch did not invalidate build metadata")
                check_trace(mode, policy, trace)
                clean = manifest(project)
                if not any(path.endswith(".gen.ts") for path in clean):
                    raise AssertionError(f"{mode}/{annotation}/{policy} emitted no TypeScript")
                annotation_files = [
                    path
                    for path in project.rglob("*")
                    if path.is_file() and path.name.endswith((".cmt", ".cmti"))
                ]
                if bool(annotation_files) != (annotation == "1"):
                    raise AssertionError(
                        f"{mode}/{annotation}/{policy} editor annotation policy differs"
                    )
                if annotation == "1":
                    check_annotations(output, project, mode, annotation_files, policy)
                source.write_text(original + "\n// same exported types\n")
                build(output, project, mode, annotation, policy, "edit")
                edited = manifest(project)
                if public_manifest(edited) != public_manifest(clean):
                    raise AssertionError(f"{mode}/{annotation}/{policy} edit changed generated outputs")
                build(output, project, mode, annotation, policy, "restart")
                if manifest(project) != edited:
                    raise AssertionError(f"{mode}/{annotation}/{policy} restart changed generated outputs")
                if baseline is not None and clean != baseline:
                    changed = sorted(path for path in clean.keys() | baseline.keys() if clean.get(path) != baseline.get(path))
                    raise AssertionError(f"{mode}/{annotation} policies differ: {changed[:8]}")
                baseline = clean
                if mode == "pair":
                    source.write_text(original + '\nlet wrong: int = "wrong"\n')
                    diagnostic = build_error(output, project, annotation, policy)
                    if baseline_error is not None and diagnostic != baseline_error:
                        raise AssertionError(f"pair/{annotation} diagnostics differ")
                    baseline_error = diagnostic
                results[f"{mode}/{annotation}/{policy}"] = clean
                print(mode, annotation, policy, len(clean), "generated files match", flush=True)
            source.write_text(original)
            stdout, _ = build(output, project, mode, annotation, "classic", "switch-back")
            if b"Cleaned previous build due to compiler update" not in stdout:
                raise AssertionError(f"{mode}/{annotation} reverse policy switch did not invalidate build metadata")
            if manifest(project) != baseline:
                raise AssertionError(f"{mode}/{annotation} reverse policy switch changed outputs")
            shutil.rmtree(project)
    (output / "results.json").write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
