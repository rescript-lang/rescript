#!/usr/bin/env python3
"""Measure classic and frozen imported interfaces on focused 201-module projects.

Run from the repository root with OUTPUT_DIRECTORY [ODD_RUN_COUNT]. The runner
requires a POSIX host with os.wait4 and the built OCaml Rewatch executable.
"""

import csv
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / "rewatch-ocaml/bench/make_immutable_interface_fixture.py"
REWATCH = ROOT / "_build/default/rewatch-ocaml/rescript_ocaml.exe"
BSC = ROOT / "_build/default/compiler/bsc/rescript_compiler_main.exe"
ANALYSIS = ROOT / "_build/default/analysis/bin/main.exe"
CMT_COMPARE = ROOT / "_build/default/rewatch-ocaml/bench/cmt_compare.exe"
RUNTIME = ROOT / "packages/@rescript/runtime"
MODES = ("values", "types", "variants", "modules", "open", "inclusion")
ANNOTATION_SUFFIXES = (".cmt", ".cmti")
GENERATED_SUFFIXES = (
    ".ast",
    ".iast",
    ".cmi",
    ".cmj",
    ".cmt",
    ".cmti",
    ".mjs",
    ".js",
    ".map",
    ".gts",
)


def run(command: list[str], env: dict[str, str], log: Path):
    started = time.perf_counter_ns()
    with log.open("wb") as output:
        process = subprocess.Popen(
            command, cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT
        )
        _, status, usage = os.wait4(process.pid, 0)
        process.returncode = os.waitstatus_to_exitcode(status)
    wall_ms = (time.perf_counter_ns() - started) / 1e6
    if process.returncode:
        raise RuntimeError(f"{' '.join(command)} failed; see {log}")
    return wall_ms, (usage.ru_utime + usage.ru_stime) * 1000, usage.ru_maxrss


def manifest(project: Path):
    return {
        str(path.relative_to(project)): hashlib.sha256(path.read_bytes()).hexdigest()
        for parent in (project / "src", project / "lib")
        if parent.exists()
        for path in parent.rglob("*")
        if path.is_file() and path.name.endswith(GENERATED_SUFFIXES)
    }


def is_annotation(path: str):
    return path.endswith(ANNOTATION_SUFFIXES)


def snapshot_annotations(project: Path, destination: Path, files):
    for relative in files:
        if is_annotation(relative):
            target = destination / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(project / relative, target)


def compare_artifacts(output, project, first, second, snapshot, label, annotations):
    missing = sorted(set(first) ^ set(second))
    changed = sorted(
        path
        for path in set(first) & set(second)
        if first[path] != second[path] and not (annotations and is_annotation(path))
    )
    if missing or changed:
        raise AssertionError(
            f"{label} differs: missing={missing[:5]}, changed={changed[:5]}"
        )
    if annotations:
        paths = sorted(path for path in first if is_annotation(path))
        if not paths:
            raise AssertionError(f"{label} produced no editor annotations")
        path_list = output / f"{label}.annotation-paths.txt"
        path_list.write_text("\n".join(paths) + "\n")
        subprocess.run(
            [str(CMT_COMPARE), "--semantic-tree", str(snapshot), str(project), str(path_list)],
            cwd=ROOT,
            check=True,
        )
        shutil.rmtree(snapshot)


def policy_environment(policy: str):
    env = os.environ.copy()
    env.update(
        {
            "REWATCH_FROZEN_VALUES": "1" if policy == "frozen" else "0",
            "REWATCH_COMPILER_DOMAINS": "1",
            "RESCRIPT_BSC_EXE": str(BSC),
            "RESCRIPT_RUNTIME": str(RUNTIME),
        }
    )
    return env


def build(output: Path, project: Path, mode: str, policy: str, iteration: int):
    env = policy_environment(policy)
    env.update(
        {
            "REWATCH_TYPECHECK_TRACE": str(
                output / f"{mode}-{policy}-{iteration}.trace.tsv"
            )
        }
    )
    run(
        [str(REWATCH), "clean", str(project)],
        env,
        output / f"{mode}-{policy}-{iteration}.clean.log",
    )
    wall_ms, cpu_ms, rss_kib = run(
        [str(REWATCH), "build", str(project)],
        env,
        output / f"{mode}-{policy}-{iteration}.build.log",
    )
    return {
        "mode": mode,
        "policy": policy,
        "iteration": iteration,
        "wall_ms": round(wall_ms, 3),
        "cpu_ms": round(cpu_ms, 3),
        "rss_kib": rss_kib,
        "trace": env["REWATCH_TYPECHECK_TRACE"],
    }, manifest(project)


def check_inclusion_edit(output: Path, project: Path, annotations: bool):
    implementation = project / "src/Api.res"
    source = implementation.read_text()
    updated = source.replace("let answer = 1", "let answer = 2", 1)
    if updated == source:
        raise AssertionError("inclusion fixture has no module answer")
    edited_manifests = {}
    edited_work = {}
    snapshot = output / "inclusion-edit-annotations"
    for policy in ("classic", "frozen"):
        implementation.write_text(source)
        env = policy_environment(policy)
        prefix = output / f"inclusion-edit-{policy}"
        run([str(REWATCH), "clean", str(project)], env, Path(f"{prefix}.clean.log"))
        run([str(REWATCH), "build", str(project)], env, Path(f"{prefix}.base.log"))
        implementation.write_text(updated)
        edit_log = Path(f"{prefix}.build.log")
        run([str(REWATCH), "build", str(project)], env, edit_log)
        edited_manifests[policy] = manifest(project)
        if policy == "classic" and annotations:
            snapshot_annotations(project, snapshot, edited_manifests[policy])
        edited_work[policy] = re.findall(
            rb"(?:Parsed|Compiled) \d+ (?:source files|modules)",
            edit_log.read_bytes(),
        )
        run(
            [str(REWATCH), "build", str(project)],
            env,
            Path(f"{prefix}.restart.log"),
        )
        if manifest(project) != edited_manifests[policy]:
            raise AssertionError(f"{policy} restart changed edited artifacts")
    implementation.write_text(source)
    compare_artifacts(
        output,
        project,
        edited_manifests["classic"],
        edited_manifests["frozen"],
        snapshot,
        "inclusion-edit",
        annotations,
    )
    if edited_work["classic"] != edited_work["frozen"]:
        raise AssertionError("inclusion edit compiler work differs")
    if not edited_work["classic"]:
        raise AssertionError("inclusion edit performed no compiler work")


def check_inclusion_error(output: Path, project: Path):
    interface = project / "src/Api.resi"
    source = interface.read_text()
    updated = source.replace("let value0: int", "let value0: string", 1)
    if updated == source:
        raise AssertionError("inclusion fixture has no value0 declaration")
    interface.write_text(updated)
    diagnostics = {}
    for policy in ("classic", "frozen"):
        env = policy_environment(policy)
        run(
            [str(REWATCH), "clean", str(project)],
            env,
            output / f"inclusion-error-{policy}.clean.log",
        )
        result = subprocess.run(
            [str(REWATCH), "build", str(project)],
            cwd=ROOT,
            env=env,
            capture_output=True,
        )
        diagnostics[policy] = result.stdout + result.stderr
        (output / f"inclusion-error-{policy}.build.log").write_bytes(
            diagnostics[policy]
        )
        if result.returncode == 0:
            raise AssertionError(f"{policy} accepted a mismatched interface")
    if diagnostics["classic"] != diagnostics["frozen"]:
        raise AssertionError("inclusion error diagnostics differ")


def check_editor_queries(output: Path, project: Path):
    source = project / "src/Consumer0.res"
    incomplete = output / "values-incomplete.res"
    incomplete.write_text("let result = Api.\n")
    queries = (
        ("hover", ["hover", str(source), "0", "20", str(source), "true"]),
        ("references", ["references", str(source), "0", "20"]),
        ("completion", ["completion", str(source), "0", "17", str(source)]),
        (
            "incomplete-completion",
            ["completion", str(source), "0", "17", str(incomplete)],
        ),
    )
    responses = {}
    for policy in ("classic", "frozen"):
        env = policy_environment(policy)
        run(
            [str(REWATCH), "clean", str(project)],
            env,
            output / f"values-editor-{policy}.clean.log",
        )
        run(
            [str(REWATCH), "build", str(project)],
            env,
            output / f"values-editor-{policy}.build.log",
        )
        for name, arguments in queries:
            result = subprocess.run(
                [str(ANALYSIS), *arguments], cwd=ROOT, env=env, capture_output=True
            )
            responses[policy, name] = (
                result.returncode,
                result.stdout,
                result.stderr,
            )
            (output / f"values-editor-{policy}-{name}.stdout").write_bytes(
                result.stdout
            )
            (output / f"values-editor-{policy}-{name}.stderr").write_bytes(
                result.stderr
            )
            if result.returncode:
                raise AssertionError(f"{policy} {name} editor query failed")
    for name, _ in queries:
        if responses["classic", name] != responses["frozen", name]:
            raise AssertionError(f"{name} editor response differs")
    expected = {
        "hover": b"int",
        "references": b'"range"',
        "completion": b'"value0"',
        "incomplete-completion": b'"value0"',
    }
    for name, marker in expected.items():
        if marker not in responses["classic", name][1]:
            raise AssertionError(f"{name} editor response is missing {marker!r}")


def main():
    if len(sys.argv) not in (2, 3):
        raise SystemExit(f"Usage: {sys.argv[0]} OUTPUT_DIRECTORY [ODD_RUN_COUNT]")
    count = int(sys.argv[2]) if len(sys.argv) == 3 else 5
    if count < 1 or count % 2 == 0:
        raise SystemExit("Run count must be a positive odd integer")
    annotations = os.getenv("REWATCH_BIN_ANNOT") == "1"
    if annotations:
        for required in (CMT_COMPARE, ANALYSIS):
            if not required.exists():
                raise SystemExit(f"Build the annotation gate tool first: {required}")
    output = Path(sys.argv[1]).resolve()
    output.mkdir(parents=True, exist_ok=False)
    samples = []
    for mode in MODES:
        project = output / mode / "project"
        subprocess.run(
            [sys.executable, str(GENERATOR), str(project), f"--{mode}-only"],
            cwd=ROOT,
            check=True,
        )
        # Warm both paths before the interleaved samples.
        for policy in ("classic", "frozen"):
            build(output, project, mode, policy, 0)
        annotation_baselines = {}
        for iteration in range(1, count + 1):
            order = ("classic", "frozen") if iteration % 2 else ("frozen", "classic")
            manifests = {}
            snapshot = output / f"{mode}-{iteration}.annotation-snapshot"
            for policy in order:
                sample, manifests[policy] = build(
                    output, project, mode, policy, iteration
                )
                if annotations:
                    current_annotations = {
                        path: digest
                        for path, digest in manifests[policy].items()
                        if is_annotation(path)
                    }
                    previous = annotation_baselines.setdefault(policy, current_annotations)
                    if previous != current_annotations:
                        raise AssertionError(
                            f"{mode} {policy} annotation bytes changed across clean builds"
                        )
                    if policy == order[0]:
                        snapshot_annotations(project, snapshot, manifests[policy])
                samples.append(sample)
                print(
                    f"{mode:8} {policy:7} {iteration}: "
                    f"{sample['wall_ms']:.1f} ms, {sample['cpu_ms']:.1f} CPU ms, "
                    f"{sample['rss_kib']} KiB RSS",
                    flush=True,
                )
            compare_artifacts(
                output,
                project,
                manifests[order[0]],
                manifests[order[1]],
                snapshot,
                f"{mode}-{iteration}",
                annotations,
            )
        for policy in ("classic", "frozen"):
            matching = [
                sample for sample in samples if sample["mode"] == mode and sample["policy"] == policy
            ]
            print(
                f"{mode:8} {policy:7} median: "
                f"{statistics.median(sample['wall_ms'] for sample in matching):.1f} ms",
                flush=True,
            )
        for policy in (("classic", "frozen") if annotations else ("classic",)):
            suffix = f"-{policy}" if annotations else ""
            (output / f"{mode}{suffix}.manifest.json").write_text(
                json.dumps(manifests[policy], indent=2, sort_keys=True) + "\n"
            )
        if mode == "values" and annotations:
            check_editor_queries(output, project)
        if mode == "inclusion":
            check_inclusion_edit(output, project, annotations)
            check_inclusion_error(output, project)
        if os.getenv("KEEP_REWATCH_IMMUTABLE_PROJECTS") != "1":
            shutil.rmtree(project)
    with (output / "samples.csv").open("w", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=samples[0].keys())
        writer.writeheader()
        writer.writerows(samples)
    (output / "metadata.txt").write_text(
        f"rewatch_sha256={hashlib.sha256(REWATCH.read_bytes()).hexdigest()}\n"
        f"bsc_sha256={hashlib.sha256(BSC.read_bytes()).hexdigest()}\n"
        f"domains=1\nruns={count}\nannotations={int(annotations)}\n"
    )


if __name__ == "__main__":
    main()
