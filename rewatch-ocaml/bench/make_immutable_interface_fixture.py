#!/usr/bin/env python3
"""Create the flat-CMI workload used by IMMUTABLE_INTERFACES.md."""

import json
import sys
from pathlib import Path


def main() -> None:
    if len(sys.argv) not in (2, 3) or (
        len(sys.argv) == 3
        and sys.argv[2]
        not in (
            "--values-only",
            "--types-only",
            "--variants-only",
            "--modules-only",
            "--open-only",
        )
    ):
        raise SystemExit(
            f"Usage: {sys.argv[0]} OUTPUT_DIRECTORY "
            "[--values-only|--types-only|--variants-only|--modules-only|--open-only]"
        )
    mode = sys.argv[2] if len(sys.argv) == 3 else "default"
    root = Path(sys.argv[1]).resolve()
    if root.exists():
        raise SystemExit(f"Output directory already exists: {root}")
    source = root / "src"
    source.mkdir(parents=True)
    (root / "rescript.json").write_text(
        json.dumps(
            {
                "name": "immutable-interface-probe",
                "sources": {"dir": "src", "subdirs": True},
                "package-specs": {"module": "esmodule", "in-source": True},
                "suffix": ".mjs",
            },
            indent=2,
        )
        + "\n"
    )
    if mode == "--modules-only":
        (source / "Api.res").write_text(
            "".join(f"let value{i} = {i}\n" for i in range(400))
            + "module type S = {type t; let answer: int}\n"
            + "module A: S = {type t = int; let answer = 1}\n"
            + 'module B: S = {type t = string; let answer = 2}\n'
            + "module Alias = A\n"
            + "module F = (X: S) => {let same = X.answer}\n"
        )
    elif mode == "--types-only":
        (source / "Api.resi").write_text(
            "".join(f"type opaque{i}\ntype alias{i} = opaque{i}\n" for i in range(200))
        )
        (source / "Api.res").write_text(
            "".join(
                f"type opaque{i} = int\ntype alias{i} = opaque{i}\n"
                for i in range(200)
            )
        )
    elif mode == "--variants-only":
        variants = "".join(
            f"type choice{i} = A{i} | B{i}(int)\n" for i in range(200)
        )
        (source / "Api.resi").write_text(variants)
        (source / "Api.res").write_text(variants)
    else:
        (source / "Api.resi").write_text(
            "".join(f"let value{i}: int\n" for i in range(400))
            + "let id: 'a => 'a\n"
            + "type box<'a> = {value: 'a}\n"
        )
        (source / "Api.res").write_text(
            "".join(f"let value{i} = {i}\n" for i in range(400))
            + "let id = x => x\n"
            + "type box<'a> = {value: 'a}\n"
        )
    for i in range(200):
        if mode == "--modules-only":
            text = (
                "module Applied = Api.F(Api.A)\n"
                f"let result = Api.value{i} + Api.A.answer + Api.B.answer "
                "+ Api.Alias.answer + Applied.same\n"
            )
        elif mode == "--open-only":
            text = (
                "open Api\n"
                f"let result = value{i} + id({i})\n"
                "let box: box<int> = {value: result}\n"
            )
        elif mode == "--types-only":
            text = (
                f"let opaque = (x: Api.opaque{i}) => x\n"
                f"let alias = (x: Api.alias{i}) => x\n"
            )
        elif mode == "--variants-only":
            text = (
                f"let selected = Api.B{i}({i})\n"
                f"let result = switch selected {{\n"
                f"| Api.A{i} => 0\n"
                f"| Api.B{i}(value) => value\n"
                f"}}\n"
            )
        else:
            text = f"let result = Api.value{i} + Api.id({i})\n" + (
                "" if mode == "--values-only" else "let box: Api.box<int> = {value: result}\n"
            )
        (source / f"Consumer{i}.res").write_text(text)
    print(root)


if __name__ == "__main__":
    main()
