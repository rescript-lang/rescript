# Owned compiler artifacts: 2026-09-30

Compared checkpoint 7 (`71e77b46b`) with checkpoint 8 on Linux aarch64,
12 logical CPUs, the Dune default development profile, and one compiler
domain. Both OCaml Rewatch executables were in the same directory. Their
SHA-256 values for the five-run measurement were
`129d2cbdafec7a6ec4a3585d137e7536b5b9db56776b2ff5afa9f148ac20e94a`
and `f23bc1d8b31e37c580a9e5f58964c07294efdf51bfab8c8e296940affc63f837`.
The standalone compiler and runtime were identical in both runs.

The five-run interleaved testrepo gate passed with annotations unset and with
`REWATCH_BIN_ANNOT=1`. Its `rust` and `ocaml` labels mean the old and new
embedded executables in this comparison.

| Annotation mode | Scenario | Old/new median wall ms | Old/new sampled peak tree RSS KiB |
| --- | --- | ---: | ---: |
| Unset | Clean | 3,155 / 3,228 | 197,892 / 208,852 |
| Unset | Unchanged | 227 / 234 | 29,316 / 31,152 |
| Unset | Single edit | 239 / 260 | 29,480 / 31,156 |
| `1` | Clean | 4,700 / 4,720 | 253,916 / 259,152 |
| `1` | Unchanged | 280 / 265 | 29,780 / 31,528 |
| `1` | Single edit | 216 / 282 | 33,072 / 33,464 |

Compiler request counts matched exactly in each mode: 1,031 clean, 4
unchanged, and 6 edit. Complete post-build file sets and artifact bytes
matched. Clean medians were close, while sampled peak RSS rose 6% without
annotations and 2% with them. Edit medians were higher in the new executable,
especially with annotations, although the five old and new samples overlap.
This checkpoint does not establish a repeatable speedup; edit latency needs
continued measurement during the later promotion gate.

The compiler serializes changed CMI and CMJ results once into owned byte
images. When annotations are enabled, complete CMT/CMTI results use the same
handoff; partial annotations continue to be written during failed requests.
The publisher checks the source identity and request generation, writes the
staging artifacts, and hands the existing frozen dependency values to the
session. Later export copies the exact staged bytes to the persistent cache.
The retained byte images have a 32 MiB session budget, with immediate disk
persistence when it fills. Editor staging and published files use atomic
replacement. Existing disk readers remain the fallback after session eviction
or restart, and environment switches can disable each owned artifact type.
When a source changes before the owned AST reaches disk, the driver also writes
the captured AST for the disk reader while the watcher schedules a fresh parse.

Focused tests checked CMI/CMJ and CMT/CMTI byte parity with standalone
compilation, dependent access after persistence, partial CMT on a type error,
stale-source rejection, atomic editor copy failure recovery, file permissions,
and the AST source-change window. The GenType interface parity runner matched five generated
TypeScript/JavaScript files and error diagnostics in clean, edit, and restart
builds for annotation modes unset, `0`, and `1`; all 28 enabled editor files
matched byte for byte. The recursive summary runner matched three TypeScript
files in each mode and scenario. `make test-all` passed before the final AST
source-change fallback; afterward, `make test`, focused OUnit (114 tests),
two OCaml Rewatch port suite runs, and `make checkformat` passed. One earlier
port run hit a missing AST during a symlink target replacement, which prompted
the fallback and its regression test.

The final binary SHA-256 is
`b9f02cbe3bae147c8de85137329e25e2227987996374f2ad152f7a2130b6a4b7`.
One-run correctness comparisons on that binary passed in both annotation modes
with equal compiler work, complete file sets, and byte-identical artifacts.
Those one-run results are smoke checks, not the five-run performance measurement.

Next: checkpoint 9, measure imported-type graph copying and migrate immutable
values in validated stages.
