# Owned GenType semantic inputs: 2026-09-30

Compared checkpoint 3 (`97186f45a`) with checkpoint 4 on Linux aarch64,
12 logical CPUs, the Dune default development profile, and one compiler
domain. Both OCaml Rewatch executables were in the same directory. Their
SHA-256 values were
`e6549bdc2bf187c4a599795f517a8024b844589f2e29ea5e99b0fb9d35f01b1a`
and `d9696e1dea3e831cf23f7a2fb0d5ac8c15a854489bb367f90f38c428695982ed`.
The standalone compiler and runtime were identical in both runs.

The five-run, interleaved testrepo gate passed. Median clean wall time was
3,434/3,401 ms before/after, with sampled peak process-tree RSS of
191,516/188,752 KiB. Unchanged medians were 216/224 ms and edit medians were
269/266 ms. Compiler request counts matched exactly: 1,031 clean, 4 unchanged,
and 6 edit. Complete post-build file sets and artifact bytes matched. The gate
used a temporary directory on the workspace filesystem because the container's
`/tmp` filesystem lacked space for the final comparison. Compare these times
only within this interleaved run.

The [raw GenType samples](checkpoint-4-2026-09-30.csv) use five interleaved
runs per annotation setting, with one unmeasured warm-up per executable and
setting. Each sample starts with a fresh fixture. The monotonic clock supplies
wall time, `wait4` supplies CPU and peak RSS, and the embedded runtime supplies
allocation.

| `REWATCH_BIN_ANNOT` | Old/new wall ms | Old/new CPU ms | Old/new allocation MB | Old/new RSS MiB | Files |
| --- | ---: | ---: | ---: | ---: | ---: |
| unset | 19.1/19.6 | 15.3/15.2 | 3.23/3.23 | 26.4/28.1 | 43 |
| `0` | 19.2/19.3 | 15.4/15.2 | 3.23/3.23 | 25.6/27.3 | 43 |
| `1` | 20.4/19.9 | 16.0/15.6 | 3.30/3.30 | 24.8/26.6 | 46 |

In a separate fixed-path comparison, all 43 generated artifacts with
annotations unset or `0`, and all 46 with annotations `1`, matched byte for
byte. This includes
TypeScript, JavaScript, CMI, CMJ, CMT, and CMTI files.

A one-domain type-check trace of the clean GenType fixture recorded one
`dependency.gentype_cmt_read` on checkpoint 3 and none on checkpoint 4.
Checkpoint 4 recorded one request-owned semantic lookup for the matching
interface. The new focused test feeds implementation and interface semantics
after removing both annotation files and checks normal interface precedence
and `@genType.ignoreInterface` precedence. Session lookup returns isolated
copies and validates both source and published file identities; misses retain
the disk reader. Recursive dependency reads remain for checkpoint 5.
The OCaml Rewatch port's integration suite also passed, including its GenType
trace assertions for implementation and interface semantic results.
