# Versioned GenType dependency summaries: 2026-09-30

Compared checkpoint 4 (`d820f2242`) with checkpoint 5 on Linux aarch64, 12
logical CPUs, the Dune default development profile, and one compiler domain.
Both OCaml Rewatch executables were in the same directory. Their SHA-256 values
were `d9696e1dea3e831cf23f7a2fb0d5ac8c15a854489bb367f90f38c428695982ed`
and `d1354dbf27c56f12ff9a87924e7cc5301b9f723a6ca33bf8aac13ca8becfea01`.
The standalone compiler and runtime were identical in both runs.

The five-run interleaved testrepo gate passed. Clean medians were 3,208/3,157
ms before/after, with sampled peak process-tree RSS of 190,316/195,412 KiB.
Unchanged medians were 266/241 ms and edit medians were 219/250 ms. Compiler
request counts matched exactly: 1,031 clean, 4 unchanged, and 6 edit. Complete
post-build file sets and artifact bytes matched. This fixture has no GenType
package, so this gate checks that other packages remain unchanged.

The [raw GenType samples](checkpoint-5-2026-09-30.csv) use five interleaved
runs per annotation setting and scenario. Each sample starts with a fresh copy
of the three-module recursive-import fixture. `unchanged` and `restart-edit`
start from a completed preparation build in a separate process. The monotonic
clock measures wall time, `wait4` measures CPU and peak RSS, and the embedded
runtime reports allocation. The two executables use the same fixture path.

| `REWATCH_BIN_ANNOT` | Scenario | Old/new wall ms | Old/new allocation MB | Old/new RSS MiB |
| --- | --- | ---: | ---: | ---: |
| unset | clean | 18.8/30.4 | 3.77/3.91 | 26.6/27.9 |
| unset | unchanged | 7.0/7.2 | 2.67/2.68 | 23.3/25.0 |
| unset | restart edit | 19.9/24.8 | 2.76/2.76 | 24.9/26.5 |
| `0` | clean | 18.8/33.6 | 3.77/3.91 | 25.8/27.1 |
| `0` | unchanged | 7.6/7.7 | 2.67/2.68 | 22.4/24.1 |
| `0` | restart edit | 18.9/24.2 | 2.76/2.76 | 24.9/26.3 |
| `1` | clean | 19.1/30.3 | 3.77/3.91 | 25.8/27.4 |
| `1` | unchanged | 7.6/7.4 | 2.67/2.68 | 22.4/24.3 |
| `1` | restart edit | 19.3/25.8 | 2.76/2.76 | 24.8/26.2 |

Each new clean build writes three summaries in the working tree and three
published copies, totaling 21,774 bytes. This preparatory checkpoint adds
repeatable latency to clean and restarted GenType builds on the small fixture.
Checkpoint 6 will remove the forced CMT/CMTI work, which is the intended
payoff; this checkpoint should not be described as a standalone speedup.

The parity runner compared three TypeScript files on clean, edit, and restart
builds for annotations unset, `0`, and `1`: all nine comparisons matched byte
for byte. The new compiler used validated summaries for recursive imports,
without falling back to dependency CMT reads. A separate fixed-path comparison
matched all 41 legacy generated artifacts byte for byte in each annotation
mode; the six summaries are the only new artifacts. Removing dependency
summaries forced the legacy CMT reader and preserved TypeScript output.
The focused validation test rejects changed source, CMI dependency, compiler
identity, GenType configuration, and damaged summary headers. The OCaml
Rewatch port integration suite checks summary publication, restart reuse,
fallback, and stale cleanup. `make test-all`, focused OUnit (108 tests), the
port integration suite, and `make checkformat` passed.
