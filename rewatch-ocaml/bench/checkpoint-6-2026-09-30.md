# Editor-only binary annotations: 2026-09-30

Compared checkpoint 5 (`c1015c37b`) with checkpoint 6 on Linux aarch64, 12
logical CPUs, the Dune default development profile, and one compiler domain.
Both OCaml Rewatch executables were in the same directory. Their SHA-256 values
were `d1354dbf27c56f12ff9a87924e7cc5301b9f723a6ca33bf8aac13ca8becfea01`
and `5ab862d426507bef0fe98ccedc8a0da3424e494f036073babf9ebc51af21ce6d`.
The standalone compiler and runtime were identical in both runs.

The five-run interleaved testrepo gate passed. Clean medians were 3,175/3,119
ms before/after, with sampled peak process-tree RSS of 188,844/194,664 KiB.
Unchanged medians were 226/230 ms and edit medians were 233/246 ms. Compiler
request counts matched exactly: 1,031 clean, 4 unchanged, and 6 edit. Complete
post-build file sets and artifact bytes matched. This fixture has no GenType
package, so it also checks that disabling editor collection preserves ordinary
build output.

The [interface fixture samples](checkpoint-6-2026-09-30.csv) and [recursive
import samples](checkpoint-6-recursive-2026-09-30.csv) each use five interleaved
runs per annotation setting. The old and new executables use the same fixture
path. The monotonic clock measures wall time, `wait4` measures CPU and peak
RSS, and the embedded runtime reports allocation. The recursive fixture
prepares unchanged and restarted edit builds in a separate process.

| Fixture | `REWATCH_BIN_ANNOT` | Old/new clean wall ms | Old/new CPU ms | Old/new allocation MB | Old/new RSS MiB |
| --- | --- | ---: | ---: | ---: | ---: |
| Interface | unset | 30.3/30.3 | 18.6/18.4 | 3.24/2.97 | 25.1/27.3 |
| Interface | `0` | 30.0/30.3 | 19.1/18.5 | 3.24/2.97 | 25.1/27.1 |
| Interface | `1` | 30.9/29.8 | 20.5/19.0 | 3.30/3.30 | 25.2/26.8 |
| Recursive | unset | 31.2/30.9 | 20.3/19.3 | 3.91/3.18 | 25.8/27.4 |
| Recursive | `0` | 30.0/29.4 | 19.0/18.5 | 3.91/3.18 | 25.9/27.2 |
| Recursive | `1` | 30.5/30.2 | 19.6/18.5 | 3.91/3.91 | 25.8/27.3 |

For the recursive fixture with annotations unset, unchanged medians were
7.8/8.2 ms and restarted edit medians were 23.8/24.4 ms. The `0` and `1`
samples showed similar small differences; see the raw CSV. When annotations
are disabled, measured allocation falls 8% on the interface fixture and 19%
on the recursive fixture. Clean wall time is approximately level. The small
fixtures show higher process RSS in the new executable, including with
annotations enabled; those samples do not establish a memory improvement.

The recursive fixture's ordinary generated files (excluding `.gts` summaries)
fell from 33 files and 32,769 bytes to 24 files and 7,753 bytes when annotations
were unset. Both versions wrote six summary copies; including them, generated
bytes fell from 54,783 to 30,667. With `REWATCH_BIN_ANNOT=1`, the ordinary
artifact file count and bytes matched exactly across versions.

The recursive-import parity runner matched three TypeScript files on clean,
edit, and restart builds in all three annotation modes. The interface parity
runner matched five TypeScript/JavaScript files in those nine scenarios and
matched diagnostics from a type error in each mode. With annotations enabled,
all 28 editor artifacts, including CMT/CMTI, matched byte for byte. The OCaml
Rewatch port integration suite checked an `0 → 1 → 0` setting change, absent
CMT/CMTI and source copies in disabled mode, interface-summary reuse, legacy
CMT fallback when enabled, and stale cleanup. A focused test checked complete
and partial CMT preservation when enabled and no CMT on GenType type errors
when disabled. `make test-all`, focused OUnit (109 tests), the port suite, and
`make checkformat` passed.

The optional browser playground build was unavailable in this environment:
`js_of_ocaml` and its compiler are not installed.
