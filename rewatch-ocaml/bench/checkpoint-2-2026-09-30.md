# Typed request comparison: 2026-09-30

Compared checkpoint 1 (`c0a00b942`) with the checkpoint 2 implementation on Linux aarch64, 12 logical CPUs, Dune default development profile. Both OCaml Rewatch executables were in the same directory. Their SHA-256 values were `4c91d882d32ebe9ff95d37e446bbe55ce1a2dfd97fd0e8a878858b28ce503531` (checkpoint 1) and `dcf6d7777f52c7714e88209d5b88f7744696b85ed2fdfed6ae16cb6d31dc21e9` (checkpoint 2). The latter was built before the final test and documentation edits, which do not affect the driver behavior.

The [raw seven-run, interleaved samples](checkpoint-2-2026-09-30.csv) use four compiler domains and the 201-module fixture. `REWATCH_BIN_ANNOT` was explicitly `0` or `1`. Each sample used a fresh fixture and one unmeasured warm-up preceded sampling. `wait4` supplied CPU and RSS; the embedded runtime supplied allocation. The table shows median wall time and RSS, plus generated file counts. Artifact byte counts matched exactly between revisions in every paired scenario.

| Scenario | Annotation | Old wall ms | New wall ms | Old RSS MiB | New RSS MiB | Files |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Wide clean | 0 | 71.3 | 72.9 | 41.1 | 43.2 | 1,409 |
| Wide clean | 1 | 114.7 | 108.6 | 47.1 | 49.2 | 2,014 |
| Wide shared interface | 0 | 70.1 | 69.8 | 40.1 | 41.4 | 1,409 |
| Wide shared interface | 1 | 99.4 | 97.8 | 44.6 | 46.5 | 2,014 |

Median embedded allocation was unchanged to two decimal places in every row: 9.43, 23.28, 11.23, and 12.42 MB respectively.

The full testrepo `performance_gate.sh` also ran five interleaved samples per scenario with one compiler domain. Old/new median wall times were 1,658/1,667 ms for clean, 42/43 ms for unchanged, and 43/42 ms for edit. Median sampled peak tree RSS was 179,308/192,596 KiB for clean. Both revisions issued the same 1,031 clean, 4 unchanged, and 6 edit logical compiler requests. The gate passed its time and memory thresholds and found identical complete file sets and byte-identical generated artifacts.

The four-domain full testrepo artifact comparison found differing WebAPI CMI files. Repeating the same clean build twice with the unchanged checkpoint 1 executable and four domains produced 203 differing WebAPI CMIs; one-domain repeats produced zero differences. Inspection showed that a changed CMI can have an identical serialized signature but differing imported dependency checksums. This existing parallel-build nondeterminism prevents a byte-parity conclusion from that four-domain fixture. The one-domain comparison above isolates the checkpoint 2 option change; resolving parallel CMI determinism remains necessary before final promotion.
