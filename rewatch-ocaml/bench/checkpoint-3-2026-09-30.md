# Independent policy comparison: 2026-09-30

Compared checkpoint 2 (`af8881afa`) with checkpoint 3 on Linux aarch64, 12 logical CPUs, the Dune default development profile, and one compiler domain. Both OCaml Rewatch executables were in the same directory. Their SHA-256 values were `dcf6d7777f52c7714e88209d5b88f7744696b85ed2fdfed6ae16cb6d31dc21e9` and `e6549bdc2bf187c4a599795f517a8024b844589f2e29ea5e99b0fb9d35f01b1a` respectively. The standalone compiler and runtime were identical in both runs.

The five-run, interleaved full testrepo gate passed. Median clean wall time was 1,686 ms before and 1,703 ms after; unchanged was 46/44 ms and edit was 43/42 ms. The clean median sampled process-tree RSS was 181,488/195,948 KiB, below the gate's 125% limit. Compiler request counts matched exactly: 1,031 clean, 4 unchanged, and 6 edit. The complete post-build file sets and all generated artifact bytes matched.

The [raw GenType samples](checkpoint-3-2026-09-30.csv) use five interleaved runs for each annotation setting with one unmeasured warm-up per executable and setting. Each sample uses a fresh fixture. The monotonic clock supplies wall time, `wait4` supplies CPU and peak RSS, and the embedded runtime supplies allocation.

| `REWATCH_BIN_ANNOT` | Old wall ms | New wall ms | Old allocation MB | New allocation MB | Old RSS MiB | New RSS MiB | Files |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| unset | 19.4 | 19.3 | 3.21 | 3.23 | 27.7 | 29.3 | 43 |
| `0` | 19.6 | 19.9 | 3.21 | 3.23 | 27.7 | 29.1 | 43 |
| `1` | 19.5 | 21.2 | 3.27 | 3.30 | 27.7 | 29.4 | 46 |

In a separate fixed-path GenType comparison, all 43 generated artifacts with annotations unset or `0`, and all 46 with annotations `1`, matched byte for byte across revisions. This includes TypeScript, JavaScript, CMI, CMJ, CMT, and CMTI files. Classic lookup keeps the disk AST roundtrip and publishes an interface to disk before releasing dependents; capture of compiled CMI and CMJ remains available in the session. This preserves the existing GenType and editor output while the policies are separated.
