# Owned AST handoff: 2026-09-30

Compared checkpoint 6 (`e477c3426`) with checkpoint 7 on Linux aarch64,
12 logical CPUs, the Dune default development profile, and one compiler
domain. Both OCaml Rewatch executables were in the same directory. Their
SHA-256 values were `5ab862d426507bef0fe98ccedc8a0da3424e494f036073babf9ebc51af21ce6d`
and `129d2cbdafec7a6ec4a3585d137e7536b5b9db56776b2ff5afa9f148ac20e94a`.
The standalone compiler and runtime were identical in both runs.

The five-run interleaved testrepo gate passed. Its labels `rust` and `ocaml`
mean the old and new embedded executables for this comparison.

| Scenario | Old/new median wall ms | Old/new sampled peak tree RSS KiB |
| --- | ---: | ---: |
| Clean | 3,136 / 3,140 | 196,304 / 198,708 |
| Unchanged | 229 / 227 | 29,240 / 33,480 |
| Single edit | 245 / 262 | 29,412 / 31,196 |

The clean wall median was level. The edit median increased 7% in this run;
the five samples overlap and do not establish a repeatable change. The clean
peak RSS median increased 1%. Unchanged and edit RSS samples were higher in
the new executable, by 14% and 6% respectively; the one-domain full-build
gate remained below its 125% memory threshold. Compiler request counts matched
exactly: 1,031 clean, 4 unchanged, and 6 edit. Complete post-build file sets
and artifact bytes matched.

The parser now captures the AST and dependencies before writing a staging
file. A successful frozen-lookup request serializes the AST once and hands it
to the session; an export worker writes that byte image to the staging and
persistent cache paths while compilation can consume the owned AST. Source
identity and the parser request generation validate the handoff. Once the
staging file exists, its disk identity is also checked. Serialized buffers
are released after export. Classic lookup and `REWATCH_OWNED_AST=0` retain
synchronous staging writes.

Focused tests checked exact AST byte parity with standalone `bsc`, compilation
before the staging file exists, dependency replacement on a newer parse,
source invalidation, changed disk identity, and deferred export timestamps.
The OCaml Rewatch port suite checked persistent cache reuse, restart, parse
publication failure, and retry in its existing integration scenarios. The
full `make test-all` run passed before the final buffer-retention cleanup;
the final `make test`, focused OUnit (111 tests), port suite, and
`make checkformat` passed afterward.
