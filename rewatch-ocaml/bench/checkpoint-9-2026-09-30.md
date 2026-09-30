# Checkpoint 9: imported type graphs

Status: validated for opt-in frozen lookup. The frozen lookup policy remains
opt-in until GenType parity and final promotion.

The final gate used Linux 7.0.12-linuxkit on aarch64, `/tmp` on overlayfs,
the Dune `dev` profile, one compiler domain, and five interleaved runs. The
Rewatch executable SHA-256 was
`b0fadd657b50fbe218dc785ea570c0dbb916da17f173c3aee1923420ef7b09a3`;
the standalone compiler SHA-256 was
`04a3c758e119ceaab33ac4eb24935807ea7e771812bcfc970f5e106403f2ea41`.

## Cost model

The 25,571-byte `Api.cmi` from the values fixture has 403 type roots and 403
type nodes. The type-graph microprobe ran 1,000 iterations on Linux/aarch64:

| Operation | Worker time | Allocated |
| --- | ---: | ---: |
| Freeze all roots | 51.2 ms | 147.5 MB |
| Thaw all roots | 46.2 ms | 97.3 MB |
| Thaw one root | 1.3 ms | 4.0 MB |
| Decode full CMI | 116.8 ms | 142.9 MB |
| Copy full signature with `Subst.signature` | 57.3 ms | 196.7 MB |

The frozen index already materializes values, types, constructors, and record
labels by name. Its view owns all mutable type nodes, layout refs, descriptors,
and member IDs, while the captured image holds immutable data. The trace shows
200 frozen value lookups in 0.28 ms with 0.38 MB allocated on the values fixture;
the analogous constructor lookups took 0.85 ms and 1.15 MB on the variants
fixture. Full signature copying occurred once per build where the implementation
was checked against a separate interface, costing 0.15–0.94 ms and
0.34–0.59 MB across these fixtures. The remaining full-signature path serves
operations that require the complete ordered signature, including inclusion.

## Module declarations and paths

Simple module aliases and module-type identifiers now use frozen path records
and direct binder-context mapping in both module and module-type declarations.
Abstract module types materialize directly. Simple declarations no longer
retain a duplicate serialized full declaration. Path-only lookups leave the
request-local type graph unmaterialized; value, type, and constructor lookups
force it when needed. OUnit checks attributes, alias presence, cached identity,
independent views, lazy graph creation, nested module-type paths, and distinct
binder contexts when a module type is instantiated twice.

Explicit traces distinguish the remaining compatibility paths. In a traced
one-run build, complete signature copying was invoked once for the explicit
interface, taking 0.26 ms and allocating 0.34 MB. Complex functor declarations
were materialized 200 times in each module fixture, taking 0.67 ms/1.20 MB
without an interface and 0.43 ms/1.12 MB with one. No fixture invoked legacy
generic component expansion of a frozen root signature. Full-signature
inclusion needs an ordered, mutable signature with freshly renamed binders;
complex functor types need nested binder substitution. These two paths remain
request-local. Their measured total is below 1 ms per build in the fixtures,
so replacing them with another complete-structure materializer has no
demonstrated benefit. The direct frozen path handles common name-based lookups.

## End-to-end comparison

`immutable_interface_gate.py` generated six 201-module fixtures and alternated
classic and frozen one-domain clean builds five times. It checked complete
generated-file sets and SHA-256 hashes after every pair. All pairs matched.
These medians are from the final isolated run after the lazy view change;
elapsed time includes process startup and build-system work.

| Fixture | Classic wall | Frozen wall | Classic CPU | Frozen CPU | Classic RSS | Frozen RSS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Values | 234.8 ms | 171.2 ms | 243.6 ms | 189.0 ms | 33,988 KiB | 35,516 KiB |
| Types | 299.0 ms | 168.2 ms | 318.9 ms | 186.3 ms | 33,616 KiB | 33,828 KiB |
| Variants | 314.8 ms | 177.6 ms | 337.2 ms | 191.4 ms | 33,792 KiB | 36,716 KiB |
| Modules | 254.1 ms | 175.8 ms | 261.5 ms | 194.4 ms | 33,056 KiB | 34,900 KiB |
| Open | 262.2 ms | 182.6 ms | 271.4 ms | 196.9 ms | 33,052 KiB | 34,528 KiB |
| Inclusion | 253.4 ms | 183.3 ms | 259.6 ms | 201.8 ms | 33,072 KiB | 34,792 KiB |

The new module paths showed no robust incremental wall-time gain beyond the
existing frozen index. The opt-in frozen path stayed faster than classic across
all fixtures, while sampled peak RSS varied by fixture and was higher for
several frozen fixtures. The full signature and complex module fallbacks remain
for a later validated change.

The inclusion fixture has an explicit interface, an abstract module type,
module aliases, a functor, and 200 dependent modules. An implementation edit
produced identical compiler work and artifact bytes under both policies, and
the restart build did not change those artifacts. Changing one interface
declaration to create an inclusion error produced identical diagnostics and
failed under both policies.

## Editor annotation parity

With `REWATCH_BIN_ANNOT=1`, the five-run gate again passed all six fixtures.
It compared generated non-annotation bytes, typedtree text, value-dependency
types and locations, and other CMT metadata across policies. It also checked
raw CMT/CMTI byte stability over five clean builds within each policy. Raw
annotation bytes differed across policies because frozen lookup avoided eager
type-node creation: for example, the first `Api.cmt` value type had internal
ID 267 in classic lookup and 44 in frozen lookup. The binding stamps, printed
typedtree, and editor results matched. Hover, references, completion, and
incomplete-source completion returned byte-identical responses in the values
fixture. The inclusion edit, restart, and mismatch diagnostic checks also
passed with annotations enabled.

| Fixture | Classic wall | Frozen wall | Classic RSS | Frozen RSS |
| --- | ---: | ---: | ---: | ---: |
| Values | 311.2 ms | 233.3 ms | 36,488 KiB | 37,060 KiB |
| Types | 350.6 ms | 250.2 ms | 37,828 KiB | 36,704 KiB |
| Variants | 360.7 ms | 238.8 ms | 38,032 KiB | 38,672 KiB |
| Modules | 322.6 ms | 247.1 ms | 35,552 KiB | 37,644 KiB |
| Open | 321.2 ms | 244.3 ms | 37,456 KiB | 37,944 KiB |
| Inclusion | 351.5 ms | 264.8 ms | 37,996 KiB | 38,560 KiB |

The annotation run used Rewatch SHA-256
`06d1dcfe17c420da64ff314b643a91e8051c625bf3dbbda076a512b1c022acb7`
and standalone compiler SHA-256
`7bf33f9bb0e131c0dbbb87ec3a0ea11e4b8af3645aef7094a0350f09e9ccec1f`.

Final `make test` (including 345 OUnit cases), the OCaml Rewatch port suite,
`make test-analysis`, and `make checkformat` passed. One earlier port run
stopped at a CMJ change assertion; the same two-step build changed the CMJ
when isolated. The complete suite passed on rerun. A later benchmark attempt
ran out of space in
`/tmp` after four fixtures; generated benchmark projects were removed and the
full six-fixture gate passed on rerun.

Next: checkpoint 10, GenType parity with frozen lookup.
