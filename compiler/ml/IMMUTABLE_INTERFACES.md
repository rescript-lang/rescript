# Immutable compiled interfaces: experiment

## Same-session module results (Goal 3)

With `REWATCH_FROZEN_VALUES=1`, the embedded compiler captures a successful
module's CMI, CMJ, CMT, and parser AST before the request ends. Rewatch stages
the interface and optimization metadata, then publishes an in-memory module
result before scheduling dependent compiler jobs. One export domain copies
independent implementation artifacts while compiler jobs continue. Interface
pairs and modules with JS post-build hooks export in their ordered scheduler
phase. All exports finish before build success. The result API in
`Rescript_compiler_driver` exposes separate interface and
optimization fingerprints, request-owned interface and CMJ views, a cloned
typed semantic result, located diagnostics, dependencies, and generated output
paths. The regular files remain available to standalone tools and new build
sessions.

The parser hands its AST and dependency list directly to the compiler and
module graph in the same session. The AST is transferred once; a later
request or a cold build reads the file. Type checking resolves a staged CMI to
a frozen interface image; JavaScript compilation resolves a staged CMJ to
frozen optimization metadata. Both lookups validate the selected source
identity and load-path order, including namespace paths and shadowing. A
`.resi` publishes the governing CMI before its `.res` is checked; the
implementation retains that interface and publishes its own CMJ. GenType
consumes the typed implementation result that was just produced, while an
explicit `.cmti` remains a disk input for its separate interface.
The session loader checks actual directory entries before allowing a
lower-case file path to shadow a staged artifact; this also avoids stale
case aliases on case-insensitive filesystems after a failed export.

Each worker request has independent inference, identifiers, and diagnostics.
The shared images contain no request-owned mutable nodes; callers receive new
views. A later request for the same input removes unfinished staged values,
and a generation check prevents an older, superseded request from staging its
result. Failed and cancelled jobs leave no pending publication. Artifact
publication errors still fail the build. A changed staged fingerprint before
export also fails the build. Rewatch compares the interface CRC and CMJ digest
separately to propagate dependency changes, falling back to artifact byte
comparison when a session fingerprint is unavailable. Deferred CMI and CMJ
exports preserve their producer timestamps when contents change, so a fresh
build process does not mistake them for newer than their compiled consumers.

Typed results are retained only for local modules and only up to 4 MiB each,
64 results, and 16 MiB of total CMT file size. The oldest entries are evicted;
graph accessors return independent values and return `None` after their backing
file changes.
The existing editor/LSP disk-CMT path remains compatible and can adopt this
API later. Parse jobs still write ASTs and copy them into the persistent cache
before graph construction, although graph discovery and downstream compiler
jobs use the in-memory parse result. External dependencies, cold starts, CLI
invocations, namespace map exports, leaf artifacts, and other artifact
consumers still use files. PPXs remain external processes.

Twenty-one interleaved pairs on 2026-09-26 used four compiler domains and two
synthetic 201-module projects. Each project has a 400-value API with 200
consumers; the second also stresses nested module signatures and a functor.
Two further 21-pair runs used the repository's 634-module `tests/tests`
project with its React dependency already built. Wall-time medians include
process launch, parse, compile, and artifact export:

| Workload | Classic | Session | Delta |
| --- | ---: | ---: | ---: |
| Values, clean | 175.05 ms | 152.23 ms | −22.82 ms (−13.04%) |
| Values, one consumer edit | 39.87 ms | 39.45 ms | −0.42 ms (−1.06%) |
| Modules, clean | 175.74 ms | 151.26 ms | −24.48 ms (−13.93%) |
| Modules, one consumer edit | 39.46 ms | 40.57 ms | +1.11 ms (+2.82%) |
| `tests/tests`, clean (42 pairs) | 1569.79 ms | 1521.18 ms | −48.61 ms (−3.10%) |
| `tests/tests`, one file edit (42 pairs) | 114.86 ms | 113.30 ms | −1.57 ms (−1.37%) |

The scheduler skips frozen dependency lookup for fewer than four initially
ready modules unless they depend on each other. This removed a small-edit
regression found in an earlier benchmark. The larger workload initially
regressed by 9.6% on clean builds;
reading the frozen feature setting once per request and memoizing successful
type lookups removed that regression. A traced real-project build reduced
frozen type lookup calls from 934,662 to 761 and their measured allocation
from 271.2 MB to 2.2 MB. In a traced clean values build, 202 AST lookups,
201 CMI lookups, and 200 CMJ lookups used session data; there were no newly
produced AST, CMI, or CMJ file reads in those categories. Four CMI reads
remained for cold standard-library inputs. An integration trace shows the
consumer compiler request starting before its producer's deferred interface
export. A same-path comparison found all 1,206
selected CMI, CMJ, and JavaScript artifacts byte-identical, with identical
stdout and stderr. The synthetic workloads stress shared imports and gain
13–14% on clean builds. The larger project has fewer shared imports; copying
its independent artifacts after all compiler jobs initially made clean builds
1–2% slower. Overlapping those exports with compiler work removed that
regression. Reusing the staged CMJ image and avoiding repeated path
canonicalization also removed work. The small-edit differences vary in sign
between runs: the two `tests/tests` edit medians were +2.48 ms and −2.15 ms.
These numbers do not predict every project or edit pattern.

## Current boundary

`Env` borrows decoded CMI graphs from a project cache, one request at a time.
The cache verifies their mutable fields at request end and restores a saved
marshaled image after a change. This avoids most repeated decoding, but it
cannot share a graph with concurrent compiler domains. The large alias cache
similarly shares a marshaled prepared image and gives each domain its own
decoded graph.

The exposed `Types.type_expr` graph is unsuitable for direct sharing. Its
`desc`, `level`, and `id` fields are mutable. `Ctype.instance` temporarily
installs `Tsubst` marks in source nodes; `Subst` does the same while copying
signatures and component declarations. Abbreviation memos, polymorphic
variant row references, object-field mutability classes, `Ident` stamps, and
record layout references are also mutable. A caller can receive declarations
or component descriptions that refer back into the imported graph.

## First measurement

A local synthetic project used a 26 KiB `Api.cmi` with 400 exported integer
values, a polymorphic identity function, and a parameterized record. Two
hundred implementation modules each import it. Four compiler domains built
201 modules. The probe used the retained project cache and the exclusive
`REWATCH_TYPECHECK_TRACE` phase timer. The numbers below are **summed worker
time**, not wall-time savings, from one traced clean build on 2026-09-25.

| Implementation-request work | Cached | Cache disabled |
| --- | ---: | ---: |
| CMI read and decode | 3.7 ms, 1.0 MB, 11 calls | 175.0 ms, 46.3 MB, 603 calls |
| Imported `Subst.signature` copies | 18.5 ms, 39.6 MB, 201 calls | 16.2 ms, 39.6 MB, 201 calls |
| Component-item construction, including `Subst` copies | 65.9 ms, 111.3 MB, 602 calls | 82.8 ms, 111.3 MB, 602 calls |
| CMI cache validation, capture, and verification | 26.2 ms, 6.0 MB | absent |

The two consumer phases still allocate about 151 MB across the cached build.
Their time varies between runs, but their allocation is stable. The current
cache eliminates most decoding and does **not** eliminate those copies. A
representation that merely freezes and then fully thaws every interface for
each request would give much of the decoding cost back. The first version
should preserve these phase labels and add materialized-node counts so the
comparison is direct.

The `frozen_type_graph_probe` microprobe used the same `Api.cmi` for 200
operations in one process. The arena had 403 type roots and 403 type nodes.
Freezing those roots took 12.8 ms and 18.7 MB; thawing all of them took 7.8
ms and 11.7 MB. Materializing only the first root 200 times took 0.14 ms and
0.7 MB. Decoding a marshaled **whole CMI** 200 times took 20.2 ms and 28.6
MB; `Subst.signature` took 11.6 ms and 39.3 MB. For `Stdlib.cmi` (30 roots,
73 nodes), 200 full thaws took 0.45 ms and 2.4 MB. These are diagnostic
single-run measurements, not equivalent operations: the arena probe omits
signature metadata and component tables. They show that selective
materialization can be cheap, while full materialization leaves substantial
consumer work.

The same probe on the larger `JsxDOMStyle.cmi` (505 roots, 1,010 nodes) took
1.5 ms and 2.9 MB for 20 full thaws; `Subst.signature` took 1.8 ms and 5.5
MB. Another run measured 2.0 versus 1.7 ms, respectively. Whole-graph thaw
is in the same time range as the existing signature copy before metadata and
component work. This favors direct frozen lookups and per-use instantiation
over whole-interface thawing.

This fixture stresses repeated flat value exports. It says little about
functors, recursive modules, large variants, deeply nested signatures, or
watch invalidation. Its trace cannot be extrapolated to the testrepo or a
production project. The testrepo's installed PPX in this workspace is a
macOS binary, so the local testrepo build failed before compiler measurements.

## Proposed representation

A project owns an `interface_image` assembled once from each accepted CMI:

```text
interface_image
  file identity + content digest
  immutable identifier table (name, original stamp, flags, binder token)
  immutable path table (identifier and path indices)
  immutable type arena (indexed graph, levels, descriptor payloads)
  immutable signature and declaration tables (indices into the same arenas)
  immutable name/component indexes (values, types, constructors, labels,
                                    modules, module types)
```

The arena uses integer edges rather than `type_expr` pointers, so it can
represent sharing and cycles without linkable source nodes. Every reference
in the image must lead to another immutable value; in particular, a `ref`,
array, mutable identifier, or mutable layout cannot be reachable through a
public accessor. The image is published to the project's workers only after
validation and complete construction. Its cache key includes the resolved
path and CMI identity; a changed or newly shadowing CMI gets a new image.
Existing CMI file format and digest semantics stay unchanged.

Each compile request owns a small view over an image. The view maps image
binder tokens to request-local `Ident.t`s and applies module and type path
substitutions without copying the image. Ordinary value lookup returns a
scheme handle. Instantiation walks that frozen scheme with a **request-local**
memo from arena index to fresh `type_expr`, preserving aliases within one use
while giving sibling uses independent inference variables. `Tpoly` bound
variables and row references need the same rule. Object-field mutability
classes become fresh cells per generalized instance, preserving class
sharing inside that instance; non-generalized occurrences instead share a
request-local cell. Abbreviation expansions and speculative trail state live
in the request view, keyed by arena index, and never write into the image.

Type constructor identity must come from the image's binder token plus its
instantiation context, rather than from a copied node's physical address.
Functor application creates a fresh request-local context; aliases that point
to the same exported constructor retain one token. This must be verified with
module inclusion, GADTs, recursive types, and polymorphic variants. Imported
record labels and constructor descriptions can be indexed immutably, but
their current mutable `lbl_all` and layout behavior needs request-local
overlays or an explicit immutable replacement before direct publication.

The checker should keep `Types.type_expr` for local inference and typed-tree
output while imported schemes use a separate immutable type. The boundary is
an API, not a convention to refrain from writing to an ordinary `type_expr`.
Any operation that genuinely needs a mutable imported graph can materialize
only the reached subgraph in the request view and count those nodes. That
fallback preserves behavior while revealing whether consumers force costly
copying.

## Prototype and integrated experiments

`Frozen_type_graph` is an initial arena for type expressions. It
snapshots identifier and path data, object-field mutability equivalence
classes, and polymorphic-variant row references; it rejects active `Tsubst`
and abbreviation memo state. A request-local view lazily materializes roots
while preserving cycles and sharing. Unit tests exercise polymorphic
instantiation, aliasing, class independence, cycles, concurrent views, and
rejection of transient state.

`Frozen_values` adds immutable indexes for exported values, all completed type
kinds, record labels, variant and extension constructors, modules, and module
types in nested signatures. It snapshots runtime layouts and inline-record
metadata. A module typed by a locally declared module type gets a separate
path-substitution context over the same immutable type arena, so repeated
uses retain distinct abstract type identities. Module aliases can traverse
to the target image, including another compilation unit. When
`REWATCH_FROZEN_VALUES=1`, `Env` builds the image once for each accepted CMI
in a project dependency cache and shares it between domains.

Each compile request gets its own view and materializes reached declarations
and type nodes. Opened imports use on-demand name sources in `Env` instead of
building every component. Module and module-type declarations are decoded and
substituted one at a time; functor applications use a request-owned component
for the reached functor. Full-signature consumers and legacy component
expansion decode private copies from immutable signature bytes. Mutable label
and constructor descriptions, runtime layout references, and attribute
payloads belong to the view. Imported member identifiers use a reserved
stamp range, so materialization does not renumber identifiers emitted by the
compiling module. A failed arena snapshot or a shape that needs contextual
substitution still uses request-owned fallback structures. The CMI format is
unchanged and the flag remains off by default.

The flag takes precedence over `REWATCH_COMBINED_SIGNATURE_CACHE`, including
its `force` setting. A namespace-open regression test confirms that the older
mutable combined snapshot is skipped. Avoiding eager expansion can change an
internal identifier stamp in a consumer's CMI, and therefore its self CRC,
even when the exported value, type, dependency CRCs, and JavaScript agree.
This can cause downstream rebuilds when switching flag settings.

A values-only variant of the same synthetic fixture used 200 consumers and
four compiler domains. Each consumer accessed two `Api` values. With the
retained CMI cache active in both configurations, one traced clean build on
2026-09-25 gave the following **summed worker time and allocation**:

| Implementation-request work | Existing path | Frozen values |
| --- | ---: | ---: |
| Component construction | 64.4 ms, 111.3 MB, 602 calls | 25.0 ms, 40.8 MB, 402 calls |
| `Api` component expansion | 200 calls | 0 calls |
| `Api` signature copy | 0.1 ms, 0.2 MB, 1 call | 0.1 ms, 0.2 MB, 1 call |
| Frozen image preparation | absent | 0.1 ms, 0.4 MB, 1 call |
| Direct frozen value lookup | absent | 1.4 ms, 0.3 MB, 600 calls |

The signature-copy count reflects a separate path-normalization improvement:
normalizing a persistent compilation-unit root now validates its CMI without
copying the whole signature. Before that change, both configurations copied
`Api`'s signature 201 times, allocating 39.6 MB. The remaining copy checks
the implementation against `Api.resi`.

Across all 201 implementation requests, summed worker time fell from 208.6
to 176.0 ms and allocation from 174.8 to 107.1 MB. All 1,610 selected CMI,
CMJ, JavaScript, and AST artifacts in the two builds were byte-identical. In
11 interleaved clean-build pairs, median wall time
was 165.38 ms with the existing path and 161.07 ms with frozen values; eight
pairs favored frozen values. This is a small directional result for a
synthetic, flat interface, not a general speed estimate. The large residual
component work comes from dependencies outside the direct `Api` lookup path.
This was the first flat-interface measurement, before the nested, extension,
and opened-name work below.

The next fixture exported 200 abstract types and 200 manifest aliases, with
each consumer mentioning one of each. On one traced clean build, 200 `Api`
component expansions disappeared. Component construction fell from 172.1 to
40.8 MB; allocation across all implementation requests fell from 287.8 to
107.3 MB. Summed worker time fell from 280.7 to 165.1 ms. The direct type
lookup phase took 1.3 ms and 0.9 MB for 2,600 calls. All 805 selected
artifacts were byte-identical.

A mixed fixture with exported values and a record type exercises record-label
lookup too. Direct record and label lookup removed its 200 `Api` expansions:
component construction fell from 111.3 to 40.8 MB, total implementation
allocation from 182.2 to 109.4 MB, and summed worker time from 220.2 to
197.5 ms. Its 805 selected artifacts were also byte-identical. These are
single traced runs, not stable wall-time estimates.

A variant fixture exported 200 variant types and had consumers construct and
match their constructors. Direct constructor lookup removed its 200 `Api`
expansions: component construction fell from 194.3 to 40.8 MB, total
implementation allocation from 281.6 to 109.5 MB, and summed worker time
from 298.7 to 175.8 ms. Its 805 selected artifacts were byte-identical.

Interleaved clean-build pairs, with tracing disabled and four compiler
domains, measured the complete Rewatch build process (11 pairs for the first
three fixtures, 17 for the last two):

| Fixture | Existing median wall time | Frozen median wall time | Frozen faster |
| --- | ---: | ---: | ---: |
| Abstract types and aliases | 184.54 ms | 165.14 ms | 11/11 pairs |
| Values and record | 170.75 ms | 165.17 ms | 7/11 pairs |
| Variants and constructors | 192.54 ms | 162.64 ms | 11/11 pairs |
| Modules, aliases, and functor use | 169.22 ms | 144.67 ms | 17/17 pairs |
| Opened values and record | 174.07 ms | 151.84 ms | 17/17 pairs |

These are short synthetic builds on one machine. The larger gains occur when
the fixture's main dependency has many declarations that the old path copied
for every consumer; the mixed fixture shows a smaller wall-time gain despite
removing the same expansion count.

The last two fixtures exercise broader interface shapes. The module fixture
exports 400 values, a module type, two modules ascribed to that type, an
alias, and a functor. Every consumer reads the modules and alias and applies
the functor. The opened fixture exports 400 values, an identity function, and
a record type; every consumer uses `open Api`. In one traced clean build with
201 implementation requests and four compiler domains:

| Fixture | Existing implementation allocation | Frozen allocation | Existing component construction | Frozen component construction | `Api` expansions |
| --- | ---: | ---: | ---: | ---: | ---: |
| Modules | 193.6 MB | 74.8 MB | 113.3 MB, 1,403 calls | 0.2 MB, 201 calls | 200 → 0 |
| Opened interface | 183.6 MB | 71.9 MB | 111.5 MB, 602 calls | 0, 0 calls | 200 → 0 |

Summed implementation worker time in those traced builds was 204.1 →
167.6 ms for modules and 248.5 → 148.6 ms for opened imports. The 17
interleaved wall-time pairs above had tracing disabled. All 1,608 selected
module-fixture artifacts and all 1,610 opened-fixture artifacts matched byte
for byte across flag settings. These synthetic builds provide directional
evidence, not a production-project speed estimate.

An initial type-image view eagerly built a substitution map for all 400
binders in every request. It added about 64 MB across the fixture and kept
the component expansions. Replacing that with immutable binder indexes and
lazy path substitution removed that view-construction cost. A separate path
normalization probe showed that asking whether `Api.opaque` is a module
forced the entire `Api` component table; the immutable top-level type-name
index now answers that check without expanding the module. These two failures
illustrate why selective materialization has to include consumers outside
ordinary type lookup.

To repeat the type-only microprobe after building the compiler, run
`opam exec -- dune build rewatch-ocaml/bench/frozen_type_graph_probe.exe`
and then invoke that executable with a CMI path and iteration count. The
synthetic project can be recreated with
`python3 rewatch-ocaml/bench/make_immutable_interface_fixture.py OUTPUT_DIRECTORY`.
It has `Api.resi` with 400 integer values, `id: 'a => 'a`, and
`type box<'a> = {value: 'a}`. Its 200 consumers each access one value and
call `Api.id`; `--values-only` omits the record use, `--types-only` generates
the abstract-type and alias fixture, `--variants-only` generates the variant
fixture, `--modules-only` exercises module types, aliases, and a functor, and
`--open-only` exercises an opened interface.
Build it with the embedded OCaml
Rewatch executable, four domains, and `REWATCH_TYPECHECK_TRACE` set to an
absolute TSV path; compare clean builds with `REWATCH_FROZEN_VALUES` absent and
set to `1`. Analyze both files with
`rewatch-ocaml/bench/analyze_typecheck_trace.js`.

Direct indexing still stops where the module shape depends on a functor
application or on a module-type identifier that cannot be resolved within the
same CMI. Those cases materialize request-owned declarations or components
from the image. Signature inclusion still requests a full copied signature.
The next validation should measure edit workloads, larger real projects, and
fallback frequency before enabling the flag by default. The goal remains to
remove repeated interface preparation without moving that cost into per-use
materialization.
