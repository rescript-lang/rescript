---
name: records
description: Commit records and Semantic Diff pages for the ReScript compiler repository. Use when committing a change, when an agent reports its work, and when someone asks to review a commit, a pull request or a branch. Every commit message is a record, a semantic diff with its reason; review pages are drawn from records with scripts/commitRecords.js, never written by hand.
---

# Commit records and Semantic Diff pages

## Principles

1. **A record is a semantic diff.** A code diff is a delta on files; a
   record is the delta on the objects the code implements: a typing rule,
   the shape of an AST node, a Lambda rewrite, the JavaScript a program
   compiles to, an error message. Each change names its object and says
   whether it is added, removed or modified, with `Before:` and `After:`
   for a modification.
2. **Every change has its reason.** The record opens with `Why:`: the
   concrete problem (the program that fails, the crash, the error message or
   the wrong output) and why the old design cannot handle it. A reader
   learns what was wrong before reading what changed.
3. **Each kind of object has its visual language.** A typing rule is an
   inference rule; an AST fragment a tree; Lambda and JavaScript highlighted
   code; an error message its text. The kinds below fix the language of each.
4. **Discretion, guided by tools.** What to show, what to hide, where a
   schematic form explains better than the exact one is the author's
   judgement. The exact forms come from the compiler
   (`scripts/recordFacts.js`, below), and every abstraction in a record is
   one those forms support.
5. **Plain words.** Write for someone who knows ReScript and compilers but
   not this codebase. Use the field's standard terms, define notation before
   the formula that uses it, and prefer spelling a set out to introducing a
   symbol. Mathematics is Unicode, never TeX. Lists, not tables.
6. **A record is the commit's history.** What was wrong and what changed
   belong in the commit. Each sentence states only what the evidence
   supports; a number says what produced it.
7. **The commit is plain text.** The message reads well in `git log`; it is
   not Markdown. Drawing happens only where records are rendered.

## The record

```
<subject: the change, in a short sentence>

Why:
    <the problem: the program, the crash, the message or the wrong output,
    and why the old design cannot handle it>

<Kind> <added|modified|removed> (<object>):
  Before:
    <the old form>
  After:
    <the new form>

Example:
    <the ReScript program that shows the change, at most 15 lines>

Effect:
    <call or case>   <old outcome> → <new outcome>

Source: <where each Before and After comes from: the compiler build, a
committed snapshot at a revision, or the pull request>

Breaking: <what existing code stops compiling or behaves differently, and
how to migrate>

Review: <a question a reviewer raises, and its answer; repeat>

Area: syntax printer frontend typechecker lambda codegen runtime rewatch
 analysis reanalyze gentype tools playground scripts ci docs tests
Output: unchanged | changed <which programs>
Tests: +added/path ~changed/path -removed/path | none, because <reason>
Measured: <quantity before -> after, and what produced it>; ...
PR: #<number>
Signed-off-by: <Name> <email>
```

- **Sections** start with `Name:` at the start of a line; the content is the
  rest of that line or a block indented by 4 spaces. In a change block,
  `Before:` and `After:` are indented by 2 spaces and their content by 4;
  `Before (lang):` and `After (lang):` draw the versions as that language.
  `Before (lang):` followed by `After (lang):` (`js`, `rescript`, `ts`,
  `ocaml`, `lambda`, `text`) as sections of their own is a pair of code
  blocks, drawn side by side.
- **A replacement is one `modified` block.** When the change replaces one
  shape by another (a node, a typed tree node, a Lambda term, the
  JavaScript a program compiles to), the block shows one example as the
  parent commit's compiler gives it, `Before:`, and as this commit's gives
  it, `After:`, both extracted (below). `added` and `removed` are for an
  object with no counterpart on the other side: a new error message, a
  deleted flag. An added object shows its form under `After:`; a removed one
  may show its form under `Before:`.
- **The final block is git trailers**: `Key: value`, continuation lines
  indented by one space, nothing after them; `git interpret-trailers --parse`
  reads every key. `Co-Authored-By` lines go before `Signed-off-by`, which is
  the last trailer.
- **Required trailers**: `Area`, `Output`, `Tests` and `Signed-off-by` on
  every commit. `Why:` and at least one change block on every commit that
  changes compiler, runtime, build system or editor tooling sources.
- **The changelog is the `changes/<name>.<category>.md` fragments** the
  commit adds or changes (`changes/README.md`). The record repeats neither
  the category nor the entry; the card reads each fragment from the commit's
  tree. A commit that makes more than one user-facing change adds one
  fragment per change.
- **The diff states the rest.** The card reads the bumped magic numbers, the
  committed generated JavaScript, the error catalogue and the fixtures from
  the commit itself, and checks them against the record and the
  repository's rules (below).

## The trailers

| Trailer | What it states | What the card checks |
| --- | --- | --- |
| `Area` | The layers the change touches, from the list in the template. They decide who reviews it and which suites apply. | The trailer is present. |
| `Output` | `unchanged` when no existing program compiles to different JavaScript; `changed <which programs>` otherwise, with a `JS` or `Lambda` change block showing the difference. | Under `unchanged`, no committed generated JavaScript (`.mjs`, `.cjs`, `.res.js` under `tests/` and `rewatch/testrepo/`, the runtime's `lib/`) is modified; new files from new tests are counted. |
| `Tests` | The tests added (`+`), changed (`~`) and removed (`-`), by path from the repository root (a directory stands for the files under it), each failing without the change; or `none, because <reason>`. Every test file the diff adds, changes or removes (`tests/`, `rewatch/tests/`, `*.test.js`) is listed, except the snapshots tests generate: committed generated JavaScript, files in `expected/` directories, `.expected` files and `rewatch/tests/snapshots/`, which may be listed. | Every listed path is in the diff; every test file in the diff other than a snapshot is listed, the missing ones named by path; `none` gives its reason, and the diff then changes no test file. |
| `Measured` | Performance before and after, `before -> after`, with the input and the command that produced each number; items separated by `; `. | Drawn as a list, each delta marked. |
| `PR` | The pull request; the card links it. | |
| `Signed-off-by` | The DCO sign-off, with the commit author's name and email, as the last trailer. | The DCO app checks it on the pull request. |

## Kinds of object

Each kind gives the object, the code that owns it, how to extract its exact
form, how it is drawn, and what a change of it brings with it.

- **`Typing`**, a typing rule: when the type checker accepts an expression,
  pattern or declaration, and at which type. Owned by `compiler/ml/typecore.ml`,
  `typetexp.ml`, `ctype.ml`, `typedecl.ml`, `typemod.ml`, `includemod.ml`.
  Extract: `recordFacts.js … types` (the inferred interface), `typed`, and
  `diagnostic` for a refusal. Drawn as an inference rule: premises side by
  side, separated by three or more spaces; a line of `─` ending with the
  rule's name in parentheses; the conclusion. Side conditions are written as
  words (`τ is an @unboxed record`). Brings a test that a program the rule
  newly accepts compiles, and a `super_errors` fixture for one it refuses.
- **`Syntax`**, the surface grammar: what the parser accepts and how a form
  is spelled. Owned by `compiler/syntax/src/res_core.ml`, `res_scanner.ml`,
  `res_grammar.ml`. Extract: `recordFacts.js … parse`. Drawn as grammar
  productions (`expr ::= expr ? expr : expr`) in mathematics. Brings a
  fixture in `tests/syntax_tests/data/parsing`.
- **`AST`**, the shape of a parsetree node, or of its v0 encoding for
  external PPXs. Owned by `compiler/ml/parsetree.ml`; the v0 wire by
  `parsetree0.ml`, `ast_mapper_to0.ml`, `ast_mapper_from0.ml`. Extract:
  `recordFacts.js … parse` (`bsc -bs-no-builtin-ppx -only-parse
  -dparsetree`, the parser's output), and `expanded` for the tree after the
  built-in PPX. Drawn as a tree: one node a line, two spaces a level,
  `label: Node` for a field; the compiler's printout pastes as it is, its
  `[` and `]` lines not drawn. A change to `parsetree.ml` bumps the ast
  magic numbers in `compiler/ext/config.ml`; `parsetree0.ml` is frozen; a
  change to the v0 mapping brings a fixture in
  `tests/syntax_tests/data/ast-mapping` or a bridge unit test.
- **`Typedtree`**, the shape of a typed tree node. Owned by
  `compiler/ml/typedtree.mli`. Extract: `recordFacts.js … typed`
  (`bsc -dtypedtree`). Drawn as a tree. A change to `typedtree`, `types` or
  `cmt_format` bumps the cmi or cmt magic number.
- **`Transform`**, what a built-in PPX produces from source: the JSX
  transform, `@deriving`, externals. Owned by `compiler/syntax/src/jsx_*.ml`
  and `compiler/frontend/`. Extract: `recordFacts.js --jsx … transform`
  (`bsc -reprint-source`). Drawn as ReScript code; `source ⇝ result` draws
  the rewrite. Brings a fixture in `tests/syntax_tests/data/ppx`.
- **`Format`**, what `rescript format` prints. Owned by
  `compiler/syntax/src/res_printer.ml`, `res_parens.ml`,
  `res_comments_table.ml`. Extract: `recordFacts.js … format`. Drawn as
  ReScript code, Before and After. Brings a fixture in
  `tests/syntax_tests/data/printer` or `idempotency`; formatted output parses
  back to the same tree with its comments.
- **`Diagnostic`**, the text of an error or warning. Owned by each layer's
  `report_error`, `compiler/ml/printtyp.ml`, `error_message_utils.ml`,
  `compiler/ext/warnings.ml`. Extract: `recordFacts.js … diagnostic`, or the
  fixture's `.expected` file. Drawn as the message text, without the code
  frame unless the frame is the point. A `super_errors` fixture added or
  removed comes with its row in `tests/ERROR_VARIANTS.md`.
- **`Matching`**, how the match compiler splits a `switch`: its clause
  matrix, exhaustiveness and unused cases. Owned by `compiler/ml/matching.ml`,
  `parmatch.ml`, `switch.ml`. Extract: `recordFacts.js … lambda` (the
  decision tree), and the matrix by reading the patterns. Drawn as a matrix:
  one row a line, columns three or more spaces apart, a `note: …` line below.
- **`Lambda`**, the intermediate language: what a construct translates to
  (`compiler/ml/translcore.ml`, `translmod.ml`) or what a pass rewrites
  (`compiler/core/lam_*.ml`). Extract: `recordFacts.js … lambda` (as
  translated) and `passes` (after each pass that changes it). Drawn as
  highlighted Lambda; a rewrite is `lhs`, a line `⇝`, `rhs`, then `when: …`
  conditions. One-letter capitals (`T`, `A`) stand for terms.
- **`IR`**, an internal representation: an OCaml type the compiler's layers
  share (`Lambda.lambda`, `Types.type_desc`, `J.expression`, a record of
  `Parsetree` that is not a node shape). Owned by the `.mli` defining it.
  Extract: the type in the diff (`git show <rev> -- <file.mli>`). Drawn as
  OCaml.
- **`Representation`**, how a ReScript value is represented at run time:
  `ReScript ↦ JavaScript`. Owned by `compiler/ml/variant_*.ml`,
  `record_runtime.ml`, `ast_untagged_variants.ml` and the code generator.
  Extract: `recordFacts.js … js` on a value. Drawn as one `↦` line a value.
  A changed representation is `Output: changed`.
- **`JS`**, the JavaScript a program compiles to. Owned by
  `compiler/core/lam_compile*.ml`, `js_*.ml`. Pinned by the committed
  `tests/tests/src/*.mjs` and the other generated files. Extract:
  `recordFacts.js … js`, or `git show <rev>:<file.mjs>` for a committed
  snapshot. Drawn as highlighted JavaScript, Before and After side by side.
  The snapshots the change modifies are the programs `Output: changed`
  names.
- **`API`**, a signature of the runtime or standard library. Owned by
  `packages/@rescript/runtime/*.resi`, `packages/@rescript/belt`. Extract:
  the `.resi`, or `recordFacts.js … types`. Drawn as ReScript. Brings tests
  in `tests/tests/src/stdlib` or `tests/docstring_tests`.
- **`TypeScript`**, what genType emits. Owned by `compiler/gentype`. Drawn
  as TypeScript. Brings a snapshot in `tests/gentype_tests`.
- **`Editor`**, what completion, hover, inlay hints or code actions return.
  Owned by `analysis/src`. Extract: the analysis test's expected output.
  Drawn as that text. Brings a fixture in `tests/analysis_tests`.
- **`Build`**, what `rescript build` or `rescript watch` does: which files it
  compiles, when it rebuilds, its configuration. Owned by `rewatch/src` and
  `rewatch-ocaml`. Drawn as prose; a `Command (sh):` section shows a
  session. Brings a test in `rewatch/tests`.
- **`Semantics`**, the meaning of a construct at run time, and
  **`Notation`**, the symbols a later formula uses. Drawn as mathematics.
- **`Code`**, an internal module, script or function added, moved or
  removed, with no other object changing. **`Docs`** and **`CI`** for
  documentation and workflows. Drawn as prose.
- **`Example`** and **`Counterexample`**: ReScript programs, drawn as code.
  **`Effect`**: `<call or case>   <old> → <new>` lines.

A section `Name (lang):` that is none of these draws its block as `lang`
code (`rescript`, `js`, `ts`, `ocaml`, `lambda`, `text`, `sh`).

## Writing the forms

- **An inference rule** is its premises on one line, separated by three or
  more spaces, a line of `─` with the rule's name in parentheses, then the
  conclusion; a blank line separates it from other formulas. One-letter names
  are variables (italic); a name applied to arguments is an operator; a
  capitalised name a constructor; a run of English words a side condition;
  `or`, `and`, `not` are words; `null`, `undefined`, `true`, `false` and
  backticked `code` are drawn as code. `_x`, `_{…}` and `^{…}` are sub- and
  superscripts; an indented line continues the formula above.
- **A tree** is indented by two spaces a level; `label: Node "name"` shows a
  field's label, `@attr` an attribute. Show only the nodes the change
  concerns; `…` stands for the rest.
- **A rewrite** (Lambda, Transform, JS) is the left side, a line holding
  `⇝`, the right side, then `when: …` lines.
- **Before and After** are drawn side by side, the lines each lacks from the
  other shaded, so keep unchanged lines identical. Code, trees and text are
  drawn as aligned rows: a line both versions have faces itself, and the
  changed lines between are padded so that the next common line faces
  itself again. A long line wraps, its continuation indented.

## Extracting the facts

`scripts/recordFacts.js` prints the exact forms a compiler build produces for
one example, the compiler's own output with colour codes removed:

```sh
node scripts/recordFacts.js <file.res> [view...]
node scripts/recordFacts.js -e 'let f = (c, a, b) => c ? a : b' parse typed lambda js
node scripts/recordFacts.js --jsx <file.res> transform js
node scripts/recordFacts.js --bsc <other build>/bsc.exe <file.res> all
```

Views: `parse`, `expanded`, `transform`, `format`, `typed`, `types`,
`lambda`, `passes`, `js`, `diagnostic`, `all` (default `types lambda js`).
The default compiler is the checkout's
`packages/@rescript/<platform>/bin/bsc.exe`, built by `make`; the first line
of the output names the compiler and the commit its checkout is at.

A modified block's two versions come from two builds, on the same example:
`After:` from this commit's build, `Before:` from a build of the parent
commit, in a second worktree:

```sh
parent=$(mktemp -d)/parent
git worktree add --detach "$parent" <rev>^ && (cd "$parent" && yarn install && make)
node scripts/recordFacts.js --bsc "$parent"/packages/@rescript/<platform>/bin/bsc.exe example.res parse
node scripts/recordFacts.js example.res parse
```

For a generated file a commit changes, the two versions are
`git show <rev>^:tests/tests/src/<file>.mjs` and
`git show <rev>:tests/tests/src/<file>.mjs`. The `Source:` line names both:
the command and view, the build of the parent commit for `Before:` and of
this commit for `After:`, or the revisions of the snapshot.

## The Semantic Diff page

```sh
node scripts/commitRecords.js --out review.html <rev>...
node scripts/commitRecords.js --out review.html <base>..<tip>...
node scripts/commitRecords.js --out review.html --pr 1234 --title "<PR title>" <base>..<tip>
node scripts/commitRecords.js --fragment --out part.html <rev>...
make semantic-diff
```

- `--pr` and `--title` head the page with the pull request's number (linked)
  and title; without them the page is headed "Semantic Diff".
  `make semantic-diff` renders `master..HEAD` to `_build/semantic-diff.html`
  and opens it.
- Each record is a card: the subject, the changelog categories and areas,
  the change blocks in their drawings (a modified block's versions side by
  side, an added or removed block's one version across the card), the
  changelog entries of its fragments, the trailers as facts, and the checks
  of the record against its diff:
  - each `changes/` fragment the commit adds or changes, validated as
    `yarn changelog:check` validates it (the placeholder PR link fails), and
    an edit of `CHANGELOG.md`, which belongs to release pull requests;
  - the required trailers `Area`, `Output` and `Tests`;
  - `Tests` against the diff, as the trailer table states;
  - committed generated JavaScript modified and added, against `Output`;
  - magic numbers bumped in `compiler/ext/config.ml`, and a change to a
    marshaled type (`parsetree`, `typedtree`, `types`, `cmt_format`) with
    none;
  - a change to the frozen `parsetree0.ml`, and a change to the v0 bridge
    without an `ast-mapping` fixture or bridge unit test;
  - `super_errors` fixtures added or removed without
    `tests/ERROR_VARIANTS.md`.
- A range adds a summary (commits, kinds changed, areas, changelog
  categories, failed checks), a changelog draft from the fragments the range
  leaves in their last version, and the files more than one commit changes.
  Use commit hashes as range bases.
- The page is one HTML file with no script, light and dark themes, phone
  width. The drawing is `scripts/recordRender.js`, a pure module (no file
  system, no git), so any other reader of commit messages draws records the
  same way. Its kinds are a typed table: a kind without a drawing does not
  type-check.
- A note on `refs/notes/records` replaces a commit's message as its record:
  use it for commits that exist already, such as squash merges.
- On GitHub, `.github/workflows/semantic_diff.yml` renders each pull
  request's range when it is opened or pushed, and comments the page's link
  (`Semantic diff: https://cdn.rescript-lang.org/semantic-diff/pr-<N>/…`).

## Enforcement

`scripts/commitRecordHook.sh` is a `commit-msg` hook. It refuses a message
without the required trailers, with a trailer after `Signed-off-by`, with an
`Output` that starts with neither `unchanged` nor `changed`, or with a
`Tests` that is neither a list of `+~-` paths nor `none, because <reason>`;
for a commit that changes compiler, runtime, build system or editor tooling
sources, it also requires `Why:` and a change block. Merge, `fixup!` and
`squash!` commits pass. A developer enables it in a checkout with

```sh
ln -sf "$PWD/scripts/commitRecordHook.sh" "$(git rev-parse --git-path hooks)/commit-msg"
```

## When to use

- Every commit carries a record in its message.
- An agent's report lists its commits; their records are the description of
  the work.
- Before review: render `<base>..<branch>` and hand over the page.
