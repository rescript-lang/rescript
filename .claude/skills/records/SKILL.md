---
name: records
description: Commit records and review pages for the ReScript compiler repository. Use when committing a change, when an agent reports its work, and when someone asks to review a commit, a pull request or a branch. Every commit message is a structured record that answers the questions ReScript reviewers ask; review pages are rendered from records with scripts/commitRecords.js, never written by hand.
---

# Commit records and review pages

A **record** is a commit message whose body has named prose sections and
whose last block is git trailers. Its fields are the questions reviewers of
this repository ask on pull requests, answered before they are asked. A
review page is rendered from records: one card per commit, and for a range a
summary, a changelog draft and the files more than one commit changes.

## The record

```
<subject: present tense, what the compiler or tool now does>

<one paragraph: the change at the level of the language or the tool>

Example (rescript):
    <the ReScript source that shows the change, at most 15 lines>

Before (js):
    <generated JavaScript before, for that source>

After (js):
    <generated JavaScript after>

Source: <how the Before/After JavaScript was produced: compiler and revision,
or the snapshot file in the diff>

Breaking: <what existing code or tooling stops working, and the migration>

Review: <a question a reviewer raised or will raise, and its answer; repeat>

Area: syntax printer frontend typechecker lambda codegen runtime rewatch
 analysis reanalyze gentype tools playground scripts ci docs tests
Output: unchanged | changed <which programs>, <evidence>
Formatting: unchanged | changed <which source shapes>
Formats: none | <bumped magic numbers: ast cmi cmt cmj>
PPX: unchanged | <effect on the frozen v0 PPX wire>
Editor: unchanged | <effect on completion, hover, hints; fixtures>
Errors: <ERROR_VARIANTS.md rows added, removed or relinked>
Tests: +added/path ~updated/path -removed/path | none, because <reason>
Measured: <quantity before -> after, and what produced it>; ...
PR: #<number>
Signed-off-by: <Name> <email>
```

- **Prose sections** are a line `Name:` or `Name (lang):` followed by text or
  a block indented by 4 spaces. `lang` is `rescript`, `js`, `ts`, `sh`,
  `rust`, `ocaml` or `text`; the renderer highlights ReScript and JavaScript.
  `Before` and `After` sections of one language render side by side with
  changed lines marked. Use `Before (rescript)` / `After (rescript)` for a
  formatter change and `Diagnostic (text)` for compiler output. Any other
  name is allowed and renders as prose or code.
- **The final block is git trailers**: `Key: value` lines, continuation lines
  indented by one space, nothing after them. `git interpret-trailers --parse`
  must read every key. `Signed-off-by` (DCO) is required on every commit.
- **No changelog in the record**: the `changes/<name>.<category>.md`
  fragment the commit adds is the single source of truth for its changelog
  category and entry (see `changes/README.md`). The record repeats neither;
  the review page reads the fragment from the diff.
- **Required keys**: `Area`, `Output`, `Tests`. `Formatting`, `Formats`,
  `PPX` and `Editor` are required whenever the change touches the syntax
  library, the parsetree, the typed tree or Lambda. Omit the others when they
  do not apply.
- **Wording** follows the writing rules: present facts, positive statements,
  claims as strong as their evidence, every number labelled by what produced
  it. A JavaScript `Before`/`After` pair says where it came from in a
  `Source:` paragraph (compiler run, or a snapshot file in the diff).
- **Lists in trailers**: `Measured` separates items with `; `
  (outside parentheses); each item renders on its own line. `Output` and the
  other state trailers start with their state word (`unchanged`, `changed`,
  `none`), followed by the detail.

## What each field states

| Field | What the record states |
| --- | --- |
| `Area` | The layers the change touches. They decide who reviews it and which suites apply; a parsetree or typed-tree change also reaches editor analysis. |
| `Output` + `Before`/`After (js)` | The generated JavaScript for the example before and after, and which existing programs compile differently. `unchanged` holds when no committed generated JS changes, or when a rebuild of the test suites is byte-identical. |
| `Formatting` + `Before`/`After (rescript)` | Whether `rescript format` rewrites existing code, and whether formatted output parses back to the same tree with its comments. |
| `Formats` | The magic numbers bumped in `compiler/ext/config.ml`. Any change to the marshaled parsetree, typed tree (`.cmi`, `.cmt`) or `.cmj` layout, constructor order included, bumps the matching number. |
| `PPX` | The effect on the frozen v0 PPX wire: `parsetree0.ml` is unchanged, and a change to the v0 mapping comes with an ast-mapping fixture. |
| `Editor` | Completion, hover and inlay hints on new or changed nodes, and the analysis fixtures that cover them. |
| `Errors` | The `tests/ERROR_VARIANTS.md` rows added, removed or relinked for changed error variants or fixtures. |
| `Tests` | The tests added, changed or removed, each observing the effect the record claims, so that it fails without the change. `none, because …` gives the reason when no test changes. |
| `Measured` | Performance before and after, with the input and the command that produced each number. |
| `Breaking` | What existing code or tooling stops working, and the migration. |
| `Review` | Questions a reviewer raised or will raise, with their answers. |
| `PR` | The pull request; the renderer links it. |
| `Signed-off-by` | The DCO sign-off, with the commit author's name and email, as the last trailer. |

## Rendering

```sh
node scripts/commitRecords.js --out review.html <rev>...
node scripts/commitRecords.js --out review.html <base>..<branch>...
node scripts/commitRecords.js --fragment --out part.html <rev>...
node scripts/commitRecords.js --out review.html --pr 1234 --title "<PR title>" <base>..<branch>
```

- `--pr` and `--title` head the page with the pull request's number (linked)
  and title; without them the page is headed "Semantic Diff".

- A revision renders one card. A range renders a summary row, a changelog
  draft grouped by category, its commits as cards, and the files more than
  one commit (or more than one range) changes. The changelog category, the
  entries and the draft come from the `changes/` fragments in the diff: a
  card shows the fragments its commit adds or changes, and a range's draft
  shows the fragments the range leaves, in their last version.
- Each card checks the record against its diff: a `changes/` fragment that
  `yarn changelog:check` rejects (such as one still carrying the placeholder
  PR link), an edit to `CHANGELOG.md`, modified generated `.mjs`, `.cjs` and `.res.js` files under
  `Output: unchanged` (new ones from new tests are counted, not flagged), a
  magic number bumped in `compiler/ext/config.ml` that
  `Formats` does not name, a parsetree change with no ast-mapping fixture, a
  new `super_errors` fixture with no `ERROR_VARIANTS.md` change, a missing
  `Tests` trailer, `Tests: none` on a diff that changes tests. A failed
  check is shown on the card. The DCO app checks `Signed-off-by`.
- The page is one self-contained HTML file: no script, no external data
  (fonts fall back to system faces), light and dark themes, phone width.
  `--fragment` omits the document skeleton for embedding.
- A note on `refs/notes/records` replaces the commit message's record. Use it
  for commits that exist already (history, a squash merge) and for review
  outcomes added later; a new commit carries its record in its message.

## When to use

- Every commit an agent makes carries a record in its message.
- An agent's report lists its commits and the rendered page; the records are
  the description of the work. A pull request description is a rendering of
  its range.
- Before review or merge: render `master..<branch>` and hand over the page.
- On GitHub, `.github/workflows/semantic_diff.yml` renders each pull
  request's range when it is opened or pushed, and comments the page's link
  (`Semantic diff: https://cdn.rescript-lang.org/semantic-diff/pr-<N>/…`).
