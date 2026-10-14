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

Changelog entry: <the exact bullet of the changes/<name>.<category>.md fragment>

Review: <a question a reviewer raised or will raise, and its answer; repeat>

Area: syntax printer frontend typechecker lambda codegen runtime rewatch
 analysis reanalyze gentype tools playground scripts ci docs tests
Changelog: breaking | feature | fix | docs | polish | internal | none
Output: unchanged | changed <which programs>, <evidence>
Formatting: unchanged | changed <which source shapes>
Formats: none | <bumped magic numbers: ast cmi cmt cmj>
PPX: unchanged | <effect on the frozen v0 PPX wire>
Editor: unchanged | <effect on completion, hover, hints; fixtures>
Errors: <ERROR_VARIANTS.md rows added, removed or relinked>
Tests: +added/path ~updated/path -removed/path
Ran: <command and result>; <command and result>; ...
Measured: <quantity before -> after, and what produced it>; ...
Depends: <PRs or commits this one stacks on or conflicts with>
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
- **Required keys**: `Area`, `Changelog`, `Output`. `Formatting`, `Formats`,
  `PPX` and `Editor` are required whenever the change touches the syntax
  library, the parsetree, the typed tree or Lambda. Omit the others when they
  do not apply.
- **Wording** follows the writing rules: present facts, positive statements,
  claims as strong as their evidence, every number labelled by what produced
  it. A JavaScript `Before`/`After` pair says where it came from in a
  `Source:` paragraph (compiler run, or a snapshot file in the diff).
- **Lists in trailers**: `Ran` and `Measured` separate items with `; `
  (outside parentheses); each item renders on its own line. `Output` and the
  other state trailers start with their state word (`unchanged`, `changed`,
  `none`), followed by the detail.

## Why these fields

Each field answers a question that recurs in the review threads of the
upstream repository (62 merged pull requests between #8606 and #8857, human
and automated review comments).

| Field | The question it answers | Where reviewers asked it |
| --- | --- | --- |
| `Area` | Which layer changed, so who reviews it and which suites apply | AGENTS.md routes by layer; AST changes broke editor analysis in #8606, #8619, #8633, #8746 |
| `Changelog` + `Changelog entry` | Is it user-facing, which section, is the link right, is it short | missing in #8611, #8667, #8691, #8692, #8693, #8694; missing or wrong link in #8627, #8631, #8633, #8634, #8662; too long in #8608; breaking or internal in #8606, #8682 |
| `Output` + `Before`/`After (js)` | What the generated JavaScript is now, and whether existing programs change | differential JS rebuild in #8606; byte-identical claim in #8769, #8773; JS shown in #8801; invalid to valid JS in #8692 |
| `Formatting` + `Before`/`After (rescript)` | Does `rescript format` rewrite existing code, and does it round-trip | churn in production code asked in #8611; round-trip and comment loss in #8614, #8627, #8674, #8677 |
| `Formats` | Does a marshaled artifact change without a magic-number bump | CMT bump asked in #8682; CMI/CMT/CMJ in #8606; Lambda tag order for `.cmj` in #8746 |
| `PPX` | Does the frozen v0 wire still carry the same tree to external PPXs | `parsetree0.ml` touched in #8619; identity PPX no longer a no-op and missing ast-mapping fixture in #8606; PPX output rejected in #8682 |
| `Editor` | Do completion, hover and inlay hints still work on the new nodes | #8606, #8619, #8746 |
| `Errors` | Is `tests/ERROR_VARIANTS.md` in step with the fixtures | #8746 |
| `Tests` | Which tests pin the change, and do they observe the claimed effect | missing coverage in #8667, #8771, #8773; assertion too weak in #8667; widths in #8677 |
| `Ran` | Which suites ran | PR test plans (#8692) and replies ("syntax and roundtrip suites pass", #8674) |
| `Measured` | Performance before and after, on what input | #8769, #8771, #8773, #8801 |
| `Depends` | Stacking and conflicts with open PRs | #8606 (conflict with #8608), #8771, #8773 (stacked) |
| `PR` | The discussion behind the change | the renderer links it |
| `Signed-off-by` | DCO | the most frequent automated finding: #8691, #8729, #8773, #8837, #8842, #8846, #8857 |
| `Breaking` | What stops working and how to migrate | #8606, #8634, #8682 |
| `Review` | Answers given in the thread, kept with the change | #8617, #8667, #8682 |

## Rendering

```sh
node scripts/commitRecords.js --out review.html <rev>...
node scripts/commitRecords.js --out review.html <base>..<branch>...
node scripts/commitRecords.js --fragment --out part.html <rev>...
```

- A revision renders one card. A range renders a summary row, a changelog
  draft grouped by category, its commits as cards, and the files more than
  one commit (or more than one range) changes.
- Each card checks the record against its diff: a changelog category with no
  new `changes/` fragment (or a `Changelog entry` that differs from the
  fragment's bullet), modified generated `.mjs`/`.cjs` files under
  `Output: unchanged` (new ones from new tests are counted, not flagged), a magic number bumped in `compiler/ext/config.ml` that
  `Formats` does not name, a parsetree change with no ast-mapping fixture, a
  new `super_errors` fixture with no `ERROR_VARIANTS.md` change. A failed
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
