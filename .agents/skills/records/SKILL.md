---
name: records
description: Commit records and Semantic Diff pages for the ReScript compiler repository. Use when committing a change, when an agent reports its work, and when someone asks to review a commit, a pull request or a branch. A commit message describes what the commit changes in the form suited to it (trees, code, diagnostics, typing rules, clause matrices, representations) Before and After; scripts/commitRecords.js renders the messages as a Semantic Diff page, one card per commit drawing its message.
---

# Commit records

A record is a commit message that describes what the commit changes, each
change in the form suited to what it changes: a parse tree or typed tree as
the tree before and after, Lambda and generated JavaScript as code before
and after, a diagnostic as its text, a typing rule as an inference rule,
the match compiler's split as a clause matrix, a runtime representation as
a mapping. `scripts/commitRecords.js` renders records as a Semantic Diff
page, one card per commit, drawing each form in its own way, Before and
After side by side.

## The message

```
<subject: what the compiler or tool does, present tense>

Why:
    <the problem the change solves>

AST modified (raw extensions):
Example:
    let f = %raw(`(a) => a`)
Before (parse):
    structure_item
      Pstr_value Nonrec
        <def>
          pattern
            Ppat_var "f"
          expression
            Pexp_extension "raw"
After (parse):
    structure_item
      Pstr_value Nonrec
        <def>
          pattern
            Ppat_var "f"
          expression
            Pexp_raw "raw"
            Raw_template "(a) => a"

Review: <a question a reviewer asks, and its answer>
Breaking: <what existing code or tooling stops working, and the migration>

Signed-off-by: Name <email>
```

A commit without object blocks, such as a script change, is its subject
and a line or two of prose:

```
Draw handles starting with a digit as code in the comment

GitHub usernames may start with a digit, so `@0x` in a commit message
would have mentioned a user; the comment now draws it as code as well.

Signed-off-by: Name <email>
```

- The subject, a description and `Signed-off-by` are required: the
  description says why the commit is made, as a line or two of prose or
  a `Why:` section, and object blocks then show what it changes. Every
  other section is optional. The message ends with its trailers, git's
  `Key: value` lines (`Co-Authored-By:`, `Fixes:` or any other one-word
  key), `Signed-off-by` last; the page lists the `Co-Authored-By` names
  and draws no other trailer.
- A section starts at a line `Why:`, `Review:` or `Breaking:` that no
  paragraph of prose runs onto (a blank line or another section above it),
  an object block's heading, an `Example:` alone on its line, or a
  `Before (<form>):` or `After (<form>):` heading, and holds the text after
  its colon and the block indented by 4 spaces or more under it; a blank
  line ends a section that holds only text. `Review:` repeats, one question each. Every other
  line is prose, and a block indented by 4 spaces outside a section is
  ReScript code. Tabs count as 4 columns.
- An **object block** is a heading `<Kind> added|modified|removed (<object>):`,
  the kind one capitalised word naming the object, then an optional
  `Example:` with a ReScript program indented by 4 spaces, then the
  object's forms, each version a `Before (<form>):` or `After (<form>):`
  heading with its content indented by 4 spaces. Text after the colon of a
  `Before`/`After` heading captions the block under it, or is the version
  itself when no block follows (`After (js): return x;`). A heading that
  leaves out the object's parentheses is drawn as its action and kind.
- A block draws the versions it has: a form with both versions side by
  side, or once, with the captions, when the two are equal; a form with
  one version across the block under its version's label. Trees, code,
  text, matrices and mappings are drawn on aligned rows, a line common to
  both versions facing itself and the changed lines shaded; reanalyze
  reports are drawn as rows of findings, aligned the same way; typing rules
  and rewrites are drawn as two figures side by side, the changed parts
  shaded: premises, conclusions, rule names and formulas, and each term of
  a rewrite against the term at its position.
- The changelog entry is the `changes/<name>.<category>.md` fragment the
  commit adds (`changes/README.md`).

## Kinds

The table lists the common kinds.

| Kind | Object |
| --- | --- |
| `AST` | a parsetree node |
| `Typedtree` | a typed tree node |
| `Typing` | a typing rule |
| `Matching` | the pattern match compiler's split |
| `Lambda` | a Lambda term or pass |
| `JS` | generated JavaScript |
| `Diagnostic` | an error or warning |
| `Format` | the formatter's output |
| `Representation` | how a value is represented in JavaScript |
| `Reanalyze` | reanalyze's findings for a program |

## Forms

| Form | Content | Drawn as |
| --- | --- | --- |
| `parse` | the parse tree | tree |
| `expanded` | the parse tree after the built-in PPX | tree |
| `typed` | the typed tree | tree |
| `lambda` | Lambda | highlighted code |
| `js` | the generated JavaScript | highlighted code |
| `rescript` | ReScript source, such as the formatter's output | highlighted code |
| `diagnostic` | what the compiler reports | text |
| `text` | any text | text |
| `rule` | inference rules and formulas | inference rule |
| `matrix` | a clause matrix | matrix |
| `representation` | how ReScript values map to JavaScript | mapping |
| `reanalyze` | the report exactly as reanalyze prints it | findings |

- A form holds a representative selection of the output, enough to see
  what changes and what stays; an example shows only the cases that
  matter. A line `…`, alone or followed by text such as
  `… 4 more like this`, marks the lines left out and is drawn muted.
- The forms are the output for the example as shown, so the locations and
  names in them point into the example. Forms taken from a larger file,
  such as a test fixture, name that file in a caption, for example
  `Before (reanalyze): tests/…/Division.res`.
- A tree is indented lines, two spaces a level, as the compiler prints it:
  `bsc -bs-no-builtin-ppx -only-parse -dparsetree file.res` prints the
  parse tree, `bsc -only-parse -dparsetree` the expanded one, and
  `bsc -dtypedtree` the typed tree.
- Code is highlighted in its language; `bsc -drawlambda` prints the Lambda.
  Code can be a rewrite: each `⇝` separates the code before it from the
  code after it (`lhs ⇝ rhs` or a chain `a ⇝ b ⇝ c` on one line, or `⇝`
  alone on a line), and `label: formula` lines after the last term are its
  side conditions.
- A `rule` group is premises, a bar of `─` with the rule's name in
  parentheses, and the conclusion; any other group is formulas, one a
  line. Groups are separated by blank lines, side-by-side formulas by three
  or more spaces. One-letter names, with their primes, are italic
  variables, `_x`, `_{…}` and `^{…}` sub- and superscripts, and `code` in
  backticks is ReScript.
- A `matrix` is one row a line, columns three or more spaces apart, each
  cell a pattern; `label: text` lines are notes under it.
- A `representation` is `ReScript ↦ JavaScript` lines; `label: text` lines
  are notes among them.
- A `reanalyze` report is findings separated by blank lines, each an
  analysis line, a location line and the message. A report shows, for
  example, one finding that disappears, one that stays and the closing
  line with the totals, and a paragraph `…` stands for the findings left
  out. Each finding is a row: the analysis tagged, the location, the
  message; an analysis that every finding names tags the form once.
  Before and After align findings by their text: a run of findings in
  both reports is one row that opens to show them, a finding one report
  lacks faces a gap, and a note that changes, such as the closing count,
  faces its other version with the differing words marked. In a built
  project such as
  `tests/analysis_tests/tests-reanalyze/deadcode` (`yarn build` there),
  `dune exec rescript-tools -- reanalyze -exception -ci -suppress src -unsuppress src/<dir>`
  prints the exception analysis's report for the files under `src/<dir>`;
  `-dce` and `-termination` select the other analyses.
- A form of another name is drawn as text.

## What a commit holds

Every commit says why it is made. A commit that changes what the compiler
or a tool does shows each representation, rule, transformation or decision
it changes, Before and After, at the level where it operates, then the
representative consequences that show why it matters: the blocks go from
the changed operation to what it affects, and one block serves both when
the operation directly changes the output shown. Each change the prose
names, such as an attribute the compiler adds or a case a decision takes,
is also shown in a block. Each block has the smallest example that shows
its change.

- A printed form shows the change when one exposes it. Otherwise a block
  shows a schematic, captioned with the function that implements it: an
  inference rule for a changed typing judgment, a clause matrix and its
  split for the match compiler, a mapping from inputs to results for a
  decision, keeping the premises, priorities and side conditions that
  matter.
- Printing each form for the example before and after the commit finds
  the layers it changes; the record keeps the excerpts that show the
  changed operation and its distinct consequences.
- When an error stops later forms from being printed, the internal forms
  come from a related example that compiles, the diagnostic from one that
  fails, and the prose says how they relate.

A commit whose changes are only organization, tests or documentation
needs no block, only its subject and a line or two of prose; a script or
a review fix that changes behaviour gets blocks like any other. A review
fix names the finding it answers.

## Rendering

```sh
make semantic-diff     # master..HEAD to _build/semantic-diff.html, opened
node scripts/commitRecords.js --out page.html [--pr <number>] [--title <text>] \
  <rev | base..tip>...
```

- `--pr` and `--title` head the page with the pull request.
  `.github/workflows/semantic_diff.yml` renders each pull request's range
  and comments the records as Markdown (`--markdown <file> --url <page>`,
  scripts/recordMarkdown.js) with the page's link, one comment that each
  push updates to hold every commit, marking those the push added or
  edited (`--previous <previous head>`). The comment first lists, under
  its own heading, the changelog entries the pull request adds, which
  describe it as a whole, while a `Breaking:` section describes its
  commit; it notes breaking commits without a breaking entry, and the
  other way round.
- In prose, `#1234` and full pull request and issue URLs link as `#1234`,
  and the 9-character short sha of a commit on the page links to its card.

## What the page shows

The page is the heading, then a card per commit. A card shows the commit's
short sha, author (in a pull request's page and comment, the GitHub login
linking the profile) and co-authors (an AI assistant by its short name,
such as Claude), date, the pull request a `(#1234)` at the end of the subject
names, and the message drawn: the subject, the prose, each object block
with its example and forms, the `Breaking:` sections as banners, and the
`Review:` sections as a list.
