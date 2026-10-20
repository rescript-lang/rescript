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

- The subject and `Signed-off-by` are required. The message ends with its
  trailers, git's `Key: value` lines (`Co-Authored-By:`, `Fixes:` or any
  other one-word key), `Signed-off-by` last; the page lists the
  `Co-Authored-By` names and draws no other trailer. Every section is
  optional.
- A section starts at a line `Why:`, `Review:`, `Breaking:`, an object
  block's heading, an `Example:` alone on its line, or a `Before (<form>):`
  or `After (<form>):` heading, and holds the text after its colon and the
  block indented by 4 spaces or more under it; a blank line ends a section
  that holds only text. `Review:` repeats, one question each. Every other
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
  both versions facing itself and the changed lines shaded; typing rules
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
- A form of another name is drawn as text.

## When to add a block

A commit gets one object block for each object it changes, with the
smallest example that shows the change. A refactor, a test, a script or a
review fix is a subject and a line or two of prose.

## Rendering

```sh
make semantic-diff     # master..HEAD to _build/semantic-diff.html, opened
node scripts/commitRecords.js --out page.html [--pr <number>] [--title <text>] \
  <rev | base..tip>...
```

- `--pr` and `--title` head the page with the pull request.
  `.github/workflows/semantic_diff.yml` renders each pull request's range
  and comments the page's link.
- In prose, `#1234` and full pull request and issue URLs link as `#1234`,
  and the 9-character short sha of a commit on the page links to its card.

## What the page shows

The page is the heading, then a card per commit. A card shows the commit's
short sha, author and date, the pull request a `(#1234)` at the end of the
subject names, and the message drawn: the subject, the prose, each object
block with its example and forms, the `Breaking:` sections as banners, the
`Review:` sections as a list, and the co-authors.
