// Inline refinement runs only inside consecutive Removed/Added line blocks.
// For each candidate line pair, an LCS of non-whitespace Lambda tokens marks
// the shared tokens. Pairing requires shared atoms (not just punctuation) to
// cover at least half the atom count of the larger line. A second dynamic
// programming table chooses the highest-scoring order-preserving line pairs,
// allowing unmatched lines on either side. Unmatched tokens in chosen pairs
// receive stronger shading; whitespace and the original text stay intact.
// Oversized or dissimilar pairs keep whole-line highlighting. Token comparison
// cells share a budget across the entire dump, and block size caps line-pairing
// work, so this refinement cannot introduce unbounded quadratic comparisons.
type part = {text: string, changed: bool}
type highlights = {before: Dict.t<array<part>>, after: Dict.t<array<part>>}
type tokens = {all: array<string>, significant: array<string>, atoms: int}
type pair = {before: array<part>, after: array<part>, score: int}

// One budget for the entire dump, not a fresh quadratic budget per line.
let maxCells = 250000
let maxBlockLines = 32
let maxLineCharacters = 4096
let maxTokens = 128

let isSpace = text => String.trim(text) === ""
let isAtom = text => !isSpace(text) && !RegExp.test(/^[()[\]{},;]+$/, text)

let tokenize = text => {
  if String.length(text) > maxLineCharacters {
    None
  } else {
    // Keep stamped identifiers and operators intact. Quoted constants include
    // escapes; whitespace is retained verbatim but does not affect alignment.
    let all =
      text
      ->String.match(/"(?:\\.|[^"\\])*"|\s+|[()[\]{},;]|[^\s()[\]{},;"]+|"/g)
      ->Option.getOr([])
      ->Array.keepSome
    let significant = all->Array.filter(token => !isSpace(token))
    if Array.length(significant) > maxTokens {
      None
    } else {
      Some({all, significant, atoms: significant->Array.filter(isAtom)->Array.length})
    }
  }
}

let parts = (tokens, matched) => {
  let index = ref(0)
  tokens.all->Array.map(text => {
    let changed = if isSpace(text) {
      false
    } else {
      let changed = !(matched->Array.getUnsafe(index.contents))
      index := index.contents + 1
      changed
    }
    {text, changed}
  })
}

let compareTokens = (left, right, budget) => {
  let n = Array.length(left.significant)
  let m = Array.length(right.significant)
  let stride = m + 1
  let cells = (n + 1) * stride
  if left.atoms === 0 || right.atoms === 0 || cells > budget.contents {
    None
  } else {
    budget := budget.contents - cells
    let table = Array.make(~length=cells, 0)
    let i = ref(n - 1)
    while i.contents >= 0 {
      let j = ref(m - 1)
      while j.contents >= 0 {
        let cell = i.contents * stride + j.contents
        table[cell] = if (
          left.significant->Array.getUnsafe(i.contents) ===
            right.significant->Array.getUnsafe(j.contents)
        ) {
          1 + table->Array.getUnsafe(cell + stride + 1)
        } else {
          Math.Int.max(table->Array.getUnsafe(cell + stride), table->Array.getUnsafe(cell + 1))
        }
        j := j.contents - 1
      }
      i := i.contents - 1
    }
    let leftMatched = Array.make(~length=n, false)
    let rightMatched = Array.make(~length=m, false)
    let shared = ref(0)
    let x = ref(0)
    let y = ref(0)
    while x.contents < n && y.contents < m {
      let token = left.significant->Array.getUnsafe(x.contents)
      if token === right.significant->Array.getUnsafe(y.contents) {
        leftMatched[x.contents] = true
        rightMatched[y.contents] = true
        if isAtom(token) {
          shared := shared.contents + 1
        }
        x := x.contents + 1
        y := y.contents + 1
      } else if (
        table->Array.getUnsafe((x.contents + 1) * stride + y.contents) >=
          table->Array.getUnsafe(x.contents * stride + y.contents + 1)
      ) {
        x := x.contents + 1
      } else {
        y := y.contents + 1
      }
    }
    // Parentheses/indentation alone must not make unrelated expressions a pair.
    let largest = Math.Int.max(left.atoms, right.atoms)
    if shared.contents === 0 || shared.contents * 2 < largest {
      None
    } else {
      Some({
        before: parts(left, leftMatched),
        after: parts(right, rightMatched),
        score: shared.contents * 100 / largest,
      })
    }
  }
}

let highlight = (lines: array<LambdaDiff.line>): highlights => {
  let result = {before: Dict.make(), after: Dict.make()}
  let budget = ref(maxCells)
  let index = ref(0)
  while index.contents < Array.length(lines) {
    if (lines->Array.getUnsafe(index.contents)).kind === Same {
      index := index.contents + 1
    } else {
      let start = index.contents
      while (
        index.contents < Array.length(lines) &&
          (lines->Array.getUnsafe(index.contents)).kind !== Same
      ) {
        index := index.contents + 1
      }
      if index.contents - start <= maxBlockLines && budget.contents > 0 {
        let block = Array.slice(lines, ~start, ~end=index.contents)
        let before = block->Array.filter(line => line.kind === Removed)
        let after = block->Array.filter(line => line.kind === Added)
        let n = Array.length(before)
        let m = Array.length(after)
        if n > 0 && m > 0 {
          let left = before->Array.map(line => tokenize(line.text))
          let right = after->Array.map(line => tokenize(line.text))
          let candidates = Array.make(~length=n * m, None)
          for i in 0 to n - 1 {
            for j in 0 to m - 1 {
              candidates[
                i * m + j
              ] = switch (left->Array.getUnsafe(i), right->Array.getUnsafe(j)) {
              | (Some(left), Some(right)) => compareTokens(left, right, budget)
              | _ => None
              }
            }
          }
          // Best ordered pairing, allowing either side to skip whole lines.
          let stride = m + 1
          let scores = Array.make(~length=(n + 1) * stride, 0)
          let i = ref(n - 1)
          while i.contents >= 0 {
            let j = ref(m - 1)
            while j.contents >= 0 {
              let cell = i.contents * stride + j.contents
              let skip = Math.Int.max(
                scores->Array.getUnsafe(cell + stride),
                scores->Array.getUnsafe(cell + 1),
              )
              scores[cell] = switch candidates->Array.getUnsafe(i.contents * m + j.contents) {
              | Some(pair) =>
                Math.Int.max(skip, pair.score + scores->Array.getUnsafe(cell + stride + 1))
              | None => skip
              }
              j := j.contents - 1
            }
            i := i.contents - 1
          }
          let x = ref(0)
          let y = ref(0)
          while x.contents < n && y.contents < m {
            let cell = x.contents * stride + y.contents
            switch candidates->Array.getUnsafe(x.contents * m + y.contents) {
            | Some(pair)
              if pair.score + scores->Array.getUnsafe(cell + stride + 1) ===
                scores->Array.getUnsafe(cell) =>
              switch (
                (before->Array.getUnsafe(x.contents)).before,
                (after->Array.getUnsafe(y.contents)).after,
              ) {
              | (Some(before), Some(after)) =>
                Dict.set(result.before, Int.toString(before), pair.before)
                Dict.set(result.after, Int.toString(after), pair.after)
              | _ => ()
              }
              x := x.contents + 1
              y := y.contents + 1
            | _ =>
              if scores->Array.getUnsafe(cell + stride) >= scores->Array.getUnsafe(cell + 1) {
                x := x.contents + 1
              } else {
                y := y.contents + 1
              }
            }
          }
        }
      }
    }
  }
  result
}

let forLine = (highlights: highlights, line: LambdaDiff.line) =>
  switch line.kind {
  | Same => None
  | Removed =>
    line.before->Option.flatMap(number => Dict.get(highlights.before, Int.toString(number)))
  | Added => line.after->Option.flatMap(number => Dict.get(highlights.after, Int.toString(number)))
  }
