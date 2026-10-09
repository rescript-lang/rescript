type kind = Same | Removed | Added
type line = {kind: kind, text: string, before: option<int>, after: option<int>}
type result = Identical | TooLarge | Changes(array<line>)

// Bound both comparison work and output size. Compiler dumps can greatly
// exceed the source size; never allocate an unbounded quadratic table.
let maxCells = 1000000
let maxCharacters = 1000000
let maxLines = 20000

// Line diff: strip identical prefixes/suffixes, then find the longest common
// subsequence (LCS) of exact lines in the remaining middle. Each table cell
// stores how many lines can still match from that pair of positions: equal
// lines take 1 + the diagonal cell; otherwise take the larger of skipping a
// line on either side. Walking the table emits Same/Removed/Added lines with
// their original line numbers (ties prefer removal for deterministic output).
// The middle costs O(rows * columns) time and space, so the limits above return
// TooLarge before allocating an excessive table. LambdaInlineDiff separately
// refines changed blocks; it never changes this line alignment or source text.
let compare = (before, after) => {
  if before === after {
    Identical
  } else if String.length(before) + String.length(after) > maxCharacters {
    TooLarge
  } else {
    let left = before->String.split("\n")
    let right = after->String.split("\n")
    let n = Array.length(left)
    let m = Array.length(right)
    let prefix = ref(0)
    while (
      prefix.contents < n &&
      prefix.contents < m &&
      left->Array.getUnsafe(prefix.contents) === right->Array.getUnsafe(prefix.contents)
    ) {
      prefix := prefix.contents + 1
    }
    let suffix = ref(0)
    while (
      suffix.contents < n - prefix.contents &&
      suffix.contents < m - prefix.contents &&
      left->Array.getUnsafe(n - suffix.contents - 1) ===
        right->Array.getUnsafe(m - suffix.contents - 1)
    ) {
      suffix := suffix.contents + 1
    }
    let rows = n - prefix.contents - suffix.contents
    let columns = m - prefix.contents - suffix.contents
    if n + m > maxLines || (rows + 1) * (columns + 1) > maxCells {
      TooLarge
    } else {
      // Longest common subsequence on the changed middle only. Keeping the
      // exact printer text avoids hiding meaningful identifier changes.
      let stride = columns + 1
      let table = Array.make(~length=(rows + 1) * stride, 0)
      let i = ref(rows - 1)
      while i.contents >= 0 {
        let j = ref(columns - 1)
        while j.contents >= 0 {
          let cell = i.contents * stride + j.contents
          table[cell] = if (
            left->Array.getUnsafe(prefix.contents + i.contents) ===
              right->Array.getUnsafe(prefix.contents + j.contents)
          ) {
            1 + table->Array.getUnsafe(cell + stride + 1)
          } else {
            Math.Int.max(table->Array.getUnsafe(cell + stride), table->Array.getUnsafe(cell + 1))
          }
          j := j.contents - 1
        }
        i := i.contents - 1
      }
      let lines: array<line> = []
      let x = ref(0)
      let y = ref(0)
      let same = () => {
        lines->Array.push({
          kind: Same,
          text: left->Array.getUnsafe(x.contents),
          before: Some(x.contents + 1),
          after: Some(y.contents + 1),
        })
        x := x.contents + 1
        y := y.contents + 1
      }
      while x.contents < prefix.contents {
        same()
      }
      while x.contents < n - suffix.contents || y.contents < m - suffix.contents {
        if (
          x.contents < n - suffix.contents &&
          y.contents < m - suffix.contents &&
          left->Array.getUnsafe(x.contents) === right->Array.getUnsafe(y.contents)
        ) {
          same()
        } else if (
          x.contents < n - suffix.contents &&
            (y.contents >= m - suffix.contents ||
              table->Array.getUnsafe(
                (x.contents - prefix.contents + 1) * stride + y.contents - prefix.contents,
              ) >=
                table->Array.getUnsafe(
                  (x.contents - prefix.contents) * stride + y.contents - prefix.contents + 1,
                ))
        ) {
          lines->Array.push({
            kind: Removed,
            text: left->Array.getUnsafe(x.contents),
            before: Some(x.contents + 1),
            after: None,
          })
          x := x.contents + 1
        } else {
          lines->Array.push({
            kind: Added,
            text: right->Array.getUnsafe(y.contents),
            before: None,
            after: Some(y.contents + 1),
          })
          y := y.contents + 1
        }
      }
      while x.contents < n {
        same()
      }
      Changes(lines)
    }
  }
}

type section = Visible(array<line>) | Collapsed(array<line>)

let sections = lines => {
  let result = []
  let index = ref(0)
  let length = Array.length(lines)
  while index.contents < length {
    let start = index.contents
    if (lines->Array.getUnsafe(start)).kind !== Same {
      while index.contents < length && (lines->Array.getUnsafe(index.contents)).kind !== Same {
        index := index.contents + 1
      }
      result->Array.push(Visible(Array.slice(lines, ~start, ~end=index.contents)))
    } else {
      while index.contents < length && (lines->Array.getUnsafe(index.contents)).kind === Same {
        index := index.contents + 1
      }
      let end_ = index.contents
      let head = start === 0 ? start : Math.Int.min(start + 3, end_)
      let tail = end_ === length ? end_ : Math.Int.max(head, end_ - 3)
      if tail - head <= 3 {
        result->Array.push(Visible(Array.slice(lines, ~start, ~end=end_)))
      } else {
        result->Array.push(Visible(Array.slice(lines, ~start, ~end=head)))
        result->Array.push(Collapsed(Array.slice(lines, ~start=head, ~end=tail)))
        result->Array.push(Visible(Array.slice(lines, ~start=tail, ~end=end_)))
      }
    }
  }
  result
}
