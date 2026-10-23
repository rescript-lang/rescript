// The beta reducer used to stack inlined-call argument bindings in reverse
// parameter order, so the last argument was evaluated first. The checked-in
// JS pins evaluation to source order: effA runs before effB.
let recorded: array<string> = []

let equalish = (a: array<int>, b: array<int>) => a == b
let copy = (a: array<int>) => Array.copy(a)
let helper = (x, y) => equalish(x, copy(y))

let rec effA = n => {
  recorded->Array.push("a")
  if n > 0 {
    effA(n - 1)
  } else {
    [n]
  }
}

let rec effB = n => {
  recorded->Array.push("b")
  if n > 0 {
    effB(n - 1)
  } else {
    [n]
  }
}

let _ = {
  helper(effA(0), effB(0))
}
