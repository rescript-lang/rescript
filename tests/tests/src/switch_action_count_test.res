// The integer switcher plans over the set of *distinct* actions it is handed:
// given enough of them across a dense range it emits a jump table, otherwise
// it tests intervals. That count depends on how far the term has been
// normalized when it arrives, because normalization merges arms that were
// written apart.
//
// Folding now happens at construction, so the switcher sees merged arms.
// These two cases are what that cost, and bought, when mk_builtin started
// folding: across the runtime, Belt and every other test module the output
// was unchanged, and only these two moved. Both plans are correct in each
// case; only the emitted code differs.
//
// They stay here because the same sensitivity applies to any future change in
// where normalization happens, and to the switcher's own thresholds - see the
// `dense` predicate in switch.ml, where `switch_min` is what refuses the jump
// table below.

// Improvement: `10 + 10` merges with the two `20` arms, so three actions
// become one, and four branches with `20` and `99` each duplicated collapse
// to two branches with neither duplicated.
let improves_when_merged = value =>
  switch value {
  | 1 => 10 + 10
  | 2 => 20
  | 3 => 20
  | _ => 99
  }

// Regression: the same merge drops the test count from three to two, below
// `switch_min`, so `dense` refuses the jump table and this becomes a chain of
// comparisons. The density check itself still passes; it is the minimum-tests
// floor that rejects it.
let effect = s => Console.log(s)

let regresses_when_merged = x =>
  switch x {
  | 1 => effect("d")
  | 2 => effect("d")
  | 3 =>
    if 3 > 2 {
      effect("d")
    } else {
      effect("z")
    }
  | 4 => effect("q")
  | 5 => effect("r")
  | _ => effect("s")
  }
