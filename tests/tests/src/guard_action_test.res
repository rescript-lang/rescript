// A guard is kept apart from its body until the match compiler knows what it
// falls through to. Two things must hold.

// 1. Folding must not erase the fact that a case is guarded: a guard that
//    folds to false is not a missing guard.
let constant_guard = switch true {
| true if false => "wrong"
| _ => "right"
}

// 2. A guarded case and a case whose body happens to be the same conditional
//    are different actions. Comparing them through a stand-in fallthrough must
//    not equate them, or one evaluation of the guard is lost.
type value = A | B | C

let calls = ref(0)

let guard = () => {
  calls := calls.contents + 1
  false
}

let both_guards_run = () => {
  calls := 0
  switch B {
  | A => ()
  | _ if guard() => ()
  | B =>
    if guard() {
      ()
    } else {
      ()
    }
  | _ => ()
  }
  calls.contents
}
