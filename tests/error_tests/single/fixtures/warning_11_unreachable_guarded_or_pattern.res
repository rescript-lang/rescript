// Each guarded or-pattern is unreachable after a case that matches
// everything. Compiling them used to fail with
// "Fatal error: Matching.comp_exit" instead of reporting them as unused.
let g = () => true

let int = x =>
  switch x {
  | _ => 2
  | 1 | 2 if g() => 3
  }

let char = c =>
  switch c {
  | _ => "any"
  | 'a' | 'b' if g() => "ab"
  }

let string = s =>
  switch s {
  | _ => "any"
  | "a" | "b" if g() => "ab"
  }

let pair = p =>
  switch p {
  | (0, _) => "zero"
  | (_, _) => "other"
  | (1, 1) | (2, 2) if g() => "same"
  }
