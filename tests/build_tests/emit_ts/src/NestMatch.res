// Nested options unwrapped, which the primitive bound on Primitive_option
// refuses.

let getOr = (o: option<'a>, d: 'a) =>
  switch o {
  | Some(v) => v
  | None => d
  }

// The generic getOr at 'a = option<int>.
let useNested = (o: option<option<int>>) => getOr(o, Some(3))

// The same call on constants, which the optimizer folds to None.
let useNestedFolded = () => getOr(Some(None), Some(3))

// A nested match.
let inner = (o: option<option<int>>) =>
  switch o {
  | Some(Some(n)) => n
  | Some(None) => -1
  | None => 0
  }
