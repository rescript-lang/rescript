// The missing case is shown as a range, not one case per character.
let f = (a, b) =>
  switch (a, b) {
  | ('a' .. 'z', 'a' .. 'z') => 1
  | (_, '\u{0}' .. '`') => 2
  }
