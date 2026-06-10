// Using backtick tagged-template syntax on a value that is not a
// `taggedTemplate` (here a plain function with the old tag-function shape)
// is now an error that points to the migration path.
let foo = (strings: array<string>, values: array<int>) => {
  ignore(strings)
  ignore(values)
  "result"
}

let res = foo`| 5 × 10 = ${5} |`
