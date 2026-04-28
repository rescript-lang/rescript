let s = "some unicode é £ "
let s = switch foo {
  | `bar` => "bar"
  | "foo" => "foo"
  | _ => "baz"
}
let s = `你好，
世界`
let s = `"`
let s = `foo`
let s = `foo ${bar} baz`
let s = `some unicode é ${bar} £ `
let s = x`foo`
let s = x`foo ${bar} baz`
let s = x`some unicode é ${bar} £ `