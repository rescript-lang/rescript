// Attribute payloads may use explicit braces around the value.
@module({"react"})
external useState: int => int = "useState"

@val({"Math.abs"})
external abs: int => int = "abs"

@scope({("Math", "random")}) @val
external nested: unit => float = "random"

type choice = | @as(1) First

let absolute = abs(-2)
let first = First
