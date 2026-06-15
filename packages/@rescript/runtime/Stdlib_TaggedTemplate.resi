/***
Utilities for working with JavaScript [tagged templates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates).

A value of type `TaggedTemplate.t<'param, 'output>` can be used directly with
backtick syntax, and the compiler emits a real JavaScript tagged template
literal at every call site — across module boundaries, when stored in a value,
or when passed as a function argument:

```rescript
@module("./sql_client.js")
external sql: TaggedTemplate.t<'a, promise<queryResult>> = "sql"

let users = await sql`SELECT * FROM users WHERE id = ${userId}`
```
*/

/**
Type of a tagged template tag. `'param` is the type accepted in `${...}`
interpolations, `'output` is the type produced by the tag.
*/
type t<'param, 'output> = taggedTemplate<'param, 'output>

/**
`make(tag)` lifts a plain ReScript function of the tag-function shape
`(array<string>, array<'param>) => 'output` into a `t<'param, 'output>`, so it
can itself be used with tagged-template backtick syntax — emitting real
JavaScript tagged-template syntax at every call site.

## Examples

```rescript
type params = I(int) | S(string)

let s = TaggedTemplate.make((strings, parameters) => {
  Array.reduceWithIndex(parameters, Array.getUnsafe(strings, 0), (acc, param, i) => {
    let suffix = Array.getUnsafe(strings, i + 1)
    let p = switch param {
    | I(i) => Int.toString(i)
    | S(s) => s
    }
    acc ++ p ++ suffix
  })
})

let greeting = s`hello ${S("Ada")} you're ${I(36)} years old!`
```
*/
let make: ((array<string>, array<'param>) => 'output) => t<'param, 'output>
