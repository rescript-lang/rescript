// External declarations keep their primitive string and FFI attributes as
// written through the v0 compatibility AST; the type checker resolves them.
external identity: 'a => 'a = "%identity"
@val external parseInt: string => int = "parseInt"
@send external join: (array<string>, string) => string = "join"
@module("./local") external local: int => int = "local"
@scope("Math") @val external max: (float, float) => float = "max"
@obj external makeProps: (~name: string, ~age: int=?, unit) => _ = ""
@send
external on: (t, @as("click") _, unit => unit) => unit = "addEventListener"
@variadic @module("path") external join2: array<string> => string = "join"
