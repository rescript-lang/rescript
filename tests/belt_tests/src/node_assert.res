@module("node:assert") external ok: (bool, ~message: string=?) => unit = "ok"
@module("node:assert")
external deepEqual: ('a, 'a, ~message: string=?) => unit = "deepStrictEqual"
@module("node:assert")
external throws: (unit => 'a, ~error: 'b=?, ~message: string=?) => unit = "throws"
