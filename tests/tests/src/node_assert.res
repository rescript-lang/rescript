@module("node:assert") external ok: (bool, ~message: string=?) => unit = "ok"
@module("node:assert") external equal: ('a, 'a, ~message: string=?) => unit = "strictEqual"
@module("node:assert") external deepEqual: ('a, 'a, ~message: string=?) => unit = "deepStrictEqual"
@module("node:assert")
external notDeepEqual: ('a, 'a, ~message: string=?) => unit = "notDeepStrictEqual"
@module("node:assert") external fail: (~message: string=?) => unit = "fail"
@module("node:assert")
external throws: (unit => unit, ~error: 'a=?, ~message: string=?) => unit = "throws"
