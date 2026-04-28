@schema
type t = {foo: string}

let foo = S.parseOrThrow(`{ "foo": "bar" }`, schema)

Console.log(foo)