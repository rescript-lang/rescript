@schema
type t = {foo: string}

let foo = S.parseOrThrow(`{ "foo": "bar" }`, ~to=schema)

Console.log(foo)
