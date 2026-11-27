let consume = (_f: unit => Iterable.t<int>) => ()
let make: unit => Generator.t<int, unit, unit> = %raw(`function* () { yield 1 }`)
let () = consume(make)
