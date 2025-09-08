let v = (true, Some(false), (true, true))

let _ = switch v {
| (true, _, _) => 1
| _ => 2
}

// switch v {
//           ^com

// switch v { | }
//             ^com

// switch v { | (t, _) }
//                ^com

// switch v { | (_, _, (f, _)) }
//                       ^com

let x = true

// switch x { |
//             ^com

// switch x { | t
//               ^com

type nestedRecord = {nested: bool}

type rec someRecord = {
  first: int,
  second: (bool, option<someRecord>),
  optThird: option<[#first | #second(someRecord)]>,
  nest: nestedRecord,
}

let f: someRecord = {
  first: 123,
  second: (true, None),
  optThird: None,
  nest: {nested: true},
}

let z = (f, true)
ignore(z)

// switch f { | }
//             ^com

// switch f { | {}}
//               ^com

// switch f { | {first,  , second }}
//                      ^com

// switch f { | {fi}}
//                 ^com

// switch z { | ({o}, _)}
//                 ^com

// switch f { | {nest: }}
//                    ^com

// switch f { | {nest: {}}}
//                      ^com

let _ = switch f {
| {first: 123, nest} =>
  ()
  // switch nest { | {}}
  //                  ^com
  nest.nested
| _ => false
}

// let {} = f
//      ^com

// let {nest: {n}}} = f
//              ^com

type someVariant = One | Two(bool) | Three(someRecord, bool)

let z = Two(true)
ignore(z)

// switch z { | Two()}
//                  ^com

// switch z { | Two(t)}
//                   ^com

// switch z { | Three({})}
//                     ^com

// switch z { | Three({}, t)}
//                         ^com

type somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]
let b: somePolyVariant = #two(true)
ignore(b)

// switch b { | #two()}
//                   ^com

// switch b { | #two(t)}
//                    ^com

// switch b { | #three({})}
//                      ^com

// switch b { | #three({}, t)}
//                          ^com

let c: array<bool> = []
ignore(c)

// switch c { | }
//             ^com

// switch c { | [] }
//               ^com

let o = Some(true)
ignore(o)

// switch o { | Some() }
//                   ^com

type multiPayloadVariant = Test(int, bool, option<bool>, array<bool>)

let p = Test(1, true, Some(false), [])

// switch p { | Test(1, )}
//                     ^com

// switch p { | Test(1, true, )}
//                           ^com

// switch p { | Test(1, , None)}
//                     ^com

// switch p { | Test(1, true, None, )}
//                                 ^com

type multiPayloadPolyVariant = [#test(int, bool, option<bool>, array<bool>)]

let v: multiPayloadPolyVariant = #test(1, true, Some(false), [])

// switch v { | #test(1, )}
//                      ^com

// switch v { | #test(1, true, )}
//                            ^com

// switch v { | #test(1, , None)}
//                      ^com

// switch v { | #test(1, true, None, )}
//                                  ^com

let s = (true, Some(true), [false])

// switch s { | () }
//               ^com

// switch s { | (true, ) }
//                     ^com

// switch s { | (true, , []) }
//                    ^com

// switch s { | (true, []) => () |  }
//                                 ^com

// switch s { | (true, []) => () | (true, , [])  }
//                                       ^com

// switch z { | One |  }
//                   ^com

// switch z { | One | Two(true | )  }
//                              ^com

// switch z { | One | Three({test: true}, true | )  }
//                                              ^com

// switch b { | #one | #two(true | )  }
//                                ^com

// switch b { | #one | #three({test: true}, true | )  }
//                                                ^com

// switch s { | (true, _, []) }
//                      ^com

type recordWithFn = {someFn: unit => unit}

let ff: recordWithFn = {someFn: () => ()}

// switch ff { | {someFn: }}
//                       ^com

let xn: exn = Obj.magic()

// switch xn { | }
//              ^com

let getThing = async () => One

// switch await getThing() { | }
//                            ^com

let res: result<someVariant, somePolyVariant> = Ok(One)

// switch res { | Ok() }
//                   ^com

// switch res { | Error() }
//                      ^com

@react.component
let make = (~thing: result<someVariant, unit>) => {
  switch thing {
  | Ok(Three(r, _)) =>
    let _x = r
  // switch r { | {first, }}
  //                     ^com
  | _ => ()
  }
}

type results = {
  query: string,
  nbHits: int,
}

type hitsUse = {results: results, hits: array<string>}

let hitsUse = (): hitsUse => Obj.magic()

// let {results: {query, nbHits}, } = hitsUse()
//                               ^com
