module Query = {
  let use = (~variables: QueryFile.Types.variables) => {
    ignore(variables)
    ""
  }
}

// let x = Query.use(~variables={location: ByAddress()})
//                                                   ^com

type nestedRecord = {nested: bool}

type rec someRecord = {
  first: int,
  second: (bool, option<someRecord>),
  optThird: option<[#first | #second(someRecord)]>,
  nest: nestedRecord,
}

type somePolyVariant = [#one | #two(bool) | #three(someRecord, bool)]

type someVariant = One | Two(bool) | Three(someRecord, bool)

type paramRecord<'a, 'b> = {
  first: 'a,
  second: 'b,
}

let record: paramRecord<someVariant, QueryFile.Types.byAddress> = {
  first: One,
  second: {city: "city"},
}

// switch record { | {first: }}
//                          ^com

// switch record { | {second: }}
//                           ^com

// TODO: Functions, aliases/definitions, records, variants, polyvariants, tuples

let res: result<someVariant, somePolyVariant> = Ok(One)

// switch res { | Ok() }
//                   ^com

// switch res { | Error() }
//                      ^com

let resOpt: result<option<someVariant>, unit> = Ok(None)

// switch resOpt { | Ok() }
//                      ^com

// switch resOpt { | Ok(Some()) }
//                           ^com
