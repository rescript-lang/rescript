@@config({
  flags: ["-bs-jsx", "4"],
})

open Mocha
open Test_utils

// Reserved words are names where the grammar expects a label: record fields,
// argument labels, JSX props, polymorphic variant tags.

type input = {
  type: string,
  mutable open: bool,
  in?: int,
  @as("data-for") for: string,
}

let make = kind => {type: kind, open: false, for: "target"}
let update = (r: input) => {...r, type: "updated", in: 1}

let classify = (r: input) =>
  switch r {
  | {type: "number", in: ?i} => i
  | {open: true} => Some(-1)
  | _ => None
  }

let describeInput = (~type as kind, ~for as target=?, ()) =>
  switch target {
  | Some(target) => kind ++ " for " ++ target
  | None => kind
  }

type token = [#type | #module | #true | #if(int)]

let tokenName = (t: token) =>
  switch t {
  | #type => "type"
  | #module => "module"
  | #true => "true"
  | #if(n) => "if " ++ Int.toString(n)
  }

module Field = {
  @react.component
  let make = (~type as kind: string, ~in as value: int=0) =>
    React.string(kind ++ ":" ++ Int.toString(value))
}

let field = <Field type="email" in=3 />

describe(__MODULE__, () => {
  test("record fields", () => {
    let r = make("text")
    eq(__LOC__, "text", r.type)
    eq(__LOC__, false, r.open)
    r.open = true
    eq(__LOC__, true, r.open)
    eq(__LOC__, Some(1), update(r).in)
    eq(__LOC__, "updated", update(r).type)
  })

  test("runtime representation", () => {
    let r = make("text")
    eq(__LOC__, Some("text"), Obj.magic(r)["type"])
    eq(__LOC__, Some("target"), Obj.magic(r)["data-for"])
  })

  test("record patterns", () => {
    eq(__LOC__, Some(2), classify({type: "number", open: false, in: 2, for: ""}))
    eq(__LOC__, Some(-1), classify({type: "text", open: true, for: ""}))
    eq(__LOC__, None, classify({type: "text", open: false, for: ""}))
  })

  test("argument labels", () => {
    eq(__LOC__, "submit", describeInput(~type="submit", ()))
    eq(__LOC__, "submit for form", describeInput(~type="submit", ~for="form", ()))
  })

  test("variant tags", () => {
    eq(
      __LOC__,
      ["type", "module", "true", "if 1"],
      [#type, #module, #true, #if(1)]->Array.map(tokenName),
    )
    eq(__LOC__, "type", Obj.magic(#type))
  })

  test("JSX props", () => {
    eq(
      __LOC__,
      "email:3",
      Obj.magic(field)["props"]["type"] ++ ":" ++ Int.toString(Obj.magic(field)["props"]["in"]),
    )
  })
})
