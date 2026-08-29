/* Pins polymorphic object fields ({"f": 'a. ...}): the Tpoly binder on an
   object field is a live surface feature (Stage E survey). Every case in
   this file must keep compiling. The rejection of an object literal for a
   polymorphic field is pinned in
   tests/build_tests/super_errors/fixtures/object_literal_for_poly_field.res. */

type poly = {"id": 'a. 'a => 'a}

/* An annotated polymorphic field is usable at several types. */
let use_poly = (o: poly) => (o["id"](1), o["id"]("x"))

/* Polymorphic fields participate in width subtyping like any field. */
let forget_extra = (o: {"id": 'a. 'a => 'a, "extra": int}): poly => (o :> poly)

/* Access through an open row preserves the field's polymorphism. */
let use_open = (o: {.."id": 'a. 'a => 'a}) => (o["id"](1), o["id"]("x"))

/* A polymorphic field of an object value produced by raw JS. */
let value: poly = %raw(`{id: x => x}`)

let pair = use_poly(value)

/* A settable polymorphic field accepts a value as polymorphic as its
   scheme, and stays usable at several types afterwards. The rejection of a
   monomorphic value is pinned in
   tests/build_tests/super_errors/fixtures/object_write_poly_field_less_general.res. */
type settable_poly = {@set "id": 'a. 'a => 'a}

let write_poly = (o: settable_poly) => {
  o["id"] = x => x
  (o["id"](1), o["id"]("x"))
}
