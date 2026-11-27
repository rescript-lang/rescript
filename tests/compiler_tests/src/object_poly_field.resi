/* Signature inclusion over object types with polymorphic fields: pins
 moregen/eqtype on Tpoly-typed fields (Stage E). */

type poly = {"id": 'a. 'a => 'a}

let use_poly: poly => (int, string)
let forget_extra: {"id": 'a. 'a => 'a, "extra": int} => poly
let use_open: {.."id": 'a. 'a => 'a} => (int, string)
let value: poly
let pair: (int, string)

type settable_poly = {@set "id": 'a. 'a => 'a}
let write_poly: settable_poly => (int, string)
