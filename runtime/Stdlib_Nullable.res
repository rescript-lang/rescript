@unboxed
type t<'a> = Primitive_js_extern.nullable<'a> =
  | Value('a)
  | @as(null) Null
  | @as(undefined) Undefined

external null: t<'a> = "#null"

external undefined: t<'a> = "#undefined"

external isNullable: t<'a> => bool = "#is_nullable"

external make: 'a => t<'a> = "%identity"

external toOption: t<'a> => option<'a> = "#nullable_to_opt"

let fromOption: option<'a> => t<'a> = option =>
  switch option {
  | Some(x) => make(x)
  | None => undefined
  }

let equal = (a, b, eq) => Stdlib_Option.equal(a->toOption, b->toOption, eq)

let compare = (a, b, cmp) => Stdlib_Option.compare(a->toOption, b->toOption, cmp)

let getOr = (value, default) =>
  switch value->toOption {
  | Some(x) => x
  | None => default
  }

let getWithDefault = getOr

let getExn: t<'a> => 'a = value =>
  switch value->toOption {
  | Some(x) => x
  | None => raise(Invalid_argument("Nullable.getExn: value is null or undefined"))
  }

external getUnsafe: t<'a> => 'a = "%identity"

let forEach = (value, f) =>
  switch value->toOption {
  | Some(x) => f(x)
  | None => ()
  }

let map = (value, f) =>
  switch value->toOption {
  | Some(x) => make(f(x))
  | None => Obj.magic(value)
  }

let mapOr = (value, default, f) =>
  switch value->toOption {
  | Some(x) => f(x)
  | None => default
  }

let mapWithDefault = mapOr

let flatMap = (value, f) =>
  switch value->toOption {
  | Some(x) => f(x)
  | None => Obj.magic(value)
  }

external ignore: t<'a> => unit = "%ignore"
