external import: 'a => promise<'a> = "%import"

type shape

let init: ((string, int, int), shape) => Primitive_object.t

let update: (shape, Primitive_object.t, Primitive_object.t) => unit
