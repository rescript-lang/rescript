// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t = Primitive_object_extern.t

external magic: 'a => 'b = "%identity"
