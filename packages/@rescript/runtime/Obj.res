// Untyped value representation `t` and the identity cast `magic`, used by Stdlib modules and user code.

type t = Primitive_object_extern.t

external magic: 'a => 'b = "%identity"
