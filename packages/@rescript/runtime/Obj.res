// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

type t = Primitive_object_extern.t

external magic: 'a => 'b = "%identity"

@deprecated("Do not use directly. This will be removed in v13")
external repr: 'a => t = "%identity"

@deprecated("Do not use directly. This will be removed in v13")
external obj: t => 'a = "%identity"

@deprecated("Do not use directly. This will be removed in v13")
external tag: t => int = "%obj_tag"

@deprecated("Do not use directly. This will be removed in v13")
external size: t => int = "%obj_size"

@deprecated("Do not use directly. This will be removed in v13")
external getField: (t, 'a) => t = "%obj_get_field"

@deprecated("Do not use directly. This will be removed in v13")
external setField: (t, 'a, t) => unit = "%obj_set_field"

@deprecated("Do not use directly. This will be removed in v13")
external dup: t => t = "%obj_dup"

@deprecated("Do not use directly. This will be removed in v13") @scope("Object")
external updateDummy: (t, t) => unit = "assign"
