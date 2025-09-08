type t

external repr: 'a => t = "%identity"
external obj: t => 'a = "%identity"
external magic: 'a => 'b = "%identity"

external tag: t => int = "%obj_tag"
external size: t => int = "%obj_size"
external getField: (t, 'a) => t = "%obj_get_field"
external setField: (t, 'a, t) => unit = "%obj_set_field"
external dup: t => t = "%obj_dup"

@scope("Object")
external updateDummy: (t, t) => unit = "assign"
