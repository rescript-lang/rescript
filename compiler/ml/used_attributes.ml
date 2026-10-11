module Attribute_name_set = Hashtbl.Make (struct
  type t = string Asttypes.loc

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let used_attributes = Attribute_name_set.create 16

(* only mark non-ghost used bs attribute *)
let mark_used_attribute ((x, _) : Parsetree.attribute) =
  if not x.loc.loc_ghost then Attribute_name_set.replace used_attributes x ()

let is_used_attribute (sloc : string Asttypes.loc) =
  Attribute_name_set.mem used_attributes sloc
