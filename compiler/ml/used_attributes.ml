module Attribute_name_set = Hash_set.Make (struct
  type t = string Asttypes.loc

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let used_attributes_key =
  Domain.DLS.new_key (fun () -> Attribute_name_set.create 16)

let used_attributes () = Domain.DLS.get used_attributes_key

let reset () = Attribute_name_set.clear (used_attributes ())

(* only mark non-ghost used bs attribute *)
let mark_used_attribute ((x, _) : Parsetree.attribute) =
  if not x.loc.loc_ghost then Attribute_name_set.add (used_attributes ()) x

let is_used_attribute (sloc : string Asttypes.loc) =
  Attribute_name_set.mem (used_attributes ()) sloc
