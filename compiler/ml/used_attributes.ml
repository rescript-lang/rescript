let used_attributes_key =
  Domain.DLS.new_key (fun () ->
      (Hash_set_poly.create 16 : string Asttypes.loc Hash_set_poly.t))

let used_attributes () = Domain.DLS.get used_attributes_key

let reset () = Hash_set_poly.clear (used_attributes ())

(* let dump_attribute fmt = (fun ( (sloc : string Asttypes.loc)) ->
       Format.fprintf fmt "@[%s@]" sloc.txt
     )

   let dump_used_attributes fmt =
     Format.fprintf fmt "Used attributes Listing Start:@.";
     Hash_set_poly.iter  used_attributes (fun attr -> dump_attribute fmt attr) ;
     Format.fprintf fmt "Used attributes Listing End:@." *)

(* only mark non-ghost used bs attribute *)
let mark_used_attribute ((x, _) : Parsetree.attribute) =
  if not x.loc.loc_ghost then Hash_set_poly.add (used_attributes ()) x

let is_used_attribute (sloc : string Asttypes.loc) =
  Hash_set_poly.mem (used_attributes ()) sloc
