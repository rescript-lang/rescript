(* Exhaustiveness checking and match compilation see a dict pattern
   [dict{"a": p}] as a record pattern of the [dict] type, whose labels are its
   keys: an optional field per key. [lowering] gives the labels to the keys of
   one match, so they line up across its cases. *)

open Types
open Typedtree

let rec contains_dict p =
  match p.pat_desc with
  | Tpat_dict _ -> true
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) -> false
  | Tpat_alias (p, _, _) | Tpat_variant (_, Some p, _) -> contains_dict p
  | Tpat_tuple ps | Tpat_construct (_, _, ps) | Tpat_array ps ->
    List.exists contains_dict ps
  | Tpat_record (fields, _, _) ->
    List.exists (fun (_, _, p, _) -> contains_dict p) fields
  | Tpat_or (p1, p2, _) -> contains_dict p1 || contains_dict p2

(* The keys of the dict patterns in [pats], in order of appearance *)
let keys pats =
  let seen = Hashtbl.create 7 in
  let keys = ref [] in
  let rec collect p =
    (match p.pat_desc with
    | Tpat_dict entries ->
      List.iter
        (fun {tdp_key = {txt = key}} ->
          if not (Hashtbl.mem seen key) then (
            Hashtbl.add seen key ();
            keys := key :: !keys))
        entries
    | _ -> ());
    iter_pattern_desc collect p.pat_desc
  in
  List.iter collect pats;
  List.rev !keys

(* An optional field of type ['a] in [dict<'a>] per key *)
let labels_of_keys keys =
  let value = Btype.newgenvar () in
  let lbl_res =
    Btype.newgenty (Tconstr (Predef.path_dict, [value], ref Mnil))
  in
  let lbl_arg =
    Btype.newgenty (Tconstr (Predef.path_option, [value], ref Mnil))
  in
  let labels =
    Array.of_list
      (List.mapi
         (fun pos key ->
           {
             lbl_name = key;
             lbl_runtime_name = key;
             lbl_res;
             lbl_arg;
             lbl_mut = Asttypes.Immutable;
             lbl_optional = true;
             lbl_pos = pos;
             lbl_all = [||];
             lbl_repres = Record_regular;
             lbl_private = Asttypes.Public;
             lbl_loc = Location.none;
             lbl_attributes = [];
           })
         keys)
  in
  Array.iter (fun lbl -> lbl.lbl_all <- labels) labels;
  labels

(* Turns the dict patterns of a match, whose patterns are [pats], into record
   patterns *)
let lowering pats =
  if not (List.exists contains_dict pats) then Fun.id
  else
    let labels = Hashtbl.create 7 in
    Array.iter
      (fun lbl -> Hashtbl.add labels lbl.lbl_name lbl)
      (labels_of_keys (keys pats));
    let rec lower p =
      match p.pat_desc with
      | Tpat_dict entries ->
        let fields =
          List.map
            (fun {tdp_key; tdp_pattern; tdp_optional} ->
              ( {tdp_key with txt = Longident.Lident tdp_key.txt},
                Hashtbl.find labels tdp_key.txt,
                lower tdp_pattern,
                tdp_optional ))
            entries
        in
        (* Record patterns are sorted in the typed tree *)
        let fields =
          List.sort
            (fun (_, lbl1, _, _) (_, lbl2, _, _) ->
              compare lbl1.lbl_pos lbl2.lbl_pos)
            fields
        in
        {p with pat_desc = Tpat_record (fields, Asttypes.Open, None)}
      | d -> {p with pat_desc = map_pattern_desc lower d}
    in
    lower
