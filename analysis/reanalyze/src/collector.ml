open Collected_types
open Common

type backend = {
  add_decl: Collected_types.decl -> unit;
  find_decl: Lexing.position -> Common.decl option;
  replace_decl: Common.decl -> unit;
  add_value_reference: value_reference -> unit;
  add_type_reference: type_reference -> unit;
  finalize: unit -> Collected_types.t;
}

type t = backend

let add_decl t decl = t.add_decl decl
let find_decl t pos = t.find_decl pos
let replace_decl t decl = t.replace_decl decl

let add_value_reference t ref = t.add_value_reference ref
let add_type_reference t ref = t.add_type_reference ref
let finalize t = t.finalize ()

let dead_common_sink () =
  let open DeadCommon in
  let add_decl = function
    | Value_decl
        {
          name;
          path;
          loc;
          module_loc;
          optional_args;
          side_effects;
          is_toplevel;
          pos_start = _;
          pos_end = _;
        } ->
      addValueDeclaration ~isToplevel:is_toplevel ~loc ~moduleLoc:module_loc
        ~optionalArgs:optional_args ~path ~sideEffects:side_effects name
    | General_decl
        {
          name;
          path;
          loc;
          module_loc;
          decl_kind;
          pos_adjustment;
          pos_start;
          pos_end;
        } ->
      addDeclaration_ ?posStart:pos_start ?posEnd:pos_end ~declKind:decl_kind
        ~path ~loc ~moduleLoc:module_loc ~posAdjustment:pos_adjustment name
  in
  let find_decl pos = PosHash.find_opt decls pos in
  let replace_decl decl = PosHash.replace decls decl.pos decl in
  let add_value_reference {loc_from; loc_to; add_file_reference} =
    addValueReference ~addFileReference:add_file_reference ~locFrom:loc_from
      ~locTo:loc_to
  in
  let add_type_reference {pos_from; pos_to} =
    TypeReferences.add pos_to pos_from
  in
  {
    add_decl = add_decl;
    find_decl = find_decl;
    replace_decl = replace_decl;
    add_value_reference = add_value_reference;
    add_type_reference = add_type_reference;
    finalize = (fun () -> Collected_types.empty);
  }

module PosTbl = Hashtbl.Make (struct
  type t = Lexing.position

  let hash pos =
    Hashtbl.hash (pos.Lexing.pos_fname, pos.pos_lnum, pos.pos_bol, pos.pos_cnum)

  let equal = ( = )
end)

let collected () =
  let decls = PosTbl.create 128 in
  let decl_order = ref [] in
  let value_refs = ref [] in
  let type_refs = ref [] in
  let insert_decl decl =
    match
      Collected_types.to_common_decl ~current_src:!Common.currentSrc
        ~current_module:!Common.currentModule decl
    with
    | None -> ()
    | Some c ->
      if not (PosTbl.mem decls c.pos) then decl_order := c.pos :: !decl_order;
      PosTbl.replace decls c.pos c
  in
  let find_decl pos = PosTbl.find_opt decls pos in
  let replace_decl decl = PosTbl.replace decls decl.pos decl in
  let add_value_reference ref = value_refs := ref :: !value_refs in
  let add_type_reference ref = type_refs := ref :: !type_refs in
  let finalize () =
    let seen = PosTbl.create 128 in
    let decls_list =
      !decl_order
      |> List.rev
      |> List.fold_left
           (fun acc pos ->
             if PosTbl.mem seen pos then acc
             else (
               PosTbl.replace seen pos ();
               match PosTbl.find_opt decls pos with
               | None -> acc
               | Some decl -> decl :: acc))
           []
    in
    {
      Collected_types.decls = decls_list;
      value_references = List.rev !value_refs;
      type_references = List.rev !type_refs;
    }
  in
  {
    add_decl = insert_decl;
    find_decl = find_decl;
    replace_decl = replace_decl;
    add_value_reference = add_value_reference;
    add_type_reference = add_type_reference;
    finalize = finalize;
  }

