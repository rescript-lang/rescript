open Parsetree

exception Map_error of string

type map_entry = {
  tag : string;
  occurrence_index : int;
  literal_hash : string;
  target_module : string;
}

let parse_map (path : string) : map_entry list =
  let json = Ext_json_parse.parse_json_from_file path in
  let expect_obj = function
    | Ext_json_types.Obj {map} -> map
    | _ -> raise (Map_error "resolution map must be a JSON object")
  in
  let expect_arr = function
    | Ext_json_types.Arr {content; _} -> Array.to_list content
    | _ -> raise (Map_error "entries must be a JSON array")
  in
  let get_field name (m : Ext_json_types.t Map_string.t) =
    match Map_string.find_opt m name with
    | Some v -> v
    | None -> raise (Map_error ("missing field: " ^ name))
  in
  let entries =
    json |> expect_obj |> get_field "entries" |> expect_arr
  in
  let to_string = function
    | Ext_json_types.Str {str} -> str
    | _ -> raise (Map_error "expected string")
  in
  let to_int = function
    | Ext_json_types.Flo {flo} -> int_of_string flo
    | _ -> raise (Map_error "expected number")
  in
  List.map
    (fun v ->
      let m = expect_obj v in
      let tag = get_field "tag" m |> to_string in
      let occurrence_index = get_field "occurrenceIndex" m |> to_int in
      let literal_hash = get_field "literalHash" m |> to_string in
      let target_module = get_field "targetModule" m |> to_string in
      {tag; occurrence_index; literal_hash; target_module})
    entries

let build_index (entries : map_entry list) :
    (string, (int, map_entry) Hashtbl.t) Hashtbl.t =
  let tbl : (string, (int, map_entry) Hashtbl.t) Hashtbl.t = Hashtbl.create 7 in
  List.iter
    (fun (e : map_entry) ->
      let subtbl =
        match Hashtbl.find_opt tbl e.tag with
        | Some t -> t
        | None ->
          let t = Hashtbl.create 5 in
          Hashtbl.add tbl e.tag t;
          t
      in
      Hashtbl.replace subtbl e.occurrence_index e)
    entries;
  tbl

let csv_hash (tag : string) (s : string) : string =
  Digest.(to_hex (string (tag ^ "\n" ^ s)))

let rewrite_structure (entries : map_entry list) (ast : structure) : structure =
  let index = build_index entries in
  let counts : (string, int) Hashtbl.t = Hashtbl.create 7 in
  let bump tag =
    let v = match Hashtbl.find_opt counts tag with Some i -> i | None -> 0 in
    let v' = v + 1 in
    Hashtbl.replace counts tag v';
    v'
  in
  let string_lit_of_payload (payload : Ast_payload.t) : string option =
    match payload with
    | PStr [ { pstr_desc = Pstr_eval (e, _attrs); _ } ] -> (
      match e.pexp_desc with
      | Pexp_constant (Pconst_string (txt, _)) -> Some txt
      | _ -> None)
    | _ -> None
  in
  let open Ast_helper in
  let rec map_mod (m : module_expr) : module_expr =
    match m.pmod_desc with
    | Pmod_extension (({txt = tag; _} as name_loc, payload)) -> (
      match string_lit_of_payload payload with
      | None -> m
      | Some s -> (
        match Hashtbl.find_opt index tag with
        | None -> m
        | Some subtbl ->
          let k = bump tag in
          (match Hashtbl.find_opt subtbl k with
          | None -> m
          | Some entry ->
            let lit_hash = csv_hash tag s in
            if lit_hash <> entry.literal_hash then
              Location.raise_errorf ~loc:name_loc.loc
                "embed map mismatch for tag %s occurrence %d" tag k;
            Mod.ident ~loc:m.pmod_loc {txt = Lident entry.target_module; loc = m.pmod_loc})))
    | Pmod_structure s -> Mod.structure ~loc:m.pmod_loc (map_str s)
    | Pmod_functor (n, mt, body) -> Mod.functor_ ~loc:m.pmod_loc n mt (map_mod body)
    | Pmod_apply (m1, m2) -> Mod.apply ~loc:m.pmod_loc (map_mod m1) (map_mod m2)
    | _ -> m
  and map_expr (e : expression) : expression =
    match e.pexp_desc with
    | Pexp_extension (({txt = tag; _} as name_loc, payload)) -> (
      match string_lit_of_payload payload with
      | None -> e
      | Some s -> (
        match Hashtbl.find_opt index tag with
        | None -> e
        | Some subtbl ->
          let k = bump tag in
          match Hashtbl.find_opt subtbl k with
          | None -> e
          | Some entry ->
            let lit_hash = csv_hash tag s in
            if lit_hash <> entry.literal_hash then
              Location.raise_errorf ~loc:name_loc.loc
                "embed map mismatch for tag %s occurrence %d" tag k;
            let id =
              Exp.ident ~loc:e.pexp_loc
                { txt = Longident.Ldot (Lident entry.target_module, "default");
                  loc = e.pexp_loc }
            in
            id))
    | _ -> e
  and map_str (s : structure) : structure =
    List.map
      (fun (si : structure_item) ->
        match si.pstr_desc with
        | Pstr_include incl ->
          let m' = map_mod incl.pincl_mod in
          if m' == incl.pincl_mod then si
          else Str.include_ ~loc:si.pstr_loc {incl with pincl_mod = m'}
        | Pstr_module mb ->
          let m' = map_mod mb.pmb_expr in
          if m' == mb.pmb_expr then si
          else Str.module_ ~loc:si.pstr_loc {mb with pmb_expr = m'}
        | Pstr_recmodule mbs ->
          let mbs' = List.map (fun mb -> {mb with pmb_expr = map_mod mb.pmb_expr}) mbs in
          Str.rec_module ~loc:si.pstr_loc mbs'
        | Pstr_value (recflag, vbs) ->
          let vbs' = List.map (fun vb -> {vb with pvb_expr = map_expr vb.pvb_expr}) vbs in
          Str.value ~loc:si.pstr_loc recflag vbs'
        | Pstr_eval (e, _attrs) ->
          let e' = map_expr e in
          if e' == e then si else Str.eval ~loc:si.pstr_loc e'
        | _ -> si)
      s
  in
  map_str ast

let write_ast_impl ~output (ast : structure) =
  let sourcefile = !Location.input_name in
  Binary_ast.write_ast ~sourcefile ~output Ml ast

let run ~in_ast ~map_path ~(out_ast : string option) : unit =
  let kind = Ext_file_extensions.classify_input (Ext_filename.get_extension_maybe in_ast) in
  match kind with
  | Impl_ast ->
    let ast = Binary_ast.read_ast_exn ~fname:in_ast Ml in
    let entries = parse_map map_path in
    let ast' = rewrite_structure entries ast in
    let out = match out_ast with Some x -> x | None -> in_ast in
    write_ast_impl ~output:out ast'
  | Intf_ast ->
    let ast = Binary_ast.read_ast_exn ~fname:in_ast Mli in
    let out = match out_ast with Some x -> x | None -> in_ast in
    let sourcefile = !Location.input_name in
    Binary_ast.write_ast ~sourcefile ~output:out Mli ast
  | _ -> Bsc_args.bad_arg ("-ast expects a .ast or .iast file: " ^ in_ast)
