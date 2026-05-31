open SharedTypes

type inlay_hint_kind = Type
let inlay_kind_to_lsp_inlay_hint = function
  | Type -> Lsp.Types.InlayHintKind.Type

let loc_item_to_type_hint ~full:{file; package} loc_item =
  match loc_item.loc_type with
  | Constant t ->
    Some
      (match t with
      | Const_int _ -> "int"
      | Const_char _ -> "char"
      | Const_string _ -> "string"
      | Const_float _ -> "float"
      | Const_int32 _ -> "int32"
      | Const_int64 _ -> "int64"
      | Const_bigint _ -> "bigint")
  | Typed (_, t, loc_kind) ->
    let from_type typ =
      typ |> Shared.type_to_string
      |> Str.global_replace (Str.regexp "[\r\n\t]") ""
    in
    Some
      (match References.defined_for_loc ~file ~package loc_kind with
      | None -> from_type t
      | Some (_, res) -> (
        match res with
        | `Declared -> from_type t
        | `Constructor _ -> from_type t
        | `Field -> from_type t))
  | _ -> None

let inlay ~source ~kind_file ~pos ~max_length ~full ~debug =
  let maxlen = try Some (int_of_string max_length) with Failure _ -> None in
  let hints = ref [] in
  let start_line, end_line = pos in
  let push loc kind =
    let range = Utils.cmt_loc_to_range loc in
    if start_line <= range.end_.line && end_line >= range.start.line then
      hints := (range, kind) :: !hints
  in
  let rec process_pattern (pat : Parsetree.pattern) =
    match pat.ppat_desc with
    | Ppat_tuple pl -> pl |> List.iter process_pattern
    | Ppat_record (fields, _) ->
      Ext_list.iter fields (fun {x = p} -> process_pattern p)
    | Ppat_array fields -> fields |> List.iter process_pattern
    | Ppat_var {loc} -> push loc Type
    | _ -> ()
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb with
    | {
     pvb_pat = {ppat_desc = Ppat_var _};
     pvb_expr =
       {
         pexp_desc =
           ( Pexp_constant _ | Pexp_tuple _ | Pexp_record _ | Pexp_variant _
           | Pexp_apply _ | Pexp_match _ | Pexp_construct _ | Pexp_ifthenelse _
           | Pexp_array _ | Pexp_ident _ | Pexp_try _ | Pexp_send _
           | Pexp_field _ | Pexp_open _
           | Pexp_fun {arity = Some _} );
       };
    } ->
      push vb.pvb_pat.ppat_loc Type
    | {pvb_pat = {ppat_desc = Ppat_tuple _}} -> process_pattern vb.pvb_pat
    | {pvb_pat = {ppat_desc = Ppat_record _}} -> process_pattern vb.pvb_pat
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (if kind_file = Files.Res then
     let parser =
       Res_driver.parsing_engine.parse_implementation_from_source
         ~for_printer:false
     in
     let {Res_driver.parsetree = structure} = parser ~source in
     iterator.structure iterator structure |> ignore);
  match full with
  | None -> None
  | Some full ->
    let result =
      !hints
      |> List.filter_map (fun ((range : Lsp.Types.Range.t), hint_kind) ->
             match
               References.get_loc_item ~full
                 ~pos:(range.start.line, range.start.character + 1)
                 ~debug
             with
             | None -> None
             | Some loc_item -> (
               let position =
                 Lsp.Types.Position.create ~line:range.start.line
                   ~character:range.end_.character
               in
               match loc_item_to_type_hint loc_item ~full with
               | Some label -> (
                 let kind = inlay_kind_to_lsp_inlay_hint hint_kind in
                 let label = ": " ^ label in
                 let result =
                   Lsp.Types.InlayHint.create ~position ~kind ~padding_left:true
                     ~padding_right:false ~label:(`String label) ()
                 in
                 match maxlen with
                 | Some value ->
                   if String.length label > value then None else Some result
                 | None -> Some result)
               | None -> None))
    in
    Some result

let code_lens ~source ~kind_file ~full ~debug =
  let lenses = ref [] in
  let push loc =
    let range = Utils.cmt_loc_to_range loc in
    lenses := range :: !lenses
  in
  (* Code lenses are only emitted for functions right now. So look for value bindings that are functions,
     and use the loc of the value binding itself so we can look up the full function type for our code lens. *)
  let value_binding (iterator : Ast_iterator.iterator)
      (vb : Parsetree.value_binding) =
    (match vb with
    | {
     pvb_pat = {ppat_desc = Ppat_var _; ppat_loc};
     pvb_expr = {pexp_desc = Pexp_fun _};
    } ->
      push ppat_loc
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator vb
  in
  let iterator = {Ast_iterator.default_iterator with value_binding} in
  (* We only print code lenses in implementation files. This is because they'd be redundant in interface files,
     where the definition itself will be the same thing as what would've been printed in the code lens. *)
  (if kind_file = Files.Res then
     let parser =
       Res_driver.parsing_engine.parse_implementation_from_source
         ~for_printer:false
     in
     let {Res_driver.parsetree = structure} = parser ~source in
     iterator.structure iterator structure |> ignore);
  match full with
  | None -> None
  | Some full ->
    let result =
      !lenses
      |> List.filter_map (fun (range : Lsp.Types.Range.t) ->
             match
               References.get_loc_item ~full
                 ~pos:(range.start.line, range.start.character + 1)
                 ~debug
             with
             | Some {loc_type = Typed (_, type_expr, _)} ->
               (* Code lenses can run commands. An empty command string means we just want the editor
                   to print the text, not link to running a command. *)
               let command =
                 Lsp.Types.Command.create
                   ~command:""
                     (* Print the type with a huge line width, because the code lens always prints on a
                       single line in the editor. *)
                   ~title:(type_expr |> Shared.type_to_string ~line_width:400)
                   ()
               in
               Some (Lsp.Types.CodeLens.create ~range ~command ())
             | _ -> None)
    in
    Some result
