open Shared_types
type cursor_at_arg = Unlabelled of int | Labelled of string

(* Produces the doc string shown below the signature help for each parameter. *)
let docs_for_label type_expr ~file ~state ~package ~supports_markdown_links =
  let types =
    Hover.find_relevant_types_from_type ~state ~file ~package type_expr
  in
  let type_names = types |> List.map (fun {Hover.name} -> name) in
  let type_definitions =
    types
    |> List.map (fun {Hover.decl; name; env; loc; path} ->
           let link_to_type_definition_str =
             if supports_markdown_links then
               Markdown.go_to_definition_text ~env ~pos:loc.Warnings.loc_start
             else ""
           in
           (* Since printing the whole name via its path can get quite long, and
              we're short on space for the signature help, we'll only print the
              fully "qualified" type name if we must (ie if several types we're
              displaying have the same name). *)
           let multiple_types_have_this_name =
             type_names
             |> List.filter (fun type_name -> type_name = name)
             |> List.length > 1
           in
           let type_name =
             if multiple_types_have_this_name then
               path |> Shared_types.path_ident_to_string
             else name
           in
           Markdown.code_block
             (Shared.decl_to_string ~print_name_as_is:true type_name decl)
           ^ link_to_type_definition_str)
  in
  type_definitions |> String.concat "\n"

let find_function_type ~debug ~source ~kind_file ~pos ~full ~state =
  (* Start by looking at the typed info at the loc of the fn *)
  match full with
  | None -> None
  | Some full -> (
    let {file; package} = full in
    let env = Query_env.from_file file in
    let fn_from_loc_item =
      match References.get_loc_item ~full ~pos ~debug:false with
      | Some {loc_type = Typed (_, type_expr, loc_kind)} -> (
        let docstring =
          match References.defined_for_loc ~file ~package ~state loc_kind with
          | None -> []
          | Some (docstring, _) -> docstring
        in
        if Debug.verbose () then
          Printf.printf "[sig_help_fn] Found loc item: %s.\n"
            (Shared.type_to_string type_expr);
        match
          Type_utils.extract_function_type2 ~env ~package:full.package ~state
            type_expr
        with
        | args, _tRet, _ when args <> [] ->
          Some (args, docstring, type_expr, package, env, file)
        | _ -> None)
      | None ->
        if Debug.verbose () then
          Printf.printf "[sig_help_fn] Found no loc item.\n";
        None
      | Some _ ->
        if Debug.verbose () then
          Printf.printf
            "[sig_help_fn] Found loc item, but not what was expected.\n";
        None
    in
    match fn_from_loc_item with
    | Some fn_from_loc_item -> Some fn_from_loc_item
    | None -> (
      (* If nothing was found there, try using the unsaved completion engine *)
      let completables =
        match source with
        | "" -> None
        | source -> (
          (* Leverage the completion functionality to pull out the type of the identifier doing the function application.
             This lets us leverage all of the smart work done in completions to find the correct type in many cases even
             for files not saved yet. *)
          match
            Completion_front_end.completion_with_parser ~debug ~source
              ~kind_file ~pos_cursor:pos
          with
          | None -> None
          | Some (completable, scope) ->
            Some
              ( completable
                |> Completion_back_end.process_completable ~debug ~full ~pos
                     ~scope ~state ~env ~for_hover:true,
                env,
                package,
                file ))
      in
      match completables with
      | Some ({kind = Value type_expr; docstring} :: _, env, package, file) ->
        let args, _, _ =
          Type_utils.extract_function_type2 type_expr ~env ~state ~package
        in
        Some (args, docstring, type_expr, package, env, file)
      | _ -> None))

(* Extracts all parameters from a parsed function signature *)
let extract_parameters ~signature ~type_str_for_parser ~label_prefix_len =
  match signature with
  | [{Parsetree.psig_desc = Psig_value {pval_type = expr}}]
    when match expr.ptyp_desc with
         | Ptyp_arrow _ -> true
         | _ -> false ->
    let rec extract_params expr params =
      match expr with
      | {
       (* Gotcha: functions with multiple arugments are modelled as a series of single argument functions. *)
       Parsetree.ptyp_desc = Ptyp_arrow {arg; ret = next_function_expr};
       ptyp_loc;
      } ->
        let start_offset =
          ptyp_loc |> Loc.start
          |> Pos.position_to_offset type_str_for_parser
          |> Option.get
        in
        let end_offset =
          arg.typ.ptyp_loc |> Loc.end_
          |> Pos.position_to_offset type_str_for_parser
          |> Option.get
        in
        (* The AST locations does not account for "=?" of optional arguments, so add that to the offset here if needed. *)
        let end_offset =
          match arg.lbl with
          | Asttypes.Optional _ -> end_offset + 2
          | _ -> end_offset
        in
        extract_params next_function_expr
          (params
          @ [
              ( arg.lbl,
                (* Remove the label prefix offset here, since we're not showing
                   that to the end user. *)
                start_offset - label_prefix_len,
                end_offset - label_prefix_len );
            ])
      | _ -> params
    in
    extract_params expr []
  | _ -> []

(* Finds what parameter is active, if any *)
let find_active_parameter ~arg_at_cursor ~args =
  match arg_at_cursor with
  | None -> (
    (* If a function only has one, unlabelled argument, we can safely assume that's active whenever we're in the signature help for that function,
       even if we technically didn't find anything at the cursor (which we don't for empty expressions). *)
    match args with
    | [(Asttypes.Nolabel, _)] -> Some 0
    | _ -> None)
  | Some (Unlabelled unlabelled_argument_index) ->
    let index = ref 0 in
    args
    |> List.find_map (fun (label, _) ->
           match label with
           | Asttypes.Nolabel when !index = unlabelled_argument_index ->
             Some !index
           | _ ->
             index := !index + 1;
             None)
  | Some (Labelled name) ->
    let index = ref 0 in
    args
    |> List.find_map (fun (label, _) ->
           match label with
           | (Asttypes.Labelled {txt = label_name} | Optional {txt = label_name})
             when label_name = name ->
             Some !index
           | _ ->
             index := !index + 1;
             None)

type constructor_info = {
  docstring: string list;
  name: string;
  args: constructor_args;
}

let find_constructor_args ~full ~env ~state ~constructor_name loc =
  match
    References.get_loc_item ~debug:false ~full
      ~pos:(Pos.of_lexing loc.Location.loc_end)
  with
  | None -> None
  | Some {loc_type = Typed (_, typ_expr, _)} -> (
    match
      Type_utils.extract_type ~env ~state ~package:full.package typ_expr
    with
    | Some ((Toption (_, TypeExpr t) as extracted_type), _) -> (
      match constructor_name with
      | "Some" ->
        Some
          {
            name = "Some";
            docstring =
              [
                Markdown.code_block
                  (Type_utils.extracted_type_to_string extracted_type);
              ];
            args = Args [(t, Location.none)];
          }
      | _ -> None)
    | Some ((Tresult {ok_type; error_type} as extracted_type), _) -> (
      match constructor_name with
      | "Ok" ->
        Some
          {
            name = "Ok";
            docstring =
              [
                Markdown.code_block
                  (Type_utils.extracted_type_to_string extracted_type);
              ];
            args = Args [(ok_type, Location.none)];
          }
      | "Error" ->
        Some
          {
            name = "Error";
            docstring =
              [
                Markdown.code_block
                  (Type_utils.extracted_type_to_string extracted_type);
              ];
            args = Args [(error_type, Location.none)];
          }
      | _ -> None)
    | Some (Tvariant {constructors}, _) ->
      constructors
      |> List.find_opt (fun (c : Constructor.t) ->
             c.cname.txt = constructor_name)
      |> Option.map (fun (c : Constructor.t) ->
             {docstring = c.docstring; name = c.cname.txt; args = c.args})
    | _ -> None)
  | _ -> None

let signature_help ~debug ~source ~kind_file ~pos
    ~allow_for_constructor_payloads ~full ~state =
  match source with
  | "" -> None
  | text -> (
    match Pos.position_to_offset text pos with
    | None -> None
    | Some offset -> (
      let pos_before_cursor = Pos.pos_before_cursor pos in
      let offset_no_white = Utils.skip_white text (offset - 1) in
      let first_char_before_cursor_no_white =
        if offset_no_white < String.length text && offset_no_white >= 0 then
          Some text.[offset_no_white]
        else None
      in
      let loc_has_cursor loc =
        loc |> Cursor_position.loc_has_cursor ~pos:pos_before_cursor
      in
      let supports_markdown_links = true in
      let result = ref None in
      let print_thing thg =
        match thg with
        | `ConstructorExpr _ -> "Constructor(expr)"
        | `ConstructorPat _ -> "Constructor(pat)"
        | `FunctionCall _ -> "FunctionCall"
      in
      let set_result (loc, thing) =
        match (thing, allow_for_constructor_payloads) with
        | (`ConstructorExpr _ | `ConstructorPat _), false -> ()
        | _ -> (
          match !result with
          | None ->
            if Debug.verbose () then
              Printf.printf "[sig_help_result] Setting because had none\n";
            result := Some (loc, thing)
          | Some (current_loc, current_thing)
            when Pos.of_lexing loc.Location.loc_start
                 > Pos.of_lexing current_loc.Location.loc_start ->
            result := Some (loc, thing);

            if Debug.verbose () then
              Printf.printf
                "[sig_help_result] Setting because loc of %s > then existing \
                 of %s\n"
                (print_thing thing)
                (print_thing current_thing)
          | Some (_, current_thing) ->
            if Debug.verbose () then
              Printf.printf
                "[sig_help_result] Doing nothing because loc of %s < then \
                 existing of %s\n"
                (print_thing thing)
                (print_thing current_thing))
      in
      let search_for_arg_with_cursor ~is_pipe_expr ~args =
        let extracted_args = extract_exp_apply_args ~args in
        let arg_at_cursor =
          let first_arg_index = if is_pipe_expr then 1 else 0 in
          let unlabelled_arg_count = ref first_arg_index in
          let last_unlabelled_arg_before_cursor = ref first_arg_index in
          let argAtCursor_ =
            extracted_args
            |> List.find_map (fun arg ->
                   match arg.label with
                   | None ->
                     let current_unlabelled_arg_count = !unlabelled_arg_count in
                     unlabelled_arg_count := current_unlabelled_arg_count + 1;
                     (* An argument without a label is just the expression, so we can use that. *)
                     if loc_has_cursor arg.exp.pexp_loc then
                       Some (Unlabelled current_unlabelled_arg_count)
                     else (
                       (* If this unlabelled arg doesn't have the cursor, record
                          it as the last seen unlabelled arg before the
                          cursor.*)
                       if pos_before_cursor >= (arg.exp.pexp_loc |> Loc.start)
                       then
                         last_unlabelled_arg_before_cursor :=
                           current_unlabelled_arg_count;
                       None)
                   | Some {name; pos_start; pos_end} -> (
                     (* Check for the label identifier itself having the cursor *)
                     match
                       pos
                       |> Cursor_position.classify_positions ~pos_start ~pos_end
                     with
                     | HasCursor -> Some (Labelled name)
                     | NoCursor | EmptyLoc -> (
                       (* If we're not in the label, check the exp. Either the exp
                          exists and has the cursor. Or the exp is a parser recovery
                          node, in which case we assume that the parser recovery
                          indicates that the cursor was here. *)
                       match
                         ( arg.exp.pexp_desc,
                           arg.exp.pexp_loc
                           |> Cursor_position.classify_loc
                                ~pos:pos_before_cursor )
                       with
                       | Pexp_extension ({txt = "rescript.exprhole"}, _), _
                       | _, HasCursor ->
                         Some (Labelled name)
                       | _ -> None)))
          in

          match argAtCursor_ with
          | None ->
            Some
              (Unlabelled
                 (!last_unlabelled_arg_before_cursor
                 +
                 if first_char_before_cursor_no_white = Some ',' then 1
                   (* If we found no argument with the cursor, we might still be
                      able to complete for an unlabelled argument, if the char
                      before the cursor is ',', like: `someFn(123, <com>)`
                      complete for argument 2, or: `someFn(123, <com>, true)`
                      complete for argument 2 as well. Adding 1 here accounts
                      for the comma telling us that the users intent is to fill
                      in the next argument. *)
                 else 0))
          | v -> v
        in
        (arg_at_cursor, extracted_args)
      in
      let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression)
          =
        (match expr with
        (* Handle pipes, like someVar->someFunc(... *)
        | {
         pexp_desc =
           Pexp_apply
             {
               funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
               args =
                 [
                   _;
                   ( _,
                     {
                       pexp_desc =
                         Pexp_apply
                           {funct = {pexp_desc = Pexp_ident _} as exp; args};
                       pexp_loc;
                     } );
                 ];
             };
        }
          when loc_has_cursor pexp_loc ->
          let arg_at_cursor, extracted_args =
            search_for_arg_with_cursor ~is_pipe_expr:true ~args
          in
          set_result
            (exp.pexp_loc, `FunctionCall (arg_at_cursor, exp, extracted_args))
        (* Look for applying idents, like someIdent(...) *)
        | {
         pexp_desc =
           Pexp_apply {funct = {pexp_desc = Pexp_ident _} as exp; args};
         pexp_loc;
        }
          when loc_has_cursor pexp_loc ->
          let arg_at_cursor, extracted_args =
            search_for_arg_with_cursor ~is_pipe_expr:false ~args
          in
          set_result
            (exp.pexp_loc, `FunctionCall (arg_at_cursor, exp, extracted_args))
        | {pexp_desc = Pexp_construct (lid, Some payload_exp); pexp_loc}
          when loc_has_cursor payload_exp.pexp_loc
               || Completion_expressions.is_expr_hole payload_exp
                  && loc_has_cursor pexp_loc ->
          (* Constructor payloads *)
          set_result (lid.loc, `ConstructorExpr (lid, payload_exp))
        | _ -> ());
        Ast_iterator.default_iterator.expr iterator expr
      in
      let pat (iterator : Ast_iterator.iterator) (pat : Parsetree.pattern) =
        (match pat with
        | {ppat_desc = Ppat_construct (lid, Some payload_pat)}
          when loc_has_cursor payload_pat.ppat_loc ->
          (* Constructor payloads *)
          set_result (lid.loc, `ConstructorPat (lid, payload_pat))
        | _ -> ());
        Ast_iterator.default_iterator.pat iterator pat
      in
      let iterator = {Ast_iterator.default_iterator with expr; pat} in
      let parser =
        Res_driver.parsing_engine.parse_implementation_from_source
          ~for_printer:false
      in
      let {Res_driver.parsetree = structure} = parser ~source in
      iterator.structure iterator structure |> ignore;
      (* Handle function application, if found *)
      match !result with
      | Some (_, `FunctionCall (arg_at_cursor, exp, _extractedArgs)) -> (
        (* Not looking for the cursor position after this, but rather the target function expression's loc. *)
        let pos = exp.pexp_loc |> Loc.end_ in
        match
          find_function_type ~source ~kind_file ~debug ~pos ~full ~state
        with
        | Some (args, docstring, type_expr, package, _env, file) ->
          if debug then
            Printf.printf "argAtCursor: %s\n"
              (match arg_at_cursor with
              | None -> "none"
              | Some (Labelled name) -> "~" ^ name
              | Some (Unlabelled index) ->
                "unlabelled<" ^ string_of_int index ^ ">");

          (* The LS protocol wants us to send both the full type signature (label) that the end user sees as the signature help, and all parameters in that label
             in the form of a list of start/end character offsets. We leverage the parser to figure the offsets out by parsing the label, and extract the
             offsets from the parser. *)

          (* A full let binding with the type text is needed for the parser to be able to parse it.  *)
          let label_prefix = "let fn: " in
          let label_prefix_len = String.length label_prefix in
          let fn_type_str = Shared.type_to_string type_expr in
          let type_str_for_parser = label_prefix ^ fn_type_str in
          let {Res_driver.parsetree = signature} =
            Res_driver.parse_interface_from_source ~for_printer:false
              ~display_filename:"<missing-file>" ~source:type_str_for_parser
          in

          let parameters =
            extract_parameters ~signature ~type_str_for_parser ~label_prefix_len
          in
          if debug then
            Printf.printf "extracted params: \n%s\n"
              (parameters
              |> List.map (fun (_, start, end_) ->
                     String.sub fn_type_str start (end_ - start))
              |> list);

          (* Figure out the active parameter *)
          let active_parameter = find_active_parameter ~arg_at_cursor ~args in

          let param_unlabelled_arg_count = ref 0 in
          let parameters_information =
            parameters
            |> List.map (fun (arg_label, start, end_) ->
                   let param_arg_count = !param_unlabelled_arg_count in
                   param_unlabelled_arg_count := param_arg_count + 1;
                   let unlabelled_arg_count = ref 0 in
                   let documentation =
                     match
                       args
                       |> List.find_opt (fun (lbl, _) ->
                              let arg_count = !unlabelled_arg_count in
                              unlabelled_arg_count := arg_count + 1;
                              match (lbl, arg_label) with
                              | ( Asttypes.Optional {txt = l1},
                                  Asttypes.Optional {txt = l2} )
                                when l1 = l2 ->
                                true
                              | Labelled {txt = l1}, Labelled {txt = l2}
                                when l1 = l2 ->
                                true
                              | Nolabel, Nolabel
                                when param_arg_count = arg_count ->
                                true
                              | _ -> false)
                     with
                     | None ->
                       Lsp.Types.MarkupContent.create
                         ~kind:Lsp.Types.MarkupKind.Markdown ~value:""
                     | Some (_, label_typ_expr) ->
                       Lsp.Types.MarkupContent.create
                         ~kind:Lsp.Types.MarkupKind.Markdown
                         ~value:
                           (docs_for_label ~supports_markdown_links ~file ~state
                              ~package label_typ_expr)
                   in
                   Lsp.Types.ParameterInformation.create
                     ~label:(`Offset (start, end_))
                     ~documentation:(`MarkupContent documentation) ())
          in
          let signatures =
            Lsp.Types.SignatureInformation.create ~label:fn_type_str
              ~parameters:parameters_information
              ?documentation:
                (match List.nth_opt docstring 0 with
                | None -> None
                | Some docs ->
                  Some
                    (`MarkupContent
                       (Lsp.Types.MarkupContent.create
                          ~kind:Lsp.Types.MarkupKind.Markdown ~value:docs)))
              ~activeParameter:
                (match active_parameter with
                | None -> Some (-1)
                | active_parameter -> active_parameter)
              ()
          in
          let signature =
            Lsp.Types.SignatureHelp.create ~signatures:[signatures]
              ~activeParameter:
                (match active_parameter with
                | None -> Some (-1)
                | active_parameter -> active_parameter)
              ~activeSignature:0 ()
          in
          Some signature
        | _ -> None)
      | Some (_, ((`ConstructorExpr (lid, _) | `ConstructorPat (lid, _)) as cs))
        -> (
        if Debug.verbose () then
          Printf.printf "[signature_help] Found constructor!\n";
        match full with
        | None ->
          if Debug.verbose () then
            Printf.printf "[signature_help] Could not load cmt\n";
          None
        | Some full -> (
          let {file} = full in
          let env = Query_env.from_file file in
          let constructor_name = Longident.last lid.txt in
          match
            find_constructor_args ~full ~state ~env ~constructor_name
              {lid.loc with loc_start = lid.loc.loc_end}
          with
          | None ->
            if Debug.verbose () then
              Printf.printf "[signature_help] Did not find constructor '%s'\n"
                constructor_name;
            None
          | Some constructor ->
            let arg_parts =
              match constructor.args with
              | Args [] -> None
              | InlineRecord fields ->
                let offset = ref 0 in
                Some
                  (`InlineRecord
                     (fields
                     |> List.map (fun (field : field) ->
                            let start_offset = !offset in
                            let arg_text =
                              Printf.sprintf "%s%s: %s" field.fname.txt
                                (if field.optional then "?" else "")
                                (Shared.type_to_string
                                   (if field.optional then
                                      Utils.unwrap_if_option field.typ
                                    else field.typ))
                            in
                            let end_offset =
                              start_offset + String.length arg_text
                            in
                            offset := end_offset + String.length ", ";
                            (arg_text, field, (start_offset, end_offset)))))
              | Args [(typ, _)] ->
                Some
                  (`SingleArg
                     ( typ |> Shared.type_to_string,
                       docs_for_label ~file:full.file ~package:full.package
                         ~state ~supports_markdown_links typ ))
              | Args args ->
                let offset = ref 0 in
                Some
                  (`TupleArg
                     (args
                     |> List.map (fun (typ, _) ->
                            let start_offset = !offset in
                            let arg_text = typ |> Shared.type_to_string in
                            let end_offset =
                              start_offset + String.length arg_text
                            in
                            offset := end_offset + String.length ", ";
                            ( arg_text,
                              docs_for_label ~file:full.file
                                ~package:full.package ~supports_markdown_links
                                ~state typ,
                              (start_offset, end_offset) ))))
            in
            let label =
              constructor.name ^ "("
              ^ (match arg_parts with
                | None -> ""
                | Some (`InlineRecord fields) ->
                  "{"
                  ^ (fields
                    |> List.map (fun (arg_text, _, _) -> arg_text)
                    |> String.concat ", ")
                  ^ "}"
                | Some (`SingleArg (arg, _)) -> arg
                | Some (`TupleArg items) ->
                  items
                  |> List.map (fun (arg_text, _, _) -> arg_text)
                  |> String.concat ", ")
              ^ ")"
            in
            let active_parameter =
              match cs with
              | `ConstructorExpr (_, {pexp_desc = Pexp_tuple items}) -> (
                let idx = ref 0 in
                let tuple_item_with_cursor =
                  items
                  |> List.find_map (fun (item : Parsetree.expression) ->
                         let current_index = !idx in
                         idx := current_index + 1;
                         if loc_has_cursor item.pexp_loc then Some current_index
                         else None)
                in
                match tuple_item_with_cursor with
                | None -> -1
                | Some i -> i)
              | `ConstructorExpr (_, {pexp_desc = Pexp_record (fields, _)}) -> (
                let field_name_with_cursor =
                  fields
                  |> List.find_map
                       (fun
                         ({lid = {loc; txt}; x = expr} :
                           Parsetree.expression Parsetree.record_element)
                       ->
                         if
                           pos_before_cursor >= Pos.of_lexing loc.loc_start
                           && pos_before_cursor
                              <= Pos.of_lexing expr.pexp_loc.loc_end
                         then Some (Longident.last txt)
                         else None)
                in
                match (field_name_with_cursor, arg_parts) with
                | Some field_name, Some (`InlineRecord fields) ->
                  let idx = ref 0 in
                  let field_index = ref (-1) in
                  fields
                  |> List.iter (fun (_, field, _) ->
                         idx := !idx + 1;
                         let current_index = !idx in
                         if field_name = field.fname.txt then
                           field_index := current_index
                         else ());
                  !field_index
                | _ -> -1)
              | `ConstructorExpr (_, expr) when loc_has_cursor expr.pexp_loc ->
                0
              | `ConstructorPat (_, {ppat_desc = Ppat_tuple items}) -> (
                let idx = ref 0 in
                let tuple_item_with_cursor =
                  items
                  |> List.find_map (fun (item : Parsetree.pattern) ->
                         let current_index = !idx in
                         idx := current_index + 1;
                         if loc_has_cursor item.ppat_loc then Some current_index
                         else None)
                in
                match tuple_item_with_cursor with
                | None -> -1
                | Some i -> i)
              | `ConstructorPat (_, {ppat_desc = Ppat_record (fields, _)}) -> (
                let field_name_with_cursor =
                  fields
                  |> List.find_map
                       (fun
                         ({lid = {loc; txt}; x = pat} :
                           Parsetree.pattern Parsetree.record_element)
                       ->
                         if
                           pos_before_cursor >= Pos.of_lexing loc.loc_start
                           && pos_before_cursor
                              <= Pos.of_lexing pat.ppat_loc.loc_end
                         then Some (Longident.last txt)
                         else None)
                in
                match (field_name_with_cursor, arg_parts) with
                | Some field_name, Some (`InlineRecord fields) ->
                  let idx = ref 0 in
                  let field_index = ref (-1) in
                  fields
                  |> List.iter (fun (_, field, _) ->
                         idx := !idx + 1;
                         let current_index = !idx in
                         if field_name = field.fname.txt then
                           field_index := current_index
                         else ());
                  !field_index
                | _ -> -1)
              | `ConstructorPat (_, pat) when loc_has_cursor pat.ppat_loc -> 0
              | _ -> -1
            in

            let constructor_name_length = String.length constructor.name in
            let params =
              match arg_parts with
              | None -> []
              | Some (`SingleArg (_, docstring)) ->
                [
                  Lsp.Types.ParameterInformation.create
                    ~label:
                      (`Offset
                         (constructor_name_length + 1, String.length label - 1))
                    ~documentation:
                      (`MarkupContent
                         (Lsp.Types.MarkupContent.create
                            ~kind:Lsp.Types.MarkupKind.Markdown ~value:docstring))
                    ();
                ]
              | Some (`InlineRecord fields) ->
                let base_offset = constructor_name_length + 2 in
                (* Account for leading '({' *)
                Lsp.Types.ParameterInformation.create
                  ~label:(`Offset (0, 0))
                  ~documentation:
                    (`MarkupContent
                       (Lsp.Types.MarkupContent.create
                          ~kind:Lsp.Types.MarkupKind.Markdown ~value:""))
                  ()
                :: (fields
                   |> List.map (fun (_, (field : field), (start, end_)) ->
                          Lsp.Types.ParameterInformation.create
                            ~label:
                              (`Offset (base_offset + start, base_offset + end_))
                            ~documentation:
                              (`MarkupContent
                                 (Lsp.Types.MarkupContent.create
                                    ~kind:Lsp.Types.MarkupKind.Markdown
                                    ~value:
                                      (field.docstring |> String.concat "\n")))
                            ()))
              | Some (`TupleArg items) ->
                (* Account for leading '(' *)
                let base_offset = constructor_name_length + 1 in
                items
                |> List.map (fun (_, docstring, (start, end_)) ->
                       Lsp.Types.ParameterInformation.create
                         ~label:
                           (`Offset (base_offset + start, base_offset + end_))
                         ~documentation:
                           (`MarkupContent
                              (Lsp.Types.MarkupContent.create
                                 ~kind:Lsp.Types.MarkupKind.Markdown
                                 ~value:docstring))
                         ())
            in
            let signatures =
              Lsp.Types.SignatureInformation.create ~label ~parameters:params
                ?documentation:
                  (match List.nth_opt constructor.docstring 0 with
                  | None -> None
                  | Some docs ->
                    Some
                      (`MarkupContent
                         (Lsp.Types.MarkupContent.create
                            ~kind:Lsp.Types.MarkupKind.Markdown ~value:docs)))
                ~activeParameter:(Some active_parameter) ()
            in
            let signature =
              Lsp.Types.SignatureHelp.create ~signatures:[signatures]
                ~activeParameter:(Some active_parameter) ~activeSignature:0 ()
            in
            Some signature))
      | _ -> None))
