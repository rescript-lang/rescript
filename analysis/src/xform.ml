(** Code transformations using the parser/printer and ast operations *)

let is_braced_expr = Res_parsetree_viewer.is_braced_expr

let extract_type_from_expr expr ~debug ~source ~kind_file ~full ~pos =
  match
    expr.Parsetree.pexp_loc
    |> Completion_front_end.find_type_of_expression_at_loc ~debug ~source
         ~kind_file
         ~pos_cursor:(Pos.of_lexing expr.Parsetree.pexp_loc.loc_start)
  with
  | Some (completable, scope) -> (
    let env = Shared_types.Query_env.from_file full.Shared_types.file in
    let completions =
      completable
      |> Completion_back_end.process_completable ~debug ~full ~pos ~scope ~env
           ~for_hover:true
    in
    let raw_opens = Scope.get_raw_opens scope in
    match completions with
    | {env} :: _ -> (
      let opens =
        Completion_back_end.get_opens ~debug ~raw_opens ~package:full.package
          ~env
      in
      match
        Completion_back_end.completions_get_completion_type2 ~debug ~full
          ~raw_opens ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let extracted_type =
          match typ with
          | ExtractedType t -> Some t
          | TypeExpr t ->
            Type_utils.extract_type t ~env ~package:full.package
            |> Type_utils.get_extracted_type
        in
        extracted_type
      | None -> None)
    | _ -> None)
  | _ -> None

module If_then_else = struct
  (* Convert if-then-else to switch *)

  let rec list_to_pat ~item_to_pat = function
    | [] -> Some []
    | x :: x_list -> (
      match (item_to_pat x, list_to_pat ~item_to_pat x_list) with
      | Some p, Some p_list -> Some (p :: p_list)
      | _ -> None)

  let rec exp_to_pat (exp : Parsetree.expression) =
    let mk_pat ppat_desc =
      Ast_helper.Pat.mk ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes ppat_desc
    in
    match exp.pexp_desc with
    | Pexp_construct (lid, None) -> Some (mk_pat (Ppat_construct (lid, None)))
    | Pexp_construct (lid, Some e1) -> (
      match exp_to_pat e1 with
      | None -> None
      | Some p1 -> Some (mk_pat (Ppat_construct (lid, Some p1))))
    | Pexp_variant (label, None) -> Some (mk_pat (Ppat_variant (label, None)))
    | Pexp_variant (label, Some e1) -> (
      match exp_to_pat e1 with
      | None -> None
      | Some p1 -> Some (mk_pat (Ppat_variant (label, Some p1))))
    | Pexp_constant c -> Some (mk_pat (Ppat_constant c))
    | Pexp_tuple e_list -> (
      match list_to_pat ~item_to_pat:exp_to_pat e_list with
      | None -> None
      | Some pat_list -> Some (mk_pat (Ppat_tuple pat_list)))
    | Pexp_record (items, None) -> (
      let item_to_pat {Parsetree.lid; x = e; opt} =
        match exp_to_pat e with
        | None -> None
        | Some p -> Some {Parsetree.lid; x = p; opt}
      in
      match list_to_pat ~item_to_pat items with
      | None -> None
      | Some pat_items -> Some (mk_pat (Ppat_record (pat_items, Closed))))
    | Pexp_record (_, Some _) -> None
    | _ -> None

  let mk_iterator ~pos ~changed =
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let new_exp =
        match e.pexp_desc with
        | Pexp_ifthenelse
            ( {
                pexp_desc =
                  Pexp_apply
                    {
                      funct =
                        {
                          pexp_desc =
                            Pexp_ident
                              {txt = Longident.Lident (("==" | "!=") as op)};
                        };
                      args = [(Nolabel, arg1); (Nolabel, arg2)];
                    };
              },
              e1,
              Some e2 )
          when Loc.has_pos ~pos e.pexp_loc -> (
          let e1, e2 = if op = "==" then (e1, e2) else (e2, e1) in
          let mk_match ~arg ~pat =
            let cases =
              [
                Ast_helper.Exp.case pat e1;
                Ast_helper.Exp.case (Ast_helper.Pat.any ()) e2;
              ]
            in
            Ast_helper.Exp.match_ ~loc:e.pexp_loc ~attrs:e.pexp_attributes arg
              cases
          in

          match exp_to_pat arg2 with
          | None -> (
            match exp_to_pat arg1 with
            | None -> None
            | Some pat1 ->
              let new_exp = mk_match ~arg:arg2 ~pat:pat1 in
              Some new_exp)
          | Some pat2 ->
            let new_exp = mk_match ~arg:arg1 ~pat:pat2 in
            Some new_exp)
        | _ -> None
      in
      match new_exp with
      | Some new_exp -> changed := Some new_exp
      | None -> Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr}

  let xform ~pos ~code_actions ~print_expr ~path structure =
    let changed = ref None in
    let iterator = mk_iterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some new_expr ->
      let range = Loc.range_of_loc new_expr.pexp_loc in
      let new_text = print_expr ~range new_expr in
      let code_action =
        Code_actions.make ~title:"Replace with switch" ~kind:RefactorRewrite
          ~uri:path ~new_text ~range
      in
      code_actions := code_action :: !code_actions
end

module Module_to_file = struct
  let mk_iterator ~pos ~changed ~path ~print_standalone_structure =
    let structure_item (iterator : Ast_iterator.iterator)
        (structure_item : Parsetree.structure_item) =
      (match structure_item.pstr_desc with
      | Pstr_module
          {pmb_loc; pmb_name; pmb_expr = {pmod_desc = Pmod_structure structure}}
        when structure_item.pstr_loc |> Loc.has_pos ~pos ->
        let range = Loc.range_of_loc structure_item.pstr_loc in
        let new_text_in_current_file = "" in
        let text_for_extracted_file =
          print_standalone_structure ~loc:pmb_loc structure
        in
        let module_name = pmb_name.txt in
        let new_file_path =
          Filename.concat (Filename.dirname path) module_name ^ ".res"
        in
        let uri = Uri.from_string new_file_path in
        let document_changes =
          [
            `CreateFile
              (Lsp.Types.CreateFile.create ~uri
                 ~options:
                   (Lsp.Types.CreateFileOptions.create ~overwrite:false
                      ~ignoreIfExists:true ())
                 ());
            `TextDocumentEdit
              (Lsp.Types.TextDocumentEdit.create
                 ~edits:
                   [
                     `TextEdit
                       (Lsp.Types.TextEdit.create ~range
                          ~newText:text_for_extracted_file);
                   ]
                 ~textDocument:
                   (Lsp.Types.OptionalVersionedTextDocumentIdentifier.create
                      ~uri ()));
            `TextDocumentEdit
              (Lsp.Types.TextDocumentEdit.create
                 ~edits:
                   [
                     `TextEdit
                       (Lsp.Types.TextEdit.create ~range
                          ~newText:new_text_in_current_file);
                   ]
                 ~textDocument:
                   (Lsp.Types.OptionalVersionedTextDocumentIdentifier.create
                      ~uri:(Uri.from_string path) ()));
          ]
        in
        changed :=
          Some
            (Code_actions.make_with_document_changes
               ~title:
                 (Printf.sprintf "Extract local module \"%s\" to file \"%s\""
                    module_name (module_name ^ ".res"))
               ~kind:RefactorRewrite ~document_changes);
        ()
      | _ -> ());
      Ast_iterator.default_iterator.structure_item iterator structure_item
    in

    {Ast_iterator.default_iterator with structure_item}

  let xform ~pos ~code_actions ~path ~print_standalone_structure structure =
    let changed = ref None in
    let iterator =
      mk_iterator ~pos ~path ~changed ~print_standalone_structure
    in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some code_action -> code_actions := code_action :: !code_actions
end

module Add_braces_to_fn = struct
  (* Add braces to fn without braces *)

  let mk_iterator ~pos ~changed =
    (* While iterating the AST, keep info on which structure item we are in.
       Printing from the structure item, rather than the body of the function,
       gives better local pretty printing *)
    let current_structure_item = ref None in

    let structure_item (iterator : Ast_iterator.iterator)
        (item : Parsetree.structure_item) =
      let saved = !current_structure_item in
      current_structure_item := Some item;
      Ast_iterator.default_iterator.structure_item iterator item;
      current_structure_item := saved
    in
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let braces_attribute =
        let loc =
          {
            Location.none with
            loc_start = Lexing.dummy_pos;
            loc_end =
              {
                Lexing.dummy_pos with
                pos_lnum = Lexing.dummy_pos.pos_lnum + 1 (* force line break *);
              };
          }
        in
        (Location.mkloc "res.braces" loc, Parsetree.PStr [])
      in
      let is_function = function
        | {Parsetree.pexp_desc = Pexp_fun _} -> true
        | _ -> false
      in
      (match e.pexp_desc with
      | Pexp_fun {rhs = body_expr}
        when Loc.has_pos ~pos body_expr.pexp_loc
             && is_braced_expr body_expr = false
             && is_function body_expr = false ->
        body_expr.pexp_attributes <-
          braces_attribute :: body_expr.pexp_attributes;
        changed := !current_structure_item
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr; structure_item}

  let xform ~pos ~code_actions ~path ~print_structure_item structure =
    let changed = ref None in
    let iterator = mk_iterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some new_structure_item ->
      let range = Loc.range_of_loc new_structure_item.pstr_loc in
      let new_text = print_structure_item ~range new_structure_item in
      let code_action =
        Code_actions.make ~title:"Add braces to function" ~kind:RefactorRewrite
          ~uri:path ~new_text ~range
      in
      code_actions := code_action :: !code_actions
end

module Add_type_annotation = struct
  (* Add type annotation to value declaration *)

  type annotation = Plain | WithParens

  let mk_iterator ~pos ~result =
    let process_pattern ?(is_unlabeled_only_arg = false)
        (pat : Parsetree.pattern) =
      match pat.ppat_desc with
      | Ppat_var {loc} when Loc.has_pos ~pos loc ->
        result := Some (if is_unlabeled_only_arg then WithParens else Plain)
      | _ -> ()
    in
    let rec process_function ~arg_num (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_fun {arg_label; lhs = pat; rhs = e} ->
        let is_unlabeled_only_arg =
          arg_num = 1 && arg_label = Nolabel
          &&
          match e.pexp_desc with
          | Pexp_fun _ -> false
          | _ -> true
        in
        process_pattern ~is_unlabeled_only_arg pat;
        process_function ~arg_num:(arg_num + 1) e
      | _ -> ()
    in
    let structure_item (iterator : Ast_iterator.iterator)
        (si : Parsetree.structure_item) =
      match si.pstr_desc with
      | Pstr_value (_recFlag, bindings) ->
        let process_binding (vb : Parsetree.value_binding) =
          (* Can't add a type annotation to a jsx component, or the compiler crashes *)
          let is_jsx_component = Utils.is_jsx_component vb in
          if not is_jsx_component then process_pattern vb.pvb_pat;
          process_function vb.pvb_expr
        in
        bindings |> List.iter (process_binding ~arg_num:1);
        Ast_iterator.default_iterator.structure_item iterator si
      | _ -> Ast_iterator.default_iterator.structure_item iterator si
    in
    {Ast_iterator.default_iterator with structure_item}

  let xform ~path ~pos ~full ~structure ~code_actions ~debug =
    let result = ref None in
    let iterator = mk_iterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some annotation -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> ()
      | Some loc_item -> (
        match loc_item.loc_type with
        | Typed (name, typ, _) ->
          let range, new_text =
            match annotation with
            | Plain ->
              ( Loc.range_of_loc
                  {loc_item.loc with loc_start = loc_item.loc.loc_end},
                ": " ^ (typ |> Shared.type_to_string) )
            | WithParens ->
              ( Loc.range_of_loc loc_item.loc,
                "(" ^ name ^ ": " ^ (typ |> Shared.type_to_string) ^ ")" )
          in
          let code_action =
            Code_actions.make ~title:"Add type annotation" ~kind:RefactorRewrite
              ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions
        | _ -> ()))
end

module Expand_catch_all_for_variants = struct
  let mk_iterator ~pos ~result =
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      (if e.pexp_loc |> Loc.has_pos ~pos then
         match e.pexp_desc with
         | Pexp_match (switch_expr, cases) -> (
           let catch_all_case =
             cases
             |> List.find_opt (fun (c : Parsetree.case) ->
                    match c with
                    | {pc_lhs = {ppat_desc = Ppat_any}} -> true
                    | _ -> false)
           in
           match catch_all_case with
           | None -> ()
           | Some catch_all_case ->
             result := Some (switch_expr, catch_all_case, cases))
         | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in
    {Ast_iterator.default_iterator with expr}

  let xform ~source ~kind_file ~path ~pos ~full ~structure ~code_actions ~debug
      =
    let result = ref None in
    let iterator = mk_iterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some (switch_expr, catch_all_case, cases) -> (
      if Debug.verbose () then
        print_endline
          "[codeAction - ExpandCatchAllForVariants] Found target switch";
      let rec find_all_constructor_names
          ?(mode : [`option | `default] = `default) ?(constructor_names = [])
          (p : Parsetree.pattern) =
        match p.ppat_desc with
        | Ppat_construct ({txt = Lident "Some"}, Some payload)
          when mode = `option ->
          find_all_constructor_names ~mode ~constructor_names payload
        | Ppat_construct ({txt}, _) -> Longident.last txt :: constructor_names
        | Ppat_variant (name, _) -> name :: constructor_names
        | Ppat_or (a, b) ->
          find_all_constructor_names ~mode ~constructor_names a
          @ find_all_constructor_names ~mode ~constructor_names b
          @ constructor_names
        | _ -> constructor_names
      in
      let get_current_constructor_names ?mode cases =
        cases
        |> List.map (fun (c : Parsetree.case) ->
               if Option.is_some c.pc_guard then []
               else find_all_constructor_names ?mode c.pc_lhs)
        |> List.flatten
      in
      let current_constructor_names = get_current_constructor_names cases in
      match
        switch_expr
        |> extract_type_from_expr ~debug ~source ~kind_file ~full
             ~pos:(Pos.of_lexing switch_expr.pexp_loc.loc_end)
      with
      | Some (Tvariant {constructors}) ->
        let missing_constructors =
          constructors
          |> List.filter (fun (c : Shared_types.Constructor.t) ->
                 current_constructor_names |> List.mem c.cname.txt = false)
        in
        if List.length missing_constructors > 0 then
          let new_text =
            missing_constructors
            |> List.map (fun (c : Shared_types.Constructor.t) ->
                   c.cname.txt
                   ^
                   match c.args with
                   | Args [] -> ""
                   | Args _ | InlineRecord _ -> "(_)")
            |> String.concat " | "
          in
          let range = Loc.range_of_loc catch_all_case.pc_lhs.ppat_loc in
          let code_action =
            Code_actions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
              ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions
        else ()
      | Some (Tpolyvariant {constructors}) ->
        let missing_constructors =
          constructors
          |> List.filter (fun (c : Shared_types.poly_variant_constructor) ->
                 current_constructor_names |> List.mem c.name = false)
        in
        if List.length missing_constructors > 0 then
          let new_text =
            missing_constructors
            |> List.map (fun (c : Shared_types.poly_variant_constructor) ->
                   Res_printer.polyvar_ident_to_string c.name
                   ^
                   match c.args with
                   | [] -> ""
                   | _ -> "(_)")
            |> String.concat " | "
          in
          let range = Loc.range_of_loc catch_all_case.pc_lhs.ppat_loc in
          let code_action =
            Code_actions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
              ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions
        else ()
      | Some (Toption (env, inner_type)) -> (
        if Debug.verbose () then
          print_endline
            "[codeAction - ExpandCatchAllForVariants] Found option type";
        let inner_type =
          match inner_type with
          | ExtractedType t -> Some t
          | TypeExpr t -> (
            match Type_utils.extract_type ~env ~package:full.package t with
            | None -> None
            | Some (t, _) -> Some t)
        in
        match inner_type with
        | Some ((Tvariant _ | Tpolyvariant _) as variant) ->
          let current_constructor_names =
            get_current_constructor_names ~mode:`option cases
          in
          let has_none_case =
            cases
            |> List.exists (fun (c : Parsetree.case) ->
                   match c.pc_lhs.ppat_desc with
                   | Ppat_construct ({txt = Lident "None"}, _) -> true
                   | _ -> false)
          in
          let missing_constructors =
            match variant with
            | Tvariant {constructors} ->
              constructors
              |> List.filter_map (fun (c : Shared_types.Constructor.t) ->
                     if
                       current_constructor_names |> List.mem c.cname.txt = false
                     then
                       Some
                         ( c.cname.txt,
                           match c.args with
                           | Args [] -> false
                           | _ -> true )
                     else None)
            | Tpolyvariant {constructors} ->
              constructors
              |> List.filter_map
                   (fun (c : Shared_types.poly_variant_constructor) ->
                     if current_constructor_names |> List.mem c.name = false
                     then
                       Some
                         ( Res_printer.polyvar_ident_to_string c.name,
                           match c.args with
                           | [] -> false
                           | _ -> true )
                     else None)
            | _ -> []
          in
          if List.length missing_constructors > 0 || not has_none_case then
            let new_text =
              "Some("
              ^ (missing_constructors
                |> List.map (fun (name, has_args) ->
                       name ^ if has_args then "(_)" else "")
                |> String.concat " | ")
              ^ ")"
            in
            let new_text =
              if has_none_case then new_text else new_text ^ " | None"
            in
            let range = Loc.range_of_loc catch_all_case.pc_lhs.ppat_loc in
            let code_action =
              Code_actions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
                ~uri:path ~new_text ~range
            in
            code_actions := code_action :: !code_actions
          else ()
        | _ -> ())
      | _ -> ())
end

module Exhaustive_switch = struct
  (* Expand expression to be an exhaustive switch of the underlying value *)
  type pos_type = Single of Pos.t | Range of Pos.t * Pos.t

  type completion_type =
    | Switch of {
        pos: Pos.t;
        switch_expr: Parsetree.expression;
        completion_expr: Parsetree.expression;
      }
    | Selection of {expr: Parsetree.expression}

  let mk_iterator_single ~pos ~result =
    let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression) =
      (match exp.pexp_desc with
      | Pexp_ident _ when Loc.has_pos_inclusive_end ~pos exp.pexp_loc ->
        (* Exhaustive switch for having the cursor on an identifier. *)
        result := Some (Selection {expr = exp})
      | Pexp_match (completion_expr, [])
        when Loc.has_pos_inclusive_end ~pos exp.pexp_loc ->
        (* No cases means there's no `|` yet in the switch, so `switch someExpr` *)
        result := Some (Switch {pos; switch_expr = exp; completion_expr})
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator exp
    in
    {Ast_iterator.default_iterator with expr}

  let mk_iterator_range ~start_pos ~end_pos ~found_selection =
    let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression) =
      let exp_start_pos = Pos.of_lexing exp.pexp_loc.loc_start in
      let exp_end_pos = Pos.of_lexing exp.pexp_loc.loc_end in

      (if exp_start_pos = start_pos then
         match !found_selection with
         | None, end_expr -> found_selection := (Some exp, end_expr)
         | _ -> ());

      (if exp_end_pos = end_pos then
         match !found_selection with
         | start_exp, _ -> found_selection := (start_exp, Some exp));

      Ast_iterator.default_iterator.expr iterator exp
    in
    {Ast_iterator.default_iterator with expr}

  let xform ~print_expr ~path ~source ~kind_file ~pos ~full ~structure
      ~code_actions ~debug =
    (* TODO: Adapt to '(' as leading/trailing character (skip one col, it's not included in the AST) *)
    let result = ref None in
    let found_selection = ref (None, None) in
    let iterator =
      match pos with
      | Single pos -> mk_iterator_single ~pos ~result
      | Range (start_pos, end_pos) ->
        mk_iterator_range ~start_pos ~end_pos ~found_selection
    in
    iterator.structure iterator structure;
    (match !found_selection with
    | Some start_exp, Some end_exp ->
      if debug then
        Printf.printf "found selection: %s -> %s\n"
          (Loc.to_string start_exp.pexp_loc)
          (Loc.to_string end_exp.pexp_loc);
      result := Some (Selection {expr = start_exp})
    | _ -> ());
    match !result with
    | None -> ()
    | Some (Selection {expr}) -> (
      match
        expr
        |> extract_type_from_expr ~debug ~source ~kind_file ~full
             ~pos:(Pos.of_lexing expr.pexp_loc.loc_start)
      with
      | None -> ()
      | Some extracted_type -> (
        let open Type_utils.Codegen in
        let exhaustive_switch =
          extracted_type_to_exhaustive_cases
            ~env:(Shared_types.Query_env.from_file full.file)
            ~full extracted_type
        in
        match exhaustive_switch with
        | None -> ()
        | Some cases ->
          let range = Loc.range_of_loc expr.pexp_loc in
          let new_text =
            print_expr ~range {expr with pexp_desc = Pexp_match (expr, cases)}
          in
          let code_action =
            Code_actions.make ~title:"Exhaustive switch" ~kind:RefactorRewrite
              ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions))
    | Some (Switch {switch_expr; completion_expr; pos}) -> (
      match
        completion_expr
        |> extract_type_from_expr ~debug ~source ~kind_file ~full ~pos
      with
      | None -> ()
      | Some extracted_type -> (
        let open Type_utils.Codegen in
        let exhaustive_switch =
          extracted_type_to_exhaustive_cases
            ~env:(Shared_types.Query_env.from_file full.file)
            ~full extracted_type
        in
        match exhaustive_switch with
        | None -> ()
        | Some cases ->
          let range = Loc.range_of_loc switch_expr.pexp_loc in
          let new_text =
            print_expr ~range
              {switch_expr with pexp_desc = Pexp_match (completion_expr, cases)}
          in
          let code_action =
            Code_actions.make ~title:"Exhaustive switch" ~kind:RefactorRewrite
              ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions))
end

module Add_doc_template = struct
  let create_template () =
    let doc_content = ["\n"; "\n"] in
    let expression =
      Ast_helper.Exp.constant
        (Parsetree.Pconst_string (String.concat "" doc_content, None))
    in
    let structure_item_desc = Parsetree.Pstr_eval (expression, []) in
    let structure_item = Ast_helper.Str.mk structure_item_desc in
    let attr_loc =
      {
        Location.none with
        loc_start = Lexing.dummy_pos;
        loc_end =
          {
            Lexing.dummy_pos with
            pos_lnum = Lexing.dummy_pos.pos_lnum (* force line break *);
          };
      }
    in
    (Location.mkloc "res.doc" attr_loc, Parsetree.PStr [structure_item])

  module Interface = struct
    let mk_iterator ~pos ~result =
      let signature_item (iterator : Ast_iterator.iterator)
          (item : Parsetree.signature_item) =
        match item.psig_desc with
        | Psig_value value_description as r
          when Loc.has_pos ~pos value_description.pval_loc
               && Process_attributes.find_doc_attribute
                    value_description.pval_attributes
                  = None ->
          result := Some (r, item.psig_loc)
        | Psig_type (_, hd :: _) as r
          when Loc.has_pos ~pos hd.ptype_loc
               && Process_attributes.find_doc_attribute hd.ptype_attributes
                  = None ->
          result := Some (r, item.psig_loc)
        | Psig_module {pmd_name = {loc}} as r ->
          if Loc.start loc = pos then result := Some (r, item.psig_loc)
          else Ast_iterator.default_iterator.signature_item iterator item
        | _ -> Ast_iterator.default_iterator.signature_item iterator item
      in
      {Ast_iterator.default_iterator with signature_item}

    let process_sig_value (value_desc : Parsetree.value_description) loc =
      let attr = create_template () in
      let new_value_binding =
        {value_desc with pval_attributes = attr :: value_desc.pval_attributes}
      in
      let signature_item_desc = Parsetree.Psig_value new_value_binding in
      Ast_helper.Sig.mk ~loc signature_item_desc

    let process_type_decl (typ : Parsetree.type_declaration) =
      let attr = create_template () in
      let new_type_declaration =
        {typ with ptype_attributes = attr :: typ.ptype_attributes}
      in
      new_type_declaration

    let process_mod_decl (mod_decl : Parsetree.module_declaration) loc =
      let attr = create_template () in
      let new_mod_decl =
        {mod_decl with pmd_attributes = attr :: mod_decl.pmd_attributes}
      in
      Ast_helper.Sig.mk ~loc (Parsetree.Psig_module new_mod_decl)

    let xform ~path ~pos ~code_actions ~signature ~print_signature_item =
      let result = ref None in
      let iterator = mk_iterator ~pos ~result in
      iterator.signature iterator signature;
      match !result with
      | Some (signature_item, loc) -> (
        let new_signature_item =
          match signature_item with
          | Psig_value value_desc ->
            Some (process_sig_value value_desc value_desc.pval_loc)
            (* Some loc *)
          | Psig_type (flag, hd :: tl) ->
            let new_first_type_decl = process_type_decl hd in
            Some
              (Ast_helper.Sig.mk ~loc
                 (Parsetree.Psig_type (flag, new_first_type_decl :: tl)))
          | Psig_module mod_decl -> Some (process_mod_decl mod_decl loc)
          | _ -> None
        in

        match new_signature_item with
        | Some signature_item ->
          let range = Loc.range_of_loc signature_item.psig_loc in
          let new_text = print_signature_item ~range signature_item in
          let code_action =
            Code_actions.make ~title:"Add Documentation template"
              ~kind:RefactorRewrite ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions
        | None -> ())
      | None -> ()
  end

  module Implementation = struct
    let mk_iterator ~pos ~result =
      let structure_item (iterator : Ast_iterator.iterator)
          (si : Parsetree.structure_item) =
        match si.pstr_desc with
        | Pstr_value (_, {pvb_pat = {ppat_loc}; pvb_attributes} :: _) as r
          when Loc.has_pos ~pos ppat_loc
               && Process_attributes.find_doc_attribute pvb_attributes = None ->
          result := Some (r, si.pstr_loc)
        | Pstr_primitive value_description as r
          when Loc.has_pos ~pos value_description.pval_loc
               && Process_attributes.find_doc_attribute
                    value_description.pval_attributes
                  = None ->
          result := Some (r, si.pstr_loc)
        | Pstr_module {pmb_name = {loc}} as r ->
          if Loc.start loc = pos then result := Some (r, si.pstr_loc)
          else Ast_iterator.default_iterator.structure_item iterator si
        | Pstr_type (_, hd :: _) as r
          when Loc.has_pos ~pos hd.ptype_loc
               && Process_attributes.find_doc_attribute hd.ptype_attributes
                  = None ->
          result := Some (r, si.pstr_loc)
        | _ -> Ast_iterator.default_iterator.structure_item iterator si
      in
      {Ast_iterator.default_iterator with structure_item}

    let process_value_binding (value_binding : Parsetree.value_binding) =
      let attr = create_template () in
      let new_value_binding =
        {
          value_binding with
          pvb_attributes = attr :: value_binding.pvb_attributes;
        }
      in
      new_value_binding

    let process_primitive (value_desc : Parsetree.value_description) loc =
      let attr = create_template () in
      let new_value_desc =
        {value_desc with pval_attributes = attr :: value_desc.pval_attributes}
      in
      Ast_helper.Str.primitive ~loc new_value_desc

    let process_module_binding (mod_bind : Parsetree.module_binding) loc =
      let attr = create_template () in
      let new_mod_binding =
        {mod_bind with pmb_attributes = attr :: mod_bind.pmb_attributes}
      in
      Ast_helper.Str.module_ ~loc new_mod_binding

    let xform ~pos ~code_actions ~path ~print_structure_item ~structure =
      let result = ref None in
      let iterator = mk_iterator ~pos ~result in
      iterator.structure iterator structure;
      match !result with
      | None -> ()
      | Some (structure_item, loc) -> (
        let new_structure_item =
          match structure_item with
          | Pstr_value (flag, hd :: tl) ->
            let new_value_binding = process_value_binding hd in
            Some
              (Ast_helper.Str.mk ~loc
                 (Parsetree.Pstr_value (flag, new_value_binding :: tl)))
          | Pstr_primitive value_desc -> Some (process_primitive value_desc loc)
          | Pstr_module mod_bind -> Some (process_module_binding mod_bind loc)
          | Pstr_type (flag, hd :: tl) ->
            let new_first_type_decl = Interface.process_type_decl hd in
            Some
              (Ast_helper.Str.mk ~loc
                 (Parsetree.Pstr_type (flag, new_first_type_decl :: tl)))
          | _ -> None
        in

        match new_structure_item with
        | Some structure_item ->
          let range = Loc.range_of_loc structure_item.pstr_loc in
          let new_text = print_structure_item ~range structure_item in
          let code_action =
            Code_actions.make ~title:"Add Documentation template"
              ~kind:RefactorRewrite ~uri:path ~new_text ~range
          in
          code_actions := code_action :: !code_actions
        | None -> ())
  end
end

let parse_implementation ~source =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsing_engine.parse_implementation_from_source
      ~for_printer:false ~source
  in
  let filter_comments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      Loc.has_pos ~pos:(Loc.start (Res_comment.loc comment)) loc
    in
    comments |> List.filter filter
  in
  let print_expr ~(range : Lsp.Types.Range.t) (expr : Parsetree.expression) =
    let structure = [Ast_helper.Str.eval ~loc:expr.pexp_loc expr] in
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filter_comments ~loc:expr.pexp_loc)
    |> Utils.indent range.start.character
  in
  let print_structure_item ~(range : Lsp.Types.Range.t)
      (item : Parsetree.structure_item) =
    let structure = [item] in
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filter_comments ~loc:item.pstr_loc)
    |> Utils.indent range.start.character
  in
  let print_standalone_structure ~(loc : Location.t) structure =
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filter_comments ~loc)
  in
  (structure, print_expr, print_structure_item, print_standalone_structure)

let parse_interface ~source =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsing_engine.parse_interface_from_source ~for_printer:false
      ~source
  in
  let filter_comments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      Loc.has_pos ~pos:(Loc.start (Res_comment.loc comment)) loc
    in
    comments |> List.filter filter
  in
  let print_signature_item ~(range : Lsp.Types.Range.t)
      (item : Parsetree.signature_item) =
    let signature_item = [item] in
    signature_item
    |> Res_printer.print_interface
         ~comments:(comments |> filter_comments ~loc:item.psig_loc)
    |> Utils.indent range.start.character
  in
  (structure, print_signature_item)

let extract_code_actions ~path ~start_pos ~end_pos ~source ~kind_file ~debug =
  let pos = start_pos in
  let code_actions = ref [] in
  match kind_file with
  | Files.Res ->
    let structure, print_expr, print_structure_item, print_standalone_structure
        =
      parse_implementation ~source
    in
    If_then_else.xform ~pos ~code_actions ~print_expr ~path structure;
    Module_to_file.xform ~pos ~code_actions ~path ~print_standalone_structure
      structure;
    Add_braces_to_fn.xform ~pos ~code_actions ~path ~print_structure_item
      structure;
    Add_doc_template.Implementation.xform ~pos ~code_actions ~path
      ~print_structure_item ~structure;

    (* This Code Action needs type info *)
    let () =
      match Cmt.load_full_cmt_from_path ~path with
      | Some full ->
        Add_type_annotation.xform ~path ~pos ~full ~structure ~code_actions
          ~debug;
        Expand_catch_all_for_variants.xform ~path ~source ~kind_file ~pos ~full
          ~structure ~code_actions ~debug;
        Exhaustive_switch.xform ~print_expr ~path ~source ~kind_file
          ~pos:
            (if start_pos = end_pos then Single start_pos
             else Range (start_pos, end_pos))
          ~full ~structure ~code_actions ~debug
      | None -> ()
    in

    !code_actions
  | Resi ->
    let signature, print_signature_item = parse_interface ~source in
    Add_doc_template.Interface.xform ~pos ~code_actions ~path ~signature
      ~print_signature_item;
    !code_actions
  | Other -> []
