open SharedTypes

let find_arg_completables ~(args : arg list) ~end_pos ~pos_before_cursor
    ~(context_path : Completable.context_path) ~pos_after_fun_expr
    ~first_char_before_cursor_no_white ~char_before_cursor ~is_piped_expr =
  let fn_has_cursor =
    pos_after_fun_expr <= pos_before_cursor && pos_before_cursor < end_pos
  in
  let all_names =
    List.fold_right
      (fun arg all_labels ->
        match arg with
        | {label = Some labelled} -> labelled.name :: all_labels
        | {label = None} -> all_labels)
      args []
  in
  let unlabelled_count = ref (if is_piped_expr then 1 else 0) in
  let some_arg_had_empty_expr_loc = ref false in
  let rec loop args =
    match args with
    | {label = Some labelled; exp} :: rest ->
      if
        labelled.pos_start <= pos_before_cursor
        && pos_before_cursor < labelled.pos_end
      then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] Completing named arg #2";
        Some (Completable.CnamedArg (context_path, labelled.name, all_names)))
      else if exp.pexp_loc |> Loc.has_pos ~pos:pos_before_cursor then (
        if Debug.verbose () then
          print_endline
            "[findArgCompletables] Completing in the assignment of labelled \
             argument";
        match
          CompletionExpressions.traverse_expr exp ~expr_path:[]
            ~pos:pos_before_cursor ~first_char_before_cursor_no_white
        with
        | None -> None
        | Some (prefix, nested) ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] Completing for labelled argument value";
          Some
            (Cexpression
               {
                 context_path =
                   CArgument
                     {
                       function_context_path = context_path;
                       argument_label = Labelled labelled.name;
                     };
                 prefix;
                 nested = List.rev nested;
               }))
      else if CompletionExpressions.is_expr_hole exp then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] found exprhole";
        Some
          (Cexpression
             {
               context_path =
                 CArgument
                   {
                     function_context_path = context_path;
                     argument_label = Labelled labelled.name;
                   };
               prefix = "";
               nested = [];
             }))
      else loop rest
    | {label = None; exp} :: rest ->
      if Debug.verbose () then
        Printf.printf "[findArgCompletable] unlabelled arg expr is: %s \n"
          (DumpAst.print_expr_item ~pos:pos_before_cursor ~indentation:0 exp);

      (* Track whether there was an arg with an empty loc (indicates parser error)*)
      if CursorPosition.loc_is_empty exp.pexp_loc ~pos:pos_before_cursor then
        some_arg_had_empty_expr_loc := true;

      if Res_parsetree_viewer.is_template_literal exp then None
      else if exp.pexp_loc |> Loc.has_pos ~pos:pos_before_cursor then (
        if Debug.verbose () then
          print_endline
            "[findArgCompletables] Completing in an unlabelled argument";
        match
          CompletionExpressions.traverse_expr exp ~pos:pos_before_cursor
            ~first_char_before_cursor_no_white ~expr_path:[]
        with
        | None ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] found nothing when traversing expr";
          None
        | Some (prefix, nested) ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] completing for unlabelled argument #2";
          Some
            (Cexpression
               {
                 context_path =
                   CArgument
                     {
                       function_context_path = context_path;
                       argument_label =
                         Unlabelled {argument_position = !unlabelled_count};
                     };
                 prefix;
                 nested = List.rev nested;
               }))
      else if CompletionExpressions.is_expr_hole exp then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] found an exprhole #2";
        Some
          (Cexpression
             {
               context_path =
                 CArgument
                   {
                     function_context_path = context_path;
                     argument_label =
                       Unlabelled {argument_position = !unlabelled_count};
                   };
               prefix = "";
               nested = [];
             }))
      else (
        unlabelled_count := !unlabelled_count + 1;
        loop rest)
    | [] ->
      let had_empty_exp_loc = !some_arg_had_empty_expr_loc in
      if fn_has_cursor then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] Function has cursor";
        match char_before_cursor with
        | Some '~' ->
          if Debug.verbose () then
            print_endline "[findArgCompletables] '~' is before cursor";
          Some (Completable.CnamedArg (context_path, "", all_names))
        | _ when had_empty_exp_loc ->
          (* Special case: `Console.log(arr->)`, completing on the pipe.
             This match branch happens when the fn call has the cursor and:
             - there's no argument label or expr that has the cursor
             - there's an argument expression with an empty loc (indicates parser error)

             In that case, it's safer to not complete for the unlabelled function
             argument (which we do otherwise), and instead not complete and let the
             completion engine move into the arguments one by one instead to check
             for completions.

             This can be handled in a more robust way in a future refactor of the
             completion engine logic. *)
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] skipping completion in fn call because \
               arg had empty loc";
          None
        | _
          when first_char_before_cursor_no_white = Some '('
               || first_char_before_cursor_no_white = Some ',' ->
          (* Checks to ensure that completing for empty unlabelled arg makes
             sense by checking what's left of the cursor. *)
          if Debug.verbose () then
            Printf.printf
              "[findArgCompletables] Completing for unlabelled argument value \
               because nothing matched and is not labelled argument name \
               completion. isPipedExpr: %b\n"
              is_piped_expr;
          Some
            (Cexpression
               {
                 context_path =
                   CArgument
                     {
                       function_context_path = context_path;
                       argument_label =
                         Unlabelled {argument_position = !unlabelled_count};
                     };
                 prefix = "";
                 nested = [];
               })
        | _ -> None)
      else None
  in
  match args with
  (* Special handling for empty fn calls, e.g. `let _ = someFn(<com>)` *)
  | [
   {label = None; exp = {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}};
  ]
    when fn_has_cursor ->
    if Debug.verbose () then
      print_endline "[findArgCompletables] Completing for unit argument";
    Some
      (Completable.Cexpression
         {
           context_path =
             CArgument
               {
                 function_context_path = context_path;
                 argument_label =
                   Unlabelled
                     {argument_position = (if is_piped_expr then 1 else 0)};
               };
           prefix = "";
           nested = [];
         })
  | _ -> loop args

let rec expr_to_context_path_inner ~(in_jsx_context : bool) (e : Parsetree.expression)
    =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) -> Some Completable.CPString
  | Pexp_constant (Pconst_integer _) -> Some CPInt
  | Pexp_constant (Pconst_float _) -> Some CPFloat
  | Pexp_construct ({txt = Lident ("true" | "false")}, None) -> Some CPBool
  | Pexp_array exprs ->
    Some
      (CPArray
         (match exprs with
         | [] -> None
         | exp :: _ -> expr_to_context_path ~in_jsx_context exp))
  | Pexp_ident {txt = Lident "->"} -> None
  | Pexp_ident {txt; loc} ->
    Some
      (CPId {path = Utils.flatten_long_ident txt; completion_context = Value; loc})
  | Pexp_field (e1, {txt = Lident name}) -> (
    match expr_to_context_path ~in_jsx_context e1 with
    | Some context_path ->
      Some
        (CPField
           {
             context_path;
             field_name = name;
             pos_of_dot = None;
             expr_loc = e1.pexp_loc;
             in_jsx = in_jsx_context;
           })
    | _ -> None)
  | Pexp_field (e1, {loc; txt = Ldot (lid, name)}) ->
    (* Case x.M.field ignore the x part *)
    Some
      (CPField
         {
           context_path =
             CPId
               {
                 path = Utils.flatten_long_ident lid;
                 completion_context = Module;
                 loc;
               };
           field_name = name;
           pos_of_dot = None;
           expr_loc = e1.pexp_loc;
           in_jsx = in_jsx_context;
         })
  | Pexp_send (e1, {txt}) -> (
    match expr_to_context_path ~in_jsx_context e1 with
    | None -> None
    | Some contex_path -> Some (CPObj (contex_path, txt)))
  | Pexp_apply
      {
        funct =
          {
            pexp_desc = Pexp_ident {txt = Lident "->"};
            pexp_loc;
            pexp_attributes;
          };
        args =
          [(_, lhs); (_, {pexp_desc = Pexp_apply {funct = d; args; partial}})];
        transformed_jsx;
      } ->
    (* Transform away pipe with apply call *)
    expr_to_context_path ~in_jsx_context
      {
        pexp_desc =
          Pexp_apply
            {funct = d; args = (Nolabel, lhs) :: args; partial; transformed_jsx};
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply
      ({
         funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
         args =
           [
             (_, lhs);
             (_, {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes});
           ];
       } as app) ->
    (* Transform away pipe with identifier *)
    expr_to_context_path ~in_jsx_context
      {
        pexp_desc =
          Pexp_apply
            {
              app with
              funct = {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes};
              args = [(Nolabel, lhs)];
            };
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply {funct = e1; args} -> (
    match expr_to_context_path ~in_jsx_context e1 with
    | None -> None
    | Some contex_path -> Some (CPApply (contex_path, args |> List.map fst)))
  | Pexp_tuple exprs ->
    let exprs_as_context_paths =
      exprs |> List.filter_map (expr_to_context_path ~in_jsx_context)
    in
    if List.length exprs = List.length exprs_as_context_paths then
      Some (CTuple exprs_as_context_paths)
    else None
  | Pexp_await e -> expr_to_context_path_inner ~in_jsx_context e
  | _ -> None

and expr_to_context_path ~(in_jsx_context : bool) (e : Parsetree.expression) =
  match
    ( Res_parsetree_viewer.expr_is_await e,
      expr_to_context_path_inner ~in_jsx_context e )
  with
  | true, Some ctx_path -> Some (CPAwait ctx_path)
  | false, Some ctx_path -> Some ctx_path
  | _, None -> None

let complete_pipe_chain ~(in_jsx_context : bool) (exp : Parsetree.expression) =
  (* Complete the end of pipe chains by reconstructing the pipe chain as a single pipe,
     so it can be completed.
     Example:
      someArray->Js.Array2.filter(v => v > 10)->Js.Array2.map(v => v + 2)->
        will complete as:
      Js.Array2.map(someArray->Js.Array2.filter(v => v > 10), v => v + 2)->
  *)
  match exp.pexp_desc with
  (* When the left side of the pipe we're completing is a function application.
     Example: someArray->Js.Array2.map(v => v + 2)-> *)
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
        args = [_; (_, {pexp_desc = Pexp_apply {funct = d}})];
      } ->
    expr_to_context_path ~in_jsx_context exp
    |> Option.map (fun ctx_path -> (ctx_path, d.pexp_loc))
    (* When the left side of the pipe we're completing is an identifier application.
       Example: someArray->filterAllTheGoodStuff-> *)
  | Pexp_apply
      {
        funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
        args = [_; (_, {pexp_desc = Pexp_ident _; pexp_loc})];
      } ->
    expr_to_context_path ~in_jsx_context exp
    |> Option.map (fun ctx_path -> (ctx_path, pexp_loc))
  | _ -> None

let completion_with_parser1 ~debug ~offset ~pos_cursor ~kind_file ?find_this_expr_loc
    text =
  let offset_no_white = Utils.skip_white text (offset - 1) in
  let pos_no_white =
    let line, col = pos_cursor in
    (line, max 0 col - offset + offset_no_white)
  in
  (* Identifies the first character before the cursor that's not white space.
     Should be used very sparingly, but can be used to drive completion triggering
     in scenarios where the parser eats things we'd need to complete.
     Example: let {whatever,     <cursor>}, char is ','. *)
  let first_char_before_cursor_no_white =
    if offset_no_white < String.length text && offset_no_white >= 0 then
      Some text.[offset_no_white]
    else None
  in
  let pos_of_dot = Pos.pos_of_dot text ~pos:pos_cursor ~offset in
  let char_at_cursor =
    if offset >= 0 && offset < String.length text then text.[offset] else '\n'
  in
  let pos_before_cursor = Pos.pos_before_cursor pos_cursor in
  let char_before_cursor, blank_after_cursor =
    match Pos.position_to_offset text pos_cursor with
    | Some offset when offset > 0 -> (
      let char_before_cursor = text.[offset - 1] in
      match char_at_cursor with
      | ' ' | '\t' | '\r' | '\n' ->
        (Some char_before_cursor, Some char_before_cursor)
      | _ -> (Some char_before_cursor, None))
    | _ -> (None, None)
  in
  let flatten_lid_check_dot ?(jsx = true) (lid : Longident.t Location.loc) =
    (* Flatten an identifier keeping track of whether the current cursor
       is after a "." in the id followed by a blank character.
       In that case, cut the path after ".". *)
    let cut_at_offset =
      let id_start = Loc.start lid.loc in
      match blank_after_cursor with
      | Some '.' ->
        if fst pos_before_cursor = fst id_start then
          Some (snd pos_before_cursor - snd id_start)
        else None
      | _ -> None
    in
    Utils.flatten_long_ident ~cut_at_offset ~jsx lid.txt
  in

  let current_ctx_path = ref None in
  let processing_fun = ref None in
  let set_current_ctx_path ctx_path =
    if !Cfg.debug_follow_ctx_path then
      Printf.printf "setting current ctxPath: %s\n"
        (Completable.context_path_to_string ctx_path);
    current_ctx_path := Some ctx_path
  in
  let reset_current_ctx_path ctx_path =
    (match (!current_ctx_path, ctx_path) with
    | None, None -> ()
    | _ ->
      if !Cfg.debug_follow_ctx_path then
        Printf.printf "resetting current ctxPath to: %s\n"
          (match ctx_path with
          | None -> "None"
          | Some ctx_path -> Completable.context_path_to_string ctx_path));
    current_ctx_path := ctx_path
  in

  let found = ref false in
  let result = ref None in
  let scope = ref (Scope.create ()) in
  let set_result_opt x =
    if !result = None then
      match x with
      | None ->
        if Debug.verbose () then
          print_endline
            "[set_result] did not set new result because result already was set";
        ()
      | Some x ->
        if Debug.verbose () then
          Printf.printf "[set_result] set new result to %s\n"
            (Completable.to_string x);
        result := Some (x, !scope)
  in
  let in_jsx_context = ref false in
  let set_result x = set_result_opt (Some x) in
  let scope_value_description (vd : Parsetree.value_description) =
    scope :=
      !scope |> Scope.add_value ~name:vd.pval_name.txt ~loc:vd.pval_name.loc
  in
  let rec scope_pattern ?context_path
      ?(pattern_path : Completable.nested_path list = [])
      (pat : Parsetree.pattern) =
    let context_path_to_save =
      match (context_path, pattern_path) with
      | maybe_context_path, [] -> maybe_context_path
      | Some context_path, pattern_path ->
        Some
          (Completable.CPatternPath
             {root_ctx_path = context_path; nested = List.rev pattern_path})
      | _ -> None
    in
    match pat.ppat_desc with
    | Ppat_any -> ()
    | Ppat_var {txt; loc} ->
      scope :=
        !scope |> Scope.add_value ~name:txt ~loc ?context_path:context_path_to_save
    | Ppat_alias (p, as_a) ->
      scope_pattern p ~pattern_path ?context_path;
      let ctx_path =
        if context_path_to_save = None then
          match p with
          | {ppat_desc = Ppat_var {txt; loc}} ->
            Some
              (Completable.CPId {path = [txt]; completion_context = Value; loc})
          | _ -> None
        else None
      in
      scope :=
        !scope |> Scope.add_value ~name:as_a.txt ~loc:as_a.loc ?context_path:ctx_path
    | Ppat_constant _ | Ppat_interval _ -> ()
    | Ppat_tuple pl ->
      pl
      |> List.iteri (fun index p ->
             scope_pattern p
               ~pattern_path:(NTupleItem {item_num = index} :: pattern_path)
               ?context_path)
    | Ppat_construct (_, None) -> ()
    | Ppat_construct ({txt}, Some {ppat_desc = Ppat_tuple pl}) ->
      pl
      |> List.iteri (fun index p ->
             scope_pattern p
               ~pattern_path:
                 (NVariantPayload
                    {
                      item_num = index;
                      constructor_name = Utils.get_unqualified_name txt;
                    }
                 :: pattern_path)
               ?context_path)
    | Ppat_construct ({txt}, Some p) ->
      scope_pattern
        ~pattern_path:
          (NVariantPayload
             {item_num = 0; constructor_name = Utils.get_unqualified_name txt}
          :: pattern_path)
        ?context_path p
    | Ppat_variant (_, None) -> ()
    | Ppat_variant (txt, Some {ppat_desc = Ppat_tuple pl}) ->
      pl
      |> List.iteri (fun index p ->
             scope_pattern p
               ~pattern_path:
                 (NPolyvariantPayload {item_num = index; constructor_name = txt}
                 :: pattern_path)
               ?context_path)
    | Ppat_variant (txt, Some p) ->
      scope_pattern
        ~pattern_path:
          (NPolyvariantPayload {item_num = 0; constructor_name = txt}
          :: pattern_path)
        ?context_path p
    | Ppat_record (fields, _) ->
      Ext_list.iter fields (fun {lid = fname; x = p} ->
          match fname with
          | {Location.txt = Longident.Lident fname} ->
            scope_pattern
              ~pattern_path:
                (Completable.NFollowRecordField {field_name = fname}
                :: pattern_path)
              ?context_path p
          | _ -> ())
    | Ppat_array pl ->
      pl
      |> List.iter
           (scope_pattern ~pattern_path:(NArray :: pattern_path) ?context_path)
    | Ppat_or (p1, _) -> scope_pattern ~pattern_path ?context_path p1
    | Ppat_constraint (p, core_type) ->
      scope_pattern ~pattern_path
        ?context_path:(TypeUtils.context_path_from_core_type core_type)
        p
    | Ppat_type _ -> ()
    | Ppat_unpack {txt; loc} ->
      scope := !scope |> Scope.add_module ~name:txt ~loc
    | Ppat_exception p -> scope_pattern ~pattern_path ?context_path p
    | Ppat_extension _ -> ()
    | Ppat_open (_, p) -> scope_pattern ~pattern_path ?context_path p
  in
  let loc_has_cursor = CursorPosition.loc_has_cursor ~pos:pos_before_cursor in
  let loc_is_empty = CursorPosition.loc_is_empty ~pos:pos_before_cursor in
  let complete_pattern ?context_path (pat : Parsetree.pattern) =
    match
      ( pat
        |> CompletionPatterns.traverse_pattern ~pattern_path:[] ~loc_has_cursor
             ~first_char_before_cursor_no_white ~pos_before_cursor,
        context_path )
    with
    | Some (prefix, nested_pattern), Some ctx_path ->
      if Debug.verbose () then
        Printf.printf "[completePattern] found pattern that can be completed\n";
      set_result
        (Completable.Cpattern
           {
             context_path = ctx_path;
             prefix;
             nested = List.rev nested_pattern;
             fallback = None;
             pattern_mode = Default;
           })
    | _ -> ()
  in
  let scope_value_binding (vb : Parsetree.value_binding) =
    let context_path =
      (* Pipe chains get special treatment here, because when assigning values
         we want the return of the entire pipe chain as a function call, rather
         than as a pipe completion call. *)
      match complete_pipe_chain ~in_jsx_context:!in_jsx_context vb.pvb_expr with
      | Some (ctx_path, _) -> Some ctx_path
      | None -> expr_to_context_path ~in_jsx_context:!in_jsx_context vb.pvb_expr
    in
    scope_pattern ?context_path vb.pvb_pat
  in
  let scope_type_kind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constr_decls ->
      constr_decls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             scope :=
               !scope
               |> Scope.add_constructor ~name:cd.pcd_name.txt ~loc:cd.pcd_loc)
    | Ptype_record label_decls ->
      label_decls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             scope :=
               !scope |> Scope.add_field ~name:ld.pld_name.txt ~loc:ld.pld_loc)
    | _ -> ()
  in
  let scope_type_declaration (td : Parsetree.type_declaration) =
    scope :=
      !scope |> Scope.add_type ~name:td.ptype_name.txt ~loc:td.ptype_name.loc;
    scope_type_kind td.ptype_kind
  in
  let scope_module_binding (mb : Parsetree.module_binding) =
    scope :=
      !scope |> Scope.add_module ~name:mb.pmb_name.txt ~loc:mb.pmb_name.loc
  in
  let scope_module_declaration (md : Parsetree.module_declaration) =
    scope :=
      !scope |> Scope.add_module ~name:md.pmd_name.txt ~loc:md.pmd_name.loc
  in

  (* Identifies expressions where we can do typed pattern or expr completion. *)
  let typed_completion_expr (exp : Parsetree.expression) =
    let debug_typed_completion_expr = false in
    if exp.pexp_loc |> CursorPosition.loc_has_cursor ~pos:pos_before_cursor then (
      if Debug.verbose () && debug_typed_completion_expr then
        print_endline "[typedCompletionExpr] Has cursor";
      match exp.pexp_desc with
      (* No cases means there's no `|` yet in the switch *)
      | Pexp_match (({pexp_desc = Pexp_ident _} as expr), []) ->
        if Debug.verbose () && debug_typed_completion_expr then
          print_endline "[typedCompletionExpr] No cases, with ident";
        if loc_has_cursor expr.pexp_loc then (
          if Debug.verbose () && debug_typed_completion_expr then
            print_endline "[typedCompletionExpr] No cases - has cursor";
          (* We can do exhaustive switch completion if this is an ident we can
             complete from. *)
          match expr_to_context_path ~in_jsx_context:!in_jsx_context expr with
          | None -> ()
          | Some context_path ->
            set_result (CexhaustiveSwitch {context_path; expr_loc = exp.pexp_loc}))
      | Pexp_match (_expr, []) ->
        (* switch x { } *)
        if Debug.verbose () && debug_typed_completion_expr then
          print_endline "[typedCompletionExpr] No cases, rest";
        ()
      | Pexp_match (expr, [{pc_lhs; pc_rhs}])
        when loc_has_cursor expr.pexp_loc
             && CompletionExpressions.is_expr_hole pc_rhs
             && CompletionPatterns.is_pattern_hole pc_lhs ->
        (* switch x { | } when we're in the switch expr itself. *)
        if Debug.verbose () && debug_typed_completion_expr then
          print_endline
            "[typedCompletionExpr] No cases (expr and pat holes), rest";
        ()
      | Pexp_match
          ( exp,
            [
              {
                pc_lhs =
                  {
                    ppat_desc =
                      Ppat_extension ({txt = "rescript.patternhole"}, _);
                  };
              };
            ] ) -> (
        (* A single case that's a pattern hole typically means `switch x { | }`. Complete as the pattern itself with nothing nested. *)
        match expr_to_context_path ~in_jsx_context:!in_jsx_context exp with
        | None -> ()
        | Some ctx_path ->
          set_result
            (Completable.Cpattern
               {
                 context_path = ctx_path;
                 nested = [];
                 prefix = "";
                 fallback = None;
                 pattern_mode = Default;
               }))
      | Pexp_match (exp, cases) -> (
        if Debug.verbose () && debug_typed_completion_expr then
          print_endline "[typedCompletionExpr] Has cases";
        (* If there's more than one case, or the case isn't a pattern hole, figure out if we're completing another
           broken parser case (`switch x { | true => () | <com> }` for example). *)
        match exp |> expr_to_context_path ~in_jsx_context:!in_jsx_context with
        | None ->
          if Debug.verbose () && debug_typed_completion_expr then
            print_endline "[typedCompletionExpr] Has cases - no ctx path"
        | Some ctx_path -> (
          if Debug.verbose () && debug_typed_completion_expr then
            print_endline "[typedCompletionExpr] Has cases - has ctx path";
          let has_case_with_cursor =
            cases
            |> List.find_opt (fun case ->
                   loc_has_cursor case.Parsetree.pc_lhs.ppat_loc)
            |> Option.is_some
          in
          let has_case_with_empty_loc =
            cases
            |> List.find_opt (fun case ->
                   loc_is_empty case.Parsetree.pc_lhs.ppat_loc)
            |> Option.is_some
          in
          if Debug.verbose () && debug_typed_completion_expr then
            Printf.printf
              "[typedCompletionExpr] Has cases - has ctx path - \
               hasCaseWithEmptyLoc: %b, hasCaseWithCursor: %b\n"
              has_case_with_empty_loc has_case_with_cursor;
          match (has_case_with_empty_loc, has_case_with_cursor) with
          | _, true ->
            (* Always continue if there's a case with the cursor *)
            ()
          | true, false ->
            (* If there's no case with the cursor, but a broken parser case, complete for the top level. *)
            set_result
              (Completable.Cpattern
                 {
                   context_path = ctx_path;
                   nested = [];
                   prefix = "";
                   fallback = None;
                   pattern_mode = Default;
                 })
          | false, false -> ()))
      | _ -> ())
  in
  let structure (iterator : Ast_iterator.iterator)
      (structure : Parsetree.structure) =
    let old_scope = !scope in
    Ast_iterator.default_iterator.structure iterator structure;
    scope := old_scope
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    let processed = ref false in
    (match item.pstr_desc with
    | Pstr_open {popen_lid} ->
      scope := !scope |> Scope.add_open ~lid:popen_lid.txt
    | Pstr_primitive vd -> scope_value_description vd
    | Pstr_value (rec_flag, bindings) ->
      if rec_flag = Recursive then bindings |> List.iter scope_value_binding;
      bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
      if rec_flag = Nonrecursive then bindings |> List.iter scope_value_binding;
      processed := true
    | Pstr_type (rec_flag, decls) ->
      if rec_flag = Recursive then decls |> List.iter scope_type_declaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if rec_flag = Nonrecursive then decls |> List.iter scope_type_declaration;
      processed := true
    | Pstr_module mb ->
      iterator.module_binding iterator mb;
      scope_module_binding mb;
      processed := true
    | Pstr_recmodule mbs ->
      mbs |> List.iter scope_module_binding;
      mbs |> List.iter (fun b -> iterator.module_binding iterator b);
      processed := true
    | Pstr_include {pincl_mod = {pmod_desc = med}} -> (
      match med with
      | Pmod_ident {txt = lid; loc}
      | Pmod_apply ({pmod_desc = Pmod_ident {txt = lid; loc}}, _) ->
        let module_name = Longident.flatten lid |> String.concat "." in
        scope := !scope |> Scope.add_include ~name:module_name ~loc
      | _ -> ())
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.structure_item iterator item
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (value_binding : Parsetree.value_binding) =
    let old_in_jsx_context = !in_jsx_context in
    if Utils.is_jsx_component value_binding then in_jsx_context := true;
    (match value_binding with
    | {pvb_pat = {ppat_desc = Ppat_constraint (_pat, core_type)}; pvb_expr}
      when loc_has_cursor pvb_expr.pexp_loc -> (
      (* Expression with derivable type annotation.
         E.g: let x: someRecord = {<com>} *)
      match
        ( TypeUtils.context_path_from_core_type core_type,
          pvb_expr
          |> CompletionExpressions.traverse_expr ~expr_path:[]
               ~pos:pos_before_cursor ~first_char_before_cursor_no_white )
      with
      | Some ctx_path, Some (prefix, nested) ->
        set_result
          (Completable.Cexpression
             {context_path = ctx_path; prefix; nested = List.rev nested})
      | _ -> ())
    | {pvb_pat = {ppat_desc = Ppat_var {loc}}; pvb_expr}
      when loc_has_cursor pvb_expr.pexp_loc -> (
      (* Expression without a type annotation. We can complete this if this
         has compiled previously and there's a type available for the identifier itself.
         This is nice because the type is assigned even if the assignment isn't complete.

         E.g: let x = {name: "name", <com>}, when `x` has compiled. *)
      match
        pvb_expr
        |> CompletionExpressions.traverse_expr ~expr_path:[] ~pos:pos_before_cursor
             ~first_char_before_cursor_no_white
      with
      | Some (prefix, nested) ->
        (* This completion should be low prio, so let any deeper completion
           hit first, and only set this TypeAtPos completion if nothing else
           here hit. *)
        Ast_iterator.default_iterator.value_binding iterator value_binding;
        set_result
          (Completable.Cexpression
             {context_path = CTypeAtPos loc; prefix; nested = List.rev nested})
      | _ -> ())
    | {
     pvb_pat = {ppat_desc = Ppat_constraint (_pat, core_type); ppat_loc};
     pvb_expr;
    }
      when loc_has_cursor value_binding.pvb_loc
           && loc_has_cursor ppat_loc = false
           && loc_has_cursor pvb_expr.pexp_loc = false
           && CompletionExpressions.is_expr_hole pvb_expr -> (
      (* Expression with derivable type annotation, when the expression is empty (expr hole).
         E.g: let x: someRecord = <com> *)
      match TypeUtils.context_path_from_core_type core_type with
      | Some ctx_path ->
        set_result
          (Completable.Cexpression
             {context_path = ctx_path; prefix = ""; nested = []})
      | _ -> ())
    | {pvb_pat; pvb_expr} when loc_has_cursor pvb_pat.ppat_loc -> (
      (* Completing a destructuring.
         E.g: let {<com>} = someVar *)
      match
        ( pvb_pat
          |> CompletionPatterns.traverse_pattern ~pattern_path:[] ~loc_has_cursor
               ~first_char_before_cursor_no_white ~pos_before_cursor,
          expr_to_context_path ~in_jsx_context:!in_jsx_context pvb_expr )
      with
      | Some (prefix, nested), Some ctx_path ->
        set_result
          (Completable.Cpattern
             {
               context_path = ctx_path;
               prefix;
               nested = List.rev nested;
               fallback = None;
               pattern_mode = Destructuring;
             })
      | _ -> ())
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator value_binding;
    in_jsx_context := old_in_jsx_context
  in
  let signature (iterator : Ast_iterator.iterator)
      (signature : Parsetree.signature) =
    let old_scope = !scope in
    Ast_iterator.default_iterator.signature iterator signature;
    scope := old_scope
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    let processed = ref false in
    (match item.psig_desc with
    | Psig_open {popen_lid} ->
      scope := !scope |> Scope.add_open ~lid:popen_lid.txt
    | Psig_value vd -> scope_value_description vd
    | Psig_type (rec_flag, decls) ->
      if rec_flag = Recursive then decls |> List.iter scope_type_declaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if rec_flag = Nonrecursive then decls |> List.iter scope_type_declaration;
      processed := true
    | Psig_module md ->
      iterator.module_declaration iterator md;
      scope_module_declaration md;
      processed := true
    | Psig_recmodule mds ->
      mds |> List.iter scope_module_declaration;
      mds |> List.iter (fun d -> iterator.module_declaration iterator d);
      processed := true
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.signature_item iterator item
  in
  let attribute (iterator : Ast_iterator.iterator)
      ((id, payload) : Parsetree.attribute) =
    (if String.length id.txt >= 4 && String.sub id.txt 0 4 = "res." then
       (* skip: internal parser attribute *) ()
     else if id.loc.loc_ghost then ()
     else if id.loc |> Loc.has_pos ~pos:pos_before_cursor then
       let pos_start, pos_end = Loc.range id.loc in
       match
         (Pos.position_to_offset text pos_start, Pos.position_to_offset text pos_end)
       with
       | Some offset_start, Some offset_end
         when offset_start >= 0 && offset_end >= offset_start ->
         (* Can't trust the parser's location
            E.g. @foo. let x... gives as label @foo.let *)
         let label =
           let raw_label =
             String.sub text offset_start (offset_end - offset_start)
           in
           let ( ++ ) x y =
             match (x, y) with
             | Some i1, Some i2 -> Some (min i1 i2)
             | Some _, None -> x
             | None, _ -> y
           in
           let label =
             match
               String.index_opt raw_label ' '
               ++ String.index_opt raw_label '\t'
               ++ String.index_opt raw_label '\r'
               ++ String.index_opt raw_label '\n'
             with
             | None -> raw_label
             | Some i -> String.sub raw_label 0 i
           in
           if label <> "" && label.[0] = '@' then
             String.sub label 1 (String.length label - 1)
           else label
         in
         found := true;
         if debug then
           Printf.printf "Attribute id:%s:%s label:%s\n" id.txt
             (Loc.to_string id.loc) label;
         set_result (Completable.Cdecorator label)
       | _ -> ()
     else if id.txt = "module" then
       match payload with
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {pexp_loc; pexp_desc = Pexp_constant (Pconst_string (s, _))},
                     _ );
             };
           ]
         when loc_has_cursor pexp_loc ->
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @module";
         set_result (Completable.CdecoratorPayload (Module s))
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_desc =
                         Pexp_record
                           ({lid = {txt = Lident "from"}; x = from_expr} :: _, _);
                     },
                     _ );
             };
           ]
         when loc_has_cursor from_expr.pexp_loc
              || loc_is_empty from_expr.pexp_loc
                 && CompletionExpressions.is_expr_hole from_expr -> (
         if Debug.verbose () then
           print_endline
             "[decoratorCompletion] Found @module with import attributes and \
              cursor on \"from\"";
         match
           ( loc_has_cursor from_expr.pexp_loc,
             loc_is_empty from_expr.pexp_loc,
             CompletionExpressions.is_expr_hole from_expr,
             from_expr )
         with
         | true, _, _, {pexp_desc = Pexp_constant (Pconst_string (s, _))} ->
           if Debug.verbose () then
             print_endline
               "[decoratorCompletion] @module `from` payload was string";
           set_result (Completable.CdecoratorPayload (Module s))
         | false, true, true, _ ->
           if Debug.verbose () then
             print_endline
               "[decoratorCompletion] @module `from` payload was expr hole";
           set_result (Completable.CdecoratorPayload (Module ""))
         | _ -> ())
       | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> (
         if Debug.verbose () then
           print_endline
             "[decoratorCompletion] Found @module with non-string payload";
         match
           CompletionExpressions.traverse_expr expr ~expr_path:[]
             ~pos:pos_before_cursor ~first_char_before_cursor_no_white
         with
         | None -> ()
         | Some (prefix, nested) ->
           if Debug.verbose () then
             print_endline "[decoratorCompletion] Found @module record path";
           set_result
             (Completable.CdecoratorPayload
                (ModuleWithImportAttributes {nested = List.rev nested; prefix}))
         )
       | _ -> ()
     else if id.txt = "jsxConfig" then
       match payload with
       | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> (
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @jsxConfig";
         match
           CompletionExpressions.traverse_expr expr ~expr_path:[]
             ~pos:pos_before_cursor ~first_char_before_cursor_no_white
         with
         | None -> ()
         | Some (prefix, nested) ->
           if Debug.verbose () then
             print_endline "[decoratorCompletion] Found @jsxConfig path!";
           set_result
             (Completable.CdecoratorPayload
                (JsxConfig {nested = List.rev nested; prefix})))
       | _ -> ()
     else if id.txt = "editor.completeFrom" then
       match payload with
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_loc;
                       pexp_desc = Pexp_construct ({txt = path; loc}, None);
                     },
                     _ );
             };
           ]
         when loc_has_cursor pexp_loc ->
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @editor.completeFrom";
         set_result
           (Completable.Cpath
              (CPId
                 {
                   path = Utils.flatten_long_ident path;
                   completion_context = Module;
                   loc;
                 }))
       | _ -> ());
    Ast_iterator.default_iterator.attribute iterator (id, payload)
  in
  let rec iterate_fn_arguments ~args ~iterator ~is_pipe
      (arg_completable : Completable.t option) =
    match arg_completable with
    | None -> (
      match !current_ctx_path with
      | None -> ()
      | Some function_context_path ->
        let current_unlabelled_count = ref (if is_pipe then 1 else 0) in
        args
        |> List.iter (fun (arg : arg) ->
               let previous_ctx_path = !current_ctx_path in
               set_current_ctx_path
                 (CArgument
                    {
                      function_context_path;
                      argument_label =
                        (match arg with
                        | {label = None} ->
                          let current = !current_unlabelled_count in
                          current_unlabelled_count := current + 1;
                          Unlabelled {argument_position = current}
                        | {label = Some {name; opt = true}} -> Optional name
                        | {label = Some {name; opt = false}} -> Labelled name);
                    });
               expr iterator arg.exp;
               reset_current_ctx_path previous_ctx_path))
    | Some arg_completable -> set_result arg_completable
  and iterate_jsx_props ~iterator (props : CompletionJsx.jsx_props) =
    props.props
    |> List.iter (fun (prop : CompletionJsx.prop) ->
           let previous_ctx_path = !current_ctx_path in
           set_current_ctx_path
             (CJsxPropValue
                {
                  path_to_component =
                    Utils.flatten_long_ident ~jsx:true props.comp_name.txt;
                  prop_name = prop.name;
                  empty_jsx_prop_name_hint = None;
                });
           expr iterator prop.exp;
           reset_current_ctx_path previous_ctx_path)
  and expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    let old_in_jsx_context = !in_jsx_context in
    let processed = ref false in
    let set_found () =
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found expr:%s\n"
          (Pos.to_string pos_cursor) (Pos.to_string pos_no_white)
          (Loc.to_string expr.pexp_loc)
    in
    (match find_this_expr_loc with
    | Some loc when expr.pexp_loc = loc -> (
      match expr_to_context_path ~in_jsx_context:!in_jsx_context expr with
      | None -> ()
      | Some ctx_path -> set_result (Cpath ctx_path))
    | _ -> ());
    let set_pipe_result ~(lhs : Parsetree.expression) ~id =
      match complete_pipe_chain ~in_jsx_context:!in_jsx_context lhs with
      | None -> (
        match expr_to_context_path ~in_jsx_context:!in_jsx_context lhs with
        | Some pipe ->
          set_result
            (Cpath
               (CPPipe
                  {
                    synthetic = false;
                    context_path = pipe;
                    id;
                    lhs_loc = lhs.pexp_loc;
                    in_jsx = !in_jsx_context;
                  }));
          true
        | None -> false)
      | Some (pipe, lhs_loc) ->
        set_result
          (Cpath
             (CPPipe
                {
                  synthetic = false;
                  context_path = pipe;
                  id;
                  lhs_loc;
                  in_jsx = !in_jsx_context;
                }));
        true
    in
    typed_completion_expr expr;
    match expr.pexp_desc with
    | Pexp_match (expr, cases)
      when cases <> []
           && loc_has_cursor expr.pexp_loc = false
           && Option.is_none find_this_expr_loc ->
      if Debug.verbose () then
        print_endline "[completionFrontend] Checking each case";
      let ctx_path = expr_to_context_path ~in_jsx_context:!in_jsx_context expr in
      let old_ctx_path = !current_ctx_path in
      cases
      |> List.iter (fun (case : Parsetree.case) ->
             let old_scope = !scope in
             if
               loc_has_cursor case.pc_rhs.pexp_loc = false
               && loc_has_cursor case.pc_lhs.ppat_loc
             then complete_pattern ?context_path:ctx_path case.pc_lhs;
             scope_pattern ?context_path:ctx_path case.pc_lhs;
             Ast_iterator.default_iterator.case iterator case;
             scope := old_scope);
      reset_current_ctx_path old_ctx_path
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "->"; loc = op_loc}};
          args =
            [
              (_, lhs);
              (_, {pexp_desc = Pexp_extension _; pexp_loc = {loc_ghost = true}});
            ];
        }
      when op_loc |> Loc.has_pos ~pos:pos_before_cursor ->
      (* Case foo-> when the parser adds a ghost expression to the rhs
         so the apply expression does not include the cursor *)
      if set_pipe_result ~lhs ~id:"" then set_found ()
    (*
       A dot completion for a tagged templated application with an expr hole.
       Example:
         sh`echo "meh"`.
    *)
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "."; loc = _}};
          args =
            [
              (* sh`echo "meh"` *)
              (_, ({pexp_desc = Pexp_apply _} as inner_expr));
              (* recovery inserted node *)
              (_, {pexp_desc = Pexp_extension ({txt = "rescript.exprhole"}, _)});
            ];
        }
      when Res_parsetree_viewer.is_tagged_template_literal inner_expr ->
      expr_to_context_path ~in_jsx_context:!in_jsx_context inner_expr
      |> Option.iter (fun cpath ->
             set_result
               (Cpath
                  (CPField
                     {
                       context_path = cpath;
                       field_name = "";
                       pos_of_dot;
                       expr_loc = expr.pexp_loc;
                       in_jsx = !in_jsx_context;
                     }));
             set_found ())
    (*
       A dot completion for a tagged templated application with an ident.
       Example:
         sh`echo "meh"`.foo
    *)
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "."; loc = _}};
          args =
            [
              (*  sh`echo "meh"` *)
              (_, ({pexp_desc = Pexp_apply _} as inner_expr));
              (* foo *)
              (_, {pexp_desc = Pexp_ident {txt = Lident field_name}});
            ];
        }
      when Res_parsetree_viewer.is_tagged_template_literal inner_expr
           && expr.pexp_loc |> Loc.has_pos ~pos:pos_before_cursor ->
      expr_to_context_path ~in_jsx_context:!in_jsx_context inner_expr
      |> Option.iter (fun cpath ->
             set_result
               (Cpath
                  (CPField
                     {
                       context_path = cpath;
                       field_name;
                       pos_of_dot;
                       expr_loc = expr.pexp_loc;
                       in_jsx = !in_jsx_context;
                     }));
             set_found ())
    | _ -> (
      if expr.pexp_loc |> Loc.has_pos ~pos:pos_no_white && !result = None then (
        set_found ();
        match expr.pexp_desc with
        | Pexp_extension ({txt = "obj"}, PStr [str_item]) ->
          Ast_iterator.default_iterator.structure_item iterator str_item
        | Pexp_extension ({txt}, _) -> set_result (CextensionNode txt)
        | Pexp_constant _ -> set_result Cnone
        | Pexp_ident lid ->
          let lid_path = flatten_lid_check_dot lid in
          if debug then
            Printf.printf "Pexp_ident %s:%s\n"
              (lid_path |> String.concat ".")
              (Loc.to_string lid.loc);
          if lid.loc |> Loc.has_pos ~pos:pos_before_cursor then
            let is_likely_module_path =
              match lid_path with
              | head :: _
                when String.length head > 0
                     && head.[0] == Char.uppercase_ascii head.[0] ->
                true
              | _ -> false
            in
            set_result
              (Cpath
                 (CPId
                    {
                      loc = lid.loc;
                      path = lid_path;
                      completion_context =
                        (if
                           is_likely_module_path
                           && expr |> Res_parsetree_viewer.is_braced_expr
                         then ValueOrField
                         else Value);
                    }))
        | Pexp_construct (lid, e_opt) -> (
          let lid_path = flatten_lid_check_dot lid in
          if debug then
            Printf.printf "Pexp_construct %s:%s %s\n"
              (lid_path |> String.concat "\n")
              (Loc.to_string lid.loc)
              (match e_opt with
              | None -> "None"
              | Some e -> Loc.to_string e.pexp_loc);
          if
            e_opt = None && (not lid.loc.loc_ghost)
            && lid.loc |> Loc.has_pos ~pos:pos_before_cursor
          then
            set_result
              (Cpath
                 (CPId
                    {loc = lid.loc; path = lid_path; completion_context = Value}))
          else
            match e_opt with
            | Some e when loc_has_cursor e.pexp_loc -> (
              match
                CompletionExpressions.complete_constructor_payload
                  ~pos_before_cursor ~first_char_before_cursor_no_white lid e
              with
              | Some result ->
                (* Check if anything else more important completes before setting this completion. *)
                Ast_iterator.default_iterator.expr iterator e;
                set_result result
              | None -> ())
            | _ -> ())
        | Pexp_field (e, field_name) -> (
          if debug then
            Printf.printf "Pexp_field %s %s:%s\n" (Loc.to_string e.pexp_loc)
              (Utils.flatten_long_ident field_name.txt |> String.concat ".")
              (Loc.to_string field_name.loc);
          if field_name.loc |> Loc.has_pos ~pos:pos_before_cursor then
            match field_name.txt with
            | Lident name -> (
              match expr_to_context_path ~in_jsx_context:!in_jsx_context e with
              | Some context_path ->
                let context_path =
                  Completable.CPField
                    {
                      context_path;
                      field_name = name;
                      pos_of_dot;
                      expr_loc = e.pexp_loc;
                      in_jsx = !in_jsx_context;
                    }
                in
                set_result (Cpath context_path)
              | None -> ())
            | Ldot (id, name) ->
              (* Case x.M.field ignore the x part *)
              let context_path =
                Completable.CPField
                  {
                    context_path =
                      CPId
                        {
                          loc = field_name.loc;
                          path = Utils.flatten_long_ident id;
                          completion_context = Module;
                        };
                    field_name =
                      (if blank_after_cursor = Some '.' then
                         (* x.M. field  --->  M. *) ""
                       else if name = "_" then ""
                       else name);
                    pos_of_dot;
                    expr_loc = e.pexp_loc;
                    in_jsx = !in_jsx_context;
                  }
              in
              set_result (Cpath context_path)
            | Lapply _ -> ()
          else if Loc.end_ e.pexp_loc = pos_before_cursor then
            match expr_to_context_path ~in_jsx_context:!in_jsx_context e with
            | Some context_path ->
              set_result
                (Cpath
                   (CPField
                      {
                        context_path;
                        field_name = "";
                        pos_of_dot;
                        expr_loc = e.pexp_loc;
                        in_jsx = !in_jsx_context;
                      }))
            | None -> ())
        | Pexp_jsx_element
            ( Jsx_unary_element
                {
                  jsx_unary_element_tag_name = comp_name;
                  jsx_unary_element_props = props;
                }
            | Jsx_container_element
                {
                  jsx_container_element_tag_name_start = comp_name;
                  jsx_container_element_props = props;
                } ) -> (
          in_jsx_context := true;
          let is_valid_tag_for_props =
            match comp_name.txt with
            | Parsetree.JsxTagInvalid _ -> false
            | _ -> true
          in
          let children =
            match expr.pexp_desc with
            | Pexp_jsx_element
                (Jsx_container_element
                   {jsx_container_element_children = children}) ->
              children
            | _ -> []
          in
          let compName_loc = comp_name.loc in
          let compName_lid =
            Ast_helper.Jsx.longident_of_jsx_tag_name comp_name.txt
          in
          let jsx_props_opt =
            if is_valid_tag_for_props then
              Some
                (CompletionJsx.extract_jsx_props
                   ~comp_name:(Location.mkloc compName_lid compName_loc)
                   ~props ~children)
            else None
          in
          let comp_name_path =
            flatten_lid_check_dot ~jsx:true
              {txt = compName_lid; loc = compName_loc}
          in
          (if debug then
             match jsx_props_opt with
             | Some jsx_props ->
               Printf.printf "JSX <%s:%s %s> _children:%s\n"
                 (comp_name_path |> String.concat ".")
                 (Loc.to_string compName_loc)
                 (jsx_props.props
                 |> List.map
                      (fun
                        ({name; pos_start; pos_end; exp} : CompletionJsx.prop) ->
                        Printf.sprintf "%s[%s->%s]=...%s" name
                          (Pos.to_string pos_start) (Pos.to_string pos_end)
                          (Loc.to_string exp.pexp_loc))
                 |> String.concat " ")
                 (match jsx_props.children_start with
                 | None -> "None"
                 | Some children_pos_start -> Pos.to_string children_pos_start)
             | None ->
               Printf.printf "JSX <%s:%s > _children:None\n"
                 (comp_name_path |> String.concat ".")
                 (Loc.to_string compName_loc));
          (* If the tag name is an uppercase path and the cursor is right after a dot (e.g., <O.|),
             prefer module member completion over JSX prop suggestions. *)
          (match comp_name.txt with
          | Parsetree.JsxUpperTag _ when blank_after_cursor = Some '.' ->
            set_result
              (Cpath
                 (CPId
                    {
                      loc = compName_loc;
                      path = comp_name_path;
                      completion_context = Module;
                    }))
          | _ -> ());
          let jsx_completable =
            match (jsx_props_opt, expr.pexp_desc) with
            | ( Some _,
                Pexp_jsx_element
                  (Jsx_container_element
                     {
                       jsx_container_element_closing_tag = None;
                       jsx_container_element_children = _ :: _;
                     }) ) ->
              None
            | Some jsx_props, _ ->
              CompletionJsx.find_jsx_props_completable ~jsx_props
                ~end_pos:(Loc.end_ expr.pexp_loc) ~pos_before_cursor
                ~pos_after_comp_name:(Loc.end_ compName_loc)
                ~first_char_before_cursor_no_white ~char_at_cursor
            | None, _ -> None
          in
          (match jsx_completable with
          | Some _ as res -> set_result_opt res
          | None -> ());
          if
            jsx_completable = None
            && compName_loc |> Loc.has_pos ~pos:pos_before_cursor
          then
            set_result
              (match comp_name_path with
              | [prefix] when Char.lowercase_ascii prefix.[0] = prefix.[0] ->
                ChtmlElement {prefix}
              | _ ->
                Cpath
                  (CPId
                     {
                       loc = compName_loc;
                       path = comp_name_path;
                       completion_context = Module;
                     }))
          else
            match jsx_props_opt with
            | Some jsx_props -> iterate_jsx_props ~iterator jsx_props
            | None -> ())
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
              args =
                [
                  (_, lhs);
                  (_, {pexp_desc = Pexp_ident {txt = Longident.Lident id; loc}});
                ];
            }
          when loc |> Loc.has_pos ~pos:pos_before_cursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->id";
          set_pipe_result ~lhs ~id |> ignore
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"; loc = op_loc}};
              args = [(_, lhs); _];
            }
          when Loc.end_ op_loc = pos_cursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->";
          set_pipe_result ~lhs ~id:"" |> ignore
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
              args = [_; (_, {pexp_desc = Pexp_apply {funct = fun_expr; args}})];
            }
          when (* Normally named arg completion fires when the cursor is right after the expression.
                  E.g in foo(~<---there
                  But it should not fire in foo(~a)<---there *)
               not
                 (Loc.end_ expr.pexp_loc = pos_cursor
                 && char_before_cursor = Some ')') -> (
          (* Complete fn argument values and named args when the fn call is piped. E.g. someVar->someFn(<com>). *)
          if Debug.verbose () then
            print_endline "[expr_iter] Complete fn arguments (piped)";
          let args = extract_exp_apply_args ~args in
          let fun_ctx_path =
            expr_to_context_path ~in_jsx_context:!in_jsx_context fun_expr
          in
          let arg_completable =
            match fun_ctx_path with
            | Some context_path ->
              find_arg_completables ~context_path ~args
                ~end_pos:(Loc.end_ expr.pexp_loc) ~pos_before_cursor
                ~pos_after_fun_expr:(Loc.end_ fun_expr.pexp_loc)
                ~char_before_cursor ~is_piped_expr:true
                ~first_char_before_cursor_no_white
            | None -> None
          in
          match arg_completable with
          | None -> (
            match fun_ctx_path with
            | None -> ()
            | Some fun_ctx_path ->
              let old_ctx_path = !current_ctx_path in
              set_current_ctx_path fun_ctx_path;
              arg_completable |> iterate_fn_arguments ~is_pipe:true ~args ~iterator;
              reset_current_ctx_path old_ctx_path)
          | Some arg_completable -> set_result arg_completable)
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
              args = [_; _];
            } ->
          (* Ignore any other pipe. *)
          ()
        | Pexp_apply {funct = fun_expr; args}
          when not
                 (Loc.end_ expr.pexp_loc = pos_cursor
                 && char_before_cursor = Some ')') -> (
          (* Complete fn argument values and named args when the fn call is _not_ piped. E.g. someFn(<com>). *)
          if Debug.verbose () then
            print_endline "[expr_iter] Complete fn arguments (not piped)";
          let args = extract_exp_apply_args ~args in
          if debug then
            Printf.printf "Pexp_apply ...%s (%s)\n"
              (Loc.to_string fun_expr.pexp_loc)
              (args
              |> List.map (fun {label; exp} ->
                     Printf.sprintf "%s...%s"
                       (match label with
                       | None -> ""
                       | Some {name; opt; pos_start; pos_end} ->
                         "~" ^ name ^ Pos.to_string pos_start ^ "->"
                         ^ Pos.to_string pos_end ^ "="
                         ^ if opt then "?" else "")
                       (Loc.to_string exp.pexp_loc))
              |> String.concat ", ");

          let fun_ctx_path =
            expr_to_context_path ~in_jsx_context:!in_jsx_context fun_expr
          in
          let arg_completable =
            match fun_ctx_path with
            | Some context_path ->
              find_arg_completables ~context_path ~args
                ~end_pos:(Loc.end_ expr.pexp_loc) ~pos_before_cursor
                ~pos_after_fun_expr:(Loc.end_ fun_expr.pexp_loc)
                ~char_before_cursor ~is_piped_expr:false
                ~first_char_before_cursor_no_white
            | None -> None
          in
          match arg_completable with
          | None -> (
            match fun_ctx_path with
            | None -> ()
            | Some fun_ctx_path ->
              let old_ctx_path = !current_ctx_path in
              set_current_ctx_path fun_ctx_path;
              arg_completable |> iterate_fn_arguments ~is_pipe:false ~args ~iterator;
              reset_current_ctx_path old_ctx_path)
          | Some arg_completable -> set_result arg_completable)
        | Pexp_send (lhs, {txt; loc}) -> (
          (* e["txt"]
             If the string for txt is not closed, it could go over several lines.
             Only take the first like to represent the label *)
          let txt_lines = txt |> String.split_on_char '\n' in
          let label = List.hd txt_lines in
          let label =
            if label <> "" && label.[String.length label - 1] = '\r' then
              String.sub label 0 (String.length label - 1)
            else label
          in
          let label_range =
            let l, c = Loc.start loc in
            ((l, c + 1), (l, c + 1 + String.length label))
          in
          if debug then
            Printf.printf "Pexp_send %s%s e:%s\n" label
              (Range.to_string label_range)
              (Loc.to_string lhs.pexp_loc);
          if
            label_range |> Range.has_pos ~pos:pos_before_cursor
            || (label = "" && pos_cursor = fst label_range)
          then
            match expr_to_context_path ~in_jsx_context:!in_jsx_context lhs with
            | Some context_path -> set_result (Cpath (CPObj (context_path, label)))
            | None -> ())
        | Pexp_fun
            {arg_label = lbl; default = default_exp_opt; lhs = pat; rhs = e} ->
          let old_scope = !scope in
          (match (!processing_fun, !current_ctx_path) with
          | None, Some ctx_path -> processing_fun := Some (ctx_path, 0)
          | _ -> ());
          let arg_context_path =
            match !processing_fun with
            | None -> None
            | Some (ctx_path, current_unlabelled_count) ->
              (processing_fun :=
                 match lbl with
                 | Nolabel -> Some (ctx_path, current_unlabelled_count + 1)
                 | _ -> Some (ctx_path, current_unlabelled_count));
              if Debug.verbose () then
                print_endline "[expr_iter] Completing for argument value";
              Some
                (Completable.CArgument
                   {
                     function_context_path = ctx_path;
                     argument_label =
                       (match lbl with
                       | Nolabel ->
                         Unlabelled {argument_position = current_unlabelled_count}
                       | Optional {txt = name} -> Optional name
                       | Labelled {txt = name} -> Labelled name);
                   })
          in
          (match default_exp_opt with
          | None -> ()
          | Some default_exp -> iterator.expr iterator default_exp);
          if loc_has_cursor e.pexp_loc = false then
            complete_pattern ?context_path:arg_context_path pat;
          scope_pattern ?context_path:arg_context_path pat;
          iterator.pat iterator pat;
          iterator.expr iterator e;
          scope := old_scope;
          processed := true
        | Pexp_let (rec_flag, bindings, e) ->
          let old_scope = !scope in
          if rec_flag = Recursive then bindings |> List.iter scope_value_binding;
          bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
          if rec_flag = Nonrecursive then bindings |> List.iter scope_value_binding;
          iterator.expr iterator e;
          scope := old_scope;
          processed := true
        | Pexp_letmodule (name, mod_expr, mod_body) ->
          let old_scope = !scope in
          iterator.location iterator name.loc;
          iterator.module_expr iterator mod_expr;
          scope := !scope |> Scope.add_module ~name:name.txt ~loc:name.loc;
          iterator.expr iterator mod_body;
          scope := old_scope;
          processed := true
        | Pexp_open (_, lid, e) ->
          let old_scope = !scope in
          iterator.location iterator lid.loc;
          scope := !scope |> Scope.add_open ~lid:lid.txt;
          iterator.expr iterator e;
          scope := old_scope;
          processed := true
        | _ -> ());
      if not !processed then Ast_iterator.default_iterator.expr iterator expr;
      in_jsx_context := old_in_jsx_context;
      match expr.pexp_desc with
      | Pexp_fun _ -> ()
      | _ -> processing_fun := None)
  in
  let typ (iterator : Ast_iterator.iterator) (core_type : Parsetree.core_type) =
    if core_type.ptyp_loc |> Loc.has_pos ~pos:pos_no_white then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found type:%s\n"
          (Pos.to_string pos_cursor) (Pos.to_string pos_no_white)
          (Loc.to_string core_type.ptyp_loc);
      match core_type.ptyp_desc with
      | Ptyp_constr (lid, _args) ->
        let lid_path = flatten_lid_check_dot lid in
        if debug then
          Printf.printf "Ptyp_constr %s:%s\n"
            (lid_path |> String.concat ".")
            (Loc.to_string lid.loc);
        if lid.loc |> Loc.has_pos ~pos:pos_before_cursor then
          set_result
            (Cpath
               (CPId {loc = lid.loc; path = lid_path; completion_context = Type}))
      | _ -> ());
    Ast_iterator.default_iterator.typ iterator core_type
  in
  let pat (iterator : Ast_iterator.iterator) (pat : Parsetree.pattern) =
    if pat.ppat_loc |> Loc.has_pos ~pos:pos_no_white then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found pattern:%s\n"
          (Pos.to_string pos_cursor) (Pos.to_string pos_no_white)
          (Loc.to_string pat.ppat_loc);
      (match pat.ppat_desc with
      | Ppat_construct (lid, _) -> (
        let lid_path = flatten_lid_check_dot lid in
        if debug then
          Printf.printf "Ppat_construct %s:%s\n"
            (lid_path |> String.concat ".")
            (Loc.to_string lid.loc);
        let completion =
          Completable.Cpath
            (CPId {loc = lid.loc; path = lid_path; completion_context = Value})
        in
        match !result with
        | Some (Completable.Cpattern p, scope) ->
          result := Some (Cpattern {p with fallback = Some completion}, scope)
        | _ -> set_result completion)
      | _ -> ());
      Ast_iterator.default_iterator.pat iterator pat)
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    let processed = ref false in
    (match me.pmod_desc with
    | Pmod_ident lid when lid.loc |> Loc.has_pos ~pos:pos_before_cursor ->
      let lid_path = flatten_lid_check_dot lid in
      if debug then
        Printf.printf "Pmod_ident %s:%s\n"
          (lid_path |> String.concat ".")
          (Loc.to_string lid.loc);
      found := true;
      set_result
        (Cpath
           (CPId {loc = lid.loc; path = lid_path; completion_context = Module}))
    | Pmod_functor (name, maybe_type, body) ->
      let old_scope = !scope in
      scope := !scope |> Scope.add_module ~name:name.txt ~loc:name.loc;
      (match maybe_type with
      | None -> ()
      | Some mt -> iterator.module_type iterator mt);
      iterator.module_expr iterator body;
      scope := old_scope;
      processed := true
    | _ -> ());
    if not !processed then Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_type (iterator : Ast_iterator.iterator)
      (mt : Parsetree.module_type) =
    (match mt.pmty_desc with
    | Pmty_ident lid when lid.loc |> Loc.has_pos ~pos:pos_before_cursor ->
      let lid_path = flatten_lid_check_dot lid in
      if debug then
        Printf.printf "Pmty_ident %s:%s\n"
          (lid_path |> String.concat ".")
          (Loc.to_string lid.loc);
      found := true;
      set_result
        (Cpath
           (CPId {loc = lid.loc; path = lid_path; completion_context = Module}))
    | _ -> ());
    Ast_iterator.default_iterator.module_type iterator mt
  in
  let type_kind (iterator : Ast_iterator.iterator)
      (type_kind : Parsetree.type_kind) =
    (match type_kind with
    | Ptype_variant [decl]
      when decl.pcd_name.loc |> Loc.has_pos ~pos:pos_no_white
           && decl.pcd_args = Pcstr_tuple [] ->
      (* "type t = Pre" could signal the intent to complete variant "Prelude",
         or the beginning of "Prefix.t" *)
      if debug then
        Printf.printf "Ptype_variant unary %s:%s\n" decl.pcd_name.txt
          (Loc.to_string decl.pcd_name.loc);
      found := true;
      set_result
        (Cpath
           (CPId
              {
                loc = decl.pcd_name.loc;
                path = [decl.pcd_name.txt];
                completion_context = Value;
              }))
    | _ -> ());
    Ast_iterator.default_iterator.type_kind iterator type_kind
  in

  let last_scope_before_cursor = ref (Scope.create ()) in
  let location (_iterator : Ast_iterator.iterator) (loc : Location.t) =
    if Loc.end_ loc <= pos_cursor then last_scope_before_cursor := !scope
  in

  let iterator =
    {
      Ast_iterator.default_iterator with
      attribute;
      expr;
      location;
      module_expr;
      module_type;
      pat;
      signature;
      signature_item;
      structure;
      structure_item;
      typ;
      type_kind;
      value_binding;
    }
  in

  if kind_file = Files.Res then (
    let parser =
      Res_driver.parsing_engine.parse_implementation_from_source
        ~for_printer:false
    in
    let {Res_driver.parsetree = str} = parser ~source:text in
    iterator.structure iterator str |> ignore;
    if blank_after_cursor = Some ' ' || blank_after_cursor = Some '\n' then (
      scope := !last_scope_before_cursor;
      set_result
        (Cpath
           (CPId {loc = Location.none; path = [""]; completion_context = Value})));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if kind_file = Resi then (
    let parser =
      Res_driver.parsing_engine.parse_interface_from_source ~for_printer:false
    in
    let {Res_driver.parsetree = signature} = parser ~source:text in
    iterator.signature iterator signature |> ignore;
    if blank_after_cursor = Some ' ' || blank_after_cursor = Some '\n' then (
      scope := !last_scope_before_cursor;
      set_result
        (Cpath
           (CPId {loc = Location.none; path = [""]; completion_context = Type})));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None

let completion_with_parser ~debug ~source ~kind_file ~pos_cursor =
  match Pos.position_to_offset source pos_cursor with
  | Some offset ->
    completion_with_parser1 ~debug ~offset ~pos_cursor ~kind_file source
  | None -> None

let find_type_of_expression_at_loc ~debug ~pos_cursor ~source ~kind_file loc =
  match source with
  | "" -> None
  | source -> (
    match Pos.position_to_offset source pos_cursor with
    | Some offset ->
      completion_with_parser1 ~find_this_expr_loc:loc ~debug ~offset ~pos_cursor
        ~kind_file source
    | None -> None)
