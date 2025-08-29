open Analysis

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module IntSet = Set.Make (Int)

(* Public API: migrate ~entryPointFile ~outputMode *)

module InsertExt = struct
  type placeholder = Labelled of string | Unlabelled of int

  let ext_labelled = "insert.labelledArgument"
  let ext_unlabelled = "insert.unlabelledArgument"

  (*
     Unlabelled argument placeholders use 0-based indexing.
     Pipe semantics: the pipe LHS occupies index 0 when resolving placeholders
     in piped templates. For inner calls that exclude the LHS (e.g. `lhs->f(x)`),
     we adjust drop positions at the call site to keep the generated call correct.
  *)
  let placeholder_of_expr = function
    | {
        Parsetree.pexp_desc =
          Pexp_extension
            ( {txt},
              PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant c}, _)}]
            );
      } ->
      if txt = ext_labelled then
        match c with
        | Pconst_string (name, _) -> Some (Labelled name)
        | _ -> None
      else if txt = ext_unlabelled then
        match c with
        | Pconst_integer (s, _) -> (
          match int_of_string_opt s with
          | Some i -> Some (Unlabelled i)
          | None -> None)
        | _ -> None
      else None
    | _ -> None
end

module ArgUtils = struct
  let map_expr_args mapper args =
    args
    |> List.map (fun (label, arg) -> (label, mapper.Ast_mapper.expr mapper arg))
end

module ExprUtils = struct
  let rec is_pipe_apply (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_apply {funct = {pexp_desc = Pexp_ident {txt = Lident "->"}}; _} ->
      true
    | Pexp_construct (_, Some e)
    | Pexp_constraint (e, _)
    | Pexp_coerce (e, _, _)
    | Pexp_let (_, _, e)
    | Pexp_sequence (e, _)
    | Pexp_letmodule (_, _, e)
    | Pexp_open (_, _, e) ->
      is_pipe_apply e
    | _ -> false
end

module MapperUtils = struct
  (* Collect placeholder usages anywhere inside an expression. *)
  let collect_placeholders (expr : Parsetree.expression) =
    let labelled = ref StringSet.empty in
    let unlabelled = ref IntSet.empty in
    let open Ast_iterator in
    let iter =
      {
        default_iterator with
        expr =
          (fun self e ->
            (match InsertExt.placeholder_of_expr e with
            | Some (InsertExt.Labelled name) ->
              labelled := StringSet.add name !labelled
            | Some (InsertExt.Unlabelled i) when i >= 0 ->
              unlabelled := IntSet.add i !unlabelled
            | _ -> ());
            default_iterator.expr self e);
      }
    in
    iter.expr iter expr;
    (!labelled, !unlabelled)

  (* Replace placeholders anywhere inside an expression using the given
     source arguments. *)
  let replace_placeholders_in_expr (expr : Parsetree.expression)
      (source_args : (Asttypes.arg_label * Parsetree.expression) list) =
    let labelled = Hashtbl.create 8 in
    let unlabelled = Hashtbl.create 8 in
    let idx = ref 0 in
    source_args
    |> List.iter (fun (lbl, arg) ->
           match lbl with
           | Asttypes.Nolabel ->
             Hashtbl.replace unlabelled !idx arg;
             incr idx
           | Asttypes.Labelled {txt} | Optional {txt} ->
             Hashtbl.replace labelled txt arg);
    let find = function
      | `Labelled name -> Hashtbl.find_opt labelled name
      | `Unlabelled i -> Hashtbl.find_opt unlabelled i
    in
    let mapper =
      {
        Ast_mapper.default_mapper with
        expr =
          (fun mapper exp ->
            match InsertExt.placeholder_of_expr exp with
            | Some (InsertExt.Labelled name) -> (
              match find (`Labelled name) with
              | Some arg -> arg
              | None -> exp)
            | Some (InsertExt.Unlabelled i) -> (
              match find (`Unlabelled i) with
              | Some arg -> arg
              | None -> exp)
            | None -> Ast_mapper.default_mapper.expr mapper exp);
      }
    in
    mapper.expr mapper expr

  let build_labelled_args_map template_args =
    template_args
    |> List.filter_map (fun (label, arg) ->
           match (label, InsertExt.placeholder_of_expr arg) with
           | ( (Asttypes.Labelled {txt = label} | Optional {txt = label}),
               Some (InsertExt.Labelled arg_name) ) ->
             Some (arg_name, label)
           | _ -> None)
    |> List.fold_left (fun map (k, v) -> StringMap.add k v map) StringMap.empty

  (*
     Pure computation of which template args to insert and which source args
     are consumed by placeholders.

     Indexing is 0-based everywhere.
     For piped application, the pipe LHS occupies index 0 in the source list
     used for placeholder resolution. If the inner call excludes the LHS
     (e.g. `lhs -> f(args)`), adjust drop positions accordingly at the call site.

     Returns:
     - template_args_to_insert: args to append to the final call
     - labelled_will_be_mapped: names of labelled source args that are consumed
     - unlabelled_positions_to_insert: 0-based indices of unlabelled source args to drop
  *)
  let get_template_args_to_insert mapper template_args source_args =
    let is_unit_expr (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_construct ({txt = Lident "()"}, None) -> true
      | _ -> false
    in

    (* For each template argument, decide whether it is a placeholder that
       should be substituted from the source call, or a concrete argument which
       should be preserved (after mapping through the mapper).
       Accumulator:
       - rev_args: arguments to append to the final call (in reverse order)
       - used_labelled: names of labelled args consumed from the source call
       - used_unlabelled: 0-based positions of unlabelled args consumed. *)
    let accumulate_template_arg (rev_args, used_labelled, used_unlabelled)
        (label, arg) =
      (* Always perform nested replacement inside the argument expression,
         and collect which placeholders were used so we can drop them from the
         original call's arguments. *)
      let labelled_used_here, unlabelled_used_here = collect_placeholders arg in
      let arg_replaced = replace_placeholders_in_expr arg source_args in
      if is_unit_expr arg_replaced then
        (rev_args, used_labelled, used_unlabelled)
      else
        ( (label, mapper.Ast_mapper.expr mapper arg_replaced) :: rev_args,
          StringSet.union used_labelled labelled_used_here,
          IntSet.union used_unlabelled unlabelled_used_here )
    in
    let rev_args, labelled_set, unlabelled_set =
      List.fold_left accumulate_template_arg
        ([], StringSet.empty, IntSet.empty)
        template_args
    in
    (List.rev rev_args, labelled_set, unlabelled_set)

  let drop_args source_args ~unlabelled_positions_to_insert ~will_be_mapped =
    let _, rev =
      List.fold_left
        (fun (idx, acc) (label, arg) ->
          match label with
          | Asttypes.Nolabel ->
            let drop = IntSet.mem idx unlabelled_positions_to_insert in
            let idx' = idx + 1 in
            if drop then (idx', acc) else (idx', (label, arg) :: acc)
          | Asttypes.Labelled {txt} | Optional {txt} ->
            if StringSet.mem txt will_be_mapped then (idx, acc)
            else (idx, (label, arg) :: acc))
        (0, []) source_args
    in
    List.rev rev

  let rename_labels source_args ~labelled_args_map =
    source_args
    |> List.map (fun (label, arg) ->
           match label with
           | Asttypes.Labelled ({loc; txt} as l) ->
             if StringMap.mem txt labelled_args_map then
               let mapped = StringMap.find txt labelled_args_map in
               (Asttypes.Labelled {loc; txt = mapped}, arg)
             else (Asttypes.Labelled l, arg)
           | Optional ({loc; txt} as l) ->
             if StringMap.mem txt labelled_args_map then
               let mapped = StringMap.find txt labelled_args_map in
               (Optional {loc; txt = mapped}, arg)
             else (Optional l, arg)
           | _ -> (label, arg))

  let apply_migration_template mapper template_args source_args =
    let labelled_args_map = build_labelled_args_map template_args in
    let template_args_to_insert, will_be_mapped, unlabelled_positions_to_insert
        =
      get_template_args_to_insert mapper template_args source_args
    in
    let dropped =
      drop_args source_args ~unlabelled_positions_to_insert ~will_be_mapped
    in
    (* Also drop any unit arguments that remain from the source call. *)
    let is_unit_expr (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_construct ({txt = Lident "()"}, None) -> true
      | _ -> false
    in
    let dropped =
      List.filter
        (fun (label, arg) ->
          match label with
          | Asttypes.Nolabel -> not (is_unit_expr arg)
          | _ -> true)
        dropped
    in
    let renamed = rename_labels dropped ~labelled_args_map in
    renamed @ template_args_to_insert

  let migrate_piped_args mapper ~template_args ~lhs ~pipe_args =
    let full_source_args = lhs :: pipe_args in
    let template_args_to_insert, will_be_mapped, unlabelled_positions_to_insert
        =
      get_template_args_to_insert mapper template_args full_source_args
    in
    let labelled_args_map = build_labelled_args_map template_args in
    let adjusted_unlabelled_to_drop =
      IntSet.fold
        (fun i acc -> if i > 0 then IntSet.add (i - 1) acc else acc)
        unlabelled_positions_to_insert IntSet.empty
    in
    let dropped =
      drop_args pipe_args
        ~unlabelled_positions_to_insert:adjusted_unlabelled_to_drop
        ~will_be_mapped
    in
    (* Drop any unit arguments that remain from the source call. *)
    let is_unit_expr (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_construct ({txt = Lident "()"}, None) -> true
      | _ -> false
    in
    let dropped =
      List.filter
        (fun (label, arg) ->
          match label with
          | Asttypes.Nolabel -> not (is_unit_expr arg)
          | _ -> true)
        dropped
    in
    let renamed = rename_labels dropped ~labelled_args_map in
    renamed @ template_args_to_insert
end

type args_ctx = {
  labelled: (string, Parsetree.expression) Hashtbl.t;
  unlabelled: (int, Parsetree.expression) Hashtbl.t;
}

let build_args_ctx args : args_ctx =
  let labelled = Hashtbl.create 8 in
  let unlabelled = Hashtbl.create 8 in
  let idx = ref 0 in
  args
  |> List.iter (fun (lbl, arg) ->
         match lbl with
         | Asttypes.Nolabel ->
           Hashtbl.replace unlabelled !idx arg;
           incr idx
         | Asttypes.Labelled {txt} | Optional {txt} ->
           Hashtbl.replace labelled txt arg);
  {labelled; unlabelled}

let find_in_args_ctx args_ctx
    (find_this : [`Labelled of string | `Unlabelled of int]) =
  match find_this with
  | `Labelled name -> Hashtbl.find_opt args_ctx.labelled name
  | `Unlabelled i -> Hashtbl.find_opt args_ctx.unlabelled i

let replace_from_args_ctx_in_expr expr args_ctx =
  let mapper =
    {
      Ast_mapper.default_mapper with
      expr =
        (fun mapper exp ->
          match InsertExt.placeholder_of_expr exp with
          | Some (InsertExt.Labelled name) -> (
            match find_in_args_ctx args_ctx (`Labelled name) with
            | Some arg -> arg
            | None -> exp)
          | Some (InsertExt.Unlabelled i) -> (
            match find_in_args_ctx args_ctx (`Unlabelled i) with
            | Some arg -> arg
            | None -> exp)
          | None -> Ast_mapper.default_mapper.expr mapper exp);
    }
  in
  mapper.expr mapper expr

let replace_from_args_in_expr expr source_args =
  replace_from_args_ctx_in_expr expr (build_args_ctx source_args)

let remap_needed_extensions (mapper : Ast_mapper.mapper)
    (ext : Parsetree.extension) : Parsetree.extension =
  match ext with
  | ({txt = "todo_"} as e), payload ->
    Ast_mapper.default_mapper.extension mapper ({e with txt = "todo"}, payload)
  | e -> Ast_mapper.default_mapper.extension mapper e

let migrate_reference_from_info (deprecated_info : Cmt_utils.deprecated_used)
    exp =
  match deprecated_info.migration_template with
  | Some e -> e
  | None -> exp

module Template = struct
  type t =
    | Apply of {
        funct: Parsetree.expression;
        args: (Asttypes.arg_label * Parsetree.expression) list;
        partial: bool;
        transformed_jsx: bool;
      }
    | Match of {expr: Parsetree.expression; cases: Parsetree.case list}

  let of_expr = function
    | {Parsetree.pexp_desc = Pexp_apply {funct; args; partial; transformed_jsx}}
      ->
      Some (Apply {funct; args; partial; transformed_jsx})
    | {Parsetree.pexp_desc = Pexp_match (expr, cases)} ->
      Some (Match {expr; cases})
    | _ -> None

  let mk_apply (exp : Parsetree.expression) ~funct ~args ~partial
      ~transformed_jsx =
    {exp with pexp_desc = Pexp_apply {funct; args; partial; transformed_jsx}}

  (* Apply a non-piped migration template to a direct call. *)
  let apply_direct ~mapper ~template ~call_args (exp : Parsetree.expression) =
    match template with
    | Match {expr; cases} ->
      {
        exp with
        pexp_desc = Pexp_match (replace_from_args_in_expr expr call_args, cases);
      }
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      let migrated_args =
        MapperUtils.apply_migration_template mapper template_args call_args
      in
      mk_apply exp ~funct:template_funct ~args:migrated_args ~partial
        ~transformed_jsx

  (* Apply a piped migration template. The `lhs` is the value being piped and
     `pipe_args` are the arguments in the right-hand call (if any). *)
  let apply_piped ~mapper ~template ~lhs ~pipe_args ~funct
      (exp : Parsetree.expression) =
    match template with
    | Match {expr; cases} ->
      {
        exp with
        pexp_desc = Pexp_match (replace_from_args_in_expr expr [lhs], cases);
      }
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      let pipe_args_mapped = ArgUtils.map_expr_args mapper pipe_args in
      let migrated_args =
        MapperUtils.migrate_piped_args mapper ~template_args ~lhs
          ~pipe_args:pipe_args_mapped
      in
      mk_apply exp ~funct
        ~args:
          [
            lhs;
            (Asttypes.Nolabel, Ast_helper.Exp.apply template_funct migrated_args);
          ]
        ~partial ~transformed_jsx

  (* Like apply_piped, but when there are no pipe args and the template has no
     arguments to insert, collapse to `funct lhs template_funct`. *)
  let apply_piped_maybe_empty ~mapper ~template ~lhs ~pipe_args ~funct
      (exp : Parsetree.expression) =
    match template with
    | Match _ -> apply_piped ~mapper ~template ~lhs ~pipe_args ~funct exp
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      if Ext_list.is_empty pipe_args then
        let template_args_to_insert, _, _ =
          MapperUtils.get_template_args_to_insert mapper template_args []
        in
        if Ext_list.is_empty template_args_to_insert then
          mk_apply exp ~funct
            ~args:[lhs; (Asttypes.Nolabel, template_funct)]
            ~partial ~transformed_jsx
        else
          mk_apply exp ~funct
            ~args:
              [
                lhs;
                ( Asttypes.Nolabel,
                  Ast_helper.Exp.apply template_funct template_args_to_insert );
              ]
            ~partial ~transformed_jsx
      else apply_piped ~mapper ~template ~lhs ~pipe_args ~funct exp

  (* Handle the special case of a single-step pipe where we are allowed to
     collapse the pipe into a direct call. *)
  let apply_single_pipe_collapse ~mapper ~template ~lhs_exp ~pipe_args
      (exp : Parsetree.expression) =
    match template with
    | Match {expr; cases} ->
      Ast_helper.Exp.match_
        (replace_from_args_in_expr expr
           ((Asttypes.Nolabel, lhs_exp)
           :: ArgUtils.map_expr_args mapper pipe_args))
        cases
    | Apply
        {
          funct = templ_f;
          args = templ_args;
          partial = tpartial;
          transformed_jsx = tjsx;
        } ->
      let pipe_args_mapped = ArgUtils.map_expr_args mapper pipe_args in
      let migrated_args =
        MapperUtils.apply_migration_template mapper templ_args
          ((Asttypes.Nolabel, lhs_exp) :: pipe_args_mapped)
      in
      mk_apply exp ~funct:templ_f ~args:migrated_args ~partial:tpartial
        ~transformed_jsx:tjsx
end

(* Apply a direct-call migration template to a call site. *)
let apply_template_direct mapper template_expr call_args exp =
  match Template.of_expr template_expr with
  | Some template -> Template.apply_direct ~mapper ~template ~call_args exp
  | None -> exp

(* Choose a template to use for piped forms, preferring a specific piped
   template when available and valid; otherwise fall back to the direct
   template. *)
let choose_template_for_piped (deprecated_info : Cmt_utils.deprecated_used) =
  match deprecated_info.migration_in_pipe_chain_template with
  | Some e -> (
    match Template.of_expr e with
    | Some t -> Some t
    | None -> None)
  | None -> (
    match deprecated_info.migration_template with
    | Some e2 -> (
      match Template.of_expr e2 with
      | Some t -> Some t
      | None -> None)
    | None -> None)

(* Apply migration for a single-step pipe if possible, else use the piped
   template. Mirrors the previous inline logic from the mapper. *)
let apply_single_step_or_piped ~mapper
    ~(deprecated_info : Cmt_utils.deprecated_used) ~lhs ~lhs_exp ~pipe_args
    ~funct exp =
  let is_single_pipe_step = not (ExprUtils.is_pipe_apply lhs_exp) in
  if
    is_single_pipe_step
    && Option.is_some deprecated_info.migration_in_pipe_chain_template
  then
    match deprecated_info.migration_template with
    | Some e -> (
      match Template.of_expr e with
      | Some t ->
        Template.apply_single_pipe_collapse ~mapper ~template:t ~lhs_exp
          ~pipe_args exp
      | None -> (
        match deprecated_info.migration_in_pipe_chain_template with
        | Some e2 -> (
          match Template.of_expr e2 with
          | Some t ->
            Template.apply_piped ~mapper ~template:t ~lhs ~pipe_args ~funct exp
          | None -> exp)
        | None -> exp))
    | None -> (
      match deprecated_info.migration_in_pipe_chain_template with
      | Some e2 -> (
        match Template.of_expr e2 with
        | Some t ->
          Template.apply_piped ~mapper ~template:t ~lhs ~pipe_args ~funct exp
        | None -> exp)
      | None -> exp)
  else
    match choose_template_for_piped deprecated_info with
    | Some t ->
      Template.apply_piped_maybe_empty ~mapper ~template:t ~lhs ~pipe_args
        ~funct exp
    | None -> exp

let makeMapper (deprecated_used : Cmt_utils.deprecated_used list) =
  let deprecated_function_calls =
    deprecated_used
    |> List.filter (fun (d : Cmt_utils.deprecated_used) ->
           match d.context with
           | Some FunctionCall -> true
           | _ -> false)
  in
  let loc_to_deprecated_fn_call =
    Hashtbl.create (List.length deprecated_function_calls)
  in
  deprecated_function_calls
  |> List.iter (fun ({Cmt_utils.source_loc} as d) ->
         Hashtbl.replace loc_to_deprecated_fn_call source_loc d);

  let deprecated_references =
    deprecated_used
    |> List.filter (fun (d : Cmt_utils.deprecated_used) ->
           match d.context with
           | Some Reference -> true
           | _ -> false)
  in
  let loc_to_deprecated_reference =
    Hashtbl.create (List.length deprecated_references)
  in
  deprecated_references
  |> List.iter (fun ({Cmt_utils.source_loc} as d) ->
         Hashtbl.replace loc_to_deprecated_reference source_loc d);

  let mapper =
    {
      Ast_mapper.default_mapper with
      extension = remap_needed_extensions;
      expr =
        (fun mapper exp ->
          match exp with
          | {pexp_desc = Pexp_ident {loc}}
            when Hashtbl.mem loc_to_deprecated_reference loc ->
            let deprecated_info =
              Hashtbl.find loc_to_deprecated_reference loc
            in
            migrate_reference_from_info deprecated_info exp
          | {
           pexp_desc =
             Pexp_apply {funct = {pexp_loc = fn_loc}; args = call_args};
          }
            when Hashtbl.mem loc_to_deprecated_fn_call fn_loc -> (
            let deprecated_info =
              Hashtbl.find loc_to_deprecated_fn_call fn_loc
            in
            let call_args = ArgUtils.map_expr_args mapper call_args in
            match deprecated_info.migration_template with
            | Some e -> apply_template_direct mapper e call_args exp
            | None -> exp)
          | {
           pexp_desc =
             Pexp_apply
               {
                 funct = {pexp_desc = Pexp_ident {txt = Lident "->"}} as funct;
                 args = (lhs_label, lhs_exp) :: (Nolabel, rhs) :: _;
               };
          } -> (
            let lhs_exp = mapper.expr mapper lhs_exp in
            let lhs = (lhs_label, lhs_exp) in
            let fn_loc_opt, pipe_args =
              match rhs with
              | {pexp_loc = fn_loc; pexp_desc = Pexp_ident _} ->
                (Some fn_loc, [])
              | {
               pexp_desc =
                 Pexp_apply
                   {
                     funct = {pexp_loc = fn_loc; pexp_desc = Pexp_ident _};
                     args = pipe_args;
                   };
              } ->
                (Some fn_loc, pipe_args)
              | _ -> (None, [])
            in
            match fn_loc_opt with
            | None -> Ast_mapper.default_mapper.expr mapper exp
            | Some fn_loc when Hashtbl.mem loc_to_deprecated_fn_call fn_loc ->
              let deprecated_info =
                Hashtbl.find loc_to_deprecated_fn_call fn_loc
              in
              apply_single_step_or_piped ~mapper ~deprecated_info ~lhs ~lhs_exp
                ~pipe_args ~funct exp
            | Some _ -> Ast_mapper.default_mapper.expr mapper exp)
          | _ -> Ast_mapper.default_mapper.expr mapper exp);
    }
  in
  mapper

let migrate ~entryPointFile ~outputMode =
  let path =
    match Filename.is_relative entryPointFile with
    | true -> Unix.realpath entryPointFile
    | false -> entryPointFile
  in
  let result =
    if Filename.check_suffix path ".res" then
      let parser =
        Res_driver.parsing_engine.parse_implementation ~for_printer:true
      in
      let {Res_driver.parsetree; comments; source} = parser ~filename:path in
      match Cmt.loadCmtInfosFromPath ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to run migration for %s because build artifacts \
              could not be found. try to build the project"
             path)
      | Some {cmt_extra_info = {deprecated_used}} ->
        let mapper = makeMapper deprecated_used in
        let astMapped = mapper.structure mapper parsetree in
        Ok
          ( Res_printer.print_implementation
              ~width:Res_printer.default_print_width astMapped ~comments,
            source )
    else if Filename.check_suffix path ".resi" then
      let parser =
        Res_driver.parsing_engine.parse_interface ~for_printer:true
      in
      let {Res_driver.parsetree = signature; comments; source} =
        parser ~filename:path
      in
      let mapper = makeMapper [] in
      let astMapped = mapper.signature mapper signature in
      Ok (Res_printer.print_interface astMapped ~comments, source)
    else
      Error
        (Printf.sprintf
           "File extension not supported. This command accepts .res and .resi \
            files")
  in
  match result with
  | Error e -> Error e
  | Ok (contents, source) when contents <> source -> (
    match outputMode with
    | `Stdout -> Ok contents
    | `File ->
      let oc = open_out path in
      Printf.fprintf oc "%s" contents;
      close_out oc;
      Ok (Filename.basename path ^ ": File migrated successfully"))
  | Ok (contents, _) -> (
    match outputMode with
    | `Stdout -> Ok contents
    | `File -> Ok (Filename.basename path ^ ": File did not need migration"))
