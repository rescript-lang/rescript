open Dead_common

let active () = true

let add_function_reference ~config ~decls ~cross_file ~(loc_from : Location.t)
    ~(loc_to : Location.t) =
  if active () then
    let pos_to = loc_to.loc_start in
    let pos_from = loc_from.loc_start in
    let has_optional_arg_state pos =
      match Declarations.find_opt_builder decls pos with
      | Some {decl_kind = Value {optional_args}} ->
        not (Optional_args.is_empty optional_args)
      | _ -> false
    in
    let should_add =
      if
        pos_to.pos_fname <> pos_from.pos_fname
        && (file_is_implementation_of pos_to.pos_fname pos_from.pos_fname
           || file_is_implementation_of pos_from.pos_fname pos_to.pos_fname)
      then has_optional_arg_state pos_to
      else has_optional_arg_state pos_from && has_optional_arg_state pos_to
    in
    if should_add then (
      if config.Dce_config.cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (pos_from |> Pos.to_string)
          (pos_to |> Pos.to_string);
      Cross_file_items.add_function_reference cross_file ~pos_from ~pos_to)

let rec has_optional_args (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> false
  | Tarrow ({lbl = Optional _}, _tTo, _, _) -> true
  | Tarrow (_, t_to, _, _) -> has_optional_args t_to
  | Tlink t -> has_optional_args t
  | Tsubst t -> has_optional_args t
  | _ -> false

let rec from_type_expr (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> []
  | Tarrow ({lbl = Optional {txt = s}}, t_to, _, _) -> s :: from_type_expr t_to
  | Tarrow (_, t_to, _, _) -> from_type_expr t_to
  | Tlink t -> from_type_expr t
  | Tsubst t -> from_type_expr t
  | _ -> []

let rec from_type_expr_with_arity (texpr : Types.type_expr) arity =
  if arity <= 0 then []
  else
    match texpr.desc with
    | _ when not (active ()) -> []
    | Tarrow ({lbl = Optional {txt = s}}, t_to, _, _) ->
      s :: from_type_expr_with_arity t_to (arity - 1)
    | Tarrow (_, t_to, _, _) -> from_type_expr_with_arity t_to (arity - 1)
    | Tlink t -> from_type_expr_with_arity t arity
    | Tsubst t -> from_type_expr_with_arity t arity
    | _ -> []

let add_references ~config ~cross_file ~(loc_from : Location.t)
    ~(loc_to : Location.t) ~(binding : Location.t) ~path
    (arg_names, arg_names_maybe) =
  if active () then (
    let pos_to = loc_to.loc_start in
    let pos_from = binding.loc_start in
    Cross_file_items.add_optional_arg_call cross_file ~pos_from ~pos_to
      ~arg_names ~arg_names_maybe;
    if config.Dce_config.cli.debug then
      let call_pos = loc_from.loc_start in
      Log_.item
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
         argNamesMaybe:%s %s@."
        (path |> Dce_path.from_path_t |> Dce_path.to_string)
        (arg_names |> String.concat ", ")
        (arg_names_maybe |> String.concat ", ")
        (call_pos |> Pos.to_string))

(** Check for optional args issues. Returns issues instead of logging.
    Uses optional_args_state map for final computed state. *)
let check ~optional_args_state ~direct_optional_arg_calls ~ann_store ~config:_
    decl : Issue.t list =
  let should_report_optional_args =
    let open Decl.Kind in
    match decl.Decl.decl_kind with
    | Value {optional_args_report = ReportOptionalArgs} -> true
    | Value {optional_args_report = ReportOptionalArgsIfDirectCall} ->
      Pos_set.mem decl.pos direct_optional_arg_calls
    | _ -> false
  in
  match decl with
  | {Decl.decl_kind = Value {optional_args}}
    when active () && should_report_optional_args
         && not
              (Annotation_store.is_annotated_gentype_or_live ann_store decl.pos)
    ->
    (* Look up computed state from map, fall back to declaration's initial state *)
    let state =
      match Optional_args_state.find_opt optional_args_state decl.pos with
      | Some s -> s
      | None -> optional_args
    in
    let loc = decl |> decl_get_loc in
    let unused_issues =
      Optional_args.fold_unused
        (fun s acc ->
          let issue : Issue.t =
            {
              name = "Warning Unused Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    dead_optional = WarningUnusedArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is never used"
                        s
                        (decl.path |> Dce_path.without_head);
                  };
            }
          in
          issue :: acc)
        state []
    in
    let redundant_issues =
      Optional_args.fold_always_used
        (fun s n_calls acc ->
          let issue : Issue.t =
            {
              name = "Warning Redundant Optional Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    dead_optional = WarningRedundantOptionalArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is always supplied (%d calls)"
                        s
                        (decl.path |> Dce_path.without_head)
                        n_calls;
                  };
            }
          in
          issue :: acc)
        state []
    in
    (* Reverse to maintain original order from iterUnused/iterAlwaysUsed *)
    List.rev unused_issues @ List.rev redundant_issues
  | _ -> []
