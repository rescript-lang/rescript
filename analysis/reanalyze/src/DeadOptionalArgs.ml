open DeadCommon

let active () = true

let add_function_reference ~config ~decls ~cross_file ~(loc_from : Location.t)
    ~(loc_to : Location.t) =
  if active () then
    let pos_to = loc_to.loc_start in
    let pos_from = loc_from.loc_start in
    (* Check if target has optional args - for filtering and debug logging *)
    let should_add =
      match Declarations.find_opt_builder decls pos_to with
      | Some {decl_kind = Value {optional_args}} ->
        not (OptionalArgs.is_empty optional_args)
      | _ -> false
    in
    if should_add then (
      if config.DceConfig.cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (pos_from |> Pos.to_string) (pos_to |> Pos.to_string);
      CrossFileItems.add_function_reference cross_file ~pos_from:pos_from
        ~pos_to:pos_to)

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

let add_references ~config ~cross_file ~(loc_from : Location.t)
    ~(loc_to : Location.t) ~(binding : Location.t) ~path (arg_names, arg_names_maybe)
    =
  if active () then (
    let pos_to = loc_to.loc_start in
    let pos_from = binding.loc_start in
    CrossFileItems.add_optional_arg_call cross_file ~pos_from:pos_from
      ~pos_to:pos_to ~arg_names:arg_names ~arg_names_maybe:arg_names_maybe;
    if config.DceConfig.cli.debug then
      let call_pos = loc_from.loc_start in
      Log_.item
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
         argNamesMaybe:%s %s@."
        (path |> DcePath.from_path_t |> DcePath.to_string)
        (arg_names |> String.concat ", ")
        (arg_names_maybe |> String.concat ", ")
        (call_pos |> Pos.to_string))

(** Check for optional args issues. Returns issues instead of logging.
    Uses optional_args_state map for final computed state. *)
let check ~optional_args_state ~ann_store ~config:_ decl : Issue.t list =
  match decl with
  | {Decl.decl_kind = Value {optional_args}}
    when active ()
         && not
              (AnnotationStore.is_annotated_gentype_or_live ann_store decl.pos)
    ->
    (* Look up computed state from map, fall back to declaration's initial state *)
    let state =
      match OptionalArgsState.find_opt optional_args_state decl.pos with
      | Some s -> s
      | None -> optional_args
    in
    let loc = decl |> decl_get_loc in
    let unused_issues =
      OptionalArgs.fold_unused
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
                        (decl.path |> DcePath.without_head);
                  };
            }
          in
          issue :: acc)
        state []
    in
    let redundant_issues =
      OptionalArgs.fold_always_used
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
                        (decl.path |> DcePath.without_head)
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
