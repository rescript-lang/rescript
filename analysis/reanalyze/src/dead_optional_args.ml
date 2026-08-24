open Dead_common

let active () = true

let rec has_optional_args (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> false
  | Tarrow (params, _) ->
    params
    |> List.exists (fun ({lbl} : Types.arg) ->
           match lbl with
           | Optional _ -> true
           | _ -> false)
  | Tlink t -> has_optional_args t
  | Tsubst t -> has_optional_args t
  | _ -> false

let add_function_reference ~config ~cross_file ~(loc_from : Location.t)
    ~(loc_to : Location.t) ~type_from ~type_to =
  (* Keep this filter type-based rather than consulting the declaration table.
     References can be collected before their target declaration is visited,
     notably in [let rec ... and ...] groups. *)
  if has_optional_args type_from && has_optional_args type_to then (
    let pos_to = loc_to.loc_start in
    let pos_from = loc_from.loc_start in
    if config.Dce_config.cli.debug then
      Log_.item "OptionalArgs.addFunctionReference %s %s@."
        (pos_from |> Pos.to_string)
        (pos_to |> Pos.to_string);
    Cross_file_items.add_function_reference cross_file ~pos_from ~pos_to)

(* The function boundary is structural: a function's optional arguments are
   exactly the optional parameters of its (one) arrow node. *)
let rec from_type_expr (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> []
  | Tarrow (params, _) ->
    params
    |> List.filter_map (fun ({lbl} : Types.arg) ->
           match lbl with
           | Optional {txt = s} -> Some s
           | _ -> None)
  | Tlink t -> from_type_expr t
  | Tsubst t -> from_type_expr t
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
let check ~optional_args_state ~optional_arg_value_escapes ~ann_store ~config:_
    decl : Issue.t list =
  match decl with
  | {Decl.decl_kind = Value {reports_optional_args = true; optional_args}}
    when active ()
         (* A live escape makes both diagnostic classes unknown: unseen callers
            may either omit or supply an optional argument, so neither "never
            used" nor "always supplied" remains trustworthy. *)
         && (not (Pos_set.mem decl.pos optional_arg_value_escapes))
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
