open DeadCommon

let active () = true

let addFunctionReference ~config ~decls ~cross_file ~(locFrom : Location.t)
    ~(locTo : Location.t) =
  if active () then
    let posTo = locTo.loc_start in
    let posFrom = locFrom.loc_start in
    (* Check if target has optional args - for filtering and debug logging *)
    let shouldAdd =
      match Declarations.find_opt_builder decls posTo with
      | Some {declKind = Value {optionalArgs}} ->
        not (OptionalArgs.isEmpty optionalArgs)
      | _ -> false
    in
    if shouldAdd then (
      if config.DceConfig.cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (posFrom |> Pos.toString) (posTo |> Pos.toString);
      CrossFileItems.add_function_reference cross_file ~pos_from:posFrom
        ~pos_to:posTo)

let rec hasOptionalArgs (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> false
  | Tarrow ({lbl = Optional _}, _tTo, _, _) -> true
  | Tarrow (_, tTo, _, _) -> hasOptionalArgs tTo
  | Tlink t -> hasOptionalArgs t
  | Tsubst t -> hasOptionalArgs t
  | _ -> false

let rec fromTypeExpr (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> []
  | Tarrow ({lbl = Optional {txt = s}}, tTo, _, _) -> s :: fromTypeExpr tTo
  | Tarrow (_, tTo, _, _) -> fromTypeExpr tTo
  | Tlink t -> fromTypeExpr t
  | Tsubst t -> fromTypeExpr t
  | _ -> []

let addReferences ~config ~cross_file ~(locFrom : Location.t)
    ~(locTo : Location.t) ~path (argNames, argNamesMaybe) =
  if active () then (
    let posTo = locTo.loc_start in
    let posFrom = locFrom.loc_start in
    CrossFileItems.add_optional_arg_call cross_file ~pos_to:posTo
      ~arg_names:argNames ~arg_names_maybe:argNamesMaybe;
    if config.DceConfig.cli.debug then
      Log_.item
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
         argNamesMaybe:%s %s@."
        (path |> DcePath.fromPathT |> DcePath.toString)
        (argNames |> String.concat ", ")
        (argNamesMaybe |> String.concat ", ")
        (posFrom |> Pos.toString))

(** Check for optional args issues. Returns issues instead of logging.
    Uses optional_args_state map for final computed state. *)
let check ~optional_args_state ~annotations ~config:_ decl : Issue.t list =
  match decl with
  | {Decl.declKind = Value {optionalArgs}}
    when active ()
         && not
              (FileAnnotations.is_annotated_gentype_or_live annotations decl.pos)
    ->
    (* Look up computed state from map, fall back to declaration's initial state *)
    let state =
      match OptionalArgsState.find_opt optional_args_state decl.pos with
      | Some s -> s
      | None -> optionalArgs
    in
    let loc = decl |> declGetLoc in
    let unused_issues =
      OptionalArgs.foldUnused
        (fun s acc ->
          let issue : Issue.t =
            {
              name = "Warning Unused Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    deadOptional = WarningUnusedArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is never used"
                        s
                        (decl.path |> DcePath.withoutHead);
                  };
            }
          in
          issue :: acc)
        state []
    in
    let redundant_issues =
      OptionalArgs.foldAlwaysUsed
        (fun s nCalls acc ->
          let issue : Issue.t =
            {
              name = "Warning Redundant Optional Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    deadOptional = WarningRedundantOptionalArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is always supplied (%d calls)"
                        s
                        (decl.path |> DcePath.withoutHead)
                        nCalls;
                  };
            }
          in
          issue :: acc)
        state []
    in
    (* Reverse to maintain original order from iterUnused/iterAlwaysUsed *)
    List.rev unused_issues @ List.rev redundant_issues
  | _ -> []
